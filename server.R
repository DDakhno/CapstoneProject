#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

rm(list=ls())
library(shiny)
library(shinyjs)
library(V8)
library(data.table)
library(dplyr)
library(stringi)
library(pryr)

source("functions.R")


punctmarks <- "[.!?]"
vectPM <- c(".","!","?")
punctmarkGroup <- "([.!?])"
wordsnogo <- c("the","a","an")

maxLinesOut <- 60
inWordLowLimit <- 3
maxWordLength <- 35  #Max. length of single word in Wordnet DB 33

db_dir <- "finalDB"

envData <- new.env()
tmpEnv <- new.env()

envData$dt_monograms <- NULL
envData$dt_bigrams <- NULL
envData$dt_trigrams <- NULL
envData$dt_four_grams <- NULL
envData$dt_pentagrams <- NULL

db_loaded <- data.table()


reloadDBs <- function(db_mono = 2, db_bi = 2, db_tri = 2, db_four = 2, db_penta = 2, db_scowl=1) {
    
    db_loaded <- "Waiting for reload data banks!"
    
    db_mono <- as.integer(db_mono)
    db_bi <- as.integer(db_bi)
    db_tri <- as.integer(db_tri)
    db_four <- as.integer(db_four)
    db_penta <- as.integer(db_penta)
    db_scowl <- as.integer(db_scowl)
    
    argv <- c(db_mono,db_bi, db_tri, db_four, db_penta, db_scowl)
    
    dt_names <- c("List used words", "One-word predictors", "Two-word predictors", "Three-word predictors", "Four-word predictors", "Reference dictionary")
    names(dt_names) <- c("db_mono", "db_bi", "db_tri", "db_four", "db_penta", "db_scowl")
    
    
    dbs <- grep("db_",dir(db_dir),value=T)
    
    dtabs <- c()
    sizes <- c()
    nrs <- c()
    
    prnte <- parent.env(parent.frame())
    
    for (i in seq_along(dt_names[1:length(dt_names)])) {
        dtt <- names(dt_names)[i]
        j <- argv[i]
        
        obj <- load(file.path(db_dir,grep(dtt,dbs,value=T)[j]))
        
        dtabs <- c(dtabs, dt_names[i])
        sizes <- c(sizes, round(object.size(get(obj))/1024/1024, 2))
        nrs <- c(nrs, nrow(get(obj)))
        
        if(dtt == "db_mono") { assign("dt_monograms", get(obj), pos = envData) }
        if(dtt == "db_bi") { assign("dt_bigrams", get(obj), pos = envData) }
        if(dtt == "db_tri") { assign("dt_trigrams", get(obj), pos = envData) }
        if(dtt == "db_four") { assign("dt_four_grams", get(obj), pos = envData) }
        if(dtt == "db_penta") { assign("dt_pentagrams", get(obj), pos = envData) }
        if(dtt == "db_scowl") { assign("dt_scowl", get(obj), pos = envData) }
    }
   
    dtabs <- c(dtabs,"Total")
    sizes <- c(sizes,sum(sizes))
    nrs <- c(nrs,"")
    
    
    db_loaded <- data.table("Databank"=dtabs,"Nr.rows"=nrs,"RAM used,Mbyte"=sizes)
    db_loaded
    
}

db_loaded <- reloadDBs()




removeWordsNogo <- function(lin,wng) {
    tmp <- strsplit(lin, split ="[[:blank:]]+" )[[1]]
    mask <- tmp %in% wng
    tmp <- tmp[!mask]
    x <- paste(tmp,split="",collapse=" ")
    x
}

toLog <- function(msg, pttrn = NULL) { if (!is.null(pttrn)) {
    tmpEnv$LOG <- c(tmpEnv$LOG,paste(msg,paste("\"",pttrn,"\"", sep=""),sep=" : "))
} else { 
    tmpEnv$LOG <- c(tmpEnv$LOG, msg) 
}
}

rtrn_sorted <- FALSE

## Analog to building the databases

###############################################
# Predefining variables and help functions
###############################################

precondString <- function(x) {
    
    ###############################################
    
    # Starting processing the line
    
    #Trimming blanks
    x <- gsub("[[:blank:]]+"," ",x)
    x <- gsub("(^[[:blank:]]+|[[:blank:]]+$)","",x)
    
    #Remove numbers
    x <- gsub("[0-9]","", x)
    
    x <- tolower(x)
    
    #Remove articles
    
    x <- removeWordsNogo(x,wordsnogo)
    
    x <- subsetUTFAnalogs(x)
    
    x <-treatPunctuations(x)
    
    x <- removeNotLatinWords(x)
    
    #Trimming blanks
    x <- gsub("[[:blank:]]+"," ",x)
    x <- gsub("(^[[:blank:]]+|[[:blank:]]+$)","",x)
    
    ## Trim to max words (leave the last max_words)
    ## Through treatPunctuations the last "word" is always " . "
    ## So, we take the last max_words+1 and trim the last one
    tmp <- unlist(strsplit(x = x, split = " ", fixed = T))
    if (length(tmp) > max_words ) {
        tmp <- tmp[((length(tmp)-max_words)+1):length(tmp)-1]
        x <- paste(tmp, sep = "", collapse=" ")
    }
    
    xpr <- x
    
    xpr
}

emptyDT <- data.table(predictor = "predictor", outcome = "outcome", freq = 1)%>%slice(0) #as default an empty data frame (nothing found)

askDB <- function(xx) {
    rtrn <- emptyDT
    lgth <- length(strsplit(xx, split = "[[:blank:]]")[[1]])
    toLog("......Expression",xx)
    toLog(paste("......Exact search in the predictor database with",lgth,"words"))
    if (!is.null(envData$dt_bigrams) & lgth == 1) rtrn <- envData$dt_bigrams[xx]
    if (!is.null(envData$dt_trigrams) & lgth == 2) rtrn <- envData$dt_trigrams[xx]
    if (!is.null(envData$dt_four_grams) & lgth == 3) rtrn <- envData$dt_four_grams[xx]
    if (!is.null(envData$dt_pentagrams) & lgth == 4) rtrn <- envData$dt_pentagrams[xx]
    na.omit(rtrn)
}



inWordPredictEngine <- function(xpr) {
    
    toLog("..In-word prediction started.")
    rtrn <- emptyDT
    
    ## Use case #1 - a word is in process of typing, so we could probably recognize it
    
    prevWords <- predPrev <-  NULL
    if (grepl(" ",xpr)) {
        prevWords <- gsub(" [^ ]+$","",xpr)
        lastWd <- gsub("^.* ","",xpr)
    } else {
        lastWd <- xpr
    }
    
    # Propositions for words at least one letter longer than the last (incomplete?) word
    toLog("....Searching for words starting with",lastWd)
    rtrn <- as.data.table(envData$dt_monograms %>% filter(outcome %like% paste("^",lastWd,".", sep = ""))%>%arrange(desc(freq)))
    
    if(nrow(rtrn) > 0) {
        toLog(paste("....Found",nrow(rtrn),"suggestions."))
        rtrn_sorted <- TRUE
    } else {
        toLog(paste(".....No suggestions found."))
    }
    
    
    
    # More words than one in the line
    # Preceding expression counts - the possible variants are pushed to top!
    
    if (! is.null(prevWords) && nrow(rtrn) > 0) { 
        
        toLog("....More than one word in search expression. Preceding expression counts, possible variants are pushed to top.")
        
        allcand <- rtrn$outcome
        predPrev <- as.data.table(makePredictions(prevWords, iwp = FALSE))
        if(nrow(predPrev) > 0) {
            favorits <- (predPrev%>%filter(outcome %like% paste("^",lastWd,".",sep="")))$outcome
        }
        if (length(favorits) > 0) {
            tmp <- data.table(rbind(predPrev[outcome %in% favorits,])) #,predPrev[!outcome %in% favorits,]%>%arrange(desc(freq))))
            toLog("......Filtering out the suggestions, dont matching the last word.")
            
            rtrn <- as.data.table(tmp)
            names(rtrn) <- c("predictor","outcome","freq")
        }         
        suppressWarnings(rm(allcand,predPrev,favorits,tmp))
        
    }
    
    if (nrow(rtrn) > 0) {
        toLog(paste("..In-word search produced", nrow(rtrn),"suggestions. Sorting by descending frequency."))
        rtrn <- rtrn %>% arrange(desc(freq))
        rtrn_sorted <- TRUE
    } else {
        toLog("..No results for in-word prediction for",lastWd)
        toLog("....Starting axillary search in the reference word list.")
        rtrn <- as.data.table(bind_rows(rtrn, envData$dt_scowl[lastWd] %>% select(predictor,outcome,freq) ))
    }
    
    rtrn
}

makePredictions <- function(xx, iwp = TRUE) {
    
    toLog("..Starting the next-word prediction.")
    
    rtrn <- emptyDT
    
    ## Line input,
    ## Regular aaaaa bbbbb ccccc ddddd (a..a  till d..d - regular English words)
    
    
    xbkp <- xx
    
    toLog("....Searching for exact match in the databases.")
    
    rtrn <- askDB(xx)
    
    # Nothing exact, trying to trim down such exprs. like Your's or I'd or apples by ending
    
    if (nrow(rtrn) == 0) {
        toLog("....Nothing found after exact search. Trying trimmed down endings like 'd,  'll or 're")
        trimmed <- gsub("('(d|l|ll|s|r[e]?)|s)$","",xx)
        toLog("......Trimmed expression", trimmed)
        if(trimmed != xx) rtrn <- askDB(trimmed)
    }
    else {
        toLog(paste("....Exact search produced",nrow(rtrn),"suggestions"))
    }
    
    ## Nothing exact found in the database :((
    ## Beginning manipulate the original search line
    
    ## Possibly, input "with my sonn ", but meant "with my son " ?
    if (nrow(rtrn) == 0 && !iwp) {
        
        toLog("....Still no suggestions:( Assumed typo like 'my sonn' for 'my son'. Iteratively shortening the last word by the last letter.")
        lgLW <- stri_length(gsub("^.* ","",xx))
        #if (lgLW <= maxWordLength) {
        while(nrow(rtrn) == 0 && lgLW >= inWordLowLimit) {
            
            xx <- gsub(".$","",xx)
            toLog("......Seeking databases with",xx)
            rtrn <- askDB(xx)
            lgLW <- stri_length(gsub("^.* ","",xx))
        }
        #}
    }
    
    
    if (nrow(rtrn) == 0 ) {
        
        #xx <- xbkp
        ## Fallback from n-gram to (n-1)-gram like
        ## aaaaa bbbbb ccccc ddddd -> bbbbb ccccc ddddd
        
        toLog("....Still no suggestions found. Falling back from n-gram to (n-1)-gram; removing the first word.")
        xx <- xbkp
        
        lgth <- length(strsplit(xx," ")[[1]])
        while (nrow(rtrn) == 0 && lgth > 1) {
            
            tmp <- strsplit(xx," ")[[1]]
            xx <- paste(tmp[2:length(tmp)], sep = "", collapse = " ")
            lgth <- length(tmp) - 1
            rm(tmp)
            toLog("......Making prediction for",xx)
            rtrn <- makePredictions(xx, iwp = FALSE)
        }
        
        ## Last word standing, still no result...
        if (nrow(rtrn) == 0 ) {
            
            toLog("....Last word standing, still no result.")
            toLog("......Going into in-word prediction for",xx)
            rtrn <- inWordPredictEngine(xx)
        }
        
    }
    
    if (nrow(rtrn) > 0) {
        toLog(paste("..Next-word search produced",nrow(rtrn),"suggestions."))
        
        if (! rtrn_sorted)  {
            rtrn <- rtrn %>% arrange(desc(freq))
            rtrn_sorted <- TRUE
        } else {
            # Nothing found anywhere - offering blind the words with the highest frequency
            toLog("Nothing found anywhere - offering blind the words with the highest frequency.")
            rtrn <- envData$dt_monograms%>%arrange(desc(freq))%>%slice(1:maxLinesOut)
            rtrn_sorted <- TRUE
        }
        rtrn
    }
}


predictionEngine <- function(strg, shiny = FALSE) {
    
    rtrn <- emptyDT
    
    
    tmpEnv$LOG <- c("")
    
    toLog("Input line", strg)
    
    inWordPrediction <<- TRUE  #in-word prediction as default (completion of the word just typed in)
    endsWithArticle <- FALSE
    endsWithDot <- FALSE
    
    if (grepl("[[:blank:]]+$", strg)) {
        inWordPrediction <- FALSE    # blanks at the end of string - last word complete, predicting the next word
        if (grepl("[[:blank:]]+(the|a|an)[[:blank:]]+$", strg)) {
            endsWithArticle <- TRUE
        }
    }
    
    toLog("In-word prediction",inWordPrediction)
    toLog("Ends with article",endsWithArticle)
    
    xpr <- precondString(strg)
    xpr <- sub(" . *$","",xpr)
    
    toLog("Effective expression",xpr)
    
    if (! xpr == ".") {  #Input string was not filled ONLY with blanks
        
        if (inWordPrediction) {
            toLog("Going into in-word prediction.")
            rtrn <- inWordPredictEngine(xpr)
        } else {
            toLog("Going to next-word prediction")
            rtrn <- makePredictions(xpr, inWordPrediction)
        }
    }
    
    # Exclude verbs after an article like " the swim" :(
    if (endsWithArticle){
        
        toLog("Input line ends with article - excluding pure verbs from suggestions.")
        tmp <- envData$dt_scowl[rtrn$outcome]
        msk <- tmp$verb == TRUE & !(tmp$noun == TRUE | tmp$adj == TRUE | tmp$adv == TRUE)
        msk[is.na(msk)] <- FALSE
        rtrn <- rtrn[!msk,]
    }
    rtrn <- na.omit(rtrn%>%arrange(desc(freq))%>%slice(1:maxLinesOut))
    if (shiny) {
        rtrn <- na.omit(rtrn%>%arrange(desc(freq))%>%slice(1:maxLinesOut))$outcome
        names(rtrn) <- rtrn
    }
    
    toLog("Returning the prediction results.")
    
    list(rtrn,tmpEnv$LOG)
    
}


wrapPE <- function(strg, shiny = FALSE) {
    exectime <- round(unclass(system.time(rtrn <- predictionEngine(strg,shiny)))[3], 4)
    rtrn[[3]] <- exectime
    rtrn
}

#focusOnUserInput <- "shinyjs.focusOnUserInput = function(){document.getElementById('userInput').focus();}"
#focusOnSelectInput <- "shinyjs.focusOnSelectInput = function(){document.getElementById('selectInput').onmouseover().focus();}"

useShinyjs()
#extendShinyjs(text = focusOnUserInput)
#extendShinyjs(text = focusOnSelectInput)


questions <- readLines("Questions.txt")
assumptions <- readLines("Assumptions.txt")
preproc_steps <- readLines("Preprocessing.txt")
eda_steps <- readLines("EDA.txt")
cocepts_pred <- readLines("Concepts.txt")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    output$questions <- renderUI(HTML(paste(questions, collapse = '<br/>')))
    output$assumptions <- renderUI(HTML(paste(assumptions, collapse = '<br/>')))
    output$preprocessing <- renderUI(HTML(paste(preproc_steps, collapse = '<br/>')))
    output$eda <- renderUI(HTML(paste(eda_steps, collapse = '<br/>')))
    output$concepts <- renderUI(HTML(paste(cocepts_pred, collapse = '<br/>')))
    
    
    
    output$memSize <- renderTable(db_loaded)
    
    
    
    sI <- sessionInfo()
    
    output$generalInfo <- renderTable({
        c(paste("Platform", sI$platform),
          paste("Version R", sI$R.version$version.string),
          paste("Locale", sI$locale),
          paste("OS running", sI$running)
        )
    })
    
    reportConsumption <- function() {
        cons <- system("echo 'Operational system';  uname -a; echo 'Uptime'; uptime; echo  'Memory consumption, MByte'; free -m", intern = T)
        output$consumption <- renderTable({
            cons
        })
    }
    
    reportConsumption()
    
    pingShiny <- function() {
        if (sI$running %like% "Windows") { 
            ping = "ping -n 10 ddakhno.shinyapps.io"
        } else {
            ping = "ping -c 10 -i .2 ddakhno.shinyapps.io"
        }
        rtt <- system(ping,intern = T)
        gsub("/"," / ",rtt[length(rtt)])
    }
    
    rtt <- pingShiny()
    output$roundTrip <- renderText(rtt)                         
    
    observe({
        if(!is.null(input$inSelect)){
            selected <- input$inSelect
            input <- input$userInput
            
            z <- ""
            if (!grepl("[[:blank:]]+$",input)) {
                # Exchanging the last incomplete word through user select
                z <- paste(gsub("[[:blank:]]+[^[:blank:]]+$","",input),selected)
            } else  {
                # Adding one of proposals to the input line
                # The input line ends with [.!?] - first letter of select to upper
                if (grepl("[.!?][[:blank:]]*$",input)) {
                    frst <- toupper(gsub("^(.).*$", "\\1",selected))
                    selected <- gsub("^.",frst,selected)
                }    
                z <- paste(input,selected)
                
            }
            
            updateTextInput(session, 'userInput', value = z)
            updateSelectInput(session, 'inSelect', choices = c() )
            #input <- input$userInput
            js$focusOnUserInput()
        }
    })
    
    
    observe({
        if(!is.null(input$userInput) && stri_length(input$userInput) > 0)   {
            rtrn <- wrapPE(input$userInput, TRUE)
            updateSelectInput(session, 'inSelect', choices = c({ rtrn[[1]] }) )
            updateSelectInput(session, 'LOG', choices = rtrn[[2]] )
            output$execTime <- renderText(rtrn[[3]])
        }
    })
    
    
    observeEvent(
        input$reloadDB, {output$memSize <- renderTable( reloadDBs(input$db_mono,input$db_bi, input$db_tri, input$db_four, input$db_penta))
        }
    )
    
    observeEvent(
        input$pingShiny, { rtt <- pingShiny()
        output$roundTrip <- renderText(rtt)
        }
    )
    
    observeEvent(
        input$saveLog, {
            if(identical(tmpEnv$LOG, c(""))) { tmpEnv$LOG = "empty log" }
            fil <- file.choose()
            writeLines(text = tmpEnv$LOG, con = fil)
        }
    )
})

