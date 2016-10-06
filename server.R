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
confEnv <- new.env()

envData$dt_monograms <- NULL
envData$dt_bigrams <- NULL
envData$dt_trigrams <- NULL
envData$dt_four_grams <- NULL
envData$dt_pentagrams <- NULL


confEnv$db_loaded <- list()
confEnv$db_default <- list(dt_monograms = 3 , dt_bigrams = 3, dt_trigrams = 3, dt_four_grams = 3, dt_pentagrams = 3, dt_scowl = 1)
confEnv$db_eq <- c("dt_monograms"="List of used words", "dt_bigrams" = "One-word predictors", "dt_trigrams" = "Two-word predictors", 
                   "dt_four_grams" =  "Three-word predictors", "dt_pentagrams" =  "Four-word predictors", "dt_scowl" = "Reference word list")

reloadDBs <- function(...) {
    
    db_requested <- list(...)
    db_requested <- lapply(db_requested, as.numeric)
    
    #print("Requested")
    #print(db_requested)
    
    on_load <- list()
    dbfiles <- grep("^d[bt]_", dir(db_dir), value=T)
    
    if (length(db_requested) == 0) {
        on_load <- confEnv$db_default     #Initial request with no parameter, load all DB in default set up.
    } else {
        for (db in names(db_requested)) {
            if (length(confEnv$db_loaded) != 0 && db %in% names(confEnv$db_loaded) && db_requested[[db]] != confEnv$db_loaded[[db]]) { # DB already loaded, but the other size - only then reload
                on_load[db] <- db_requested[db]
            }
            if(! db %in% names(confEnv$db_loaded) && db %in% names(confEnv$db_default)) { # DB not loaded by now - load anyway
                on_load[db] <- db_requested[db]
            }
        }
    }
    
    #print("On-load")
    #print(on_load)
    
    for(db in names(on_load)){
        
        if (on_load[[db]] > 0) { # DB should bie loaded
            obj <- load(file.path(db_dir,grep(db,dbfiles,value=T)[on_load[[db]]]))
            assign(db, get(obj), pos = envData)
            confEnv$db_loaded[db] <- on_load[[db]]
        }
        
        if (on_load[[db]] == 0) { #DB unload if parameter == 0
            rm(list=db, envir = envData)
            confEnv$db_loaded[db] <- NULL
            invisible(gc())
        }
    }
    
    rm(db_joined)
    invisible(gc())
    
    dtabs <- sizes <- nrs <- c()
    
    db_list <- ls(envData)
    
    for (dt in names(confEnv$db_eq)) {
        dtabs <- c(dtabs, confEnv$db_eq[dt])
        if (dt %in% db_list) {
            obj <- get(dt, env = envData)
            sizes <- c(sizes, round(object.size(obj)/1024/1024, 2))
            nrs <- c(nrs, nrow(obj))
            rm(obj)
        } else {
            sizes <- c(sizes, 0)
            nrs <- c(nrs, 0)
        }
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
            tmp <- data.table(rbind(predPrev[outcome %in% favorits,])) 
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
        trimmed <- gsub("'((d|l|ll|s|r[e]?)|s)$","",xx)
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
        while(nrow(rtrn) == 0 & lgLW >= inWordLowLimit) {
            
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
        }
    }else {
        # Nothing found anywhere - offering blind the words with the highest frequency
        toLog("Nothing found anywhere - offering blind the words with the highest frequency.")
        rtrn <- envData$dt_monograms%>%arrange(desc(freq))%>%slice(1:maxLinesOut)
        rtrn_sorted <- TRUE
    }
    rtrn
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
    
    if (! rtrn_sorted) {
        rtrn <- rtrn%>%arrange(desc(freq))
        rtrn_sorted <- TRUE
    }
    
    if (nrow(rtrn) > maxLinesOut ) {
        rtrn <- rtrn%>%slice(1:maxLinesOut)
    }
    
    if (shiny) {
        rtrn <- rtrn$outcome
        names(rtrn) <- rtrn
    }
    
    toLog("Returning the prediction results.")
    
    list(rtrn,tmpEnv$LOG)
    
}


wrapPE <- function(strg, shiny = FALSE) {
    exectime <- round(unclass(system.time(rtrn <- predictionEngine(strg,shiny)))[3], 4)
    rtrn[[3]] <- exectime
    tmpEnv$rtrn <- rtrn
    rtrn
}


useShinyjs()

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
                # In-word prediction
                # Exchanging the last incomplete word through user select
                z <- gsub("[^[:blank:]]+$",selected,input)
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
        input$reloadDB, { output$memSize <- renderText("WAIT!")
            output$memSize <- renderTable(reloadDBs(dt_monograms=input$dt_monograms, dt_bigrams=input$dt_bigrams, dt_trigrams=input$dt_trigrams,
                                                                 dt_four_grams=input$dt_four_grams, dt_pentagrams=input$dt_pentagrams))
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
            writeLines(text = c(tmpEnv$LOG,tmpEnv$rtrn[[3]]), con = fil)
        }
    )
})

