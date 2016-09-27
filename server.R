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
library(data.table)
library(dplyr)
library(stringi)
library(pryr)

homedir <- "/home/d/R-Work/CapstoneProject"
setwd(homedir)
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
    }
    
    dtabs <- c(dtabs,"Total")
    sizes <- c(sizes,sum(sizes))
    nrs <- c(nrs,"")
    
    
    db_loaded <- data.table("Databank"=dtabs,"Nr.rows"=nrs,"RAM used,Mbyte"=sizes)
    print(db_loaded)
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



## Analog to building the databases

###############################################
# Prededining variables and help functions
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
    
    ## Trim to max words (leave teh last max_words)
    ## Throug treatPunctuations the last "word" is always " . "
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
    if (!is.null(envData$dt_bigrams) & lgth == 1) rtrn <- envData$dt_bigrams[xx]
    if (!is.null(envData$dt_trigrams) & lgth == 2) rtrn <- envData$dt_trigrams[xx]
    if (!is.null(envData$dt_four_grams) & lgth == 3) rtrn <- envData$dt_four_grams[xx]
    if (!is.null(envData$dt_pentagrams) & lgth == 4) rtrn <- envData$dt_pentagrams[xx]
    na.omit(rtrn)
}



inWordPredictEngine <- function(xpr) {
    
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
    rtrn <- as.data.table(envData$dt_monograms %>% filter(outcome %like% paste("^",lastWd,".", sep = ""))%>%arrange(desc(freq)))
    
    # More words than one in the line
    # Preceding expression counts - the possible variants are pushed to top!
    
    if (! is.null(prevWords) && nrow(rtrn) > 0) { 
        allcand <- rtrn$outcome
        predPrev <- as.data.table(makePredictions(prevWords, iwp = FALSE))
        if(nrow(predPrev) > 0) {
            favorits <- (predPrev%>%filter(outcome %like% paste("^",lastWd,".",sep="")))$outcome
        }
        if (length(favorits) > 0) {
            tmp <- data.table(rbind(predPrev[outcome %in% favorits,])) #,predPrev[!outcome %in% favorits,]%>%arrange(desc(freq))))
            rtrn <- as.data.table(tmp)
            names(rtrn) <- c("predictor","outcome","freq")
        }         
        suppressWarnings(rm(allcand,predPrev,favorits,tmp))
        
    }
    
    if (nrow(rtrn) > 0) {
        rtrn <- rtrn %>% arrange(desc(freq))
    } 
    
    rtrn
}

makePredictions <- function(xx, iwp = TRUE) { 
    
    rtrn <- emptyDT
    
    ## Line input, 
    ## Regular aaaaa bbbbb ccccc ddddd (a..a  till d..d - regular English words)
    
    
    xbkp <- xx
    
    rtrn <- askDB(xx)
    
    # Nothing exact, trying to trim down such exprs. like Your's or I'd or apples by ending
    if (nrow(rtrn) == 0) {
        trimmed <- gsub("('(d|l|ll|s|r[e]?)|s)$","",xx)
        if(trimmed != xx) rtrn <- askDB(trimmed)
    }
    
    ## Nothing exact found in the database :((
    ## Beginning manipulate the original search line
    
    ## Possibly, input "with my sonn ", but meant "with my son " ?
    if (nrow(rtrn) == 0 && !iwp) {
        lgLW <- stri_length(gsub("^.* ","",xx))
        #if (lgLW <= maxWordLength) {
        while(nrow(rtrn) == 0 && lgLW >= inWordLowLimit) {
            xx <- gsub(".$","",xx)
            rtrn <- askDB(xx)
            lgLW <- stri_length(gsub("^.* ","",xx))
        }
        #}
    }
    
    
    if (nrow(rtrn) == 0 ) {
        
        #xx <- xbkp
        ## Fallback from n-gram to (n-1)-gram like
        ## aaaaa bbbbb ccccc ddddd -> bbbbb ccccc ddddd
        
        xx <- xbkp
        
        lgth <- length(strsplit(xx," ")[[1]]) 
        while (nrow(rtrn) == 0 && lgth > 1) {
            #remove the first word from expression
            tmp <- strsplit(xx," ")[[1]]
            xx <- paste(tmp[2:length(tmp)], sep = "", collapse = " ")
            lgth <- length(tmp) - 1
            rm(tmp)
            rtrn <- makePredictions(xx, iwp = FALSE)
        }
        
        ## Last word standing, still no result...
        if (nrow(rtrn) == 0 ) {
            rtrn <- inWordPredictEngine(xx)
        }
        
    }
    
    if (nrow(rtrn) > 0) {
        rtrn <- rtrn %>% arrange(desc(freq))
    } else {
        # Nothing found anyware - offering blind the words with the highest frequency 
        rtrn <- dt_monograms%>%arrange(desc(freq))%>%slice(1:maxLinesOut)
    }
    rtrn
}

predictionEngine <- function(strg, shiny = FALSE) {
    
    rtrn <- emptyDT
    
    inWordPrediction <<- TRUE  #in-word prediction as default (completion of the word just typed in)
    endsWithArticle <- FALSE
    endsWithDot <- FALSE
    
    if (grepl("[[:blank:]]+$", strg)) {
        inWordPrediction <- FALSE    # blanks at the end of string - last word complete, predicting the next word
        if (grepl("(the|a|an)[[:blank:]]+$", strg)) {
            endsWithArticle <- TRUE
        }
    }
    
    
    xpr <- precondString(strg)
    xpr <- sub(" . *$","",xpr)
    
    if (! xpr == ".") {  #Input string was not filled ONLY with blanks
        
        if (inWordPrediction) {
            rtrn <- inWordPredictEngine(xpr) 
        } else {
            rtrn <- makePredictions(xpr, inWordPrediction)
        }
    }
    
    # Exclude verbs after an article like " the swim" :(
    if (endsWithArticle){
        tmp <- dt_scowl[rtrn$outcome]
        msk <- tmp$verb == TRUE & !(tmp$noun == TRUE | tmp$adj == TRUE | tmp$adv == TRUE)
        msk[is.na(msk)] <- FALSE
        rtrn <- rtrn[!msk,]
    }
    rtrn <- na.omit(rtrn%>%arrange(desc(freq))%>%slice(1:maxLinesOut))
    if (shiny) {
        rtrn <- na.omit(rtrn%>%arrange(desc(freq))%>%slice(1:maxLinesOut))$outcome
        names(rtrn) <- rtrn
    }
    rtrn
    
}

focusOnUserInput <- "shinyjs.focusOnUserInput = function(){document.getElementById('userInput').focus();}"
#focusOnSelectInput <- "shinyjs.focusOnSelectInput = function(){document.getElementById('selectInput').onmouseover().focus();}"

useShinyjs()
extendShinyjs(text = focusOnUserInput)
#extendShinyjs(text = focusOnSelectInput)

print("search()")
print(search())     

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    output$memSize <- renderTable(db_loaded)
    
    print(ls())
    
    
    print(ls(envData))                            
    
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
            updateSelectInput(session, 'inSelect', choices = c({ predictionEngine(input$userInput, TRUE) }) )
            # isolate(input$userInput)
            #js$focusOnSelectInput()
        }
    })
    
    observeEvent(
        input$reloadDB, {output$memSize <- renderTable( reloadDBs(input$db_mono,input$db_bi, input$db_tri, input$db_four, input$db_penta))
        }
    )

    
    
}
)


