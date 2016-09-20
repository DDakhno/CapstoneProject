setwd("/home/d/R-Work/CapstoneProject")

rm(list=ls())
invisible(gc())

library(data.table)
library(dplyr)
library(stringi)

homedir <- "/home/d/R-Work/CapstoneProject"
setwd(homedir)
source("functions.R")


punctmarks <- "[.!?]"
vectPM <- c(".","!","?")
punctmarkGroup <- "([.!?])"
wordsnogo <- c("the","a","an")

wlimit <- 3

dt_monograms <- NULL
dt_bigrams <- NULL
dt_trigrams <- NULL
dt_four_grams <- NULL
dt_pentagrams <- NULL

# for (dt in c("dt_monograms",  "dt_bigrams", "dt_trigrams", "dt_four_grams",  "dt_pentagrams" )) {
# #for (dt in c("dt_monograms", "dt_bigrams", "dt_trigrams")) {
#         load(file.path("DB",dt,paste(dt,".Rdata",sep="", collapse=".")))
#         ls()
#         if (dt == "dt_monograms") {
#                 setkey(db_joined, word)
#         } else {
#                 setkey(db_joined, predictor)
#         }
#         
#         if(dt == "dt_monograms") { dt_monograms <- copy(db_joined) }
#         if(dt == "dt_bigrams") { dt_bigrams <- copy(db_joined) }
#         if(dt == "dt_trigrams") { dt_trigrams <- copy(db_joined) }
#         if(dt == "dt_four_grams") { dt_four_grams <- copy(db_joined) }
#         if(dt == "dt_pentagrams") { dt_pentagrams <- copy(db_joined) }
#         rm(db_joined)
#         load("finalDB/dt_wordnet.Rda")
#         gc()
# }


## Loading the reference word list (wordnet)
load("finalDB/dt_wordnet.Rda")
load("finalDB/db_mono_5up_23MB.Rdata")
dt_monograms <- db_mono_5up_23MB
rm(db_mono_5up_23MB)

## Loading the trimmed expression databases

load("finalDB/db_mono_5up_23MB.Rdata")
dt_monograms <- db_mono_5up_23MB
rm(db_mono_5up_23MB)

load("finalDB/db_bi_10up_48MB.Rdata")
dt_bigrams <- db_bi_10up_48MB
rm(db_bi_10up_48MB)

load("finalDB/db_tri_8up_99MB.Rdata")
dt_trigrams <- db_tri_8up_99MB
rm(db_tri_8up_99MB)

load("finalDB/db_four_8up_76MB.Rdata")
dt_four_grams <- db_four_8up_76MB
rm(db_four_8up_76MB)

load("finalDB/db_penta_7up_76MB.Rdata")
dt_pentagrams <- db_penta_7up_76MB
rm(db_penta_7up_76MB)
invisible(gc())



removeWordsNogo <- function(lin,wng) {
        tmp <- strsplit(lin, split ="[[:blank:]]+" )[[1]]
        mask <- tmp %in% wng
        tmp <- tmp[!mask]
        x <- paste(tmp,split="",collapse=" ")
        x
}

predictionEngine <- function(strg) {
        
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
        
        inWordPredictEngine <- function(xpr) {
                rtrn = data.frame()
                ## Use case #1 - a word is in process of typing, so we could probably recognize it by the stem
                
                rtrn <- dt_monograms %>% filter(word %like% paste("^",xpr, sep = ""))
                
                ## Use case #2 - the word is probably too long. Reducing in steps up to length wlimit 
                if (nrow(rtrn) == 0) {
                        while(stri_length(xpr) >= wlimit && nrow(rtrn) == 0) {
                                #rtrn <- na.omit(dt_wordnet[xpr])
                                rtrn <- na.omit(dt_wordnet%>%filter(word %like% paste("^",xpr, sep="", collapse="")))
                                xpr <- gsub(".$","",xpr)
                        }
                }
                rtrn
        }
        
        makePredictions <- function(xx) {
                
                limit <- 60
                ## Line input, 
                ## Regular aaaaa bbbbb ccccc ddddd (a..a  till d..d - regular English words)
                
                xbkp <- xx
                
                askDB <- function(xx,lgth) {
                        if (!is.null(dt_bigrams) & lgth == 1) rtrn <- na.omit(dt_bigrams[xx][1:limit])
                        if (!is.null(dt_trigrams) & lgth == 2) rtrn <- na.omit(dt_trigrams[xx])
                        if (!is.null(dt_four_grams) & lgth == 3) rtrn <- na.omit(dt_four_grams[xx][1:limit])
                        if (!is.null(dt_pentagrams) & lgth == 4) rtrn <- na.omit(dt_pentagrams[xx][1:limit])
                }
                
                lgth <- length(strsplit(xx, split = "[[:blank:]]")[[1]])
                
                rtrn <- askDB(xx,lgth)
                
                # Nothing exact, trying to trim down such exprs. like Your's or I'd or apples by ending
                if (nrow(rtrn) == 0) {
                        trimmed <- gsub("('(d|l|ll|s|r[e]?)|s)$","",xx)
                        if(trimmed != xx) rtrn <- askDB(xx)
                }
                
                ## Nothing exact found in the database :((
                ## Beginning manipulate the original search line
                xx <- xbkp
                
                if (nrow(rtrn) == 0 ) {
                        
                        ## Fallback from n-gram to (n-1)-gram like
                        ## aaaaa bbbbb ccccc ddddd -> bbbbb ccccc ddddd
                        while (nrow(rtrn) == 0 && lgth > 1) {
                                #remove the first word from expression
                                tmp <- strsplit(xx," ")[[1]]
                                xx <- paste(tmp[2:length(tmp)], sep = "", collapse = " ")
                                lgth <- length(tmp) - 1
                                rm(tmp)
                                rtrn <- makePredictions(xx)
                        }
                        
                        ## Last word standing, still no result...
                        if (nrow(rtrn) == 0 ) {
                                rtrn <- inWordPredictEngine(xx)
                                if (nrow(rtrn) != 0){
                                        rtrn <- na.omit(dt_bigrams[rtrn$words_EN,][1:limit]) # Yes, we can!
                                } else { #Still nothing ?! ...Return the top-limit words
                                        
                                }
                                
                        }
                        
                }
                if (nrow(rtrn) > 0) {
                        na.omit(rtrn %>% arrange(desc(freq)) %>% slice(1:limit))
                } else {
                        rtrn
                }
                
                
        }
        
        rtrn <- data.frame() #as default an empty data frame (nothing found)
        
        xpr <- precondString(strg)
        xpr <- sub(" . *$","",xpr)
        
        makePredictions(xpr)
        
}
