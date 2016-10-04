#!/usr/local/bin/Rscript

#Cleaning the workspace
rm(list = ls())
invisible(gc())

homedir <- "/home/d/R-Work/CapstoneProject"
setwd(homedir)
source("functions.R")

suppressMessages(library(tm))
library(stringi)
suppressMessages(library(data.table))
suppressMessages(library(dplyr))
library(tau)
library(parallel)


START <- as.integer(Sys.time())

## Modelling relevant configuration
dictToLower <- FALSE
dictToTrimmComb <- TRUE

docsToLower <- TRUE
docsToStem  <- FALSE
docsToTrimmComb <- dictToTrimmComb

prob_train <- .15  #train volume in proportion to all corpus lines
# prob_test <- prob_train/2  #test to the whole corpus
# prob_valid <- prob_train/2 #validation to the whole corpus
##########################################################################################

tmst <- as.integer(Sys.time())
## Technical configuration
datadir <- file.path(homedir,"data")
tmpdir <- paste(homedir,"/tmp",prob_train,"_",tmst, sep = "")
endicts <- file.path(datadir,"en_dicts")
scowldir <- file.path(datadir,"SCOWL")
projectcorpURL <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
projectcorpFile <- rev(stri_split(str = projectcorpURL,fixed = "/")[[1]])[1]
result_file <- file.path(tmpdir,"results.log")

## Downloading and unziping the data

if (!dir.exists(file.path(tmpdir,"before"))) {
        dir.create(file.path(tmpdir,"before"),recursive = T)
        dir.create(file.path(tmpdir,"after"),recursive = T)
}

if (!dir.exists(endicts)) dir.create(endicts,recursive = T)
if (!dir.exists(scowldir))    dir.create(scowldir,recursive = T)

## Check, if the project data file is here; if not, download and unpack

if (!dir.exists(file.path(datadir,"final","en_US"))) {
        setwd(datadir)
        projectcorpFile <- file.path(datadir,projectcorpFile)
        if (!file.exists(projectcorpFile))
                download.file(url = projectcorpURL,destfile = projectcorpFile,mode = "wb")
        engfiles <- grep("en_US.*txt",unzip(zipfile = projectcorpFile, list = T)[[1]],value = T)
        unzip(zipfile = projectcorpFile, files = engfiles)
}


## Status after downloading and unpacking the text corpus
dir(datadir)
dir(file.path(datadir,"final","en_US"))
dir(file.path(scowldir))
dir(file.path(datadir,"en_dicts"))


## Building train corpus

setwd(file.path(datadir,"final/en_US"))
system("wc -l *.txt >fileinfo.log")
fileinfo <- read.table("fileinfo.log")
system("rm fileinfo.log")

ndoc <- nrow(fileinfo)-1
trainCorpus <- Corpus(VectorSource(1:ndoc))

i <- 1
while (i < nrow(fileinfo)) {
        
        nr <- fileinfo[i,1]
        mask <- as.logical(rbinom( nr, 1, prob = prob_train))
        fil <- as.character(fileinfo[i,2])
        trainCorpus[[i]]$content <- readLines(fil, ok = T, warn = F)[mask]
        rm(mask)
        i <- i + 1
}


writeCorpus(x = trainCorpus,path = file.path(tmpdir,"before"))


invisible(gc())


## Building reference word lists,echo=FALSE, cache=F, split= F}

if (!dir.exists(file.path(tmpdir,"before"))) {
        dir.create(file.path(tmpdir,"before"),recursive = T)
        dir.create(file.path(tmpdir,"after"),recursive = T)
}

if (!dir.exists(endicts)) dir.create(endicts,recursive = T)
if (!dir.exists(scowldir))    dir.create(scowldir,recursive = T)

## Check, if the project data file is here; if not, download and unpack

if (!dir.exists(file.path(datadir,"final","en_US"))) {
        setwd(datadir)
        projectcorpFile <- file.path(datadir,projectcorpFile)
        if (!file.exists(projectcorpFile))
                download.file(url = projectcorpURL,destfile = projectcorpFile,mode = "wb")
        engfiles <- grep("en_US.*txt",unzip(zipfile = projectcorpFile, list = T)[[1]],value = T)
        unzip(zipfile = projectcorpFile, files = engfiles)
}

# Building the Wordnet dictionary
# Wordnet requests are not performant enough for verification of the really long
# lists over the standard R interface. We make brute force hacking of wordnet lists
# at the UNIX-console

system(paste("grep -vh '^ ' /usr/share/wordnet/index*| cut -f1 -d' '| sort -u > ",endicts,"/dict_wordnet.txt",sep=""))

## We use the SCOWL english word lists as a concurrent english word list

# scowlgz <- file.path(scowldir,"scowl-2016.06.26.tar.gz")
# 
# if (!file.exists(file.path(tmpdir,"scowl-words.txt"))) {
#         if (!file.exists(scowlgz))
#                 download.file(url = "http://vorboss.dl.sourceforge.net/project/wordlist/SCOWL/2016.06.26/scowl-2016.06.26.tar.gz",
#                               destfile = scowlgz, mode = "wb")
#         untar(tarfile = scowlgz, exdir = scowldir)
#         system(paste("cat ",scowldir,"/*/final/*| sort -u > ",endicts,"/dict_scowl.txt","",sep=""))
# }

dictCorp <- Corpus(DirSource(endicts, encoding = "UTF-8", pattern = ".*.txt"))

trimmCombinations <- function(x) gsub("_"," ",x)

## Words like more, times, where, minutes,  office or from can not be found after stemming,
## so we could reject stemming building dictionary
## We probably still need punctuation (the word pair/triplets bilidng might stop an
## something like ",")
## Order matters: tolower, than removeWords

if (dictToLower) { dictCorp <- tm_map(dictCorp, content_transformer(tolower)) }

dictCorp <- tm_map(dictCorp, stripWhitespace)

if (dictToTrimmComb) { dictCorp <- tm_map(dictCorp, content_transformer(trimmCombinations)) }

dictCorp <- tm_map(dictCorp, content_transformer(unique)) 
words_EN <- unique(c(dictCorp[[1]]$content))

## Currently not used, time consuming
# dictCorp <- tm_map(dictCorp, stemDocument)
# stems_EN <- unique(c(dictCorp[[1]]$content,dictCorp[[2]]$content))

rm(dictCorp)
invisible(gc())


## Selecting english words from the dictionary as data table is about FORTY times faster,
## than construct like wrds %in% words_EN

dt_words_EN <- data.table(freq = rep(-1,length(words_EN)), words_EN)
setkey(dt_words_EN, "words_EN")
setwd(homedir)
save(dt_words_EN, file = "finalDB/dt_words_EN.Rdata")
#dt_stems_EN <- data.table(stems_EN)
#setkey(dt_stems_EN,"stems_EN")
#rm(list=c("words_EN","stems_EN"))
rm(list=c("words_EN"))
invisible(gc())


## Status after downloading the dictionaries
dir(datadir)
dir(file.path(datadir,"final","en_US"))
dir(file.path(scowldir))
dir(file.path(datadir,"en_dicts"))

## Building tidy data,echo=FALSE, cache=F}

# For function definitions see functions.R

trList <- list(
        removeNumbers,
        stripWhitespace
)

workTrainCorpus <- tm_map(trainCorpus, tm_reduce, trList)

rm(trainCorpus)
invisible(gc())


for (comp in seq_along(workTrainCorpus)) {
        workTrainCorpus[[comp]]$content <- subsetUTFAnalogs(workTrainCorpus[[comp]]$content)
}

if (docsToLower) workTrainCorpus <- tm_map(workTrainCorpus, content_transformer(tolower))

workTrainCorpus <- tm_map(workTrainCorpus, removeWords, wordsnogo)

workTrainCorpus <- tm_map(workTrainCorpus, content_transformer(filterAwayNonLatinLines))


for(i in seq_along(workTrainCorpus)) {
        workTrainCorpus[[i]]$content <- treatPunctuations(workTrainCorpus[[i]]$content)
}

for(i in seq_along(workTrainCorpus)) {
        workTrainCorpus[[i]]$content <- removeNotLatinWords(workTrainCorpus[[i]]$content)
}

tidyTrainCorpus <- tm_map(workTrainCorpus, stripWhitespace)

writeCorpus(x = tidyTrainCorpus,path = file.path(tmpdir,"after"))

save(tidyTrainCorpus,file = file.path(tmpdir,"tidyTrainCorpus.Rdata"))

paste("Exe. time:",(as.integer(Sys.time()) - START)/60,"min.")

rm(workTrainCorpus)
invisible(gc())

## Monograms 
## Library tau

monograms <- textcnt(c(tidyTrainCorpus[[1]]$content, tidyTrainCorpus[[2]]$content, 
                       tidyTrainCorpus[[3]]$content), method = "string", split = " ", n = 1L, decreasing = T)
monograms <- unclass(monograms)
#Primary selection of the valid words


mask <- grepl(punctmarks,names(monograms))
monograms <- monograms[!mask]
# mask <- grepl("['*+]",names(monograms))
#monograms <- monograms[!mask]
wordFreqDescNorm <- monograms #for code compatbility reasons
dt_monograms <- data.table(freq = monograms, word = names(monograms))
setkey(dt_monograms, word)
save(dt_monograms, file = file.path(tmpdir,"dt_monograms.Rdata"))
invisible(gc())

## Top-20 single words

sm <- sum(wordFreqDescNorm)

round(wordFreqDescNorm[1:20]/sm, 4)
sum(cumsum(wordFreqDescNorm)/sm <= .1)
sum(cumsum(wordFreqDescNorm)/sm <= .2)
sum(cumsum(wordFreqDescNorm)/sm <= .5)
sum(cumsum(wordFreqDescNorm)/sm <= .9)

## Bigrams 
## Library tau

bigrams <- textcnt(c(tidyTrainCorpus[[1]]$content, tidyTrainCorpus[[2]]$content, 
                     tidyTrainCorpus[[3]]$content), method = "string", split = " ", n = 2L, decreasing = T)
bigrams <- unclass(bigrams)

#Selecting only the wordpairs without separators and "'"
mask <- grepl("(^[.,!?]+[[:blank:]]|[[:blank:]][.,!?]+[[:blank:]]|[[:blank:]][.,!?]+[[:blank:]]*$)",names(bigrams))
bigrams <- bigrams[!mask]
# mask <- grepl("['*+]",names(bigrams))
# bigrams <- bigrams[!mask]

dt_bigrams <- data.table( predictor = gsub(" [^ ]*$","",names(bigrams)), 
                         outcome = gsub("^.* ","",names(bigrams)),
                         freq = bigrams)
setkey(dt_bigrams,predictor)

save(bigrams, file = file.path(tmpdir,"bigrams.Rdata"))
save(dt_bigrams, file = file.path(tmpdir,"dt_bigrams.Rdata"))
rm(bigrams)
rm(mask)
invisible(gc())

## Bigrams, echo=T, comment='', collapse=T}
head(dt_bigrams %>% arrange(desc(freq)),20)

## Trigrams 
## The same for word triplets

trigrams <- textcnt(c(tidyTrainCorpus[[1]]$content, tidyTrainCorpus[[2]]$content, 
                      tidyTrainCorpus[[3]]$content), method = "string", split = " ", n = 3L, decreasing = T)
trigrams <- unclass(trigrams)

#Selecting only the wordpairs without separators and "'"
mask <- grepl("(^[.,!?]+[[:blank:]]|[[:blank:]][.,!?]+[[:blank:]]|[[:blank:]][.,!?]+[[:blank:]]*$)",names(trigrams))
trigrams <- trigrams[!mask]
# mask <- grepl("['*+]",names(trigrams))
# trigrams <- trigrams[!mask]

dt_trigrams <- data.table(predictor = gsub(" [^ ]*$","",names(trigrams)), 
                          outcome = gsub("^.* ","",names(trigrams)),
                          freq = trigrams)
setkey(dt_trigrams,predictor)
save(trigrams, file = file.path(tmpdir,"trigrams.Rdata"))
save(dt_trigrams, file = file.path(tmpdir,"dt_trigrams.Rdata"))
rm(trigrams)
rm(mask)
invisible(gc())

##
head(dt_trigrams %>% arrange(desc(freq)),20)


## Four_grams echo=F}
## The same for four words

four_grams <- textcnt(c(tidyTrainCorpus[[1]]$content, tidyTrainCorpus[[2]]$content, 
                        tidyTrainCorpus[[3]]$content), method = "string", split = " ", n = 4L, decreasing = T)
four_grams <- unclass(four_grams)

#Selecting only the word sequences without separators and "'"
mask <- grepl("(^[.,!?]+[[:blank:]]|[[:blank:]][.,!?]+[[:blank:]]|[[:blank:]][.,!?]+[[:blank:]]*$)",names(four_grams))
four_grams <- four_grams[!mask]

dt_four_grams <- data.table(predictor = gsub(" [^ ]*$","",names(four_grams)), 
                            outcome = gsub("^.* ","",names(four_grams)),
                            freq = four_grams)
setkey(dt_four_grams, predictor)

save(four_grams, file = file.path(tmpdir,"four_grams.Rdata"))
save(dt_four_grams, file = file.path(tmpdir,"dt_four_grams.Rdata"))
rm(four_grams)
rm(mask)
invisible(gc())

head(dt_four_grams %>% arrange(desc(freq)),20)

## Pentagrams
## The same for five words

pentagrams <- textcnt(c(tidyTrainCorpus[[1]]$content, tidyTrainCorpus[[2]]$content, 
                        tidyTrainCorpus[[3]]$content), method = "string", split = " ", n = 5L, decreasing = T)
pentagrams <- unclass(pentagrams)

#Selecting only the wordp sequences without separators and "'"
mask <- grepl("(^[.,!?]+[[:blank:]]|[[:blank:]][.,!?]+[[:blank:]]|[[:blank:]][.,!?]+[[:blank:]]*$)",names(pentagrams))
pentagrams <- pentagrams[!mask]

dt_pentagrams <- data.table( predictor = gsub(" [^ ]*$","",names(pentagrams)), 
                            outcome = gsub("^.* ","",names(pentagrams)),
                            freq = pentagrams)

setkey(dt_pentagrams,predictor)

save(pentagrams, file = file.path(tmpdir,"pentagrams.Rdata"))
save(dt_pentagrams, file = file.path(tmpdir,"dt_pentagrams.Rdata"))
rm(pentagrams)
rm(mask)
rm(tidyTrainCorpus)
invisible(gc())

## Top-20 pentagrams
head(dt_pentagrams %>% arrange(desc(freq)),20)

#########################################################################################################
# Frame preconditions for defining the prediction model
#########################################################################################################
# Criterion                     Bigrams              Trigrams         Fourgrams            Pentagrams
#-----------------------------+------------------+-------------------+------------------+---------------------
# Total nr unique, Mio.                 3.2             6.8             7.5                     6.8
# Sum. freq                                             9,6
# Predictors, unique,                   .250            2.8             5.8                     6.4
# Unique/Total                          .08             .4              .77                     .93
# DB size in memory, Mb                 86              294             515                     572
########################################################################################################
# Purifying the dt_trigrams
#-------------------------------------------------------+----------------+------------------------------
# Step                                                          Nrow            Object.size,Mbyte
#-------------------------------------------------------+----------------+------------------------------
# Original                                                      6696991         288.90
# freq > 1                                                      749942          28.43
# freq > 2                                                      342777          12.97
#
#
########################################################################################################

## Trimming the databases(datatables)


trimDataTable <- function(x) {
        print(paste("Original DB:",x))
        db <- get(x)
        print(paste("Nrow:",nrow(db)))
        print(paste("Size in memory, MB:",object.size(db)/1024/1024))
        print(paste("Max. freq :",max(db$freq)))
        print(paste("Mean. freq :",mean(db$freq)))
        print(paste("Median. freq :",median(db$freq)))
        print(paste("Proportion unique predictors:", length(unique(db$predictor))/nrow(db)))
        print("-----------------------")

        print(paste("Nrow, freq > 1 :", nrow(db %>% filter(freq > 1))))
        print(paste("Size in MB:, freq > 1 :", object.size(db %>% filter(freq > 1))/1024/1024))
        print(paste("Nrow, freq > 2 :", nrow(db %>% filter(freq > 2))))
        print(paste("Size in MB:, freq > 2 :", object.size(db %>% filter(freq > 2))/1024/1024))
        print(paste("Nrow, freq > 3 :", nrow(db %>% filter(freq > 3))))
        print(paste("Size in MB:, freq > 3 :", object.size(db %>% filter(freq > 3))/1024/1024))
        print(paste("Nrow, freq > 4 :", nrow(db %>% filter(freq > 4))))
        print(paste("Size in MB:, freq > 4 :", object.size(db %>% filter(freq > 4))/1024/1024))
        print("-----------------------")
        print("Trimming the DB by freq > 1")
        db_trimmed <- db %>% filter(freq > 1)
        print(paste("Nrow:",nrow(db_trimmed)))
        print(paste("Size in memory, MB:",object.size(db_trimmed)/1024/1024))
        print(paste("Max. freq :",max(db_trimmed$freq)))
        print(paste("Mean. freq :",mean(db_trimmed$freq)))
        print(paste("Median. freq :",median(db_trimmed$freq)))
        print(paste("Proportion unique predictors:", length(unique(db_trimmed$predictor))/nrow(db_trimmed)))
        
        print(paste("Nr. usages in trimmed to all usages :",sum(db_trimmed$freq)/sum(db$freq)))
        
        print("-----------------------")
        print("=========================================")
        db_trimmed
}

for (x in c("dt_bigrams","dt_trigrams","dt_four_grams","dt_pentagrams")) trimDataTable(x)


paste("Exe. time:",(as.integer(Sys.time()) - START)/60,"min.")