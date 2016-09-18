#!/usr/local/bin/Rscript

#Cleaning the workspace
rm(list = ls())
invisible(gc())

suppressMessages(library(tm))
library(stringi)
suppressMessages(library(data.table))
suppressMessages(library(dplyr))
library(tau)


START <- as.integer(Sys.time())

## Modelling relevant configuration
dictToLower <- TRUE
dictToTrimmComb <- TRUE

docsToLower <- dictToLower
docsToStem  <- FALSE
docsToTrimmComb <- dictToTrimmComb

prob_train <- .15  #train volume in proportion to all corpus lines
# prob_test <- prob_train/2  #test to the whole corpus
# prob_valid <- prob_train/2 #validation to the whole corpus
##########################################################################################

tmst <- as.integer(Sys.time())
## Technical configuration
homedir <- "/home/d/R-Work/CapstoneProject"
datadir <- file.path(homedir,"data")
tmpdir <- paste(homedir,"/tmp",prob_train,"_",tmst, sep = "")
endicts <- file.path(datadir,"en_dicts")
scowldir <- file.path(datadir,"SCOWL")
projectcorpURL <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
projectcorpFile <- rev(stri_split(str = projectcorpURL,fixed = "/")[[1]])[1]
result_file <- file.path(tmpdir,"results.log")

## Downloading and unziping the data

setwd(homedir)

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

scowlgz <- file.path(scowldir,"scowl-2016.06.26.tar.gz")

if (!file.exists(file.path(tmpdir,"scowl-words.txt"))) {
        if (!file.exists(scowlgz))
                download.file(url = "http://vorboss.dl.sourceforge.net/project/wordlist/SCOWL/2016.06.26/scowl-2016.06.26.tar.gz",
                              destfile = scowlgz, mode = "wb")
        untar(tarfile = scowlgz, exdir = scowldir)
        system(paste("cat ",scowldir,"/*/final/*| sort -u > ",endicts,"/dict_scowl.txt","",sep=""))
}

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
words_EN <- unique(c(dictCorp[[1]]$content,dictCorp[[2]]$content))

## Currently not used, time consuming
# dictCorp <- tm_map(dictCorp, stemDocument)
# stems_EN <- unique(c(dictCorp[[1]]$content,dictCorp[[2]]$content))

rm(dictCorp)
invisible(gc())


## Selecting english words from the dictionary as data table is about FORTY times faster,
## than construct like wrds %in% words_EN

dt_words_EN <- data.table(freq = rep(-1,length(words_EN)), words_EN)
setkey(dt_words_EN, "words_EN")
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

wordsnogo <- c("the","a","an")

trList <- list(
        removeNumbers,
        stripWhitespace
)

workTrainCorpus <- tm_map(trainCorpus, tm_reduce, trList)

rm(trainCorpus)
invisible(gc())


# Make reasonable substitutions for UTF-8 expressions and chars

subsetUTFAnalogs <- function(str) {
        # Throwing away unconventional quotes as well as exprs. like "°" or "€"
        x <- gsub("\xe2\x80\x9c|\xe2\x80\x9d|\xe2\x80\xa6|\xe2\x80\x93|\xe2\x82\xac|\xc2\xba|\xc2\xbd|\xe2\x99\xa5","", x = str, perl = T,fixed = F)
        x <- gsub("\xe2\x80\x99|\xe2\x80\x98","'", x , perl = T, fixed = F)
        x <- gsub("\xe2\x80\x94", "-", x)
        x
}

for (comp in seq_along(workTrainCorpus)) {
        workTrainCorpus[[comp]]$content <- subsetUTFAnalogs(workTrainCorpus[[comp]]$content)
}

if (docsToLower) workTrainCorpus <- tm_map(workTrainCorpus, content_transformer(tolower))

workTrainCorpus <- tm_map(workTrainCorpus, removeWords, wordsnogo)

filterAwayNonLatinLines <- function(lins) {
        mask <- Encoding(lins) == "UTF-8" | grepl("\xe3|\xe4|\xe5|\xe6|\xe7|\xe8|\xe9|\xd0",x = lins, useBytes = T) 
        lins[!mask]
}

workTrainCorpus <- tm_map(workTrainCorpus, content_transformer(filterAwayNonLatinLines))

punctmarks <- "[,.!?]"
vectPM <- c(".",",","!","?")
punctmarkGroup <- "([,.!?])"

## All printable ASCII characters
## gsub("[\x21-\x7e]","","abZ{")

treatPunctuations <- function(x) {
        
        # Removing  " - "
        x <- gsub("[[:blank:]]+[-]+[[:blank:]]+"," ",x)
        
        # Removing all parenthesis of all kinds
        #... sorry, smilies ;)
        #x <- gsub("\x28\x29\x7b\x7d\x5b\x5d\x7c"," ",x)
        
        x <- gsub("[(]+([a-zA-Z])", ". \\2", x)
        x <- gsub("([a-zA-Z])[.!?]*[)]+", "\\1 . ", x)
        
        
        # Removing the isolated quote sequences, like "is "" hah"  -> "is hah"
        x <- gsub("[[:blank:]]+[\x22\x27]+[[:blank:]]+", " ", x)
        
        # Converting the double quotes into single one in-words
        ## sal"teen -> sal'teen
        x <- gsub("([a-z])\"([a-z])","\\1'\\2",x)
        
        ## Unquoting the citation, making a separate sentence 
        #### In the middle of the line
        #Removing the quotes at the begining of a citation, separating with point. (""It is...)
        x <- gsub("[[:blank:]]+[\x27\x22]+([a-zA-Z])"," . \\1",x)
        
        #Removing the quotes at the end of a citation,  separating with point. (was great!')
        x <- gsub("([a-zA-Z.,!?])[\x27\x22]+[[:blank:]]+","\\1 . ",x)
                  
        ### At the begin of the line
        x <- gsub("^[\x22\x27]+([^\x22\x27]*)[\x22\x27]+[[:blank:]]+", "\\1 . ", x)
        
        ### at the end of line
        x <- gsub("([^\x22\x27]+)[\x22\x27]+[[:blank:]]*[.!?]*[[:blank:]]*$", " . \\1 .", x)
        

        # Substituting multiple punctuation characters at the end of the line with " ."
        x <- gsub(paste(punctmarks,"{2,}[[:blank:]]*$",sep="")," .", x, perl = T)
        
        # Formating the end-of-sentence (characters ".!?")
        x <- gsub(punctmarkGroup," \\1 ", x, perl = T)
        
        # Last resort '" OR ''
        # x <- gsub("(^[\x22\x27]+|[\x22\x27]+$)","",x)
        x <- gsub("[\x22\x27]+","",x)
        
        
        # Anyway, the point at the end of the line
        x <- sub("$"," .",x)
        x
}
# # English words may begin with letters, numbers ("25th") or lately "#" (as a hashtag) and "@"
# # ... and consist of letters, numbers, points ("US.","s.u.v.") and "'"
#       x <- gsub("[[:blank:]]+[^a-zA-Z0-9#@]+([#@]+[a-zA-Z0-9]+[a-zA-Z0-9\x27]+)", " \\1", x)
# 
# # English words may end with letters, numbers 
# # ... and consist of letters, numbers ("b-52"), points ("US.","s.u.v.") and "'"
#       x <- gsub("([a-zA-Z.0-9\x27])[^a-zA-Z.0-9\x27[:blank:]][[:blank:]]+", "\\1 ", x)

#         # Trimming the longer sequences of punctuation characters in the middle of the line 
#         #  x <- gsub(paste(punctmarkGroup,"{2,}",sep="")," . ", x, perl = T)
#         
#         # Removing the longer [_-><] sequencies in the middle of the line
#         # x <- gsub("([_><-]{2,})"," ", x, perl = T)
#         
#         # Transfering the "<word><punctmark>"  into "<word> <punctmark>  "
#         x <- gsub(paste("([^[:blank:]])",punctmarkGroup,sep=""),"\\1 \\2", x, perl = T)
#         # Formating the end-of-sentence (characters ".!?")
#         x <- gsub(punctmarkGroup," \\1 ", x, perl = T)
#         # Putting "." at the end of the line (if not closed by author or before)
#         x <- gsub("([^,.!?]) +$","\\1 .",x)
#         # Trimming the excessive white spaces/tabs
#         x <- gsub("[[:blank:]]{2,}"," ", x, perl = T)
#         #Trimming the blanks et the end of line - not used as better for tokenizing
#         #x <- gsub("[[:blank:]]*$","", x, perl = T)
#         x
# }

for(i in seq_along(workTrainCorpus)) {
        workTrainCorpus[[i]]$content <- treatPunctuations(workTrainCorpus[[i]]$content)
}



removeNotLatinWords <- function(x) gsub("[\x21-\x7e]*[^\x20-\x7e]+[\x21-\x7e]*", "", x)

for(i in seq_along(workTrainCorpus)) {
        workTrainCorpus[[i]]$content <- removeNotLatinWords(workTrainCorpus[[i]]$content)
}


tidyTrainCorpus <- tm_map(workTrainCorpus, stripWhitespace)

writeCorpus(x = tidyTrainCorpus,path = file.path(tmpdir,"after"))

save(tidyTrainCorpus,file = file.path(tmpdir,"tidyTrainCorpus.Rdata"))

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

dt_bigrams <- data.table(freq = bigrams, 
                         predictor = gsub(" [^ ]*$","",names(bigrams)), 
                         outcome = gsub("^.* ","",names(bigrams))
)
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

dt_trigrams <- data.table(freq = trigrams, 
                          predictor = gsub(" [^ ]*$","",names(trigrams)), 
                          outcome = gsub("^.* ","",names(trigrams))
)
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

#Selecting only the wordpairs without separators and "'"
mask <- grepl("(^[.,!?]+[[:blank:]]|[[:blank:]][.,!?]+[[:blank:]]|[[:blank:]][.,!?]+[[:blank:]]*$)",names(four_grams))
four_grams <- four_grams[!mask]

dt_four_grams <- data.table(freq = four_grams, 
                            predictor = gsub(" [^ ]*$","",names(four_grams)), 
                            outcome = gsub("^.* ","",names(four_grams))
)
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

#Selecting only the wordpairs without separators and "'"
mask <- grepl("(^[.,!?]+[[:blank:]]|[[:blank:]][.,!?]+[[:blank:]]|[[:blank:]][.,!?]+[[:blank:]]*$)",names(pentagrams))
pentagrams <- pentagrams[!mask]

dt_pentagrams <- data.table(freq = pentagrams, 
                            predictor = gsub(" [^ ]*$","",names(pentagrams)), 
                            outcome = gsub("^.* ","",names(pentagrams))
)

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


#         # Concetps and algorithms for prediction model.
#         
#         The most simple prediction model at this point is using of the calculated and raked n-grams frequencies for generation 
# of the suggestions for the next word. There are some practical questions to be answered building the feasible practical 
# solution.
# 
# 
# 1. We will give the ranked by frequency lists of bi-, tri- and four-grams a trial as prediction mechanism.
# 2. All the technical routines for selection of the lists are as described in the source code (hidden) above.
# 2. We will probably need not only the in-line-prediction (word by word), but the in-word-prediction as well (predicting 
#                                                                                                              the ending of the word being just typed).
# 2. We need some fallback mechanisms for missing predictors in database, typos and so on.
# 1. The controversy between bias and variance of statistical model is ubiquitous. In our case we have data with 
# a maximal variance. It is a vivid mapping of the test data set, but probably some to vivid. That should be kept in 
# mind for advanced modeling.
# 2. The next omnipresent controversy is that between the statistical performance of the model and hardware performance of 
# implementation.
# 3. For the sake of technical performance (execution times meant) I have packed the word lists into indexed data tables, 
# each for a kind of n-grams.
# 3. The other facet of technical performance is memory consumption of the databases. It is definitely the matter for further 
# research. 
# 3. It is not only the capacity of hardware, limiting the depth of the prediction, but the willingness of the end user to scroll 
# through the suggestion lists. Already at this stage it makes no sense to plan delivering all `r max(bigrams)` variants for the 
# predictor "`r names(bigrams)[1]`" to the end user.
# 4. The limitations put by hardware and user interfaces of the gadgets like smartphone reinforce the considerations above.
# 5. We need for development much enough resources, to produce  the parsimonious and efficient end product. 
# 
# 






## Evaluating the possible memory savings through the limit of 30 words per predictor
## Maximal about 12% for the anyway smaller bigrams.

## Bigrams
# tb <- table(gsub(" [^ ]*$","",names(bigrams)))
# mask <- tb <= 30
# sum(!mask)/length(mask)
# rm(mask)
# 
# ## Trigrams
# tb <- table(gsub(" [^ ]*$","",names(trigrams)))
# mask <- tb <= 30
# sum(!mask)/length(mask)
# rm(mask)
# 
# ## Four-grams
# tb <- table(gsub(" [^ ]*$","",names(four_grams)))
# mask <- tb <= 30
# sum(!mask)/length(mask)


## Postponed as possible memory consumption optimizer
# ## Frequency of bigram predictors in the trigrams
# tb <- table(gsub(" [^ ]*$","",names(trigrams)))
# 
# mask <- tb > limit
# trimmed_trigramms <- trigrams[!mask]
# 
# for (pred in names(tb[mask])) {
#         print(pred)
#         #Using vector
#         #mask <- grepl(paste("^",pred,sep = ""), names(trigrams))
#        # trimmed_trigramms <-  c(trimmed_trigramms,trigrams[mask][1:limit])
#         # Using data table
#         trimmed_trigramms <-  c(trimmed_trigramms,dt_trigrams[pred,][1:limit])
# }
# dt_trigrams <- data.table(freq = trigrams, 
# predictor = gsub(" [^ ]*$","",names(trigrams)), 
# outcome = gsub("^.* ","",names(trigrams))
# )
# setkey(dt_trigrams,predictor)
# and so on...

## Reorganize the dt_words_EN
# invisible(gc())
# 
# used_words <- sort(wordFreqDescNorm[dt_words_EN[names(wordFreqDescNorm),]$words_EN], decreasing = T)
# rest_words <- dt_words_EN[!names(wordFreqDescNorm),]$words_EN
# dummies <- rep(0,length(rest_words))
# names(dummies) <- rest_words
# all_words <- c(used_words,dummies)
# nms <- names(all_words)
# 
# dt_words_EN <- data.table(freq = all_words, words_EN = nms)
# #dt_words_EN <- dt_words_EN %>% arrange(freq)
# setkey(dt_words_EN, words_EN)

# rm(wordFreqDescNorm)
# rm(used_words)
# rm(dummies)
# rm(rest_words)
# rm(all_words)
#rm(dt_words_tmp)
# invisible(gc())



## Possible savings through limiting of the redundance of the predictors to max. top 30

