suppressMessages(library(tm))
library(stringi)
suppressMessages(library(data.table))
suppressMessages(library(dplyr))
suppressMessages(library(wordcloud))
library(cluster)
library(tau)
 
#Cleaning the workspace
rm(list = ls())
invisible(gc())


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

## Technical configuration
homedir <- "/home/d/R-Work/CapstoneProject"
datadir <- file.path(homedir,"data")
tmpdir <- paste(homedir,"/tmp",prob_train, sep = "")
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

if (docsToLower) workTrainCorpus <- tm_map(workTrainCorpus, content_transformer(tolower))

workTrainCorpus <- tm_map(workTrainCorpus, removeWords, wordsnogo)

punctmarks <- "[,.!?]"
vectPM <- c(".",",","!","?")
punctmarkGroup <- "([,.!?])"

## All printable ASCII characters
## gsub("[\x21-\x7e]","","abZ{")

treatPucntuations <- function(x) {
        # Removing quotes of different kinds
        # x <- gsub("(\xe2\x80\x9c|\xe2\x80\x9d|\x22)","",x)
        
        # Substituting punctuation characters at the end of the line with " ."
        x <- gsub(paste(punctmarks,"{2,}[[:blank:]]*$",sep="")," .", x, perl = T)
        
        # Trimming the longer sequences of punctuation characters in the middle of the line 
        #  x <- gsub(paste(punctmarkGroup,"{2,}",sep="")," . ", x, perl = T)
        
        # Removing the longer [_-><] sequencies in the middle of the line
        # x <- gsub("([_><-]{2,})"," ", x, perl = T)
        
        # Transfering the "<word><punctmark>"  into "<word> <punctmark>  "
        x <- gsub(paste("([^[:blank:]])",punctmarkGroup,sep=""),"\\1 \\2", x, perl = T)
        # Formating the end-of-sentence (characters ".!?")
        x <- gsub(punctmarkGroup," \\1 ", x, perl = T)
        # Putting "." at the end of the line (if not closed by author or before)
        x <- gsub("([^,.!?]) +$","\\1 .",x)
        # Trimming the excessive white spaces/tabs
        x <- gsub("[[:blank:]]{2,}"," ", x, perl = T)
        #Trimming the blanks et the end of line - not used as better for tokenizing
        #x <- gsub("[[:blank:]]*$","", x, perl = T)
        x
}

workTrainCorpus <- tm_map(workTrainCorpus, content_transformer(treatPucntuations))

workTrainCorpusBak <- workTrainCorpus

workTrainCorpus <- workTrainCorpusBak

## Removing words not in the EN dictionary (with the characters outside the ASCII span "-" 
## over "a-zA-Z" upto "~")
cntinitial <- 0
for (i  in length(workTrainCorpus)) {
        for (l in workTrainCorpus[[i]]$content) cntinitial <- cntinitial + length(strsplit(l, split = "[[:blank:]]+")[[1]])
}


## It is probably more reasonable to throw away the whole line, containing 
## the non-latin characters/bytes?

filterNotLatinWords <- function(x) gsub("[[:blank:]]*[^[:blank:]]*[^\x20-\x7e][^[:blank:]]*", "", x)

for (i in 1:length(workTrainCorpus)) {
        workTrainCorpus[[i]]$content <- filterNotLatinWords(workTrainCorpus[[i]]$content)
}

cntlatin <- 0
for (i  in length(workTrainCorpus)) {
        for (l in workTrainCorpus[[i]]$content) cntlatin <- cntlatin + length(strsplit(l, split = "[[:blank:]]+")[[1]])
}


tidyTrainCorpus <- VCorpus(VectorSource(1:length(workTrainCorpus)))

pickOnlyEnglishWrds <- function(x) {
        rslt <- c()
        for (lin in x)  {
                wrds <- strsplit(lin, split = "[[:blank:]]+")[[1]]
                mask <- wrds %in% vectPM | !is.na(dt_words_EN[wrds,]$freq)
                y <- paste(wrds[mask], collapse = " ")
                y <- gsub(paste("(",punctmarks,")","([[:blank:]]*\\1)*",sep="")," . ",y)
                rslt <- c(rslt,y)
                
        }
        rslt
}

# for (i in 1:length(workTrainCorpus)) {
#         tidyTrainCorpus[[i]]$content <- pickOnlyEnglishWrds(workTrainCorpus[[i]]$content)
# }

# cnteng <- 0
# for (i  in length(workTrainCorpus)) {
#         for (l in workTrainCorpus[[i]]$content) cnteng <- cnteng + length(strsplit(l, split = "[[:blank:]]+")[[1]])
# }

tidyTrainCorpus <- workTrainCorpus

rm(workTrainCorpus)
invisible(gc())

## Monograms 
## Library tau

monograms <- textcnt(c(tidyTrainCorpus[[1]]$content, tidyTrainCorpus[[2]]$content, 
                       tidyTrainCorpus[[3]]$content), method = "string", split = " ", n = 1L)
monograms <- unclass(monograms)
#Selecting only the wordpairs without separators and "'"
mask <- grepl(punctmarks,names(monograms))
monograms <- monograms[!mask]
mask <- grepl("['*+]",names(monograms))
monograms <- monograms[!mask]
monograms <- sort(monograms, decreasing = T)
wordFreqDescNorm <- monograms #for code compatbility reasons
dt_monograms <- data.table(freq = monograms, word = names(monograms))
setkey(dt_monograms, word)
invisible(gc())

## Top-20 single words

round(wordFreqDescNorm[1:20]*100,4)

sum(cumsum(wordFreqDescNorm) <= .1)
sum(cumsum(wordFreqDescNorm) <= .2)
sum(cumsum(wordFreqDescNorm) <= .5)
sum(cumsum(wordFreqDescNorm) <= .9)

## Bigrams 
## Library tau

bigrams <- textcnt(c(tidyTrainCorpus[[1]]$content, tidyTrainCorpus[[2]]$content, 
                     tidyTrainCorpus[[3]]$content), method = "string", split = " ", n = 2L)
bigrams <- unclass(bigrams)

#Selecting only the wordpairs without separators and "'"
mask <- grepl(punctmarks,names(bigrams))
bigrams <- bigrams[!mask]
mask <- grepl("['*+]",names(bigrams))
bigrams <- bigrams[!mask]
bigrams <- sort(bigrams, decreasing = T)

dt_bigrams <- data.table(freq = bigrams, 
                         predictor = gsub(" [^ ]*$","",names(bigrams)), 
                         outcome = gsub("^.* ","",names(bigrams))
)
setkey(dt_bigrams,predictor)

save(bigrams, file = file.path(tmpdir,"bigrams.Rdata"))
rm(bigrams)
rm(mask)
invisible(gc())

## Bigrams, echo=T, comment='', collapse=T}
head(dt_bigrams %>% arrange(desc(freq)),20)

## Trigrams 
## The same for word triplets

trigrams <- textcnt(c(tidyTrainCorpus[[1]]$content, tidyTrainCorpus[[2]]$content, 
                      tidyTrainCorpus[[3]]$content), method = "string", split = " ", n = 3L)
trigrams <- unclass(trigrams)

#Selecting only the wordpairs without separators and "'"
mask <- grepl(punctmarks,names(trigrams))
trigrams <- trigrams[!mask]
mask <- grepl("['*+]",names(trigrams))
trigrams <- trigrams[!mask]
trigrams <- sort(trigrams, decreasing = T)

dt_trigrams <- data.table(freq = trigrams, 
                          predictor = gsub(" [^ ]*$","",names(trigrams)), 
                          outcome = gsub("^.* ","",names(trigrams))
)
setkey(dt_trigrams,predictor)
save(trigrams, file = file.path(tmpdir,"trigrams.Rdata"))
rm(trigrams)
rm(mask)
invisible(gc())

##
head(dt_trigrams %>% arrange(desc(freq)),20)


## Four_grams echo=F}
## The same for four words

four_grams <- textcnt(c(tidyTrainCorpus[[1]]$content, tidyTrainCorpus[[2]]$content, 
                        tidyTrainCorpus[[3]]$content), method = "string", split = " ", n = 4L)
four_grams <- unclass(four_grams)

#Selecting only the wordpairs without separators and "'"
mask <- grepl(paste("(",punctmarks,"|['*+])",sep=""),names(four_grams))
four_grams <- four_grams[!mask]
four_grams <- sort(four_grams, decreasing = T)

dt_four_grams <- data.table(freq = four_grams, 
                            predictor = gsub(" [^ ]*$","",names(four_grams)), 
                            outcome = gsub("^.* ","",names(four_grams))
)
setkey(dt_four_grams, predictor)

save(four_grams, file = file.path(tmpdir,"four_grams.Rdata"))
rm(four_grams)
rm(mask)
invisible(gc())

head(dt_four_grams %>% arrange(desc(freq)),20)

## Pentagrams
## The same for five words

pentagrams <- textcnt(c(tidyTrainCorpus[[1]]$content, tidyTrainCorpus[[2]]$content, 
                        tidyTrainCorpus[[3]]$content), method = "string", split = " ", n = 5L)
pentagrams <- unclass(pentagrams)

#Selecting only the wordpairs without separators and "'"
mask <- grepl(paste("(",punctmarks,"|['*+])",sep=""),names(pentagrams))
pentagrams <- pentagrams[!mask]
pentagrams <- sort(pentagrams, decreasing = T)

dt_pentagrams <- data.table(freq = pentagrams, 
                            predictor = gsub(" [^ ]*$","",names(pentagrams)), 
                            outcome = gsub("^.* ","",names(pentagrams))
)

setkey(dt_pentagrams,predictor)

save(pentagrams, file = file.path(tmpdir,"pentagrams.Rdata"))
rm(pentagrams)
rm(mask)
rm(tidyTrainCorpus)
invisible(gc())

## Top-20 pentagrams
head(dt_pentagrams %>% arrange(desc(freq)),20)


#The total number of  unique     bigrams         
length(bigrams)
#The total number of  unique    trigrams:       
length(trigrams)  
#The total number of  unique    four-grams:    
length(four_grams)  
#The total number of  unique    pentagrams:      
length(pentagrams)


        
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
# ----------------------------------------------------------------------------------  
#         
#         # Quick and plain prototype of prediction engine
#         
#         The questions posed above are to intriguing to wait with the answers. Experiences gathered in the former stage oblige.
# 
# Here is a demonstration of the prediction engine prototype as built in a short time.  
# 
# - The order of evaluation of predictor against database is sequence of 3, then 2 and 1 words.
# - The lower limit for in-word completion set at 3 characters.
# - The upper limit for the number of suggestions set at 30 words.
# - As a fallback for predictor missed in the database is used a derivate, shortened by one word at front 
# ("nice moonlight evening" -> "moonlight evening"). The procedure is iterated repeated when needed.
# - The in-word prediction works with the same list, allowing the completion of the short chunks.
# - If the char sequence can not be found in the word list, it is iterative shortened and compared against the 
# database.
# - The last resort is suggesting the top-30 words form the common list.
# - Right after the punctuation marks come no suggestions, the play begins from scratch with the first word.
# - The incredible performance of the indexed data tables and dplyr package are extensively used.
# - Actual output of the engine includes the data like frequency, the predictor and suggestion (last column).





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
invisible(gc())

used_words <- sort(wordFreqDescNorm[dt_words_EN[names(wordFreqDescNorm),]$words_EN], decreasing = T)
rest_words <- dt_words_EN[!names(wordFreqDescNorm),]$words_EN
dummies <- rep(0,length(rest_words))
names(dummies) <- rest_words
all_words <- c(used_words,dummies)
nms <- names(all_words)

dt_words_EN <- data.table(freq = all_words, words_EN = nms)
#dt_words_EN <- dt_words_EN %>% arrange(freq)
setkey(dt_words_EN, words_EN)

# rm(wordFreqDescNorm)
# rm(used_words)
# rm(dummies)
# rm(rest_words)
# rm(all_words)
#rm(dt_words_tmp)
# invisible(gc())


## Possible savings through limiting of the redundance of the predictors to max. top 30

linesLimit <- 30
wordLengthLimit <- 3

unstemmExprToRegex <- function(x) {
        
        wrds <- strsplit(x, split = "[[:blank:]]+")[[1]]
        wrds
        wrds <- sapply(wrds, stemDocument)
        paste("^",paste(wrds,  collapse = "[^[:blank:]]* "),sep = "", collapse = "")
}

predictEngine <- function(xpr) {
        
        max_words = 4 #Maximal length of the predictor
        
        rtrn <- data.frame() #as default an empty data frame (nothing found)
        
        condExpr <- function(xpr) {
                
                ## Essentially the function treatPucntuations used for the main text processing before
                xpr <- treatPucntuations(xpr)
                
                # Trimming to  the last punctuation mark : "red ., grape" -> "grape"
                #we begin from scratch after the punctuation characters like .,?!
                xpr <- gsub("([ .])+$","",xpr, perl = T)
                
                #Remove the same words ("wordsnogo") as in the previously produced text (articles)
                rgxpr <- paste(" +(",paste(wordsnogo,sep="|", collapse="|"),") +",sep="")
                xpr <- gsub(rgxpr," ",xpr)
                #To lower
                xpr <- tolower(xpr)
                # Removing blanks at front and back
                xpr <- gsub("(^[[:blank:]]+|[[:blank:]]+$)","",xpr)
                # Leaving max max_words in predictor (4 by now)
                tt <- stri_split(xpr, regex = "[[:blank:]]+")[[1]]
                lng <- length(tt)
                if (lng > max_words) {
                        xpr <- paste(tt[lng-3],tt[lng-2],tt[lng-1],tt[lng], collapse = " ")
                } else {
                        xpr <- paste(tt, collapse = " ")
                }
                
                gsub(rgxpr," ",xpr)
                
        }
        
        removeFirstWord <- function(xpr) {
                tt <- stri_split(xpr, regex = "[[:blank:]]+")[[1]]
                lng <- length(tt)
                if (lng > 1) {
                        xpr <- paste(tt[2:lng], collapse = " ")
                }
                xpr
        }
        
        lastWord <- function(xpr) {
                gsub("^.*  *","",condExpr(xpr))
        }
        lastButOneWord <- function(xpr) {
                tt <- stri_split(xpr, regex = "[[:blank:]]+")[[1]]
                lng <- length(tt)
                if (lng > 1) {
                        xpr <- paste(tt[lng-1], collapse = " ")
                }
                xpr
        }
        
        askDB <- function(xpr) {
                xpr <- condExpr(xpr)
                lng <- stri_count(xpr, fixed = " ")+1
                
                if (lng == 1) rtrn <- na.omit(dt_bigrams[xpr,])
                if (lng == 2) rtrn <- na.omit(dt_trigrams[xpr,])
                if (lng == 3) rtrn <- na.omit(dt_four_grams[xpr,])
                if (lng == 4) rtrn <- na.omit(dt_pentagrams[xpr,])
                head(arrange(rtrn,desc(freq)),linesLimit)
        }
        
        xpr <- xprOrig <- condExpr(xpr)
        
        if (xpr != "") {
                #we don't deal with the foreign and mixed inputs
                if (Encoding(xpr) == "unknown") {  
                        
                        rtrn <- askDB(xpr)
                        
                        ## Nothing exact found in the database :((
                        
                        # Trying to reduce the ONLY the last word of preidctor to the reasonabe( (?): "aa bb ccc" -> "aa bb cc" -> "aa bb c"
                        if (nrow(rtrn) == 0 ) { 
                                xpr <- xprOrig
                                lng <- lngOrig <- stri_count(xpr, fixed = " ")+1
                                
                                # Condition Original number of words &&  more than one word (see in-word-prediction)
                                while (lng > 1 && lng == lngOrig && nrow(rtrn) == 0) {
                                        xpr <-gsub(".$","",xpr)
                                        xpr <- condExpr(xpr)
                                        lng <- stri_count(xpr, fixed = " ",case_insensitive = T)+1
                                        if (lng == lngOrig)
                                                rtrn <- askDB(xpr)
                                } 
                                
                        }
                        
                        ## Still nothing after shortening the last one.
                        ## Fallback from n-gram to (n-1)-gram like - "aa bb cc" -> "bb cc"
                        ## Original predictor value!
                        
                        if (nrow(rtrn) == 0) {
                                #beginnig from scratch
                                xpr <- xprOrig 
                                
                                if (xpr != "") {
                                        lng <- stri_count(xpr, fixed = " ", case_insensitive = T)+1
                                        
                                        if (lng > 1) {
                                                while (lng > 1 && nrow(rtrn) == 0) {
                                                        #remove the first word from expression : "aa bb cc" -> "bb cc"
                                                        xpr <- removeFirstWord(xpr)
                                                        lng <- stri_count(xpr, fixed = " ",case_insensitive = T)+1
                                                        if (lng > 1) {
                                                                rtrn <- askDB(xpr)
                                                        } 
                                                }
                                        } 
                                }
                        }
                        xpr <- xprOrig
                        lng <- stri_count(xpr, fixed = " ",case_insensitive = T)+1
                        
                        # Falling back to one - last word in expression
                        if (nrow(rtrn) == 0) rtrn <- askDB(lastWord(xpr))
                        
                        # Still nothing...
                        if (nrow(rtrn) == 0) rtrn <- askDB(inWordPredictEngine(lastWord(xpr))[[2]][1])
                        # A nonsense last word - trying last but one.
                        if (nrow(rtrn) == 0) rtrn <- askDB(inWordPredictEngine(lastButOneWord(xpr))[[2]][1])
                        # .......((((
                        if (nrow(rtrn) == 0) rtrn <- dt_words_EN[1:linesLimit,]
                        
                }
                rtrn
        } else { rtrn } # No prediction for empty strings, chinese, russian, hebrew and the rest of strange languages!
}        

inWordPredictEngine <- function(xpr) {
        
        rtrn = data.frame()
        if (stri_length(xpr) > 0) {
                ## Use case #1 - a word is in process of typing - so to short, we could probably recognize it by the stem
                rtrn <- dt_words_EN %>% filter(words_EN %like% paste("^",xpr, sep = ""))
                
                ## Use case #2 - the word is probably too long. Reducing in steps up to length wordLengthLimit 
                if (nrow(rtrn) == 0) {
                        while(stri_length(xpr) >= wordLengthLimit && nrow(rtrn) == 0) {
                                xpr <- gsub(".$","",xpr)
                                xpr
                                rtrn <- na.omit(dt_words_EN[xpr,])
                                rtrn
                        }
                        
                        # Absolutely nothing found  -> simply give the list of the most top-[limit]
                        if (nrow(rtrn) == 0){
                                rtrn <- dt_words_EN[1:linesLimit,] # Yes, we can! Even, if these are the trivials.
                        }
                }
        }
        else {
                rtrn <- dt_words_EN[1:linesLimit,] # Yes, we can! Even, if these are the trivials.
        }
        rtrn
}


## Short engine tests

## Testing the prediciton prototype, 
## We are probably not perfect yet ;-)

#Normal case
system.time(rslt <- predictEngine("red"))
head(rslt)

#Normal case
system.time(rslt <- predictEngine("it was"))
head(rslt)

# Predictor not found in DB - shortening the second word
system.time(rslt <- predictEngine("it readd"))
head(rslt)

# Control for the second part
system.time(rslt <- predictEngine("readdd"))
head(rslt)

# Predictor ends with punctuation mark - nothing to be returned
system.time(rslt <- predictEngine("red river ,"))
head(rslt)

# After foreign words no suggestions delivered
system.time(rslt <- predictEngine("red уменьшению"))
head(rslt)



