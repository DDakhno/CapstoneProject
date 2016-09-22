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
system("free; uptime")

START <- as.numeric(Sys.time())

###########################################################################################
###########################################################################################
# Exploration data analysis
###########################################################################################
###########################################################################################

###########################################################################################
## Modelling relevant configuration
dictToLower <- TRUE
dictToTrimmComb <- TRUE

docsToLower <- dictToLower
docsToStem  <- FALSE
docsToTrimmComb <- dictToTrimmComb

prob_train <- .08 #train probability resp. all corpus lines
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


# rf <- file(result_file, "wa")
# write.table(t(data.table(prob_train,prob_test, prob_valid, dictToLower,dictToStem,dictToTrimmComb,docsToLower,
#                          docsToStem,docsToTrimmComb)),rf, col.names = F, quote = F)
# close(rf)
###########################################################################################


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

system.time(dictCorp <- Corpus(DirSource(endicts, encoding = "UTF-8", pattern = ".*.txt")))

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
system("free; uptime")

## Selecting english words from the dichtionary as data table is about FORTY times faster,
## than construct like wrds %in% words_EN

dt_words_EN <- data.table(freq = rep(-1,length(words_EN)), words_EN)
setkey(dt_words_EN, "words_EN")
#dt_stems_EN <- data.table(stems_EN)
#setkey(dt_stems_EN,"stems_EN")
#rm(list=c("words_EN","stems_EN"))
rm(list=c("words_EN"))
invisible(gc())
system("free; uptime")

#Building the main document corpus

## We should not read in the the memory huge files to use only a trickle, 
## so we implement some spare equivalent

setwd(file.path(datadir,"final/en_US"))
system("wc -l *.txt >fileinfo.log")
fileinfo <- read.table("fileinfo.log")
system("rm fileinfo.log")

#Partitioning the data into train/test/validation

system("free; uptime")

ndoc <- nrow(fileinfo)-1
trainCorpus <- Corpus(VectorSource(1:ndoc))
# testCorpus <- Corpus(VectorSource(1:ndoc))
# validCorpus <- Corpus(VectorSource(1:ndoc))

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

system("free; uptime")

#Inspecting corpora
object.size(trainCorpus)/(1024*1024)
inspect(trainCorpus)
# inspect(testCorpus)
# inspect(validCorpus)

#######################################################################################################
# Preprocessing the work data set
#######################################################################################################


wordsnogo <- c("the","a","an") 

trList <- list(
        removeNumbers,
        stripWhitespace
)

system.time(workTrainCorpus <- tm_map(trainCorpus, tm_reduce, trList))
rm(trainCorpus)
invisible(gc())
system("free; uptime")

if (docsToLower) workTrainCorpus <- tm_map(workTrainCorpus, content_transformer(tolower))

workTrainCorpus <- tm_map(workTrainCorpus, removeWords, wordsnogo)

punctmarks <- "[,.!?]"
vectPM <- c(".",",","!","?")
punctmarkGroup <- "([,.!?;()])"

## All printable ASCII characters

## gsub("[\x21-\x7e]","","abZ{")

treatPucntuations <- function(x) {
        # Removing quotes of different kinds
        x <- gsub("(\xe2\x80\x9c|\xe2\x80\x9d|\x22)","",x)
        
        # Substituting punctuation characters at the end of the line with " ."
        x <- gsub(paste(punctmarks,"{2,}[[:blank:]]*$",sep="")," .", x, perl = T)
        
        # Removing the longer [.,_-><] sequencies in the middle of the line
        x <- gsub("([.,_><-]{2,})"," ", x, perl = T)
        
        # Transfering the "<word><punctmark>"  into "<word> <punctmark>  "
        x <- gsub(paste("([^[:blank:]])",punctmarkGroup,sep=""),"\\1 \\2", x, perl = T)
        # Formating the end-of-sentence (characters ".!?")
        x <- gsub(punctmarkGroup," \\1 ", x, perl = T)
        # Putting "." at the end of the line (if not closed by author or before)
        x <- gsub(paste(punctmarkGroup,"[[:blank:]]*$",sep=""),"\\1 . ", x, perl = T)
        # Trimming the excessive white spaces/tabs
        x <- gsub("[[:blank:]]{2,}"," ", x, perl = T)
        #Trimming the blanks et the end of line - not used as better for tokenizing
        #x <- gsub("[[:blank:]]*$","", x, perl = T)
        x
}

workTrainCorpus <- tm_map(workTrainCorpus, content_transformer(treatPucntuations))

## Removing words not in the EN dictionary

filterNotLatinWords <- function(x) gsub("[^[:blank:]]*[^\x20-\x7e]+[^[:blank:]]*", "", x)

workTrainCorpus <- tm_map(workTrainCorpus, content_transformer(filterNotLatinWords))

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

tidyTrainCorpus <- VCorpus(VectorSource(1:length(workTrainCorpus)))

for (i in 1:length(workTrainCorpus)) {
        tidyTrainCorpus[[i]]$content <- pickOnlyEnglishWrds(workTrainCorpus[[i]]$content)
}

rm(workTrainCorpus)
invisible(gc())
system("free; uptime")


if (docsToStem) {
        system.time(tidyTrainCorpus <- tm_map(tidyTrainCorpus, stemDocument))
}
if (docsToTrimmComb) tidyTrainCorpus <- tm_map(tidyTrainCorpus, content_transformer(trimmCombinations))

#Creating Term-Document Matrices

dtmTr <- DocumentTermMatrix(x = tidyTrainCorpus)
dim(dtmTr)
object.size(dtmTr)

#Normalizing the frequencies in the matrix
cnt <- sum(sum(dtmTr))
dtmTr <- dtmTr/cnt

#Single word frequency, descending
wordFreqDescNorm <- sort(colSums(as.matrix(dtmTr)), decreasing = T)

length(wordFreqDescNorm)

paste(cat(names(wordFreqDescNorm[1:20]), sep = ","))

## Twenty percent of the words/stems are only the first twenty seven in the list.
## Less than 300 word stemms constitute the half of the word incidence
sum(cumsum(wordFreqDescNorm) <= .1)
sum(cumsum(wordFreqDescNorm) <= .2)
sum(cumsum(wordFreqDescNorm) <= .5)
sum(cumsum(wordFreqDescNorm) <= .9)

##Plotting the frequencies

plot(x = 1:300, y = wordFreqDescNorm[1:300]*100, type = "s",main = "Frequencies of the top-300 english words", 
     xlab = "Index of word by frequency (desc.)", ylab = "Frequency,%", frame.plot = T)


## Word associations - dropped for the reasons of perfrormance (exec.times) and restricted
## usability for the given use case

#system.time(assocTop300 <- findAssocs(dtmTr, names(wordFreqDescNorm[1:300]), corlimit=0.9))


################################################################################################
# Clustering by Term Similarity
################################################################################################

## Hierarchal Clustering

## Filtering out the sparse words 

dtmTr <- removeSparseTerms(dtmTr, sparse = .34)
dtmTr100 <- dtmTr[,(colnames(dtmTr) %in% names(wordFreqDescNorm[1:100]))]
dtmTr50 <- dtmTr[,(colnames(dtmTr) %in% names(wordFreqDescNorm[1:50]))]
d <- dist(t(dtmTr100), method="euclidian")   
fit <- hclust(d=d, method="ward.D2")   
fit 
plot(fit, hang=-1)
groups <- cutree(fit, k=5)
rect.hclust(fit, k=5, border="red")

dtmTr50 <- dtmTr[,(colnames(dtmTr) %in% names(wordFreqDescNorm[1:50]))]
d <- dist(t(dtmTr50), method="euclidian")   
fit <- hclust(d=d, method="ward.D2")   
fit 
plot(fit, hang=-1)
groups <- cutree(fit, k=5)
rect.hclust(fit, k=5, border="red")


## K-means clustering

library(fpc)   
d <- dist(t(dtmTr50), method="euclidian")   
kfit <- kmeans(d, 3)   
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)  

rm(dtmTr)
invisible(gc())

################################################################################################
# Word clouds
################################################################################################


set.seed(142)   
wordcloud(names(wordFreqDescNorm), wordFreqDescNorm, min.freq=.001, scale=c(5, .1), colors=brewer.pal(6, "Dark2")) 


################################################################################################
# Word pairs
################################################################################################

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

rm(mask)
invisible(gc())

## The top-20 two-word-combinations are

bigrams[1:20]

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

rm(mask)
invisible(gc())

## The top-20 three-word-combinations are

trigrams[1:20]

## The same for four words

four_grams <- textcnt(c(tidyTrainCorpus[[1]]$content, tidyTrainCorpus[[2]]$content, 
                        tidyTrainCorpus[[3]]$content), method = "string", split = " ", n = 4L)
four_grams <- unclass(four_grams)

#Selecting only the wordpairs without separators and "'"
mask <- grepl(paste("(",punctmarks,"|['*+])",sep=""),names(four_grams))
four_grams <- four_grams[!mask]
four_grams <- sort(four_grams, decreasing = T)

rm(mask)
invisible(gc())

rm(tidyTrainCorpus)
invisible(gc())


## The top-20 four-word-combinations are

four_grams[1:20]


## Comparative statistical perfrormance of n-grams

## Taking the an n-gram for the "outcome", we can define the n-gram without a last word as "predictor"
## (e.g. "go to" is a predictor for "go to bed"). Using this approach we try to explore the potential
## statistical performance of predictors of the length 1, 2 and 3 (bi-, tri- and four-grams used respectively).


par(mfrow = c(1,1))


plot_predictor_performance <- function(ngramms) {
        clrs <- c("blue","red", "green")
        ngr_names <- c("monograms","bigrams","trigrams","four-grams")
        j <- 1
        for (ngr in ngramms) {
                
                predictors <- ngr
                names(predictors) <- gsub(" [^ ]+$","",names(predictors))
                top50predictors <- sort(table(names(predictors)),decreasing = T)[1:50]
                maxx <- max(top50predictors)
                cummtrx <- matrix(nrow = maxx, ncol = 50)
                cnt <- 1
                for (pred in names(top50predictors)) {
                        pred <- paste("^",pred,"$",sep = "", collapse = "")
                        mask <- grepl(pred, names(predictors))
                        pred_frq <- predictors[mask]
                        rm(mask)
                        cummtrx[,cnt] <- c(pred_frq, rep(x = 0, times = maxx - length(pred_frq)))
                        cnt <- cnt + 1
                }
                colnames(cummtrx) <- gsub(" [^ ]+$","",names(top50predictors))
                clr <- clrs[j]
                rS <- rowSums(cummtrx)
                cS <- cumsum(rS)
                fullsum <- sum(rS)
                plot(cS, col = clr , type = 'l', 
                     main = paste("Predicting for Top-50",ngr_names[j+1]),
                     sub = paste("First",sub("s$","",ngr_names[j]),"as predictor"),
                     xlab = paste("Avg. ranking over the top-50 ",ngr_names[j]),
                     ylab = paste("Total nr. of observations"))
                abline(a = fullsum*.5, b = 0, lty = "dashed", lwd = .5)
                abline(a = fullsum*.95, b = 0, lty = "dashed", lwd = .5)
                thrshld <- max(cumsum(rS)[cumsum(rS)/sum(rS) <= .95])
                thrshld <- which(cS == thrshld)
                abline(v = thrshld, b = 1, lty = "dashed", lwd = .5)
                thrshld <- max(cumsum(rS)[cumsum(rS)/sum(rS) <= .5])
                thrshld <- which(cS == thrshld)
                abline(v = thrshld, b = 1, lty = "dashed", lwd = .5)
                j <- j + 1
        }
}

plot_predictor_performance(list(bigrams, trigrams, four_grams))

#############################################################################################
#############################################################################################
# Prototyping the first predicting enginge
#############################################################################################
#############################################################################################

## It is the right place to answer the questions relating the technical performance and reasonable
## hardware ressource consumption of the prediction engine

## Possible savings throufh limiting of the redundance of the predictors to max. top 30

limit <- 30

## In-word prediction db

dt_inWord <- data.table(wordFreqDescNorm, names(wordFreqDescNorm))

## Bigrams
tb <- table(gsub(" [^ ]*$","",names(bigrams)))
mask <- tb <= 30
sum(!mask)/length(mask)
rm(mask)

## Trigrams
tb <- table(gsub(" [^ ]*$","",names(trigrams)))
mask <- tb <= 30
sum(!mask)/length(mask)
rm(mask)

## Four-grams
tb <- table(gsub(" [^ ]*$","",names(four_grams)))
mask <- tb <= 30
sum(!mask)/length(mask)

rm(tb, mask)
invisible(gc())


dt_bigrams <- data.table(freq = bigrams, 
                         predictor = gsub(" [^ ]*$","",names(bigrams)), 
                         outcome = gsub("^.* ","",names(bigrams))
)
setkey(dt_bigrams,predictor)

rm(bigrams)
invisible(gc())

dt_trigrams <- data.table(freq = trigrams, 
                          predictor = gsub(" [^ ]*$","",names(trigrams)), 
                          outcome = gsub("^.* ","",names(trigrams))
)
setkey(dt_trigrams,predictor)

rm(trigrams)
invisible(gc())

dt_four_grams <- data.table(freq = four_grams, 
                            predictor = gsub(" [^ ]*$","",names(four_grams)), 
                            outcome = gsub("^.* ","",names(four_grams))
)

rm(four_grams)
invisible(gc())

setkey(dt_four_grams,predictor)

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

## Reorganize the dt_dt_words_EN

used_words <- sort(wordFreqDescNorm[dt_words_EN[names(wordFreqDescNorm),]$words_EN], decreasing = T)
rest_words <- dt_words_EN[!names(wordFreqDescNorm),]$words_EN

rm(wordFreqDescNorm)
invisible(gc())

nulls <- rep(0,length(rest_words))
names(nulls) <- rest_words

rm(rest_words)
invisible(gc())

all_words <- c(used_words, nulls)

rm(nulls, used_words)
invisible(gc())

dt_words_EN <- data.table(freq = all_words, words_EN = names(all_words))
setkey(dt_words_EN, words_EN)


rm(all_words)
invisible(gc())


predictEngine <- function(xpr) {
        limit <- 30
        wlimit <- 3
        rtrn <- data.frame() #as default an empty data frame (nothing found)
        if (Encoding(xpr) == "unknown") {  #we don't deal with the foreign and mixed inputs
                #Normalizing expression
                xpr <- gsub("[[:blank:]]+"," ", xpr) # "red        grape" -> "red grape"
                xpr <- gsub("(^[[:blank:]]+|[[:blank:]]+$)","", xpr) # "  red grape    " -> "red grape"
                
                xpr <- sub(paste("^.*",punctmarks,"([[:blank:]]+|$)", sep=""),"",xpr) # "red . grape" > ""grape"
                #we begin from scratch after the punctuation characters like .,?!
                
                lgth <- length(strsplit(xpr, split = "[[:blank:]]")[[1]])
                if (lgth == 1) rtrn <- na.omit(dt_bigrams[xpr,][1:limit])
                if (lgth == 2) rtrn <- na.omit(dt_trigrams[xpr,][1:limit])
                if (lgth == 3) rtrn <- na.omit(dt_four_grams[xpr,][1:limit])
                
                ## Nothing found in the database :((
                if (nrow(rtrn) == 0 ) { 
                        
                        ## Fallback from n-gram to (n-1)-gram like - The last resort is suggesting the top-30 words form the common list.
                        rtrn <- data.frame()
                        while (nrow(rtrn) == 0 && lgth > 1) {
                                #remove the first word from expression
                                xpr <- sub("^[[:blank:]]*[^[:blank:]]+[[:blank:]]*", "", xpr)
                                lgth <- length(strsplit(xpr, split = "[[:blank:]]")[[1]])
                                rtrn <- predictEngine(xpr)
                        } 
                        
                        ## Last word standing, still no result...
                        if (nrow(rtrn) == 0 ) { 
                                rtrn <- inWordPredictEngine(xpr)
                                if (nrow(rtrn) != 0){
                                        rtrn <- na.omit(dt_bigrams[rtrn$words_EN,][1:limit]) # Yes, we can!
                                }
                        }
                        rtrn
                        
                } else { rtrn }
                
        } else { rtrn } # No prediction for chinese, russian, hebrew and the rest strange languages!
}

inWordPredictEngine <- function(xpr) {
        rtrn = data.frame()
        ## Use case #1 - a word is in process of typing, so we could probably recognize it by the stem
        
        rtrn <- dt_inWord %>% filter(V2 %like% paste("^",xpr, sep = ""))
        
        ## Use case #2 - the word is probably too long. Reducing in steps up to length wlimit 
        if (nrow(rtrn) == 0) {
                while(stri_length(xpr) >= wlimit && nrow(rtrn) == 0) {
                        rtrn <- na.omit(dt_words_EN[xpr,])
                        xpr <- gsub(".$","",xpr)
                }
        }
        rtrn
}
#######################################################################################
##Total execution time


##Total execution time

as.numeric(Sys.time()) - START

system("free; uptime")

#######################################################################################
#######################################################################################
# Building the application
#######################################################################################
#######################################################################################

## Decisions

# Only the Wordnet dictionary (performance - memory!)
# Early filtering away the notn-english stems (performance - memory!)