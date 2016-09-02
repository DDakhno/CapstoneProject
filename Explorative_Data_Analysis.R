library(tm)
library(stringi)
library(wordnet)
library(data.table)

#Cleaning the workspace
rm(list = ls())


START <- as.numeric(Sys.time())

###############################################################################
## Modelling relevant configuration
dictToLower <- TRUE
dictToStem  <- TRUE
dictToTrimmComb <- TRUE

docsToLower <- dictToLower
docsToStem  <- dictToStem
docsToTrimmComb <- dictToTrimmComb

prob_train <- .04 #train to the whole corpus
prop_test <- .5  #test to test+validation
homedir <- "/home/d/R-Work/CapstoneProject"
datadir <- paste(homedir,"data",sep="/")
tmpdir <- paste(homedir,"/tmp",prob_train, sep = "")
endicts <- paste(datadir,"/en_dicts", sep = "")
scowldir <- paste(datadir,"SCOWL",sep = "/")
projectcorpURL <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
projectcorpFile <- rev(stri_split(str = projectcorpURL,fixed = "/")[[1]])[1]
result_file <- paste(tmpdir,"results.log",sep="/")


rf <- file(result_file, "wa")
write.table(t(data.table(prob_train,dictToLower,dictToStem,dictToTrimmComb,docsToLower,
                    docsToStem,docsToTrimmComb)),rf, col.names = F, quote = F)
close(rf)
###############################################################################

setwd(homedir)

if (!dir.exists(paste(tmpdir,"before",sep="/"))) {
    dir.create(paste(tmpdir,"before",sep="/"),recursive = T)
    dir.create(paste(tmpdir,"after",sep="/"),recursive = T)
}

if (!dir.exists(endicts)) dir.create(endicts,recursive = T)
if (!dir.exists(scowldir))    dir.create(scowldir,recursive = T)

## Check, if the project data file is here; if not, download and unpack

if (!dir.exists(paste(datadir,"final/en_US"))) {
    setwd(datadir)
    projectcorpFile <- paste(datadir,projectcorpFile,sep="/")
    if (!file.exists(projectcorpFile))
        download.file(url = projectcorpURL,destfile = projectcorpFile,mode = "wb")
        engfiles <- grep("en_US.*txt",unzip(zipfile = projectcorpFile, list = T)[[1]],value = T)
        unzip(zipfile = projectcorpFile, files = engfiles)
}

# Wordnet requests are not performant enough for verification of the really long
# lists over the standard R interface. We make brute force hacking of wordnet lists
# at the UNIX-console

system(paste("grep -vh '^ ' /usr/share/wordnet/index*| cut -f1 -d' '| sort -u > ",endicts,"/dict_wordnet.txt",sep=""))

## We use the SCOWL english word lists as a concurrent english word list


scowlgz <- paste(scowldir,"scowl-2016.06.26.tar.gz",sep="/")

if (!file.exists(paste(tmpdir,"scowl-words.txt",sep="/"))) {
    if (!file.exists(scowlgz))
        download.file(url = "http://vorboss.dl.sourceforge.net/project/wordlist/SCOWL/2016.06.26/scowl-2016.06.26.tar.gz",
                      destfile = scowlgz, mode = "wb")
    untar(tarfile = scowlgz, exdir = scowldir)
    system(paste("cat ",scowldir,"/*/final/*| sort -u > ",endicts,"/dict_scowl.txt","",sep=""))
}

system.time(dictCorp <- Corpus(DirSource(endicts, encoding = "UTF-8", pattern = ".*.txt")))

trimmCombinations <- function(x) gsub("_"," ",x)
separateUnicode <- function(x) gsub("_"," ",x)

## Words like more, times, where, minutes,  office or from can not be found after stemming,
## so we could reject stemming building dictionary
## We probably still need punctuation (the word pair/triplets bilidng might stop an
## something like ",")
## Order matters: tolower, than removeWords

if (dictToLower) dictCorp <- tm_map(dictCorp, content_transformer(tolower))
if (dictToStem) { dictCorp <- tm_map(dictCorp, stemDocument)
                  dictCorp <- tm_map(dictCorp, content_transformer(unique)) 
                }
dictCorp <- tm_map(dictCorp, stripWhitespace)
if (dictToTrimmComb) dictCorp <- tm_map(dictCorp, content_transformer(trimmCombinations))

## Transfering the dictionary to data table for performance reasons (see further).
na.seq <- rep(x = NA,abs(length(dictCorp[[1]]$content) - length(dictCorp[[2]]$content)))

dtabEnDict <- data.table(dictCorp[[1]]$content,c(dictCorp[[2]]$content,na.seq))
names(dtabEnDict) <- sub(".txt","",names(dictCorp))
setkey(dtabEnDict,"dict_wordnet")

#Building the main document corpus
setwd(homedir)
system.time(corpEnDocs <- Corpus(DirSource(paste(datadir,"final/en_US", sep = "/"),
                                encoding = "UTF-8")))

#Inspecting corporaDocs <- tm_map(corpEnDocs, content_transform
inspect(corpEnDocs)


#Partitioning the data into train/test/validation

trainCorpus <- Corpus(VectorSource(c("trainBlogs","trainNews","trainTwitter")))
testCorpus <- Corpus(VectorSource(c("testBlogs","testNews","testTwitter")))
validCorpus <- Corpus(VectorSource(c("validBlogs","validNews","validTwitter")))

for (i in seq_along(corpEnDocs)) {

    nl <- length(corpEnDocs[[i]]$content)
    set.seed(54321)
    mask <- as.logical(rbinom(nl, 1, prob_train))
    #content(trainCorpus[[i]]) <- content(corpEnDocs[[i]])[mask]
    trainCorpus[[i]]$content <- corpEnDocs[[i]]$content[mask]
    ##Yet not needed at this stage
    # content(testCorpus[[i]]) <- content(corpEnDocs[[i]])[!mask]
    # set.seed(54321)
    # nl <- length(testCorpus[[i]]$content)
    # mask <- as.logical(rbinom(nl, 1, prop_test))
    # content(validCorpus[[i]]) <- content(testCorpus[[i]])[mask]
    # content(testCorpus[[i]]) <- content(testCorpus[[i]])[!mask]
}

rm(corpEnDocs)
gc()

#From here on, working only with trainCorpus

setwd(paste(tmpdir,"before",sep = "/"))
writeCorpus(trainCorpus)
setwd(homedir)


wordsnogo <- c("the","a","an")

trList <- list(
  stripWhitespace,
  removePunctuation,
  removeNumbers,
  content_transformer(tolower),
  #stemDocument,
  stripWhitespace
)

system.time(workTrainCorpus <- tm_map(trainCorpus, tm_reduce, trList))

workTrainCorpus <- tm_map(workTrainCorpus, removeWords, wordsnogo)

## Removing words with non-englisch chars
fltrNoUtf <- function(x) gsub("[^ ]*[^0-9a-zA-Z\x27-_ ][^ ]*","",x)

workTrainCorpus <- tm_map(workTrainCorpus, content_transformer(fltrNoUtf))

if (docsToLower) workTrainCorpus <- tm_map(workTrainCorpus, content_transformer(tolower))

if (docsToStem) {
  system.time(workTrainCorpus <- tm_map(workTrainCorpus, stemDocument))
}
if (docsToTrimmComb) workTrainCorpus <- tm_map(workTrainCorpus, content_transformer(trimmCombinations))
inspect(workTrainCorpus)


setwd(paste(tmpdir,"after",sep = "/"))
writeCorpus(workTrainCorpus)
setwd(homedir)

#Creating Term-Document Matrices

system.time(dtmTr <- DocumentTermMatrix(x = workTrainCorpus))
dim(dtmTr)
object.size(dtmTr)

#Normalizing the incidence of the words
cnt <- sum(sum(dtmTr))
dtmTrNorm <- dtmTr/cnt

setwd(tmpdir)
dput(dtmTrNorm, "dtmTrNorm.R")
setwd(homedir)

#Single word frequency, descending
wordFreqDescNorm <- sort(colSums(as.matrix(dtmTrNorm)), decreasing = T)
paste(cat(names(wordFreqDescNorm[1:20]), sep = ","))

sum(cumsum(wordFreqDescNorm) <= .1)
sum(cumsum(wordFreqDescNorm) <= .2)
sum(cumsum(wordFreqDescNorm) <= .5)
sum(cumsum(wordFreqDescNorm) <= .9)

#Filtering out the sparse words
#like removeSparseTerms(dtmTr, 0.4)
#But, we have only 3 documents, so only two effective grades of sparsity
#(.33 or .66 - scale, not sensitive enough)

#Filtering out only english words with wordnet dictionary - to slow (there is no batch modus)

# findAnyEnWd <- function(wd, fltr) {
#     if (!grepl("^[a-zA-Z-]+$",wd)) return(FALSE)
#     if (fltr == "WildcardFilter") wd <- paste(wd,"*", sep="")
#     filter <- getTermFilter(fltr, wd, TRUE)
#     terms <- getIndexTerms("NOUN", 1, filter)
#     if (!is.null(terms)) return(TRUE)
#     terms <- getIndexTerms("ADJECTIVE", 1, filter)
#     if (!is.null(terms)) return(TRUE)
#     terms <- getIndexTerms("ADVERB", 1, filter)
#     if (!is.null(terms)) return(TRUE)
#     terms <- getIndexTerms("VERB", 1, filter)
#     if (!is.null(terms)) return(TRUE)
#     if (wd %in% c("the",stopwords())) return(TRUE)
#     return(FALSE)
# }

# findAnyEnWdValues <- function(wd, fltr) {
#     print(wd)
#     if (!grepl("^[a-zA-Z-]+$",wd)) return(FALSE)
#     filter <- getTermFilter(fltr, wd, TRUE)
#     terms <- getIndexTerms("NOUN", 5, filter)
#     if (!is.null(terms)) return(paste("NOUN",sapply(terms, getLemma)))
#     terms <- getIndexTerms("ADJECTIVE", 5, filter)
#     if (!is.null(terms)) return(paste("ADJECTIVE",sapply(terms, getLemma)))
#     terms <- getIndexTerms("ADVERB", 5, filter)
#     if (!is.null(terms)) return(paste("ADVERB",sapply(terms, getLemma)))
#     terms <- getIndexTerms("VERB",5, filter)
#     if (!is.null(terms)) return(paste("VERB",sapply(terms, getLemma)))
#     if (wd %in% c("the",stopwords())) return(wd)
#     return(FALSE)
# }

##Benchmarking different filter types (exec.time and accuracy)
##Versatile, but extreme low performance for all filters :((
##
# for (fltr in getFilterTypes()) {
#     print(fltr)
#     print(system.time(eW <- sapply(X = names(wordFreqDescNorm)[1:200],FUN = findAnyEnWd, fltr)))
#     print(which(eW == FALSE,arr.ind = T)[1])
#     print(sum(eW))
# }

mask <- Encoding(names(wordFreqDescNorm)) == "UTF-8"
UtfWords <- wordFreqDescNorm[mask]
wordsInLatChar <- wordFreqDescNorm[!mask]


## It is about 1% of all words, that are not in latin/english charset (coded in UTF-8)
length(UtfWords)/length(wordFreqDescNorm)

setkey(x = dtabEnDict, "dict_wordnet")
system.time(engWordsWN <- dtabEnDict[names(wordsInLatChar)]$dict_wordnet)

#...and the number of words identified as english is..........
length(engWordsWN)

## The same for SCOWL word list

setkey(x = dtabEnDict, "dict_scowl")
system.time(engWordsSC <- dtabEnDict[names(wordsInLatChar)]$dict_scowl)

#...and the number of words identified as english is..........
length(engWordsSC)


#Word associations

maxrow <- 5
maxcol <- length(wordFreqDescNorm)

wordAssocTab <- matrix(NA,1, ncol = maxcol + 3)
wordAssocTab <- wordAssocTab[-1,]


myfunc <- function(x) { for (i in 1:maxrow) {
    wd <- names(x)[i]
    fA <- findAssocs(dtmTr, wd, 0.8)[1:maxcol]
    cat(paste(i,wd,""))
    wordAssocTab <- rbind(wordAssocTab,c(names(x)[i],max(fA[[1]]),min(fA[[1]]),
                                         names(fA[[1]])[1:maxcol]))
}
    wordAssocTab
}

system.time(wordAssocTab <- myfunc(wordFreqDescNorm))

##Word pairs with bash/AWK
setwd(paste(tmpdir,"before",sep="/"))
system(paste(homedir,"select_pairs_and_tripls.sh",sep = "/"))
setwd(paste(tmpdir,"after",sep="/"))
system(paste(homedir,"select_pairs_and_tripls.sh",sep = "/"))

##Word pairs direct


#######################################################################################

##Total execution time

as.numeric(Sys.time()) - START
#######################################################################################

##SUMMARY

##Performance
####################################################################################
#                           Exec. time, sec.
# prob_train    tm_map/tm_reduce    tm_map/mybatch  DocumentTermMatrix  dict,nr.words
# .02           16.115              18.568          17.263              62543
# .04           31.928              39.194          95.246              96205
# .08           64.525              90.114          452.092             148651
# .12           116.614             118.003         1064.488            193718
# .16           135.440                             1813.862            263042
#####################################################################################

## Top-20 words by the total number of words in the DTM (decreasing incidence)
#####################################################################################
# > set.seed(54321)
# > mask <- as.logical(rbinom(nl, 1, prob_train))
# > content(trainCorpus[[i]]) <- content(corpEnDocs[[i]])[mask]
#
# prob_train dict,nr.words      Top-20 words
# .02           63047       will,one,just,get,said,like,time,can,day,year,make,
#                                   love,new,know,good,dont,now,work,peopl,say
# .04           96167       will,just,one,said,like,get,time,can,day,year,make,love,new,know,
#                                   good,now,dont,work,say,peopl
# .08           149201      will,just,one,said,like,get,time,can,day,year,make,love,new,know,
#                                   good,dont,now,work,peopl,say
# .12           193718      will,one,said,just,get,like,time,can,day,year,make,love,new,know,
#                                   good,now,dont,work,peopl,say
# .16           263042      will,just,one,said,like,get,time,can,day,year,make,love,new,know,
#                                   good,now,dont,work,say,peopl
######################################################################################


