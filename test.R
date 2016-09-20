#!/usr/local/bin/Rscript

library(data.table)
library(dplyr)

homedir <- "/home/d/R-Work/CapstoneProject"
dbdir <- file.path(homedir,"DB")

setwd(homedir)

START <- as.integer(Sys.time())

for (db in c("dt_monograms","dt_bigrams","dt_trigrams","dt_four_grams","dt_pentagrams")) {
        #for (db in c("dt_bigrams")) {
        rm(dbList)
        dbList <- list()
        gc()
        setwd(homedir)
        dbFiles <- system(paste("find . -name ",db,".Rdata", sep = ""), intern = T)
        cnt <- 1
        
        for (onedb in dbFiles) {
                load(onedb)
                xx <- get(db)
                if (db != "dt_monograms") {
                        setkey(xx, predictor, outcome)
                } else {
                        setkey(xx, word)
                }
                dbList[[cnt]] <- xx
                cnt <- cnt + 1
        }
        
        db_joined <- data.table()
        
        joinDBs <- function(xx,yy) {
                #db_joined <- merge(dbList[[1]],dbList[[2]], all=TRUE, by = c("predictor","outcome"))
                if (nrow(db_joined) == 0) {
                        db_joined <- merge(xx, yy, all=TRUE)
                } else {
                        db_joined <- merge(db_joined, yy, all=TRUE)  
                }
                if (db != "dt_monograms") {
                        setkey(db_joined, predictor, outcome)
                } else {
                        setkey(db_joined, word)
                }
                mask <- is.na(db_joined)
                db_joined[mask] <- 0
                db_joined$freq <- db_joined$freq.x + db_joined$freq.y
                if (db != "dt_monograms") {
                        db_joined <- db_joined %>% select(predictor,outcome,freq)
                } else {
                        db_joined <- db_joined %>% select(freq, word)
                }
                db_joined
        }
        
        i <- 1
        while ( i < length(dbList)) {
                db_joined <- joinDBs(dbList[[i]], dbList[[i+1]])
                i <- i + 1
        }
        
        dbdirNow <- file.path(dbdir,db) 
        if (! dir.exists(dbdirNow)) dir.create(dbdirNow, recursive = T)
        setwd(dbdirNow)
        save(db_joined, file = paste(db,".Rdata", sep=""))
        rm(db_joined)
}

paste("Exe. time:",as.integer(Sys.time()) - START,"sec.")
