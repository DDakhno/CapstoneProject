#!/usr/local/bin/Rscript

library(data.table)
library(dplyr)

homedir <- "/home/d/R-Work/CapstoneProject"
dbdir <- file.path(homedir,"DB")

setwd(homedir)

START <- as.integer(Sys.time())

joinDBs <- function(xx,yy) {
        #db_joined <- merge(dbList[[1]],dbList[[2]], all=TRUE, by = c("predictor","outcome"))
        
        db_joined <- merge(xx, yy, all=TRUE)
        
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

for (db in c("dt_monograms","dt_bigrams","dt_trigrams","dt_four_grams","dt_pentagrams")) {
        #for (db in c("dt_bigrams")) {
        
        setwd(homedir)
        dbFiles <- system(paste("find . -name ",db,".Rdata", sep = ""), intern = T)
        dbFiles <- grep( "DB", dbFiles, value = T, invert = T)
        
        print(dbFiles)
        
        db_joined <- data.table()
        
        cnt <- 1
        
        while (cnt < length(dbFiles)) {
                
                if (nrow(db_joined) == 0) { 
                        load(dbFiles[cnt])
                        xx <- get(gsub("^.*/(.*).Rdata","\\1",dbFiles[cnt]))
                        rm(list=(gsub("^.*/(.*).Rdata","\\1",dbFiles[cnt])))
                }
                
                load(dbFiles[cnt+1])
                print(dbFiles[cnt+1])
                
                load(dbFiles[cnt+1])
                yy <- get(gsub("^.*/(.*).Rdata","\\1",dbFiles[cnt+1]))
                rm(list=(gsub("^.*/(.*).Rdata","\\1",dbFiles[cnt+1])))
                
                if (db != "dt_monograms") {
                        if (nrow(db_joined) == 0) { 
                                setkey(xx, predictor, outcome)
                        }
                        setkey(yy, predictor, outcome)
                } else {
                        if (nrow(db_joined) == 0) { 
                                setkey(xx, word)
                        }
                        setkey(yy, word)
                }
                
                
                if (nrow(db_joined) == 0) {
                        db_joined <- joinDBs(xx, yy)
                } else {
                        db_joined <- joinDBs(db_joined, yy)
                }
                
                cnt <- cnt+1
                
                rm(xx)
                rm(yy)
                gc()
        }
        
        dbdirNow <- file.path(dbdir,db) 
        if (! dir.exists(dbdirNow)) dir.create(dbdirNow, recursive = T)
        setwd(dbdirNow)
        save(db = db_joined, file = paste(db,".Rdata", sep=""))
        rm(db_joined)
        gc()
}

setwd(homedir)

paste("Exe. time:",as.integer(Sys.time()) - START,"sec.")
