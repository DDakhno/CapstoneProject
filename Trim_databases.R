library(data.table)
library(dplyr)

START <- as.integer(Sys.time())
dbs <- c("DB/dt_monograms/dt_monograms.Rdata" , "DB/dt_bigrams/dt_bigrams.Rdata" , "DB/dt_trigrams/dt_trigrams.Rdata", "DB/dt_pentagrams/dt_pentagrams.Rdata")dbs <- c("DB/dt_four_grams/dt_four_grams.Rdata")

max_rank <- 150

myrank <- function(x) {
    
    if(x == lastx ) {
        cnt <<- cnt + 1
    } else {
        cnt <<- 1
    }
    lastx <<- x
    cnt
}

myrank2 <- function(x,y) {
    
    if(x == lastx & ! y  == lasty) {
        cnt <<- cnt + 1
    } else if (x == lastx &  y  == lasty ){
        cnt <<- cnt
    } else {
        cnt <<- 1
    }
    lastx <<- x
    lasty <<- y
    cnt
}

for (db in dbs) {
    
    print(db)
    
    load(db)
    
    print("Loaded")
    
    if (grepl("monogram",db)) {
        
        # Cleaning from the apparently non-regular words and expressions (mainly from Twitter)
        db_joined <- as.data.table(db_joined %>% filter(! predictor %like% "^(##.*|[=<:\x5b\x5e\x7e\x7c\x7b_`]{2,})|^[^a-zA-Z]+$|([:;])$" ))
        
        
    } else {
        
        # Cleaning from the apparently non-regular words and expressions (mainly from Twitter)
        
        print("Cleaning")
        
        db_joined <- as.data.table(db_joined %>% filter(predictor %like% "^[a-z#@][a-z ]" & ! predictor %like% " [^a-z#@][a-z ]" & ! predictor %like% "[;:!.]$|[;:!.] "))
        db_joined <- as.data.table(db_joined %>% filter(outcome %like% "^[a-z#@][a-z]" & ! outcome %like% "[;:!.]$"))
        
        
        
        print("Rearranging by predictor, desc(freq)")
        
        db_joined <- as.data.table(db_joined%>%arrange(predictor,desc(freq)))
        
        print("Trimming")
        
        lastx <- ""
        lasty <- 0
        cnt <- 0
        
        print(system.time(ranks <- sapply(db_joined$predictor, myrank)))
        
        mask <- ranks <= max_rank
        db_joined$rank <- ranks
        db_joined2 <- as.data.table(db_joined)[mask]
    }
    
    setkey(db_joined,predictor)
    
    save(db_joined, file=gsub("[.]",paste("_max_rank_",max_rank,".",sep = ""),db))
    rm(db_joined)
    gc()
}

