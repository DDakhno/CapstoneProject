library(data.table)
library(dplyr)

START <- as.integer(Sys.time())
#dbs <- c("DB/dt_monograms/dt_monograms.Rdata" , "DB/dt_bigrams/dt_bigrams.Rdata" , "DB/dt_trigrams/dt_trigrams.Rdata", "DB/dt_four_grams/dt_four_grams.Rdata", "DB/dt_pentagrams/dt_pentagrams.Rdata")
dbs <- c("DB/dt_pentagrams/dt_pentagrams.Rdata")

max_rank <- 150

myrank <- function(x) {
    
    if(x == lastx) {
        cnt <<- cnt + 1
    } else {
        cnt <<- 1
    }
    lastx <<- x
    cnt
}

for (db in dbs) {
    
    print(db)
    
    load(db)
    
    if (grepl("monogram",db)) {
        
        # Cleaning from the apparently non-regular words and expressions (mainly from Twitter)
        db_joined <- as.data.table(db_joined %>% filter(! predictor %like% "^(##.*|[=<:\x5b\x5e\x7e\x7c\x7b_`]{2,})|^[^a-zA-Z]+$|([:;])$" ))
        
        
    } else {
        
        # Cleaning from the apparently non-regular words and expressions (mainly from Twitter)
        # Removing "words" like "##$&' or "joined;", leaving regular words and hashtags
        
        db_joined <- as.data.table(db_joined %>% slice %>% filter(predictor %like% "^[a-z#][a-z]" & ! predictor %like% " [^a-z#]" & ! predictor %like% "[;:!.]$|[;:!.] "))
        
        
        lastx <- ""
        cnt <- 0
       
        print(system.time(ranks <- sapply(db_joined$predictor, myrank)))
        
        mask <- ranks <= max_rank
        db_joined$rank <- ranks
        db_joined <- as.data.table(db_joined[mask])
    }
    
    setkey(db_joined,predictor)
    
    save(db_joined, file=gsub("[.]",paste("_max_rank_",max_rank,".",sep = ""),db))
}

