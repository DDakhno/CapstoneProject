punctmarks <- "[.!?]"
vectPM <- c(".","!","?")
punctmarkGroup <- "([.!?])"
wordsnogo <- c("the","a","an")

max_words <- 4

treatPunctuations <- function(x) {
        
        # Removing  " - "
        x <- gsub("[[:blank:]]+[-]+[[:blank:]]+"," ",x)
        
        # Removing "..."
        x <- gsub("[.][.][.]"," . ",x)
        
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
        
        # Removing the ":" or ";" at the end of words
        x <- gsub("([a-z])[:;] ","\\1 ",x)
        x <- gsub("([a-z])[:;]$","\\1",x)
        
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
        x <- gsub(punctmarkGroup," . ", x, perl = T)
        
        #Remove ! " % () * + - / , 
        x <- gsub("[\x21\x22\x25\x2a\x2b\x2c\x2d]+", "", x)
        
       # x <- gsub("[.]", " . ", x)
        
        # Anyway, the point at the end of the line
        x <- sub("$"," .",x)
        
        x <- gsub("[.][[:blank:]]+[.]", " . ",x)
        
        x
}

#Subset non-asci chars like like "°" or "€" a.s.o.
subsetUTFAnalogs <- function(str) {
        # Throwing away unconventional quotes as well as exprs. like "°" or "€"
        x <- gsub("\xe2\x80\x9c|\xe2\x80\x9d|\xe2\x80\xa6|\xe2\x80\x93|\xe2\x82\xac|\xc2\xba|\xc2\xbd|\xe2\x99\xa5","", x = str, perl = T,fixed = F)
        x <- gsub("\xe2\x80\x99|\xe2\x80\x98","'", x , perl = T, fixed = F)
        x <- gsub("\xe2\x80\x94", "-", x)
        x
}

removeNotLatinWords <- function(x) gsub("[\x21-\x7e]*[^\x20-\x7e]+[\x21-\x7e]*", "", x)

filterAwayNonLatinLines <- function(lins) {
        mask <- Encoding(lins) == "UTF-8" | grepl("\xe3|\xe4|\xe5|\xe6|\xe7|\xe8|\xe9|\xd0",x = lins, useBytes = T) 
        lins[!mask]
}