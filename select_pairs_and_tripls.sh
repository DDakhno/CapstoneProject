#!/bin/bash

cat *txt | tr 'A-Z' 'a-z' | /usr/bin/time -f "%E %M" mawk '
{
    gsub(/ *[\.:?!] *$/,"",$0)
    gsub(/[\(\)\":]/,"")
    gsub(/\./, " . ")
    gsub(/,/, " , ")
    gsub(/ *\. *\.+ */," . ")

    for (i = 1 ;  i < NF;  i++) {   wp = $i" "$(i+1)
                                    if(wp !~ /[\.,/]/) {pairs[$i" "$(i+1)]++ } #else { print wp }
    }
    for (i = 1 ;  i < NF-1;  i++) { tripl = $i" "$(i+1)" "$(i+2)
                                    if(tripl !~ /[\.,/]/) {tripls[tripl]++ } #else { print tripl }
    }
}
END {
        for (i in pairs) { print pairs[i],i > "word_pairs.log" }
        for (i in tripls) { print tripls[i],i > "word_triplets.log" }
}
'

sort -rgk1 word_pairs.log >tmp.log && mv tmp.log word_pairs.log
sort -rgk1 word_triplets.log >tmp.log && mv tmp.log word_triplets.log

