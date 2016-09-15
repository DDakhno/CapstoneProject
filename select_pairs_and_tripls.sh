#!/bin/bash

cat *txt | tr 'A-Z' 'a-z' | /usr/bin/time -f "%E %M" mawk '
{
    gsub(/ *[\.:?!] *$/,"",$0)
    gsub(/[\(\)\":]/,"")
    gsub(/\./, " . ")
    gsub(/,/, " , ")
    gsub(/ *\. *\.+ */," . ")

    gsub(/ +(the|a|an) +/," ",$0)
    for (i = 1 ;  i < NF;  i++) {   wp = $i" "$(i+1)
                                    if(wp !~ /[\.,/]/) {pairs[$i" "$(i+1)]++ } #else { print wp }
    }
    for (i = 1 ;  i < NF-1;  i++) { tripl = $i" "$(i+1)" "$(i+2)
                                    if(tripl !~ /[\.,/]/) {tripls[tripl]++ } #else { print tripl }
    }
    for (i = 1 ;  i < NF-2;  i++) { four = $i" "$(i+1)" "$(i+2)" "$(i+3)
                                    if( four !~ /[\.,/]/) {fours[four]++ } #else { print four }
    }

}
END {
        for (i in pairs) { print pairs[i],i > "word_pairs.log" }
        for (i in tripls) { print tripls[i],i > "word_triplets.log" }
        for (i in fours) { print fours[i],i > "word_fours.log" }
}
'

sort -rgk1 word_pairs.log >tmp.log && mv tmp.log word_pairs.log
sort -rgk1 word_triplets.log >tmp.log && mv tmp.log word_triplets.log
sort -rgk1 word_fours.log >tmp.log && mv tmp.log word_fours.log


