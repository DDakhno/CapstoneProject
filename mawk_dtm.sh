/usr/bin/time -f "%E %M" mawk 'FNR == 1 { print "FILENAME",FILENAME }
{gsub(/[^[:alnum:] ]/," ")}
{print tolower($0)}'  *.txt| sed 's/ /\n/g'| /usr/bin/time -f "%E %M" mawk '
/^$/ { next }
/^FILENAME/ { flag = 1; next}
flag == 1 { fnam = $0;  arfnam[fnam]++; flag = 0; next}
{ arr[fnam,$0]++ ; arrgl[$0]++; cntall++ }
END { strg = "word" ; for (i in arfnam) { strg = strg" "i }; print strg" ALL %ofTOTAL"
    for (wd in arrgl) { strg = wd; for (fn in arfnam) {strg = strg" "arr[fn,wd]+0 }
                        strg = strg" "arrgl[wd]" "(arr[fn,wd]+0)*100/cntall
                        print strg
    }
}'| sort -rgk5 > dtm.log
