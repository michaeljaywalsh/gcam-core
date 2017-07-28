#!/bin/sh


FILE=Combined_Join_GCAM_Clean_AEZ

#Strip Off Header
cat ${FILE}.csv | sed '1d' > xxgcam

 exec 3<&0
 exec 0<xxgcam
 while read line
  do
   POLY_ID=`echo $line | awk -F "," '{print $7}'`
   if [ ! -f aez_${POLY_ID}.csv ]; then
     head -1 ${FILE}.csv > aez_${POLY_ID}.csv
   else
     echo $line >> aez_${POLY_ID}.csv
   fi
  done
  exec 0<&3


rm -f xxgcam
