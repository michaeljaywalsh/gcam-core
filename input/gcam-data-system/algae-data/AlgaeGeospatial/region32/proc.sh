#!/bin/sh


FILE=Combined_Joined_GCAM_Clean

#Strip Off Header
cat ${FILE}.csv | sed '1d' > xxgcam

 exec 3<&0
 exec 0<xxgcam
 while read line
  do
   POLY_ID=`echo $line | awk -F "," '{print $7}'`
   if [ ! -f region32_${POLY_ID}.csv ]; then
     head -1 ${FILE}.csv > region32_${POLY_ID}.csv
   else
     echo $line >> region32_${POLY_ID}.csv
   fi
  done
  exec 0<&3


rm -f xxgcam
