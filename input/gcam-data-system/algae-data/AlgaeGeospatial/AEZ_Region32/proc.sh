#!/bin/sh


FILE=Combined_Joined_GCAM_Clean

#Nanno_30cm_BIO_ANN_AVE,Nanno_30cm_BIO_WIN_AVE,Nanno_30cm_BIO_SPR_AVE,Nanno_30cm_BIO_SUM_AVE,Nanno_30cm_BIO_FAL_AVE,ID,GRIDCODE

#Strip Off Header
cat ${FILE}.csv | sed '1d' > xxgcam

 exec 3<&0
 exec 0<xxgcam
 while read line
  do
   #POLY_ID = AEZ_Code _ 
   POLY_ID=`echo $line | awk -F "," '{print $6 "_" $7}'`
   if [ ! -f AEZ_Region32_${POLY_ID}.csv ]; then
     head -1 ${FILE}.csv > AEZ_Region32_${POLY_ID}.csv
   else
     echo $line >> AEZ_Region32_${POLY_ID}.csv
   fi
  done
  exec 0<&3


rm -f xxgcam
