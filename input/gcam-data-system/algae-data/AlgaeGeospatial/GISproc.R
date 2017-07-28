
library(plyr)

setwd('E:/Dropbox (ScienceandIndustry)/AlgaeGCAM/Geospatial/Data')

daily_to_ann <- function(daily){
  return(daily * 365 / 1000)
}

get_reg_name <- function(regnum){
  return(as.character(reg32.names[reg32.names$GCAM_region_ID==regnum,]$region))
}

reg32.names <- read.csv("GCAM_region_names.csv",comment.char='#')

files <- list.files(path="AEZ_region32",pattern = ".csv")
  
datalist <- lapply(files, function(x){read.csv(file=file.path("AEZ_region32",x),header=T)})  
df <- ldply(datalist)
#add regions
df['region'] <- mapply(get_reg_name,df$REG32_CODE)
#remove zeros
df <- df[!apply(df[,1:4] == 0, 1, FUN = any, na.rm = TRUE),]

#medians by group
medians = aggregate(.~region+AEZ_CODE,df,median)[1:3]
#get average of top 5
top10 = aggregate(.~region+AEZ_CODE,df,quantile,probs=.95)[1:3]
names(medians) <- c("region","AEZ","Daily")
names(top10) <- c("region","AEZ","Daily")
medians$Annual <- mapply(daily_to_ann,medians$Daily)
top10$Annual <- mapply(daily_to_ann,top10$Daily)
#remove unnecessary AEZs
medians<- subset(medians, AEZ<13)
top10<- subset(top10, AEZ<13)
medians <- medians[with(medians,order(region)),]
top10 <- top10[with(top10,order(region)),]
write.csv(top10,"top10.csv")