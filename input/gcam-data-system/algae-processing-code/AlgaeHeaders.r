#Helper Functions
library(tidyverse)
library(stringr)
library(openxlsx)
source("../_common/headers/GCAM_header.R")

setwd("/home/michael/GCAM/gcam-core/input/gcam-data-system/algae-data")


#Update From DropBox
#file.copy("~/Dropbox (ScienceandIndustry)/AlgaeGCAM/Model/Algae_GCAM_ProcessData.xlsx","../algae-data",overwrite=TRUE)
#file.copy("~/Dropbox (ScienceandIndustry)/AlgaeGCAM/Model/Ccoef_FC.xml","../algae-data",overwrite=TRUE)
#file.copy("~/Dropbox (ScienceandIndustry)/AlgaeGCAM/Model/AlgaeGeospatial","../algae-data",recursive=TRUE,overwrite=TRUE)

RegNotIncluded <- c('Canada','Russia','Europe_Eastern','European Free Trade Association')
regnames <- read_csv("../_common/mappings/GCAM_region_names_32reg.csv",skip=3) %>%
  select(-GCAM_region_ID) %>%
  filter(region != 'Taiwan')
algregnames <- regnames %>% 
  filter(!region %in% RegNotIncluded)

years <- tibble(year=c(1975,1990,seq(2005,2100,5)))
baseyears <- tibble(year=c(1975,1990,2005,2010))
algregyears <- as_tibble(merge(algregnames,years))
regyears <- as_tibble(merge(regnames,years))

ProcessData <- "../algae-data/Algae_GCAM_ProcessData.xlsx"



#improvement function
logimp <- function(year,fi=0.1,midyear=2050){
  imp <- 1 + 1 / (1 + exp(-fi*(year-midyear)))
  return(imp)
}

mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}


getInput <- function(inputname,dir="../../"){
  #function to get input files from GCAM database for extra processing
  files <- list.files(dir,pattern=paste(inputname,".csv",sep = ""),recursive=TRUE,full.names = TRUE)
  if (length(files)>1){
    print("Found ", length(files)," files. Returning first")
  }
  data <- tbl_df(read_csv(files[1],skip = 4))
  return(data)
}

#helper:
signif_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame
  # digits: number of digits to round
  numeric_columns <- sapply(x, class) == 'numeric'
  x[numeric_columns] <-  signif(x[numeric_columns], digits)
  x
}