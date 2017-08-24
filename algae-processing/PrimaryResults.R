library(rgcam)
library(tidyverse)
library(RColorBrewer)
#this.dir <- dirname(parent.frame(2)$ofile)
#setwd(this.dir)
#source('../input/gcam-data-system/algae-processing-code/AlgaeHeaders.r')

conn <- localDBConn('../output', 'Algae_basexdb')
QueryFile.Em = '../output/queries/emQ.xml'
QueryFile.LUC = '../output/queries/LUCQ.xml'
QueryFile.Alg = '../output/queries/AlgaeQueries.xml'
scenarios = c('Ref',
              'Ref+A-CC',
              'B-REF',
              'B-REF+A',
              'B-FFICT',
              'B-FFICT+A',
              'B-FFICT+A+CC',
              'B-UCT',
              'B-UCT+A',
              'B-UCT+A+CC')

scen <- 'B-UCT+A'
ref <- 'B-UCT'

getInput <- function(inputname,dir="../input"){
  #function to get input files from GCAM database for extra processing
  files <- list.files(dir,pattern=paste(inputname,".csv",sep = ""),recursive=TRUE,full.names = TRUE)
  if (length(files)>1){
    print("Found ", length(files)," files. Returning first")
  }
  data <- tbl_df(read_csv(files[1],skip = 4))
  return(data)
}


if("alg.prj" %in% ls()){
  alg.prj <- loadProject('postdata/alg.dat')
} else {
  for (s in scenarios){
    alg.prj <- addScenario(conn,'postdata/alg.dat', s,QueryFile.Em)
  }
}

n2o.gwp <- 310
ch4.gwp <- 21
co2.gwp <- 3.6666666666

cases <- read_csv("categories.csv")

sectem.co2 <- getQuery(alg.prj,"CO2 emissions by sector") %>% 
  spread(scenario,value=value,fill=0) %>% 
  gather(scenarios,key=scenario,value="value") %>%
  mutate(value = co2.gwp * value) %>%
  left_join(cases,by='sector')

sectem.luc <- group_by(getQuery(alg.prj,"Land Use Change Emission (future)"),scenario,year) %>% 
  filter(year>=2020) %>%
  drop_na() %>%
  diffScenarios(c(scen,ref),keycols = c("region","year")) %>% 
  group_by(year) %>%
  summarise(CO2=sum(value)) %>% 
  mutate(sector='LUC') %>%
  mutate(CO2 = co2.gwp * CO2) %>%
  left_join(cases,by='sector')


sectem.n2o <- group_by(getQuery(alg.prj,"N2O emissions by subsector"),scenario,year) %>% 
  spread(scenario,value=value,fill=0) %>% 
  gather(scenarios,key=scenario,value="value") %>%
  left_join(cases,by='sector')

sectem.ch4 <- group_by(getQuery(alg.prj,"CH4 emissions by subsector"),scenario,year) %>% 
  spread(scenario,value=value,fill=0) %>% 
  gather(scenarios,key=scenario,value="value") %>%
  left_join(cases,by='sector')

emissions.co2 <- sectem.co2 %>% filter(year>=2020) %>%
  diffScenarios(c(scen,ref),keycols = c("category","year")) %>% 
  group_by(category,year) %>% 
  summarise(CO2 = sum(value)) %>%
  bind_rows(sectem.luc) %>% select(-sector)

emissions.n2o <- sectem.n2o %>% filter(year>=2020) %>%
  diffScenarios(c(scen,ref),keycols = c("category","year")) %>% 
  group_by(category,year) %>% 
  summarise(N2O = sum(value))

emissions.ch4 <- sectem.ch4 %>% filter(year>=2020) %>%
  diffScenarios(c(scen,ref),keycols = c("category","year")) %>% 
  group_by(category,year) %>% 
  summarise(CH4 = sum(value))

emissions <- emissions.co2 %>% 
  full_join(emissions.n2o,by=c('category','year')) %>% 
  full_join(emissions.ch4,by=c('category','year')) %>%
  replace_na(list(CO2=0),CO2) %>%
  replace_na(list(CH4=0),CH4) %>%
  replace_na(list(N2O=0),N2O)

emissions.eq <- emissions %>%
  mutate(N2O = N2O * n2o.gwp) %>%
  mutate(CH4 = CH4 * ch4.gwp) %>%
  mutate(total = CO2 + N2O + CH4)

net <- emissions.eq %>% select(-c(CO2,N2O,CH4)) %>% group_by(year) %>% drop_na() %>% summarise(total=sum(total))

ggplot(emissions.eq, aes(year,total)) + geom_col(aes(fill=category)) + 
  ylab("Change in" ~CO[2]~ "emissions from Reference (MT CO2eq)")  +
  geom_point(data = net,aes(year,total))

