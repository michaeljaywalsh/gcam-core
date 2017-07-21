#Algae Data System
library(tidyverse)
library(stringr)
source(paste("../_common/headers/GCAM_header.R"))

#TODO's
#Set percision
#DoubleCheck LOGITs, not matching up in KDiff, dummy LOGIT Tags are showing up in new model


regnames <- read_csv("../_common/mappings/GCAM_region_names_32reg.csv",skip=3) %>%
  select(-GCAM_region_ID)

years = tibble(year=c(1975,1990,seq(2005,2100,5)))
baseyears = tibble(year=c(1975,1990,2005,2010))
AltFoodON.swt <- c(0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1) #Use 0 to shut off
FeedON.swt <- c(0,0,0,0,0,.2,.4,.6,.8,1,1,1,1,1,1,1,1,1,1,1,1,1) #use 0 to shut off
FeedDemand.shwt <- c(0,0,0,0,0,.2,.4,.6,.8,1,1,1,1,1,1,1,1,1,1,1,1,1) #use 0 to shutoff
ALGPDN = TRUE
CCU = TRUE

algregnames <- regnames %>% 
  filter(!region %in% c('Canada','Russia','Taiwan','Europe_Eastern','European Free Trade Association'))


#NEED TO IMPROVE to extract directly from the data
aeznames <- read_csv("../algae-data/Technology.GCAM/algAEZ.csv",skip=3)


aezyears <- as_tibble(merge(aeznames,years))
algregyears <- as_tibble(merge(algregnames,years))

#Copy over algae production data -- Could integrate in the future
yields <- read_csv("../algae-data/Geospatial.GCAM/top10.csv") %>%
  select(region,X6,Annual) %>%
  rename(AgSupplySubsector=X6)


##DEFINE KEY VALUES 
Algae_yield_cost_factor <- 0.7608 #$1975/m2/y
Algae_electricity_factor <- 0.0137 #GJ/m2/y
Algae_N_fert <- 0.01938 # kg N / kg alg
Algae_P_fert <- 0.0096899 #kg DAP / kg alg
Algae_CO2_fert <- 0.606765 #kg C/ kg alg

AltFood.elec <- 0
AltFood.gas <- 0.002762
AltFood.alg <-1 
AltFood.cost <- 0.0252
Calorie.alg <- 4.5

LandCarbon.Veg <- function(yield){
  yield/8.25*34.5/1000
}
LandCarbon.Soil <- function(AEZ){
  soil<-c(3,4,5,6,6,6,6,7,8,9,9,9,9,10,11,12,12,12)
  return(soil[AEZ])
}



#data settings:
cultbatch <- "batch_ag_alg.xml" 
demandbatch <- "batch_demand_input_ALT.xml"


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

getTable <- function(datatable){
  as_tibble(read_csv(paste0("../aglu-data/level2/L203.",datatable,".csv"),
                     skip=4))
}


#Equivalence Table
AlgA.EQUIV_TABLE <- tibble(group.name="LogitType",tag1="dummy-logit-tag",tag2="relative-cost-logit",tag3="absolute-cost-logit")
write_mi_data(AlgA.EQUIV_TABLE,"EQUIV_TABLE","AGLU_LEVEL2_DATA","AlgA.EQUIV_TABLE", "AGLU_XML_BATCH", cultbatch)


#Harvests Per Year
AlgA.AgHAtoCL <- aezyears %>% 
  mutate(AgSupplySector = "Algae") %>%
  rename(AgSupplySubsector = AEZ) %>%
  mutate(AgProductionTechnology = AgSupplySubsector) %>%
  mutate(harvests.per.year = 1) %>%
  select(region,AgSupplySector,AgSupplySubsector,AgProductionTechnology,year,harvests.per.year)
write_mi_data(AlgA.AgHAtoCL,"AgHAtoCL","AGLU_LEVEL2_DATA","AlgA.AgHAtoCL", "AGLU_XML_BATCH", "batch_ag_alg.xml")

#Set Yields
AlgA.AgYield <- aezyears %>% 
  mutate(AgSupplySector = "Algae") %>%
  rename(AgSupplySubsector = AEZ) %>%
  mutate(AgProductionTechnology = AgSupplySubsector) %>%
  full_join(yields,by=c('region','AgSupplySubsector')) %>%
  mutate(yield = Annual * logimp(year)) %>%
  select(region,AgSupplySector,AgSupplySubsector,AgProductionTechnology,year,yield)
#Shutoff for Production
if (ALGPDN == FALSE){AlgA.AgYield$yield <- 0}
write_mi_data(AlgA.AgYield,"AgYield","AGLU_LEVEL2_DATA","AlgA.AgYield", "AGLU_XML_BATCH", "batch_ag_alg.xml")


#Cost
AlgA.AgCost <- AlgA.AgYield %>%
  mutate(nonLandVariableCost = Algae_yield_cost_factor/yield) %>%
  select(region,AgSupplySector,AgSupplySubsector,AgProductionTechnology,year,nonLandVariableCost)
write_mi_data(AlgA.AgCost,"AgCost","AGLU_LEVEL2_DATA","AlgA.AgCost", "AGLU_XML_BATCH", "batch_ag_alg.xml")

#Fertilizer
AlgA.AgCoef_Fert <- AlgA.AgYield %>%
  mutate(minicam.energy.input = 'N fertilizer',
         coefficient = Algae_N_fert) %>%
  select(region,AgSupplySector,AgSupplySubsector,AgProductionTechnology,year,minicam.energy.input,coefficient)
write_mi_data(AlgA.AgCoef_Fert,"AgCoef","AGLU_LEVEL2_DATA","AlgA.AgCoef_Fert", "AGLU_XML_BATCH", "batch_ag_alg.xml")


#Electricity
AlgA.AgCoef_Elec <- AlgA.AgYield %>%
  mutate(minicam.energy.input = 'elect_td_ind',
         coefficient = Algae_electricity_factor/yield) %>%
  select(region,AgSupplySector,AgSupplySubsector,AgProductionTechnology,year,minicam.energy.input,coefficient)
write_mi_data(AlgA.Coef_Elec,"AgCoef","AGLU_LEVEL2_DATA","AlgA.AgCoef_Elec", "AGLU_XML_BATCH", "batch_ag_alg.xml")

#CCU Fertilizer
if (CCU){
  AlgA.AgCoef_FC <- AlgA.AgYield %>%
    mutate(minicam.energy.input = 'feedstock carbon',coefficient = Algae_CO2_fert) %>%
    select(region,AgSupplySector,AgSupplySubsector,AgProductionTechnology,year,minicam.energy.input,coefficient)
  write_mi_data(AlgA.AgCoef_FC,"AgCoef","AGLU_LEVEL2_DATA","AlgA.AgCoef_FC", "AGLU_XML_BATCH", "batch_ag_alg.xml")
}

#ACL - Subsector
AlgA.AgSupplySubsector_ACL <- tibble(region=character(),AgSupplySector=character(),AgSupplySubsector=character(),logit.type=character())
write_mi_data(AlgA.AgSupplySubsector_ACL,"AgSupplySubsector_absolute-cost-logit","AGLU_LEVEL2_DATA","AlgA.AgSupplySubsector_absolute-cost-logit", "AGLU_XML_BATCH", cultbatch)

#RCL - Subsector
AlgA.AgSupplySubsector_RCL <- aeznames %>% 
  mutate(AgSupplySector = "Algae") %>%
  rename(AgSupplySubsector = AEZ) %>%
  mutate(logit.type = "relative-cost-logit")
write_mi_data(AlgA.AgSupplySubsector_RCL,"AgSupplySubsector_relative-cost-logit","AGLU_LEVEL2_DATA","AlgA.AgSupplySubsector_relative-cost-logit", "AGLU_XML_BATCH", cultbatch)

#AgSupplySubsector
AlgA.AgSupplySubsector <- aeznames %>% 
  mutate(AgSupplySector = "Algae") %>%
  rename(AgSupplySubsector = AEZ) %>%
  mutate(logit.year.fillout = 1975, logit.exponent = -3)
write_mi_data(AlgA.AgSupplySubsector,"AgSupplySubsector","AGLU_LEVEL2_DATA","AlgA.AgSupplySubsector", "AGLU_XML_BATCH", cultbatch)

#ACL - Sector
AlgA.AgSupplySector_ACL <- tibble(region=character(),AgSupplySector=character(),logit.type=character())
write_mi_data(AlgA.AgSupplySector_ACL,"AgSupplySector_absolute-cost-logit","AGLU_LEVEL2_DATA","AlgA.AgSupplySector_absolute-cost-logit", "AGLU_XML_BATCH", cultbatch)

#RCL - Sector
AlgA.AgSupplySector_RCL <- algregnames %>% 
  mutate(AgSupplySector = "Algae", logit.type = "relative-cost-logit") 
write_mi_data(AlgA.AgSupplySector_RCL,"AgSupplySector_relative-cost-logit","AGLU_LEVEL2_DATA","AlgA.AgSupplySector_relative-cost-logit", "AGLU_XML_BATCH", cultbatch)

#AgSupplySector
AlgA.AgSupplySector <- algregnames %>% 
  mutate(AgSupplySector = "Algae",
         output.unit="Mt", input.unit = "thous km2", price.unit="1975$/kg",calPrice=0.2,
         market=region,logit.year.fillout=1975,logit.exponent=-3)
write_mi_data(AlgA.AgSupplySector,"AgSupplySector","AGLU_LEVEL2_DATA","AlgA.AgSupplySector", "AGLU_XML_BATCH", cultbatch)

###AltFood #Does not include co-production!!!! 
AltFoodYears<- years %>% mutate(sector.name='AltFood',subsector.name='AlgaeFood',technology='AlgaeFood')

AltF.GlobalTechCoef <- bind_rows(AltFoodYears %>% mutate(minicam.energy.input='elect_td_ind',coefficient=AltFood.elec),
                                 AltFoodYears %>% mutate(minicam.energy.input='wholesale gas',coefficient=AltFood.gas),
                                 AltFoodYears %>% mutate(minicam.energy.input='Algae',coefficient=AltFood.alg)) %>% 
  select(sector.name,subsector.name,technology,year,minicam.energy.input,coefficient)
write_mi_data(AltF.GlobalTechCoef,"GlobalTechCoef","AGLU_LEVEL2_DATA","AltF.GlobalTechCoef", "AGLU_XML_BATCH", cultbatch)

AltF.GlobalTechCost <- AltFoodYears %>% mutate(minicam.non.energy.input="non-energy",input.cost=AltFood.cost) %>%
  select(sector.name,subsector.name,technology,year,minicam.non.energy.input,input.cost)
write_mi_data(AltF.GlobalTechCost,"GlobalTechCost","AGLU_LEVEL2_DATA","AltF.GlobalTechCost", "AGLU_XML_BATCH", cultbatch)
 
AltF.Supplysector_RCL <- algregnames %>% 
  mutate(supplysector = "AltFood", logit.type="relative-cost-logit")
write_mi_data(AltF.Supplysector_RCL,"Supplysector_relative-cost-logit","AGLU_LEVEL2_DATA","AltF.Supplysector_relative-cost-logit", "AGLU_XML_BATCH", cultbatch)

AltF.Supplysector_ACL <- tibble(region=character(),supplysector=character(),logit.type=character())
write_mi_data(AltF.Supplysector_ACL,"Supplysector_absolute-cost-logit","AGLU_LEVEL2_DATA","AltF.Supplysector_absolute-cost-logit", "AGLU_XML_BATCH", cultbatch)

AltF.Supplysector <- algregnames %>% 
  select(region) %>% 
  mutate(supplysector="AltFood",output.unit="Mt",input.unit="Mt",
         price.unit="1975$/kg",logit.year.fillout=1975,logit.exponent=-1.5)
write_mi_data(AltF.Supplysector,"Supplysector","AGLU_LEVEL2_DATA","AltF.Supplysector", "AGLU_XML_BATCH", cultbatch)

AltF.Subsector_RCL <- algregnames %>% 
  mutate(supplysector = "AltFood", subsector = "AlgaeFood", logit.type="relative-cost-logit")
write_mi_data(AltF.Supplysector_RCL,"SubsectorLogit_relative-cost-logit","AGLU_LEVEL2_DATA","AltF.SubsectorLogit_relative-cost-logit", "AGLU_XML_BATCH", cultbatch)

AltF.Subsector_ACL <- tibble(region=character(),supplysector=character(),subsector=character(),logit.type=character())
write_mi_data(AltF.Subsector_ACL,"SubsectorLogit_absolute-cost-logit","AGLU_LEVEL2_DATA","AltF.SubsectorLogit_absolute-cost-logit", "AGLU_XML_BATCH", cultbatch)

AlgF.SubsectorAll <- algregnames %>% 
  mutate(supplysector = "AltFood", subsector = "AlgaeFood", logit.year.fillout=1975,
         logit.exponent=-6,year.fillout=2020,share.weight=1, apply.to ="share-weight", 
         from.year=2020, to.year=2100, interpolation.function="fixed")
write_mi_data(AlgF.SubsectorAll,"SubsectorAll","AGLU_LEVEL2_DATA","AlgF.SubsectorAll", "AGLU_XML_BATCH", cultbatch)

AlgF.StubTechProd <- algregnames %>% 
  mutate(supplysector = "AltFood", subsector = "AlgaeFood", stub.technology = "AlgaeFood") %>% 
  merge(baseyears) %>%
  mutate(calOutputValue=0,share.weight=1975,subs.share.weight=0,tech.share.weight=0)
write_mi_data(AlgF.StubTechProd,"StubTechProd","AGLU_LEVEL2_DATA","AlgF.StubTechProd", "AGLU_XML_BATCH", cultbatch)

AltF.StubTech <- algregnames %>% 
  mutate(supplysector = "AltFood", subsector="Algae Food", stub.technology="Algae Food")
write_mi_data(AltF.StubTech,"StubTech","AGLU_LEVEL2_DATA","AltF.StubTech", "AGLU_XML_BATCH", cultbatch)

AltF.GlobalTechShrwt <- AltFoodYears %>% 
  mutate(share.weight = AltFoodON.swt) %>%
  select(sector.name,subsector.name,technology,year,share.weight)
write_mi_data(AltF.GlobalTechShrwt,"GlobalTechShrwt","AGLU_LEVEL2_DATA","AltF.GlobalTechShrwt", "AGLU_XML_BATCH", cultbatch)

#Feed
FeedYears<- years %>% mutate(sector.name='FeedCrops',subsector.name='AltFood',technology='AltFood')

Feed.Supplysector_RCL <- algregnames %>% 
  mutate(supplysector = "FeedCrops", logit.type="relative-cost-logit") 
write_mi_data(Feed.Supplysector_RCL,"Supplysector_relative-cost-logit","AGLU_LEVEL2_DATA","Feed.Supplysector_relative-cost-logit", "AGLU_XML_BATCH", cultbatch)

Feed.Supplysector_ACL <- tibble(region=character(),supplysector=character(),logit.type=character())
write_mi_data(Feed.Supplysector_ACL,"Supplysector_absolute-cost-logit","AGLU_LEVEL2_DATA","Feed.Supplysector_absolute-cost-logit", "AGLU_XML_BATCH", cultbatch)

Feed.Subsector_RCL <- algregnames %>% 
  mutate(supplysector = "FeedCrops", subsector = "AltFood", logit.type="relative-cost-logit")
write_mi_data(Feed.Supplysector_RCL,"SubsectorLogit_relative-cost-logit","AGLU_LEVEL2_DATA","Feed.SubsectorLogit__relative-cost-logit", "AGLU_XML_BATCH", cultbatch)

Feed.Subsector_ACL <- tibble(region=character(),supplysector=character(),subsector=character(),logit.type=character())
write_mi_data(Feed.Subsector_ACL,"SubsectorLogit_absolute-cost-logit","AGLU_LEVEL2_DATA","Feed.SubsectorLogit_absolute-cost-logit", "AGLU_XML_BATCH", cultbatch)

Feed.StubTechProd <- algregnames %>% 
  mutate(supplysector = "FeedCrops", subsector = "AltFood", stub.technology = "AltFood") %>% 
  merge(tibble(year=c(1975,1990,2005,2010))) %>%
  mutate(calOutputValue=0,share.weight=1975,subs.share.weight=0,tech.share.weight=0)
write_mi_data(Feed.StubTechProd,"StubTechProd","AGLU_LEVEL2_DATA","Feed.StubTechProd", "AGLU_XML_BATCH", cultbatch)

Feed.GlobalTechShrwt <- FeedYears %>% 
  mutate(share.weight = FeedON.swt) %>%
  select(sector.name,subsector.name,technology,year,share.weight)
write_mi_data(Feed.GlobalTechShrwt,"GlobalTechShrwt","AGLU_LEVEL2_DATA","Feed.GlobalTechShrwt", "AGLU_XML_BATCH", cultbatch)

Feed.StubTech <- algregnames %>% 
  mutate(supplysector = "FeedCrops", subsector="AltFood", stub.technology="AltFood")
write_mi_data(Feed.StubTech,"StubTech","AGLU_LEVEL2_DATA","Feed.StubTech", "AGLU_XML_BATCH", cultbatch)

Feed.GlobalTechCoef <- FeedYears %>% 
  mutate(sector.name="FeedCrops",minicam.energy.input='AltFood',coefficient=1) %>% 
  select(sector.name,subsector.name,technology,year,minicam.energy.input,coefficient)
write_mi_data(Feed.GlobalTechCoef,"GlobalTechCoef","AGLU_LEVEL2_DATA","Feed.GlobalTechCoef", "AGLU_XML_BATCH", cultbatch)

Feed.SubsectorAll <- algregnames %>% 
  mutate(supplysector = "FeedCrops", subsector = "AltFood", logit.year.fillout=1975,
         logit.exponent=-6,year.fillout=2020,share.weight=1, apply.to ="share-weight", 
         from.year=2020, to.year=2100, interpolation.function="fixed")
write_mi_data(Feed.SubsectorAll,"SubsectorAll","AGLU_LEVEL2_DATA","Feed.SubsectorAll", "AGLU_XML_BATCH", cultbatch)


#Land
Land.LN3_NewTech <- aeznames %>% 
  mutate(LandAllocatorRoot='root') %>% rename(LandLeaf=AEZ) %>%
  mutate(LandNode1 = gsub("Algae","AgroForestLand",LandLeaf))%>%
  mutate(LandNode2 = gsub("Algae","AgroForest_NonPasture",LandLeaf))%>%
  mutate(LandNode3 = gsub("Algae","CropLand",LandLeaf)) %>%
  mutate(year.fillout = 2020,isNewTechnology=1) %>%
  select(region, LandAllocatorRoot, LandNode1, LandNode2, LandNode3, LandLeaf, year.fillout, isNewTechnology)
write_mi_data(Land.LN3_NewTech,"LN3_NewTech","AGLU_LEVEL2_DATA","Land.LN3_NewTech", "AGLU_XML_BATCH", cultbatch,node_rename=T)

Land.LN3_MgdCarbon <- aeznames %>% 
  mutate(LandAllocatorRoot='root') %>% rename(LandLeaf=AEZ) %>%
  mutate(LandNode1 = gsub("Algae","AgroForestLand",LandLeaf)) %>%
  mutate(LandNode2 = gsub("Algae","AgroForest_NonPasture",LandLeaf)) %>%
  mutate(LandNode3 = gsub("Algae","CropLand",LandLeaf)) %>% 
  left_join(yields,by=c("LandLeaf"="AgSupplySubsector","region")) %>%
  mutate(hist.veg.carbon.density = LandCarbon.Veg(Annual)) %>% 
  mutate(hist.soil.carbon.density = LandCarbon.Soil(as.numeric(substring(LandLeaf, 9, 11)))) %>%
  mutate(veg.carbon.density = hist.veg.carbon.density, soil.carbon.density=hist.soil.carbon.density,
         mature.age.year.fillout=1975, mature.age=0,min.veg.carbon.density = 0, min.soil.carbon.density =0) %>% 
  select(region, LandAllocatorRoot, LandNode1, LandNode2, LandNode3, LandLeaf, hist.veg.carbon.density, hist.soil.carbon.density, veg.carbon.density, soil.carbon.density, mature.age.year.fillout, mature.age, min.veg.carbon.density, min.soil.carbon.density)
write_mi_data(Land.LN3_MgdCarbon,"LN3_MgdCarbon","AGLU_LEVEL2_DATA","Land.LN3_MgdCarbon", "AGLU_XML_BATCH", cultbatch,node_rename=T)

###Demand Recycling Demand

#EQUIV_TABLE - Copy
getTable("EQUIV_TABLE") %>%
  write_mi_data("EQUIV_TABLE","AGLU_LEVEL2_DATA","DAlt.EQUIV_TABLE", "AGLU_XML_BATCH", demandbatch)


getTable("Supplysector_relative-cost-logit") %>%
  bind_rows(algregnames %>%
              mutate(supplysector='FoodDemand_Crops_Flex',logit.type='relative-cost-logit')) %>%
  write_mi_data("Supplysector_relative-cost-logit","AGLU_LEVEL2_DATA","DAlt.Supplysector_relative-cost-logit", "AGLU_XML_BATCH", demandbatch)
      
getTable("Supplysector_absolute-cost-logit") %>%
  write_mi_data("Supplysector_absolute-cost-logit","AGLU_LEVEL2_DATA","DAlt.Supplysector_absolute-cost-logit", "AGLU_XML_BATCH", demandbatch)

getTable("Supplysector_demand") %>%
  bind_rows(algregnames %>% 
              mutate(supplysector='FoodDemand_Crops_Flex',output.unit='Pcal',
                     input.unit='Mt',price.unit='1975$/Mt',logit.year.fillout=1975,logit.exponent=-6)) %>%
  write_mi_data("Supplysector","AGLU_LEVEL2_DATA","DAlt.Supplysector_demand", "AGLU_XML_BATCH", demandbatch)            

getTable("SubsectorLogit_relative-cost-logit") %>%
  mutate_cond(subsector=="Corn" | subsector=="OilCrop",
              supplysector=c("FoodDemand_Crops_Flex")) %>%
  bind_rows(algregnames %>% 
              mutate(supplysector='FoodDemand_Crops_Flex',subsector="AltFood",
                     logit.type='relative-cost-logit')) %>%
  write_mi_data("SubsectorLogit_relative-cost-logit","AGLU_LEVEL2_DATA","DAlt.SubsectorLogit_relative-cost-logit", "AGLU_XML_BATCH", demandbatch)


getTable("SubsectorLogit_absolute-cost-logit") %>%
  write_mi_data("SubsectorLogit_absolute-cost-logit","AGLU_LEVEL2_DATA","DAlt.SubsectorLogit_absolute-cost-logit", "AGLU_XML_BATCH", demandbatch)

getTable("SubsectorAll_demand") %>%
  mutate_cond(subsector=="Corn" | subsector=="OilCrop",
              supplysector="FoodDemand_Crops_Flex") %>%
  bind_rows(algregnames %>% 
              mutate(supplysector='FoodDemand_Crops_Flex',subsector="AltFood",
                     logit.year.fillout = 1957, logit.exponent = -6, 
                     year.fillout = 1975, share.weight=1, apply.to='share-weight',
                     from.year=2010, to.year=2100, interpolation.function="fixed")) %>%
  write_mi_data("SubsectorAll","AGLU_LEVEL2_DATA","DAlt.SubsectorAll_demand", "AGLU_XML_BATCH", demandbatch)

 getTable("GlobalTechCoef_demand") %>%
  mutate_cond(subsector.name=="Corn" | subsector.name=="OilCrop",
              sector.name="FoodDemand_Crops_Flex") %>%
  bind_rows(years %>% 
              mutate(sector.name='FoodDemand_Crops_Flex', subsector.name="AltFood",
                     technology='AltFood',minicam.energy.input="AltFood",coefficient=1)) %>%
  write_mi_data("GlobalTechCoef","AGLU_LEVEL2_DATA","DAlt.GlobalTechCoef_demand", "AGLU_XML_BATCH", demandbatch)


getTable("GlobalTechShrwt_demand") %>%
  mutate_cond(subsector.name=="Corn" | subsector.name=="OilCrop",
              sector.name="FoodDemand_Crops_Flex") %>%
  bind_rows(years %>%
              mutate(sector.name='FoodDemand_Crops_Flex', subsector.name="AltFood",
                     technology='AltFood',share.weight=FeedDemand.shwt)) %>%
write_mi_data("GlobalTechShrwt","AGLU_LEVEL2_DATA","DAlt.GlobalTechShrwt_demand", "AGLU_XML_BATCH", demandbatch)


getTable("StubTech_demand") %>% 
  mutate_cond(subsector=="Corn" | subsector=="OilCrop",
              supplysector="FoodDemand_Crops_Flex") %>%
  bind_rows(algregnames %>% 
              mutate(supplysector='FoodDemand_Crops_Flex',subsector="AltFood",
                     technology="AltFood")) %>%
  write_mi_data("StubTech","AGLU_LEVEL2_DATA","DAlt.StubTech_demand", "AGLU_XML_BATCH", demandbatch)

getTable("StubTechProd_food_crop") %>% 
  mutate_cond(subsector=="Corn" | subsector=="OilCrop",
              supplysector="FoodDemand_Crops_Flex") %>%
  write_mi_data("StubTechProd","AGLU_LEVEL2_DATA","DAlt.StubTechProd_food_crop", "AGLU_XML_BATCH", demandbatch)

getTable("StubTechProd_food_meat") %>%
  write_mi_data("StubTechProd","AGLU_LEVEL2_DATA","DAlt.StubTechProd_food_meat", "AGLU_XML_BATCH", demandbatch)

getTable("StubTechProd_nonfood_crop") %>%
  write_mi_data("StubTechProd","AGLU_LEVEL2_DATA","DAlt.StubTechProd_nonfood_crop", "AGLU_XML_BATCH", demandbatch)

getTable("StubTechProd_nonfood_meat") %>%
  write_mi_data("StubTechProd","AGLU_LEVEL2_DATA","DAlt.StubTechProd_nonfood_meat", "AGLU_XML_BATCH", demandbatch)

getTable("StubTechProd_For") %>%
  write_mi_data("StubTechProd","AGLU_LEVEL2_DATA","DAlt.StubTechProd_For", "AGLU_XML_BATCH", demandbatch)

getTable("StubTechFixOut_exp") %>%
  write_mi_data("StubTechFixOut","AGLU_LEVEL2_DATA","DAlt.StubTechFixOut_exp", "AGLU_XML_BATCH", demandbatch)

getTable("StubCalorieContent_crop") %>% 
  mutate_cond(subsector=="Corn" | subsector=="OilCrop",
              supplysector="FoodDemand_Crops_Flex") %>%
  bind_rows(algregnames %>% 
              mutate(supplysector = "FoodDemand_Crops_Flex", subsector="AltFood",stub.technology="AltFood",
                minicam.energy.input = "AltFood", efficiency=Calorie.alg,market.name=region)) %>%
  write_mi_data("StubCalorieContent","AGLU_LEVEL2_DATA","DAlt.StubCalorieContent_crop", "AGLU_XML_BATCH", demandbatch)

getTable("StubCalorieContent_meat") %>%
  write_mi_data("StubCalorieContent","AGLU_LEVEL2_DATA","DAlt.StubCalorieContent_meat", "AGLU_XML_BATCH", demandbatch)

getTable("PerCapitaBased") %>% 
  bind_rows(algregnames %>% 
              mutate(energy.final.demand='FoodDemand_Crops_Flex',perCapitaBased=1)) %>% 
  write_mi_data("PerCapitaBased","AGLU_LEVEL2_DATA","DAlt.PerCapitaBased", "AGLU_XML_BATCH", demandbatch)

#The recalibrate base year shares for food markets into flex and non-flex
getTable("BaseService") %>% 
  filter(energy.final.demand!='FoodDemand_Crops') %>%
  bind_rows(getTable("StubTechProd_food_crop") %>% 
              mutate_cond(subsector=="Corn" | subsector=="OilCrop",
                          supplysector="FoodDemand_Crops_Flex") %>% 
              select(-c(stub.technology,share.weight.year,subs.share.weight,tech.share.weight)) %>%
              group_by(supplysector,region,year) %>%
              summarise(base.service=sum(calOutputValue)) %>%
              rename(energy.final.demand=supplysector)) %>%
  write_mi_data("PriceElasticity","AGLU_LEVEL2_DATA","DAlt.BaseService", "AGLU_XML_BATCH", demandbatch)


getTable("IncomeElasticity") %>%
  bind_rows(getTable("IncomeElasticity") %>% 
              filter(energy.final.demand=='FoodDemand_Crops') %>% 
              mutate(energy.final.demand='FoodDemand_Crops_Flex')) %>%
  write_mi_data("IncomeElasticity","AGLU_LEVEL2_DATA","DAlt.IncomeElasticity", "AGLU_XML_BATCH", demandbatch)

  
getTable("PriceElasticity") %>% 
  mutate_cond(energy.final.demand=="FoodDemand_Meat",price.elasticity=0) %>%
  bind_rows(algregyears %>% 
              mutate(energy.final.demand="FoodDemand_Crops_Flex",price.elasticity=0)) %>%
  write_mi_data("PriceElasticity","AGLU_LEVEL2_DATA","DAlt.PriceElasticity", "AGLU_XML_BATCH", demandbatch)

insert_file_into_batchxml( "AGLU_XML_BATCH", cultbatch, "AGLU_XML_FINAL", "ag_alg.xml", "", xml_tag="outFile" )
insert_file_into_batchxml( "AGLU_XML_BATCH", demandbatch, "AGLU_XML_FINAL", "demand_input_ALT.xml", "", xml_tag="outFile" )

#Example
#java -Xmx2g -jar ../_common/ModelInterface/src/CSVToXML.jar ../aglu-processing-code/xml-batch/batch_demand_input_ALT.xml

  