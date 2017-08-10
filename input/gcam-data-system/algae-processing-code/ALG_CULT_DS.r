#Algae Data System


source("../algae-processing-code/AlgaeHeaders.r")



#TODOs 
#Add option for coprodcution

ALGAE = TRUE
USECCU = FALSE


#Output settings:
algXML <- "alg_cult.xml"

if (USECCU){algXML<- str_replace(algXML,".xml","_CCU.xml")}
cultbatch <- paste0("batch_",algXML) 
#clean old files 
unlink(paste0("../xml/aglu-xml/",algXML))
unlink(paste0("../aglu-processing-code/xml-batch/",cultbatch))

#NEED TO IMPROVE to extract directly from the data
aeznames <- read_csv("../algae-data/AlgaeGeospatial/algAEZ.csv",skip=3)
aezyears <- as_tibble(merge(aeznames,years))


## Get External Variables
Algae_yield_cost_factor  <- as.numeric(names(read.xlsx(ProcessData,namedRegion = 'Algae_yield_cost_factor'))) #$1975/m2/y
Algae_electricity_factor <- as.numeric(names(read.xlsx(ProcessData,namedRegion = 'Algae_electricity_factor'))) #GJ/m2/y
Algae_N_fert <- as.double(names(read.xlsx(ProcessData,namedRegion = 'Algae_N_fertilizer'))) # kg N / kg alg
Algae_P_fert <- as.numeric(names(read.xlsx(ProcessData,namedRegion = 'Algae_P_fertilizer'))) #kg DAP / kg alg
Algae_CO2_fert <- as.numeric(names(read.xlsx(ProcessData,namedRegion = 'Algae_CO2_fertilizer'))) #kg C/ kg alg

LandCarbon.Veg <- function(yield){
  yield/8.25*34.5/1000
}
LandCarbon.Soil <- function(AEZ){
  soil<-c(3,4,5,6,6,6,6,7,8,9,9,9,9,10,11,12,12,12)/2
  return(soil[AEZ])
}

#Copy over algae production data -- Could integrate in the future
yields <- read_csv("../algae-data/AlgaeGeospatial/top10.csv") %>%
  select(region,X6,Annual) %>%
  rename(AgSupplySubsector=X6)

#Equivalence Table
AlgC.EQUIV_TABLE <- tibble(group.name="LogitType",tag1="dummy-logit-tag",tag2="relative-cost-logit",tag3="absolute-cost-logit")
write_mi_data(AlgC.EQUIV_TABLE,"EQUIV_TABLE","AGLU_LEVEL2_DATA","AlgC.EQUIV_TABLE", "AGLU_XML_BATCH", cultbatch)


#Harvests Per Year
AlgC.AgHAtoCL <- aezyears %>% 
  mutate(AgSupplySector = "Algae") %>%
  rename(AgSupplySubsector = AEZ) %>%
  mutate(AgProductionTechnology = AgSupplySubsector) %>%
  mutate(harvests.per.year = 1) %>%
  select(region,AgSupplySector,AgSupplySubsector,AgProductionTechnology,year,harvests.per.year)
write_mi_data(AlgC.AgHAtoCL,"AgHAtoCL","AGLU_LEVEL2_DATA","AlgC.AgHAtoCL", "AGLU_XML_BATCH", cultbatch)

#Set Yields
AlgC.AgYield <- aezyears %>% 
  mutate(AgSupplySector = "Algae") %>%
  rename(AgSupplySubsector = AEZ) %>%
  mutate(AgProductionTechnology = AgSupplySubsector) %>%
  full_join(yields,by=c('region','AgSupplySubsector')) %>%
  mutate(yield = Annual * logimp(year)) %>%
  select(region,AgSupplySector,AgSupplySubsector,AgProductionTechnology,year,yield) %>% 
  signif_df(4)

#Shutoff for Production
if(ALGAE){
  write_mi_data(AlgC.AgYield,"AgYield","AGLU_LEVEL2_DATA","AlgC.AgYield", "AGLU_XML_BATCH", cultbatch)
  } else {
  write_mi_data(mutate(AlgC.AgYield,yield=0),"AgYield","AGLU_LEVEL2_DATA","AlgC.AgYield", "AGLU_XML_BATCH", cultbatch)}


#Cost
AlgC.AgCost <- AlgC.AgYield %>%
  mutate(nonLandVariableCost = Algae_yield_cost_factor/yield) %>%
  select(region,AgSupplySector,AgSupplySubsector,AgProductionTechnology,year,nonLandVariableCost) %>% 
  signif_df(4)
write_mi_data(AlgC.AgCost,"AgCost","AGLU_LEVEL2_DATA","AlgC.AgCost", "AGLU_XML_BATCH", cultbatch)

#Fertilizer
AlgC.AgCoef_Fert <- AlgC.AgYield %>%
  mutate(minicam.energy.input = 'N fertilizer',
         coefficient = Algae_N_fert) %>%
  select(region,AgSupplySector,AgSupplySubsector,AgProductionTechnology,year,minicam.energy.input,coefficient)  %>% 
  signif_df(4)
write_mi_data(AlgC.AgCoef_Fert,"AgCoef","AGLU_LEVEL2_DATA","AlgC.AgCoef_Fert", "AGLU_XML_BATCH", cultbatch)


#Electricity
AlgC.AgYield %>%
  mutate(minicam.energy.input = 'elect_td_ind',
         coefficient = Algae_electricity_factor/yield) %>%
  select(region,AgSupplySector,AgSupplySubsector,AgProductionTechnology,year,minicam.energy.input,coefficient) %>% 
  signif_df(4) %>% 
write_mi_data("AgCoef","AGLU_LEVEL2_DATA","AlgC.AgCoef_Elec", "AGLU_XML_BATCH", cultbatch)

#CCU Fertilizer
if (USECCU){
  AlgC.AgYield %>%
    mutate(minicam.energy.input = 'feedstock carbon',coefficient = Algae_CO2_fert) %>%
    select(region,AgSupplySector,AgSupplySubsector,AgProductionTechnology,year,minicam.energy.input,coefficient) %>% 
    signif_df(4) %>%
    write_mi_data("AgCoef","AGLU_LEVEL2_DATA","AlgC.AgCoef_FC", "AGLU_XML_BATCH", cultbatch)
}

#ACL - Subsector
AlgC.AgSupplySubsector_ACL <- tibble(region=character(),AgSupplySector=character(),AgSupplySubsector=character(),logit.type=character())
write_mi_data(AlgC.AgSupplySubsector_ACL,"AgSupplySubsector_absolute-cost-logit","AGLU_LEVEL2_DATA","AlgC.AgSupplySubsector_absolute-cost-logit", "AGLU_XML_BATCH", cultbatch)

#RCL - Subsector
AlgC.AgSupplySubsector_RCL <- aeznames %>% 
  mutate(AgSupplySector = "Algae") %>%
  rename(AgSupplySubsector = AEZ) %>%
  mutate(logit.type = "relative-cost-logit") %>%
  select(region,AgSupplySector,AgSupplySubsector,logit.type)
write_mi_data(AlgC.AgSupplySubsector_RCL,"AgSupplySubsector_relative-cost-logit","AGLU_LEVEL2_DATA","AlgC.AgSupplySubsector_relative-cost-logit", "AGLU_XML_BATCH", cultbatch)

#AgSupplySubsector
AlgC.AgSupplySubsector <- aeznames %>% 
  mutate(AgSupplySector = "Algae") %>%
  rename(AgSupplySubsector = AEZ) %>%
  mutate(logit.year.fillout = 1975, logit.exponent = -3) %>%
  select(region,AgSupplySector,AgSupplySubsector,logit.year.fillout,logit.exponent)
write_mi_data(AlgC.AgSupplySubsector,"AgSupplySubsector","AGLU_LEVEL2_DATA","AlgC.AgSupplySubsector", "AGLU_XML_BATCH", cultbatch)

#ACL - Sector
AlgC.AgSupplySector_ACL <- tibble(region=character(),AgSupplySector=character(),logit.type=character())
write_mi_data(AlgC.AgSupplySector_ACL,"AgSupplySector_absolute-cost-logit","AGLU_LEVEL2_DATA","AlgC.AgSupplySector_absolute-cost-logit", "AGLU_XML_BATCH", cultbatch)

#RCL - Sector
AlgC.AgSupplySector_RCL <- algregnames %>% 
  mutate(AgSupplySector = "Algae", logit.type = "relative-cost-logit") 
write_mi_data(AlgC.AgSupplySector_RCL,"AgSupplySector_relative-cost-logit","AGLU_LEVEL2_DATA","AlgC.AgSupplySector_relative-cost-logit", "AGLU_XML_BATCH", cultbatch)

#AgSupplySector
AlgC.AgSupplySector <- algregnames %>% 
  mutate(AgSupplySector = "Algae",
         output.unit="Mt", input.unit = "thous km2", price.unit="1975$/kg",calPrice=0.2,
         market=region,logit.year.fillout=1975,logit.exponent=-3)
write_mi_data(AlgC.AgSupplySector,"AgSupplySector","AGLU_LEVEL2_DATA","AlgC.AgSupplySector", "AGLU_XML_BATCH", cultbatch)

#Land
AlgL.LN3_NewTech <- aeznames %>% 
  mutate(LandAllocatorRoot='root') %>% rename(LandLeaf=AEZ) %>%
  mutate(LandNode1 = gsub("Algae","AgroForestLand",LandLeaf))%>%
  mutate(LandNode2 = gsub("Algae","AgroForest_NonPasture",LandLeaf))%>%
  mutate(LandNode3 = gsub("Algae","CropLand",LandLeaf)) %>%
  mutate(year.fillout = 2020,isNewTechnology=1) %>%
  select(region, LandAllocatorRoot, LandNode1, LandNode2, LandNode3, LandLeaf, year.fillout, isNewTechnology)
write_mi_data(AlgL.LN3_NewTech,"LN3_NewTech","AGLU_LEVEL2_DATA","AlgL.LN3_NewTech", "AGLU_XML_BATCH", cultbatch,node_rename=T)

AlgL.LN3_MgdCarbon <- aeznames %>% 
  mutate(LandAllocatorRoot='root') %>% rename(LandLeaf=AEZ) %>%
  mutate(LandNode1 = gsub("Algae","AgroForestLand",LandLeaf)) %>%
  mutate(LandNode2 = gsub("Algae","AgroForest_NonPasture",LandLeaf)) %>%
  mutate(LandNode3 = gsub("Algae","CropLand",LandLeaf)) %>% 
  left_join(yields,by=c("LandLeaf"="AgSupplySubsector","region")) %>%
  mutate(hist.veg.carbon.density = LandCarbon.Veg(Annual)) %>% 
  mutate(hist.soil.carbon.density = LandCarbon.Soil(as.numeric(substring(LandLeaf, 9, 11)))) %>%
  mutate(veg.carbon.density = hist.veg.carbon.density, soil.carbon.density=hist.soil.carbon.density,
         mature.age.year.fillout=1975, mature.age=0,min.veg.carbon.density = 0, min.soil.carbon.density =0) %>% 
  select(region, LandAllocatorRoot, LandNode1, LandNode2, LandNode3, LandLeaf, hist.veg.carbon.density, hist.soil.carbon.density, veg.carbon.density, soil.carbon.density, mature.age.year.fillout, mature.age, min.veg.carbon.density, min.soil.carbon.density) %>% 
  signif_df(4)
write_mi_data(AlgL.LN3_MgdCarbon,"LN3_MgdCarbon","AGLU_LEVEL2_DATA","AlgL.LN3_MgdCarbon", "AGLU_XML_BATCH", cultbatch,node_rename=T)

insert_file_into_batchxml( "AGLU_XML_BATCH", cultbatch, "AGLU_XML_FINAL", algXML, "", xml_tag="outFile" )

#Example
system(paste0('java -Xmx2g -jar ../_common/ModelInterface/src/CSVToXML.jar ../aglu-processing-code/xml-batch/',cultbatch))
  