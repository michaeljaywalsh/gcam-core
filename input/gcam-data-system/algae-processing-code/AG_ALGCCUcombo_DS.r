#Algae Data System


source("../algae-processing-code/AlgaeHeaders.r")

CCUMode <- 'gas'
secoutput.sector = "electricity"
suffix <- ""
comboXML <- "ag_algccu.xml"
combobatch <- paste0('batch_ag_algccu_',CCUMode,suffix,'.xml')
TechDict <- list(gas="gas (CC CCU)",biomass="biomass (IGCC CCU)",coal="coal (IGCC CCU")
CCSDict <- list(gas="gas (CC CCS)",biomass="biomass (IGCC CCS)",coal="coal (IGCC CCS)")
CCUshwt <- c(0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1) #Use 0 to shut off
InputDict <- list(gas="wholesale gas",biomass="regional biomass",coal="regional coal")

ALGAE = TRUE
ALGFOOD = TRUE
COPRODUCTION = TRUE

StartYear = 1975

#Global Tech Share Weights 1975-2100
AltAlgFood.shwt <- c(0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
AltAlgCoprd.shwt <-  c(0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1) #Use 0 to shut off
FeedAlt.shwt <- c(0,0,0,0,0,.2,.4,.6,.8,1,1,1,1,1,1,1,1,1,1,1,1,1) #use 0 to shut off
DemandAlt.shwt <- c(0,0,0,0,0,.2,.4,.6,.8,1,1,1,1,1,1,1,1,1,1,1,1,1) #use 0 to shutoff

FixedFoodDemand = TRUE

unlink(paste0("../xml/aglu-xml/",algXML))
unlink(paste0("../aglu-processing-code/xml-batch/",combobatch))


#NEED TO IMPROVE to extract directly from the data
aeznames <- read_csv("../algae-data/AlgaeGeospatial/algAEZ.csv",skip=3)
aezyears <- as_tibble(merge(aeznames,years))


## Get External Variables
Algae_yield_cost_factor  <- as.numeric(names(read.xlsx(ProcessData,namedRegion = 'Algae_yield_cost_factor'))) #$1975/m2/y
Algae_electricity_factor <- as.numeric(names(read.xlsx(ProcessData,namedRegion = 'Algae_electricity_factor'))) #GJ/m2/y
Algae_N_fert <- as.double(names(read.xlsx(ProcessData,namedRegion = 'Algae_N_fertilizer'))) # kg N / kg alg
Algae_P_fert <- as.numeric(names(read.xlsx(ProcessData,namedRegion = 'Algae_P_fertilizer'))) #kg DAP / kg alg
Algae_CO2_fert <- as.numeric(names(read.xlsx(ProcessData,namedRegion = 'Algae_CO2_fertilizer'))) #kg C/ kg alg

AltFood.elec <- as.numeric(names(read.xlsx(ProcessData,namedRegion = 'AltFood_electricity')))
AltFood.gas <- as.numeric(names(read.xlsx(ProcessData,namedRegion = 'AltFood_wholesale_gas')))
AltFood.alg <- 1 
AltFood.cost <- as.numeric(names(read.xlsx(ProcessData,namedRegion = 'AltFood_input_cost')))
Calorie.alg <- 4.5

LandCarbon.Veg <- function(yield){
  yield/8.25*34.5/1000
}
LandCarbon.Soil <- function(AEZ){
  soil<-c(3,4,5,6,6,6,6,7,8,9,9,9,9,10,11,12,12,12)/2
  return(soil[AEZ])
}


#Get Data to Prepare for Inversion
Old <- left_join(getInput("L223.GlobalTechOMvar_elec"),getInput("L223.GlobalTechOMfixed_elec") ) %>% 
  left_join(getInput("L223.GlobalTechCapital_elec")) %>%
  left_join(getInput("L223.GlobalTechEff_elec")) %>%
  left_join(getInput("L223.GlobalTechCapture_elec")) %>%
  left_join(getInput("L223.GlobalTechEff_elec")) %>%
  select(-c(input.OM.var,input.OM.fixed,input.capital,storage.market)) %>%
  filter(technology == CCSDict[[CCUMode]], year>=StartYear) %>%
  mutate(PrimaryFuelCO2coef = CO2Coef) %>%
  mutate(FCSecOut = PrimaryFuelCO2coef/efficiency * remove.fraction) #CC/elecltricity

#Copy over algae production data -- Could integrate in the future
yields <- read_csv("../algae-data/AlgaeGeospatial/top10.csv") %>%
  select(region,X6,Annual) %>%
  rename(AgSupplySubsector=X6)

#Equivalence Table
AlgACC.EQUIV_TABLE <- tibble(group.name="LogitType",tag1="dummy-logit-tag",tag2="relative-cost-logit",tag3="absolute-cost-logit")
write_mi_data(AlgACC.EQUIV_TABLE,"EQUIV_TABLE","AGLU_LEVEL2_DATA","AlgACC.EQUIV_TABLE", "AGLU_XML_BATCH", combobatch)


#Harvests Per Year
AlgACC.AgHAtoCL <- aezyears %>% 
  mutate(AgSupplySector = "Algae") %>%
  rename(AgSupplySubsector = AEZ) %>%
  mutate(AgProductionTechnology = AgSupplySubsector) %>%
  mutate(harvests.per.year = 1) %>%
  select(region,AgSupplySector,AgSupplySubsector,AgProductionTechnology,year,harvests.per.year)
write_mi_data(AlgACC.AgHAtoCL,"AgHAtoCL","AGLU_LEVEL2_DATA","AlgACC.AgHAtoCL", "AGLU_XML_BATCH", combobatch)

#Set Yields
AlgACC.AgYield <- aezyears %>% 
  mutate(AgSupplySector = "Algae") %>%
  rename(AgSupplySubsector = AEZ) %>%
  mutate(AgProductionTechnology = AgSupplySubsector) %>%
  full_join(yields,by=c('region','AgSupplySubsector')) %>%
  mutate(yield = Annual * logimp(year)) %>%
  select(region,AgSupplySector,AgSupplySubsector,AgProductionTechnology,year,yield) %>% 
  signif_df(4)

#Shutoff for Production
if(ALGAE){
  write_mi_data(AlgACC.AgYield,"AgYield","AGLU_LEVEL2_DATA","AlgACC.AgYield", "AGLU_XML_BATCH", combobatch)
} else {
  write_mi_data(mutate(AlgACC.AgYield,yield=0),"AgYield","AGLU_LEVEL2_DATA","AlgACC.AgYield", "AGLU_XML_BATCH", combobatch)}


#Cost
AlgACC.AgCost <- AlgACC.AgYield %>%
  mutate(nonLandVariableCost = Algae_yield_cost_factor/yield) %>%
  select(region,AgSupplySector,AgSupplySubsector,AgProductionTechnology,year,nonLandVariableCost) %>% 
  signif_df(4)
write_mi_data(AlgACC.AgCost,"AgCost","AGLU_LEVEL2_DATA","AlgACC.AgCost", "AGLU_XML_BATCH", combobatch)

#Fertilizer
AlgACC.AgCoef_Fert <- AlgACC.AgYield %>%
  mutate(minicam.energy.input = 'N fertilizer',
         coefficient = Algae_N_fert) %>%
  select(region,AgSupplySector,AgSupplySubsector,AgProductionTechnology,year,minicam.energy.input,coefficient)  %>% 
  signif_df(4)
write_mi_data(AlgACC.AgCoef_Fert,"AgCoef","AGLU_LEVEL2_DATA","AlgACC.AgCoef_Fert", "AGLU_XML_BATCH", combobatch)


#Feedstock Energy
AlgACC.AgYield %>%
  merge(select(Old,year,FCSecOut)) %>%
  as_tibble() %>%
  replace_na(list(FCSecOut=max(.$FCSecOut,na.rm=TRUE))) %>% 
  mutate(minicam.energy.input = InputDict[[CCUMode]],
         coefficient = Algae_electricity_factor/yield * FCSecOut / Algae_CO2_fert) %>%
  select(region,AgSupplySector,AgSupplySubsector,AgProductionTechnology,year,minicam.energy.input,coefficient) %>% 
  signif_df(4) %>% 
  write_mi_data("AgCoef","AGLU_LEVEL2_DATA","AlgACC.AgCoef_Elec", "AGLU_XML_BATCH", combobatch)


#Elec SecOut 
bind_rows(AlgACC.AgYield %>%
            merge(select(Old,year,FCSecOut)) %>%
            as_tibble() %>% select(-c(yield,FCSecOut)) %>% 
              mutate(fractional.secondary.output=secoutput.sector,
                     price=0,fraction.produced=0),
          AlgACC.AgYield %>%
            merge(select(Old,year,FCSecOut)) %>%
            as_tibble() %>% select(-c(yield,FCSecOut)) %>% 
            mutate(fractional.secondary.output=secoutput.sector,
                     price=0.1,fraction.produced=1)) %>% 
  write_mi_data("AgCoef","AGLU_LEVEL2_DATA","AlgACC.AgTechFractSecOut", "AGLU_XML_BATCH", combobatch)

#Elec SecOut 
AlgACC.AgYield %>%
  merge(select(Old,year,FCSecOut)) %>%
  as_tibble() %>%
  replace_na(list(FCSecOut=max(.$FCSecOut,na.rm=TRUE))) %>% 
  mutate(fractional.secondary.output=secoutput.sector,
         output.ratio = Algae_CO2_fert / FCSecOut - Algae_electricity_factor/yield ) %>%
  select(region,AgSupplySector,AgSupplySubsector,AgProductionTechnology,year,fractional.secondary.output,output.ratio) %>% 
  signif_df(4) %>% 
  write_mi_data("AgCoef","AGLU_LEVEL2_DATA","AlgACC.AgTechFractSecOut", "AGLU_XML_BATCH", combobatch)

#Need to write a AgTech Secondary Output

#Fuel Input
AlgACC.AgYield %>%
  mutate(minicam.energy.input = 'elect_td_ind',
         coefficient = Algae_electricity_factor/yield) %>%
  select(region,AgSupplySector,AgSupplySubsector,AgProductionTechnology,year,minicam.energy.input,coefficient) %>% 
  signif_df(4) %>% 
  write_mi_data("AgCoef","AGLU_LEVEL2_DATA","AlgACC.AgCoef_Elec", "AGLU_XML_BATCH", combobatch)



#ACL - Subsector
AlgACC.AgSupplySubsector_ACL <- tibble(region=character(),AgSupplySector=character(),AgSupplySubsector=character(),logit.type=character())
write_mi_data(AlgACC.AgSupplySubsector_ACL,"AgSupplySubsector_absolute-cost-logit","AGLU_LEVEL2_DATA","AlgACC.AgSupplySubsector_absolute-cost-logit", "AGLU_XML_BATCH", combobatch)

#RCL - Subsector
AlgACC.AgSupplySubsector_RCL <- aeznames %>% 
  mutate(AgSupplySector = "Algae") %>%
  rename(AgSupplySubsector = AEZ) %>%
  mutate(logit.type = "relative-cost-logit") %>%
  select(region,AgSupplySector,AgSupplySubsector,logit.type)
write_mi_data(AlgACC.AgSupplySubsector_RCL,"AgSupplySubsector_relative-cost-logit","AGLU_LEVEL2_DATA","AlgACC.AgSupplySubsector_relative-cost-logit", "AGLU_XML_BATCH", combobatch)

#AgSupplySubsector
AlgACC.AgSupplySubsector <- aeznames %>% 
  mutate(AgSupplySector = "Algae") %>%
  rename(AgSupplySubsector = AEZ) %>%
  mutate(logit.year.fillout = 1975, logit.exponent = -3) %>%
  select(region,AgSupplySector,AgSupplySubsector,logit.year.fillout,logit.exponent)
write_mi_data(AlgACC.AgSupplySubsector,"AgSupplySubsector","AGLU_LEVEL2_DATA","AlgACC.AgSupplySubsector", "AGLU_XML_BATCH", combobatch)

#ACL - Sector
AlgACC.AgSupplySector_ACL <- tibble(region=character(),AgSupplySector=character(),logit.type=character())
write_mi_data(AlgACC.AgSupplySector_ACL,"AgSupplySector_absolute-cost-logit","AGLU_LEVEL2_DATA","AlgACC.AgSupplySector_absolute-cost-logit", "AGLU_XML_BATCH", combobatch)

#RCL - Sector
AlgACC.AgSupplySector_RCL <- algregnames %>% 
  mutate(AgSupplySector = "Algae", logit.type = "relative-cost-logit") 
write_mi_data(AlgACC.AgSupplySector_RCL,"AgSupplySector_relative-cost-logit","AGLU_LEVEL2_DATA","AlgACC.AgSupplySector_relative-cost-logit", "AGLU_XML_BATCH", combobatch)

#AgSupplySector
AlgACC.AgSupplySector <- algregnames %>% 
  mutate(AgSupplySector = "Algae",
         output.unit="Mt", input.unit = "thous km2", price.unit="1975$/kg",calPrice=0.2,
         market=region,logit.year.fillout=1975,logit.exponent=-3)
write_mi_data(AlgACC.AgSupplySector,"AgSupplySector","AGLU_LEVEL2_DATA","AlgACC.AgSupplySector", "AGLU_XML_BATCH", combobatch)

###AltFood #Does not include co-production!!!! 
AltFoodYears<- years %>% mutate(sector.name='AltFood',subsector.name='Algae Food',technology='Algae Food')

AltF.GlobalTechCoef <- bind_rows(AltFoodYears %>% mutate(minicam.energy.input='elect_td_ind',coefficient=AltFood.elec),
                                 AltFoodYears %>% mutate(minicam.energy.input='wholesale gas',coefficient=AltFood.gas),
                                 AltFoodYears %>% mutate(minicam.energy.input='Algae',coefficient=AltFood.alg)) %>% 
  select(sector.name,subsector.name,technology,year,minicam.energy.input,coefficient) %>% 
  signif_df(4)
write_mi_data(AltF.GlobalTechCoef,"GlobalTechCoef","AGLU_LEVEL2_DATA","AltF.GlobalTechCoef", "AGLU_XML_BATCH", combobatch)

AltF.GlobalTechCost <- AltFoodYears %>% mutate(minicam.non.energy.input="non-energy",input.cost=AltFood.cost) %>%
  select(sector.name,subsector.name,technology,year,minicam.non.energy.input,input.cost) %>% 
  signif_df(4)
write_mi_data(AltF.GlobalTechCost,"GlobalTechCost","AGLU_LEVEL2_DATA","AltF.GlobalTechCost", "AGLU_XML_BATCH", combobatch)

AltF.Supplysector_RCL <- algregnames %>% 
  mutate(supplysector = "AltFood", logit.type="relative-cost-logit")
write_mi_data(AltF.Supplysector_RCL,"Supplysector_relative-cost-logit","AGLU_LEVEL2_DATA","AltF.Supplysector_relative-cost-logit", "AGLU_XML_BATCH", combobatch)

AltF.Supplysector_ACL <- tibble(region=character(),supplysector=character(),logit.type=character())
write_mi_data(AltF.Supplysector_ACL,"Supplysector_absolute-cost-logit","AGLU_LEVEL2_DATA","AltF.Supplysector_absolute-cost-logit", "AGLU_XML_BATCH", combobatch)

AltF.Supplysector <- algregnames %>% 
  select(region) %>% 
  mutate(supplysector="AltFood",output.unit="Mt",input.unit="Mt",
         price.unit="1975$/kg",logit.year.fillout=1975,logit.exponent=-1.5)
write_mi_data(AltF.Supplysector,"Supplysector","AGLU_LEVEL2_DATA","AltF.Supplysector", "AGLU_XML_BATCH", combobatch)

AltF.Subsector_RCL <- algregnames %>% 
  mutate(supplysector = "AltFood", subsector = "Algae Food", logit.type="relative-cost-logit")
write_mi_data(AltF.Subsector_RCL,"SubsectorLogit_relative-cost-logit","AGLU_LEVEL2_DATA","AltF.SubsectorLogit_relative-cost-logit", "AGLU_XML_BATCH", combobatch)

AltF.Subsector_ACL <- tibble(region=character(),supplysector=character(),subsector=character(),logit.type=character())
write_mi_data(AltF.Subsector_ACL,"SubsectorLogit_absolute-cost-logit","AGLU_LEVEL2_DATA","AltF.SubsectorLogit_absolute-cost-logit", "AGLU_XML_BATCH", combobatch)

AltF.SubsectorAll <- algregnames %>% 
  mutate(supplysector = "AltFood", subsector = "Algae Food", logit.year.fillout=1975,
         logit.exponent=-6,year.fillout=2020,share.weight=1, apply.to ="share-weight", 
         from.year=2020, to.year=2100, interpolation.function="fixed")
write_mi_data(AltF.SubsectorAll,"SubsectorAll","AGLU_LEVEL2_DATA","AltF.SubsectorAll", "AGLU_XML_BATCH", combobatch)

AltF.StubTechProd <- algregnames %>% 
  mutate(supplysector = "AltFood", subsector = "Algae Food", stub.technology = "Algae Food") %>% 
  merge(baseyears) %>%
  mutate(calOutputValue=0,share.weight.year=1975,subs.share.weight=0,tech.share.weight=0)
write_mi_data(AltF.StubTechProd,"StubTechProd","AGLU_LEVEL2_DATA","AltF.StubTechProd", "AGLU_XML_BATCH", combobatch)

AltF.StubTech <- algregnames %>% 
  mutate(supplysector = "AltFood", subsector="Algae Food", stub.technology="Algae Food")
write_mi_data(AltF.StubTech,"StubTech","AGLU_LEVEL2_DATA","AltF.StubTech", "AGLU_XML_BATCH", combobatch)

AltF.GlobalTechShrwt <- AltFoodYears %>% 
  mutate(share.weight = AltAlgFood.shwt) %>%
  select(sector.name,subsector.name,technology,year,share.weight)
write_mi_data(AltF.GlobalTechShrwt,"GlobalTechShrwt","AGLU_LEVEL2_DATA","AltF.GlobalTechShrwt", "AGLU_XML_BATCH", combobatch)

#Feed
FeedYears<- years %>% mutate(sector.name='FeedCrops',subsector.name='AltFood',technology='AltFood')

Feed.Supplysector_RCL <- algregnames %>% 
  mutate(supplysector = "FeedCrops", logit.type="relative-cost-logit") 
write_mi_data(Feed.Supplysector_RCL,"Supplysector_relative-cost-logit","AGLU_LEVEL2_DATA","Feed.Supplysector_relative-cost-logit", "AGLU_XML_BATCH", combobatch)

Feed.Supplysector_ACL <- tibble(region=character(),supplysector=character(),logit.type=character())
write_mi_data(Feed.Supplysector_ACL,"Supplysector_absolute-cost-logit","AGLU_LEVEL2_DATA","Feed.Supplysector_absolute-cost-logit", "AGLU_XML_BATCH", combobatch)

Feed.Subsector_RCL <- algregnames %>% 
  mutate(supplysector = "FeedCrops", subsector = "AltFood", logit.type="relative-cost-logit")
write_mi_data(Feed.Subsector_RCL,"SubsectorLogit_relative-cost-logit","AGLU_LEVEL2_DATA","Feed.SubsectorLogit__relative-cost-logit", "AGLU_XML_BATCH", combobatch)

Feed.Subsector_ACL <- tibble(region=character(),supplysector=character(),subsector=character(),logit.type=character())
write_mi_data(Feed.Subsector_ACL,"SubsectorLogit_absolute-cost-logit","AGLU_LEVEL2_DATA","Feed.SubsectorLogit_absolute-cost-logit", "AGLU_XML_BATCH", combobatch)

Feed.StubTechProd <- algregnames %>% 
  mutate(supplysector = "FeedCrops", subsector = "AltFood", stub.technology = "AltFood") %>% 
  merge(tibble(year=c(1975,1990,2005,2010))) %>%
  mutate(calOutputValue=0,share.weight.year=year,subs.share.weight=0,tech.share.weight=0)
write_mi_data(Feed.StubTechProd,"StubTechProd","AGLU_LEVEL2_DATA","Feed.StubTechProd", "AGLU_XML_BATCH", combobatch)

Feed.GlobalTechShrwt <- FeedYears %>% 
  mutate(share.weight = FeedAlt.shwt) %>%
  select(sector.name,subsector.name,technology,year,share.weight) 
write_mi_data(Feed.GlobalTechShrwt,"GlobalTechShrwt","AGLU_LEVEL2_DATA","Feed.GlobalTechShrwt", "AGLU_XML_BATCH", combobatch)

Feed.StubTech <- algregnames %>% 
  mutate(supplysector = "FeedCrops", subsector="AltFood", stub.technology="AltFood")
write_mi_data(Feed.StubTech,"StubTech","AGLU_LEVEL2_DATA","Feed.StubTech", "AGLU_XML_BATCH", combobatch)

Feed.GlobalTechCoef <- FeedYears %>% 
  mutate(sector.name="FeedCrops",minicam.energy.input='AltFood',coefficient=1) %>% 
  select(sector.name,subsector.name,technology,year,minicam.energy.input,coefficient) 
write_mi_data(Feed.GlobalTechCoef,"GlobalTechCoef","AGLU_LEVEL2_DATA","Feed.GlobalTechCoef", "AGLU_XML_BATCH", combobatch)

Feed.SubsectorAll <- algregnames %>% 
  mutate(supplysector = "FeedCrops", subsector = "AltFood", logit.year.fillout=1975,
         logit.exponent=-6,year.fillout=2020,share.weight=1, apply.to ="share-weight", 
         from.year=2020, to.year=2100, interpolation.function="fixed")
write_mi_data(Feed.SubsectorAll,"SubsectorAll","AGLU_LEVEL2_DATA","Feed.SubsectorAll", "AGLU_XML_BATCH", combobatch)


#Land
Land.LN3_NewTech <- aeznames %>% 
  mutate(LandAllocatorRoot='root') %>% rename(LandLeaf=AEZ) %>%
  mutate(LandNode1 = gsub("Algae","AgroForestLand",LandLeaf))%>%
  mutate(LandNode2 = gsub("Algae","AgroForest_NonPasture",LandLeaf))%>%
  mutate(LandNode3 = gsub("Algae","CropLand",LandLeaf)) %>%
  mutate(year.fillout = 2020,isNewTechnology=1) %>%
  select(region, LandAllocatorRoot, LandNode1, LandNode2, LandNode3, LandLeaf, year.fillout, isNewTechnology)
write_mi_data(Land.LN3_NewTech,"LN3_NewTech","AGLU_LEVEL2_DATA","Land.LN3_NewTech", "AGLU_XML_BATCH", combobatch,node_rename=T)

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
  select(region, LandAllocatorRoot, LandNode1, LandNode2, LandNode3, LandLeaf, hist.veg.carbon.density, hist.soil.carbon.density, veg.carbon.density, soil.carbon.density, mature.age.year.fillout, mature.age, min.veg.carbon.density, min.soil.carbon.density) %>% 
  signif_df(4)
write_mi_data(Land.LN3_MgdCarbon,"LN3_MgdCarbon","AGLU_LEVEL2_DATA","Land.LN3_MgdCarbon", "AGLU_XML_BATCH", combobatch,node_rename=T)



insert_file_into_batchxml( "AGLU_XML_BATCH", combobatch, "AGLU_XML_FINAL", comboXML, "", xml_tag="outFile" )
system(paste0('java -Xmx2g -jar ../_common/ModelInterface/src/CSVToXML.jar ../aglu-processing-code/xml-batch/',combobatch))

