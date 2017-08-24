#Algae Data System


source("../algae-processing-code/AlgaeHeaders.r")

CCUMode <- 'gas'
secoutput.sector = "electricity"
suffix <- ""
comboXML <- "alg_cult_combo.xml"
combobatch <- paste0('batch_alg_cult_combo_',CCUMode,suffix,'.xml')
TechDict <- list(gas="gas (CC CCU)",biomass="biomass (IGCC CCU)",coal="coal (IGCC CCU")
CCSDict <- list(gas="gas (CC CCS)",biomass="biomass (IGCC CCS)",coal="coal (IGCC CCS)")
CCUshwt <- c(0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1) #Use 0 to shut off
InputDict <- list(gas="wholesale gas",biomass="regional biomass",coal="regional coal")

ALGAE = TRUE
ALGFOOD = TRUE
COPRODUCTION = TRUE

StartYear = 1975

FixedFoodDemand = TRUE

unlink(paste0("../xml/aglu-xml/",comboXML))
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


###Following Sheets Inverts CCU to CCS 
CO2Coef <- getInput("L202.CarbonCoef") %>% 
  filter(PrimaryFuelCO2Coef.name==InputDict[[CCUMode]]) %>% 
  summarise(mean(PrimaryFuelCO2Coef)) %>% as.numeric()

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
AlgCCC.EQUIV_TABLE <- tibble(group.name="LogitType",tag1="dummy-logit-tag",tag2="relative-cost-logit",tag3="absolute-cost-logit")
write_mi_data(AlgCCC.EQUIV_TABLE,"EQUIV_TABLE","AGLU_LEVEL2_DATA","AlgCCC.EQUIV_TABLE", "AGLU_XML_BATCH", combobatch)


#Harvests Per Year
AlgCCC.AgHAtoCL <- aezyears %>% 
  mutate(AgSupplySector = "Algae") %>%
  rename(AgSupplySubsector = AEZ) %>%
  mutate(AgProductionTechnology = AgSupplySubsector) %>%
  mutate(harvests.per.year = 1) %>%
  select(region,AgSupplySector,AgSupplySubsector,AgProductionTechnology,year,harvests.per.year)
write_mi_data(AlgCCC.AgHAtoCL,"AgHAtoCL","AGLU_LEVEL2_DATA","AlgCCC.AgHAtoCL", "AGLU_XML_BATCH", combobatch)

#Set Yields
AlgCCC.AgYield <- aezyears %>% 
  mutate(AgSupplySector = "Algae") %>%
  rename(AgSupplySubsector = AEZ) %>%
  mutate(AgProductionTechnology = AgSupplySubsector) %>%
  full_join(yields,by=c('region','AgSupplySubsector')) %>%
  mutate(yield = Annual * logimp(year)) %>%
  select(region,AgSupplySector,AgSupplySubsector,AgProductionTechnology,year,yield) %>% 
  signif_df(4)

#Shutoff for Production
if(ALGAE){
  write_mi_data(AlgCCC.AgYield,"AgYield","AGLU_LEVEL2_DATA","AlgCCC.AgYield", "AGLU_XML_BATCH", combobatch)
} else {
  write_mi_data(mutate(AlgCCC.AgYield,yield=0),"AgYield","AGLU_LEVEL2_DATA","AlgCCC.AgYield", "AGLU_XML_BATCH", combobatch)}


#Need to sum up OM var fixed 
AlgCCC.AgCost <- AlgCCC.AgYield %>%
  mutate(nonLandVariableCost = Algae_yield_cost_factor/yield) %>%
  select(region,AgSupplySector,AgSupplySubsector,AgProductionTechnology,year,nonLandVariableCost) %>% 
  signif_df(4)
write_mi_data(AlgCCC.AgCost,"AgCost","AGLU_LEVEL2_DATA","AlgCCC.AgCost", "AGLU_XML_BATCH", combobatch)

#Fertilizer
AlgCCC.AgCoef_Fert <- AlgCCC.AgYield %>%
  mutate(minicam.energy.input = 'N fertilizer',
         coefficient = Algae_N_fert) %>%
  select(region,AgSupplySector,AgSupplySubsector,AgProductionTechnology,year,minicam.energy.input,coefficient)  %>% 
  signif_df(4)
write_mi_data(AlgCCC.AgCoef_Fert,"AgCoef","AGLU_LEVEL2_DATA","AlgCCC.AgCoef_Fert", "AGLU_XML_BATCH", combobatch)


#Feedstock Energy
AlgCCC.AgYield %>%
  merge(select(Old,year,FCSecOut)) %>%
  as_tibble() %>%
  replace_na(list(FCSecOut=max(.$FCSecOut,na.rm=TRUE))) %>% 
  mutate(minicam.energy.input = InputDict[[CCUMode]],
         coefficient = Algae_electricity_factor/yield * FCSecOut / Algae_CO2_fert) %>%
  select(region,AgSupplySector,AgSupplySubsector,AgProductionTechnology,year,minicam.energy.input,coefficient) %>% 
  signif_df(4) %>% 
  write_mi_data("AgCoef","AGLU_LEVEL2_DATA","AlgCCC.AgCoef_Fuel", "AGLU_XML_BATCH", combobatch)


#Elec SecOut 
bind_rows(AlgCCC.AgYield %>%
            merge(select(Old,year,FCSecOut)) %>%
            as_tibble() %>% select(-c(yield,FCSecOut)) %>% 
              mutate(fractional.secondary.output=secoutput.sector,
                     price=0,fraction.produced=0),
          AlgCCC.AgYield %>%
            merge(select(Old,year,FCSecOut)) %>%
            as_tibble() %>% select(-c(yield,FCSecOut)) %>% 
            mutate(fractional.secondary.output=secoutput.sector,
                     price=0.1,fraction.produced=1)) %>% 
  select(c(region,AgSupplySector,AgSupplySubsector,AgProductionTechnology,year,fractional.secondary.output,price,fraction.produced)) %>%
  write_mi_data("AgTechFractProd","AGLU_LEVEL2_DATA","AlgCCC.AgTechFractProd", "AGLU_XML_BATCH", combobatch)

#Elec SecOut 
AlgCCC.AgYield %>%
  merge(select(Old,year,FCSecOut)) %>%
  as_tibble() %>%
  replace_na(list(FCSecOut=max(.$FCSecOut,na.rm=TRUE))) %>% 
  mutate(fractional.secondary.output=secoutput.sector,
         output.ratio = Algae_CO2_fert / FCSecOut - Algae_electricity_factor/yield ) %>%
  select(region,AgSupplySector,AgSupplySubsector,AgProductionTechnology,year,fractional.secondary.output,output.ratio) %>% 
  signif_df(4) %>% 
  write_mi_data("AgTechFractSecOut","AGLU_LEVEL2_DATA","AlgCCC.AgTechFractSecOut", "AGLU_XML_BATCH", combobatch)

#Need to write a AgTech Secondary Output



#ACL - Subsector
AlgCCC.AgSupplySubsector_ACL <- tibble(region=character(),AgSupplySector=character(),AgSupplySubsector=character(),logit.type=character())
write_mi_data(AlgCCC.AgSupplySubsector_ACL,"AgSupplySubsector_absolute-cost-logit","AGLU_LEVEL2_DATA","AlgCCC.AgSupplySubsector_absolute-cost-logit", "AGLU_XML_BATCH", combobatch)

#RCL - Subsector
AlgCCC.AgSupplySubsector_RCL <- aeznames %>% 
  mutate(AgSupplySector = "Algae") %>%
  rename(AgSupplySubsector = AEZ) %>%
  mutate(logit.type = "relative-cost-logit") %>%
  select(region,AgSupplySector,AgSupplySubsector,logit.type)
write_mi_data(AlgCCC.AgSupplySubsector_RCL,"AgSupplySubsector_relative-cost-logit","AGLU_LEVEL2_DATA","AlgCCC.AgSupplySubsector_relative-cost-logit", "AGLU_XML_BATCH", combobatch)

#AgSupplySubsector
AlgCCC.AgSupplySubsector <- aeznames %>% 
  mutate(AgSupplySector = "Algae") %>%
  rename(AgSupplySubsector = AEZ) %>%
  mutate(logit.year.fillout = 1975, logit.exponent = -3) %>%
  select(region,AgSupplySector,AgSupplySubsector,logit.year.fillout,logit.exponent)
write_mi_data(AlgCCC.AgSupplySubsector,"AgSupplySubsector","AGLU_LEVEL2_DATA","AlgCCC.AgSupplySubsector", "AGLU_XML_BATCH", combobatch)

#ACL - Sector
AlgCCC.AgSupplySector_ACL <- tibble(region=character(),AgSupplySector=character(),logit.type=character())
write_mi_data(AlgCCC.AgSupplySector_ACL,"AgSupplySector_absolute-cost-logit","AGLU_LEVEL2_DATA","AlgCCC.AgSupplySector_absolute-cost-logit", "AGLU_XML_BATCH", combobatch)

#RCL - Sector
AlgCCC.AgSupplySector_RCL <- algregnames %>% 
  mutate(AgSupplySector = "Algae", logit.type = "relative-cost-logit") 
write_mi_data(AlgCCC.AgSupplySector_RCL,"AgSupplySector_relative-cost-logit","AGLU_LEVEL2_DATA","AlgCCC.AgSupplySector_relative-cost-logit", "AGLU_XML_BATCH", combobatch)

#AgSupplySector
AlgCCC.AgSupplySector <- algregnames %>% 
  mutate(AgSupplySector = "Algae",
         output.unit="Mt", input.unit = "thous km2", price.unit="1975$/kg",calPrice=0.2,
         market=region,logit.year.fillout=1975,logit.exponent=-3)
write_mi_data(AlgCCC.AgSupplySector,"AgSupplySector","AGLU_LEVEL2_DATA","AlgCCC.AgSupplySector", "AGLU_XML_BATCH", combobatch)


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

