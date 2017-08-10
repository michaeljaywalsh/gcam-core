#CCU Data System
#This file inverts the GCAM CCS technologes to ouptut 
#Currently no shutdown decider properties are included

source("../algae-processing-code/AlgaeHeaders.r")



CCUMode <- 'gas'
suffix <- ""
CCUXML <- paste0("CCU_",CCUMode,suffix,".xml")
CCUBatch <- paste0('batch_CCU_',CCUMode,suffix,'.xml')
TechDict <- list(gas="gas (CC CCU)",biomass="biomass (IGCC CCU)",coal="coal (IGCC CCU")
CCSDict <- list(gas="gas (CC CCS)",biomass="biomass (IGCC CCS)",coal="coal (IGCC CCS)")
CCUshwt <- c(0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1) #Use 0 to shut off
InputDict <- list(gas="wholesale gas",biomass="regional biomass",coal="regional coal")

unlink(paste0("../xml/energy-xml/",CCUXML))
unlink(paste0("../energy-processing-code/xml-batch/",CCUBatch))

StartYear = 2015
secoutput.sector <- "electricity" #"elect_td_ind" "electricity"

#Equivalence Table
getInput('L223.EQUIV_TABLE') %>% 
write_mi_data("EQUIV_TABLE","ENERGY_LEVEL2_DATA","CCU.EQUIV_TABLE", "ENERGY_XML_BATCH", CCUBatch)


#RCL
regnames %>% 
  mutate(supplysector = "feedstock carbon") %>%
  mutate(logit.type = "relative-cost-logit") %>%
write_mi_data("Supplysector_relative-cost-logit","ENERGY_LEVEL2_DATA","CCU.Supplysector_relative-cost-logit", "ENERGY_XML_BATCH", CCUBatch)

#ACL
tibble(region=character(),supplysector=character(),logit.type=character()) %>%
  write_mi_data("Supplysector_absolute-cost-logit","ENERGY_LEVEL2_DATA","CCU.Supplysector_absolute-cost-logit", "ENERGY_XML_BATCH", CCUBatch)

#Supplysector
regnames %>% 
  mutate(supplysector = "feedstock carbon",
         output.unit="Mt", input.unit = "EJ or Mt", price.unit="1975$/kg",logit.year.fillout=1975,logit.exponent=-3) %>%
write_mi_data("Supplysector","ENERGY_LEVEL2_DATA","CCU.Supplysector", "ENERGY_XML_BATCH", CCUBatch)

#RCL Subsector
regnames %>% 
  mutate(supplysector = "feedstock carbon", subsector = CCUMode, logit.type = "relative-cost-logit") %>%
  write_mi_data("SubsectorLogit_relative-cost-logit","ENERGY_LEVEL2_DATA","CCU.SubsectorLogit_relative-cost-logit", "ENERGY_XML_BATCH", CCUBatch)

#ACL Subsector
tibble(region=character(),supplysector=character(),subsector=character(),logit.type=character()) %>%
  write_mi_data("SubsectorLogit_absolute-cost-logit","ENERGY_LEVEL2_DATA","CCU.SubsectorLogit_absolute-cost-logit", "ENERGY_XML_BATCH", CCUBatch)

#Interperolation
regnames %>% 
  mutate(supplysector = "feedstock carbon", subsector = CCUMode, apply.to="share-weight",
         from.year=2010, to.year=2300, to.value=1, interpolation.function="s-curve") %>%
  write_mi_data("SubsectorInterpTo","ENERGY_LEVEL2_DATA","CCU.SubsectorInterpTo", "ENERGY_XML_BATCH", CCUBatch)

#SubsectorShrwtFllt
regnames %>% 
  mutate(supplysector = "feedstock carbon", subsector = CCUMode,
         year.fillout=2100, share.weight=1) %>%
  write_mi_data("SubsectorShrwtFllt","ENERGY_LEVEL2_DATA","CCU.SubsectorShrwtFllt", "ENERGY_XML_BATCH", CCUBatch)

#SubsectorLogit
regnames %>% 
  mutate(supplysector = "feedstock carbon", subsector = CCUMode,
         logit.year.fillout=1975, logit.exponent=-6) %>%
  write_mi_data("SubsectorLogit","ENERGY_LEVEL2_DATA","CCU.SubsectorLogit", "ENERGY_XML_BATCH", CCUBatch)

#GlobalTechShrwt
years %>%
  mutate(sector.name = "feedstock carbon",subsector.name=CCUMode,
         technology=TechDict[[CCUMode]],share.weight=CCUshwt) %>%
  select(sector.name,subsector.name,technology,year,share.weight) %>%
  write_mi_data("GlobalTechShrwt","ENERGY_LEVEL2_DATA","CCU.GlobalTechShrwt", "ENERGY_XML_BATCH", CCUBatch)

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
  mutate(FCSecOut = PrimaryFuelCO2coef/efficiency * remove.fraction) %>% #CC/elecltricity
  mutate(lcoe = (OM.var/1000 + OM.fixed/(8766 * capacity.factor) + capital.overnight * fixed.charge.rate/(8766 * capacity.factor)) / 0.003600457) 
  
CCUtemplate <- years %>% filter(year >= StartYear) %>%
  mutate(sector.name = "feedstock carbon", subsector.name=CCUMode,technology=TechDict[[CCUMode]]) %>%
  select(sector.name,subsector.name,technology,year)

#OMVar
CCUtemplate %>%
  mutate(input.OM.var="OM-var") %>%
  bind_cols(transmute(Old,OM.var=OM.var/FCSecOut)) %>% 
  signif_df(4) %>%
  write_mi_data("GlobalTechOMvar","ENERGY_LEVEL2_DATA","CCU.GlobalTechOMvar", "ENERGY_XML_BATCH", CCUBatch)
#Is there a better way to do this than bind_cols 
  
#OMFixed
CCUtemplate %>%
  mutate(input.OM.fixed="OM-fixed") %>%
  bind_cols(transmute(Old,OM.fixed=OM.fixed/FCSecOut)) %>% 
  bind_cols(transmute(Old,capacity.factor=capacity.factor)) %>%
  signif_df(4) %>%
  write_mi_data("GlobalTechOMfixed","ENERGY_LEVEL2_DATA","CCU.GlobalTechOMfixed", "ENERGY_XML_BATCH", CCUBatch)


#coefficient
CCUtemplate %>% 
  mutate(minicam.energy.input = Old$minicam.energy.input) %>%
  bind_cols(transmute(Old,coefficient=1/(FCSecOut * efficiency))) %>%
  signif_df(4) %>%
  write_mi_data("GlobalTechCoef","ENERGY_LEVEL2_DATA","CCU.GlobalTechCoef", "ENERGY_XML_BATCH", CCUBatch)


#Capital
CCUtemplate %>%
  mutate(input.capital="capital") %>%
  bind_cols(transmute(Old,capital.overnight=capital.overnight/FCSecOut)) %>% 
  bind_cols(transmute(Old,fixed.charge.rate=fixed.charge.rate)) %>%
  bind_cols(transmute(Old,capacity.factor=capacity.factor)) %>%
  signif_df(4) %>%
  write_mi_data("GlobalTechCapital","ENERGY_LEVEL2_DATA","CCU.GlobalTechCapital", "ENERGY_XML_BATCH", CCUBatch)

#FossilEffKeyword
CCUtemplate %>% 
  mutate(average.fossil.efficiency=1) %>%
  write_mi_data("AvgFossilEffKeyword","ENERGY_LEVEL2_DATA","CCU.AvgFossilEffKeyword", "ENERGY_XML_BATCH", CCUBatch)

#StubTech
regnames %>%
  mutate(supplysector = "feedstock carbon",subsector=CCUMode,
         stub.technology=TechDict[[CCUMode]]) %>%
  select(region,supplysector,subsector,stub.technology) %>%
  write_mi_data("StubTech","ENERGY_LEVEL2_DATA","CCU.StubTech", "ENERGY_XML_BATCH", CCUBatch)

#Fractional Secondary Output
bind_rows(CCUtemplate %>% 
            mutate(technology=TechDict[[CCUMode]],fractional.secondary.output=secoutput.sector,
                   price=0,fraction.produced=0),
          CCUtemplate %>% 
            mutate(technology=TechDict[[CCUMode]],fractional.secondary.output=secoutput.sector,
                   price=0.1,fraction.produced=1)) %>%
  write_mi_data("GlobalTechFractProd","ENERGY_LEVEL2_DATA","CCU.GlobalTechFractProd", "ENERGY_XML_BATCH", CCUBatch)

CCUtemplate %>% 
  mutate(fractional.secondary.output=secoutput.sector) %>%
  bind_cols(transmute(Old,output.ratio=1/FCSecOut)) %>%
  signif_df(4) %>%
  write_mi_data("GlobalTechFractSecOut","ENERGY_LEVEL2_DATA","CCU.GlobalTechFractSecOut", "ENERGY_XML_BATCH", CCUBatch)

insert_file_into_batchxml( "ENERGY_XML_BATCH", CCUBatch, "ENERGY_XML_FINAL", CCUXML,"", xml_tag="outFile" )
system(paste0('java -Xmx2g -jar ../_common/ModelInterface/src/CSVToXML.jar ../energy-processing-code/xml-batch/',CCUBatch))

