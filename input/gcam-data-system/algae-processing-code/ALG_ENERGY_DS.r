#Algae Energy Data System
#This file defines GCAM Algae Energy Sector Technologies (HTL)
#Creates pass though sector algae for htl that applies carbon coef and 
# converts biomass to energy value 
#Uses 
#   Alg.P to describe passthrough sector
#   Alg.E to describe HTL/conversion sector
#TODO: Add Secondary Output For Nitrogen
source("../algae-processing-code/AlgaeHeaders.r")

enalgXML <- "alg_energy.xml"
enbatch <- paste0('batch_',enalgXML)
#clean old files 
unlink(paste0("../xml/energy-xml/",enalgXML))
unlink(paste0("../energy-processing-code/xml-batch/",enbatch))

HTL_Algae.factor <- as.numeric(names(read.xlsx(ProcessData,namedRegion = 'HTL_Algae')))
HTL_Gas.factor <- as.numeric(names(read.xlsx(ProcessData,namedRegion = 'HTL_wholesale_gas')))
HTL_Elec.factor <- as.numeric(names(read.xlsx(ProcessData,namedRegion = 'HTL_elect_td_ind')))
HTL_Cost.factor <- as.numeric(names(read.xlsx(ProcessData,namedRegion = 'HTL_input_cost')))
HTL_N_out.factor <- as.numeric(names(read.xlsx(ProcessData,namedRegion = 'HTL_SecOut_N_fertilizer')))

algaeforhtl.shwt <- c(0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1) #Probably doesn't need to be changed
HTL_Algae.shwt <- c(0,0,0,0,0,.2,.4,.6,.8,1,1,1,1,1,1,1,1,1,1,1,1,1) 



#Equivalence Table
tibble(group.name="LogitType",tag1="dummy-logit-tag",tag2="relative-cost-logit",tag3="absolute-cost-logit") %>%
  write_mi_data("EQUIV_TABLE","ENERGY_LEVEL2_DATA","AlgP.EQUIV_TABLE", "ENERGY_XML_BATCH",enbatch)

#RCL
algregnames %>% 
  mutate(supplysector = "algae for htl") %>%
  mutate(logit.type = "relative-cost-logit") %>%
  write_mi_data("Supplysector_relative-cost-logit","ENERGY_LEVEL2_DATA","AlgP.Supplysector_relative-cost-logit", "ENERGY_XML_BATCH", enbatch)

#ACL
tibble(region=character(),supplysector=character(),logit.type=character()) %>%
  write_mi_data("Supplysector_absolute-cost-logit","ENERGY_LEVEL2_DATA","AlgP.Supplysector_absolute-cost-logit", "ENERGY_XML_BATCH", enbatch)

#Supplysector
algregnames %>% 
  mutate(supplysector = "algae for htl",
         output.unit="EJ", input.unit = "EJ", price.unit="1975$/GJ",logit.year.fillout=1975,logit.exponent=-3) %>%
  write_mi_data("Supplysector","ENERGY_LEVEL2_DATA","AlgP.Supplysector", "ENERGY_XML_BATCH", enbatch)

#RCL Subsector
algregnames %>% 
  mutate(supplysector = "algae for htl", subsector = "algae for htl", logit.type = "relative-cost-logit") %>%
  write_mi_data("SubsectorLogit_relative-cost-logit","ENERGY_LEVEL2_DATA","AlgP.SubsectorLogit_relative-cost-logit", "ENERGY_XML_BATCH", enbatch)

#ACL Subsector
tibble(region=character(),supplysector=character(),subsector=character(),logit.type=character()) %>%
  write_mi_data("SubsectorLogit_absolute-cost-logit","ENERGY_LEVEL2_DATA","AlgP.SubsectorLogit_absolute-cost-logit", "ENERGY_XML_BATCH", enbatch)

#SubsectorLogit
algregnames %>% 
  mutate(supplysector = "algae for htl", subsector = "algae for htl",
         logit.year.fillout=1975, logit.exponent=-6) %>%
  write_mi_data("SubsectorLogit","ENERGY_LEVEL2_DATA","AlgP.SubsectorLogit", "ENERGY_XML_BATCH", enbatch)

#SubsectorShrwtFllt
algregnames %>% 
  mutate(supplysector = "algae for htl", subsector = "algae for htl",
         year.fillout=1975, share.weight=1) %>%
  write_mi_data("SubsectorShrwtFllt","ENERGY_LEVEL2_DATA","AlgP.ubsectorShrwtFllt", "ENERGY_XML_BATCH", enbatch)

#Interperolation
algregnames %>% 
  mutate(supplysector = "algae for htl", subsector = "algae for htl", apply.to="share-weight",
         from.year=2020, to.year=2100, interpolation.function="fixed") %>%
  write_mi_data("SubsectorInterp","ENERGY_LEVEL2_DATA","AlgP.SubsectorInterp", "ENERGY_XML_BATCH", enbatch)


#StubTech
algregnames %>%
  mutate(supplysector = "algae for htl",subsector="algae for htl",
         stub.technology="algae for htl") %>%
  write_mi_data("StubTech","ENERGY_LEVEL2_DATA","AlgP.StubTech", "ENERGY_XML_BATCH", enbatch)

#GlobalTechCost
years %>%
  mutate(sector.name = "algae for htl",subsector.name="algae for htl", technology="algae for htl",
         minicam.non.energy.input = "non-energy", input.cost = 0) %>%
  select(c(sector.name, subsector.name, technology, year, minicam.non.energy.input, input.cost)) %>%
  write_mi_data("GlobalTechCost","ENERGY_LEVEL2_DATA","AlgP.GlobalTechCost", "ENERGY_XML_BATCH", enbatch)

#GlobalTechCoef
years %>%
  mutate(sector.name = "algae for htl",subsector.name="algae for htl", technology="algae for htl",
         minicam.energy.input = "Algae", coefficient = HTL_Algae.factor) %>%
  select(c(sector.name, subsector.name, technology, year, minicam.energy.input, coefficient)) %>% 
  signif_df(4) %>%
  write_mi_data("GlobalTechCoef","ENERGY_LEVEL2_DATA","AlgP.GlobalTechCoef", "ENERGY_XML_BATCH", enbatch)


years %>%
  mutate(sector.name = "algae for htl",subsector.name="algae for htl", technology="algae for htl",
         share.weight = HTL_Algae.shwt) %>%
  select(c(sector.name, subsector.name, technology, year, share.weight)) %>%
  write_mi_data("GlobalTechShrwt","ENERGY_LEVEL2_DATA","AlgP.GlobalTechShrwt", "ENERGY_XML_BATCH", enbatch)

#StubTech
algregnames %>%
  mutate(supplysector = "refining",subsector="biomass liquids",
         stub.technology="algae htl biodiesel") %>%
  write_mi_data("StubTech","ENERGY_LEVEL2_DATA","AlgE.StubTech", "ENERGY_XML_BATCH", enbatch)

###### Energy
#GlobalPrefix Frame:
htl.PreGlobal <- years %>%
  mutate(sector.name = "refining",subsector.name="biomass liquids", technology="algae htl biodiesel") %>%
  select(c(sector.name, subsector.name, technology, year))

#GlobalTechCost
htl.PreGlobal %>%
  mutate(minicam.non.energy.input = "non-energy", input.cost = HTL_Cost.factor) %>%
  signif_df(4) %>%
  write_mi_data("GlobalTechCost","ENERGY_LEVEL2_DATA","AlgE.GlobalTechCost", "ENERGY_XML_BATCH", enbatch)


#GlobalTechCoef
bind_rows(htl.PreGlobal %>% 
            mutate(minicam.energy.input = "elect_td_ind", coefficient = HTL_Elec.factor),
          htl.PreGlobal %>% 
            mutate(minicam.energy.input = "wholesale gas", coefficient = HTL_Gas.factor),
          htl.PreGlobal %>% 
            mutate(minicam.energy.input = "algae for htl", coefficient = 1)) %>%
  signif_df(4) %>%
  write_mi_data("GlobalTechCoef","ENERGY_LEVEL2_DATA","AlgE.GlobalTechCoef", "ENERGY_XML_BATCH", enbatch)

#GlobalTechShwt
htl.PreGlobal %>%
  mutate(share.weight = HTL_Algae.shwt) %>%
  write_mi_data("GlobalTechShrwt","ENERGY_LEVEL2_DATA","AlgE.GlobalTechShrwt", "ENERGY_XML_BATCH", enbatch)

#GlobalTechInterp
tibble(sector.name="refining", subsector.name="biomass liquids", technology = "algae htl biodiesel", 
       apply.to = "share-weight", from.year=2020, to.year=2100, interpolation.function="linear") %>%
  write_mi_data("GlobalTechInterp","ENERGY_LEVEL2_DATA","AlgE.GlobalTechInterp", "ENERGY_XML_BATCH", enbatch)

#StubTechProd_Alg
algregyears %>% filter(year <= 2010) %>% 
  mutate(supplysector = "refining",subsector="biomass liquids", stub.technology="algae htl biodiesel",
                 calOutputValue = 0, year.share.weight=year, subs.share.weight = 0, share.weight=0) %>%
  left_join(getInput("L222.StubTechProd_refining") %>% 
              filter(stub.technology == "biodiesel" & subs.share.weight == 1) %>% 
              select(region,year,subs.share.weight),
            by=c("region","year")) %>% 
  rename(subs.share.weight =subs.share.weight.y) %>% select(-subs.share.weight.x) %>%
  replace_na(list(subs.share.weight=0)) %>% 
  select(region, supplysector, subsector, stub.technology, year, calOutputValue, year.share.weight, subs.share.weight, share.weight) %>%
  write_mi_data("StubTechProd","ENERGY_LEVEL2_DATA","AlgE.StubTechProd_Alg", "ENERGY_XML_BATCH", enbatch)


#Fractional Secondary Output for N Fertilizer
# bind_rows(htl.PreGlobal %>% 
#             mutate(fractional.secondary.output=secoutput.sector,
#                    price=0,fraction.produced=0),
#           htl.PreGlobal %>% 
#             mutate(fractional.secondary.output=secoutput.sector,
#                    price=0.01,fraction.produced=1)) %>%
#   write_mi_data("GlobalTechFractProd","ENERGY_LEVEL2_DATA","AlgE.GlobalTechFractProd", "ENERGY_XML_BATCH", enbatch)
# 
# htl.PreGlobal %>% 
#   mutate(fractional.secondary.output="N fertilizer", output.ratio=HTL_N_out.factor) %>%
#   signif_df(4) %>%
#   write_mi_data("GlobalTechFractSecOut","ENERGY_LEVEL2_DATA","AlgE.GlobalTechFractSecOut", "ENERGY_XML_BATCH", enbatch)


insert_file_into_batchxml( "ENERGY_XML_BATCH", enbatch, "ENERGY_XML_FINAL", enalgXML,"", xml_tag="outFile" )
system(paste0('java -Xmx2g -jar ../_common/ModelInterface/src/CSVToXML.jar ../energy-processing-code/xml-batch/',enbatch))

