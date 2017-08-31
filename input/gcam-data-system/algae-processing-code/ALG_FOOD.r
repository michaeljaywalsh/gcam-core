#Algae Data System


source("../algae-processing-code/AlgaeHeaders.r")



#TODOs 
#Add option for coprodcution
#COPRODUCTION = FALSE

#Global Tech Share Weights 1975-2100
AltAlgFood.shwt <-c(0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
AltCoP.shwt <- # c(0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1) #Use 0 to shut off
FeedAlt.shwt <- c(0,0,0,0,0,0.05,.1,.2,.3,.4,.5,.6,.7,.8,.9,1,1,1,1,1,1,1) #use 0 to shut off
DemandAlt.shwt <- c(0,0,0,0,0,0.05,.1,.2,.3,.4,.5,.6,.7,.8,.9,1,1,1,1,1,1,1) #use 0 to shutoff

FixedFoodDemand = TRUE

#Output settings:
foodXML <- "alg_food_DBF.xml"
demandXML <- "demand_input_ALT.xml"

foodbatch <- paste0("batch_",foodXML) 
demandbatch <- paste0("batch_",demandXML)
#clean old files 
unlink(paste0("../xml/aglu-xml/",foodXML))
unlink(paste0("../aglu-processing-code/xml-batch/",foodbatch))
unlink(paste0("../xml/aglu-xml/",demandXML))
unlink(paste0("../aglu-processing-code/xml-batch/",demandbatch))

## Get External Variables
AltFood.elec <- as.numeric(names(read.xlsx(ProcessData,namedRegion = 'AltFood_electricity')))
AltFood.gas <- as.numeric(names(read.xlsx(ProcessData,namedRegion = 'AltFood_wholesale_gas')))
AltFood.alg <- 1 
AltFood.cost <- as.numeric(names(read.xlsx(ProcessData,namedRegion = 'WE_input_cost')))
CoProd.elec <- as.numeric(names(read.xlsx(ProcessData,namedRegion = 'WE_electricity')))
CoProd.gas <- as.numeric(names(read.xlsx(ProcessData,namedRegion = 'WE_wholesale_gas')))
CoProd.alg <- as.numeric(names(read.xlsx(ProcessData,namedRegion = 'WE_Algae')))
CoProd.cost <- as.numeric(names(read.xlsx(ProcessData,namedRegion = 'AltFood_input_cost')))
CoProd.secout <- as.numeric(names(read.xlsx(ProcessData,namedRegion = 'WE_SecOut_BiomassOil'))) 
Calorie.alg <- 4.5

###AltFood #Does not include co-production!!!! 
#Equivalence Table
AlgF.EQUIV_TABLE <- tibble(group.name="LogitType",tag1="dummy-logit-tag",tag2="relative-cost-logit",tag3="absolute-cost-logit")
write_mi_data(AlgF.EQUIV_TABLE,"EQUIV_TABLE","AGLU_LEVEL2_DATA","AlgF.EQUIV_TABLE", "AGLU_XML_BATCH", foodbatch)

AltFoodYears<- years %>% mutate(sector.name='AltFood',subsector.name='Algae Food',technology='Algae Food')

AltF.GlobalTechCoef <- bind_rows(AltFoodYears %>% mutate(minicam.energy.input='elect_td_ind',coefficient=AltFood.elec),
                                 AltFoodYears %>% mutate(minicam.energy.input='wholesale gas',coefficient=AltFood.gas),
                                 AltFoodYears %>% mutate(minicam.energy.input='Algae',coefficient=AltFood.alg)) %>% 
  select(sector.name,subsector.name,technology,year,minicam.energy.input,coefficient) %>% 
  signif_df(4)
write_mi_data(AltF.GlobalTechCoef,"GlobalTechCoef","AGLU_LEVEL2_DATA","AltF.GlobalTechCoef", "AGLU_XML_BATCH", foodbatch)

AltF.GlobalTechCost <- AltFoodYears %>% mutate(minicam.non.energy.input="non-energy",input.cost=AltFood.cost) %>%
  select(sector.name,subsector.name,technology,year,minicam.non.energy.input,input.cost) %>% 
  signif_df(4)
write_mi_data(AltF.GlobalTechCost,"GlobalTechCost","AGLU_LEVEL2_DATA","AltF.GlobalTechCost", "AGLU_XML_BATCH", foodbatch)
 
AltF.Supplysector_RCL <- algregnames %>% 
  mutate(supplysector = "AltFood", logit.type="relative-cost-logit")
write_mi_data(AltF.Supplysector_RCL,"Supplysector_relative-cost-logit","AGLU_LEVEL2_DATA","AltF.Supplysector_relative-cost-logit", "AGLU_XML_BATCH", foodbatch)

AltF.Supplysector_ACL <- tibble(region=character(),supplysector=character(),logit.type=character())
write_mi_data(AltF.Supplysector_ACL,"Supplysector_absolute-cost-logit","AGLU_LEVEL2_DATA","AltF.Supplysector_absolute-cost-logit", "AGLU_XML_BATCH", foodbatch)

AltF.Supplysector <- algregnames %>% 
  select(region) %>% 
  mutate(supplysector="AltFood",output.unit="Mt",input.unit="Mt",
         price.unit="1975$/kg",logit.year.fillout=1975,logit.exponent=-1.5)
write_mi_data(AltF.Supplysector,"Supplysector","AGLU_LEVEL2_DATA","AltF.Supplysector", "AGLU_XML_BATCH", foodbatch)

AltF.Subsector_RCL <- algregnames %>% 
  mutate(supplysector = "AltFood", subsector = "Algae Food", logit.type="relative-cost-logit")
write_mi_data(AltF.Subsector_RCL,"SubsectorLogit_relative-cost-logit","AGLU_LEVEL2_DATA","AltF.SubsectorLogit_relative-cost-logit", "AGLU_XML_BATCH", foodbatch)

AltF.Subsector_ACL <- tibble(region=character(),supplysector=character(),subsector=character(),logit.type=character())
write_mi_data(AltF.Subsector_ACL,"SubsectorLogit_absolute-cost-logit","AGLU_LEVEL2_DATA","AltF.SubsectorLogit_absolute-cost-logit", "AGLU_XML_BATCH", foodbatch)

AltF.SubsectorAll <- algregnames %>% 
  mutate(supplysector = "AltFood", subsector = "Algae Food", logit.year.fillout=1975,
         logit.exponent=-6,year.fillout=2020,share.weight=1, apply.to ="share-weight", 
         from.year=2020, to.year=2100, interpolation.function="fixed")
write_mi_data(AltF.SubsectorAll,"SubsectorAll","AGLU_LEVEL2_DATA","AltF.SubsectorAll", "AGLU_XML_BATCH", foodbatch)

AltF.StubTechProd <- algregnames %>% 
  mutate(supplysector = "AltFood", subsector = "Algae Food", stub.technology = "Algae Food") %>% 
  merge(baseyears) %>%
  mutate(calOutputValue=0,share.weight.year=1975,subs.share.weight=0,tech.share.weight=0)
write_mi_data(AltF.StubTechProd,"StubTechProd","AGLU_LEVEL2_DATA","AltF.StubTechProd", "AGLU_XML_BATCH", foodbatch)

AltF.StubTech <- algregnames %>% 
  mutate(supplysector = "AltFood", subsector="Algae Food", stub.technology="Algae Food")
write_mi_data(AltF.StubTech,"StubTech","AGLU_LEVEL2_DATA","AltF.StubTech", "AGLU_XML_BATCH", foodbatch)

AltF.GlobalTechShrwt <- AltFoodYears %>% 
  mutate(share.weight = AltAlgFood.shwt) %>%
  select(sector.name,subsector.name,technology,year,share.weight)
write_mi_data(AltF.GlobalTechShrwt,"GlobalTechShrwt","AGLU_LEVEL2_DATA","AltF.GlobalTechShrwt", "AGLU_XML_BATCH", foodbatch)

#Coproduction
tibble(group.name="LogitType",tag1="dummy-logit-tag",tag2="relative-cost-logit",tag3="absolute-cost-logit") %>%
  write_mi_data("EQUIV_TABLE","AGLU_LEVEL2_DATA","CoP.EQUIV_TABLE", "AGLU_XML_BATCH", foodbatch)

CoPYears<- years %>% mutate(sector.name='AltFood',subsector.name='Algae Coproduction',technology='Algae Coproduction')

bind_rows(CoPYears %>% mutate(minicam.energy.input='elect_td_ind',coefficient=CoProd.elec),
          CoPYears %>% mutate(minicam.energy.input='wholesale gas',coefficient=CoProd.gas),
          CoPYears %>% mutate(minicam.energy.input='Algae',coefficient=CoProd.alg)) %>% 
  select(sector.name,subsector.name,technology,year,minicam.energy.input,coefficient) %>% 
  signif_df(4) %>%
  write_mi_data("GlobalTechCoef","AGLU_LEVEL2_DATA","CoP.GlobalTechCoef", "AGLU_XML_BATCH", foodbatch)

CoPYears %>% mutate(minicam.non.energy.input="non-energy",input.cost=CoProd.cost) %>%
  select(sector.name,subsector.name,technology,year,minicam.non.energy.input,input.cost) %>% 
  signif_df(4) %>%
  write_mi_data("GlobalTechCost","AGLU_LEVEL2_DATA","CoP.GlobalTechCost", "AGLU_XML_BATCH", foodbatch)

algregnames %>% 
  mutate(supplysector = "AltFood", logit.type="relative-cost-logit") %>% 
  write_mi_data("Supplysector_relative-cost-logit","AGLU_LEVEL2_DATA","CoP.Supplysector_relative-cost-logit", "AGLU_XML_BATCH", foodbatch)

tibble(region=character(),supplysector=character(),logit.type=character()) %>%
  write_mi_data("Supplysector_absolute-cost-logit","AGLU_LEVEL2_DATA","CoP.Supplysector_absolute-cost-logit", "AGLU_XML_BATCH", foodbatch)

algregnames %>% 
  select(region) %>% 
  mutate(supplysector="AltFood",output.unit="Mt",input.unit="Mt",
         price.unit="1975$/kg",logit.year.fillout=1975,logit.exponent=-1.5) %>%
  write_mi_data("Supplysector","AGLU_LEVEL2_DATA","CoP.Supplysector", "AGLU_XML_BATCH", foodbatch)

algregnames %>% 
  mutate(supplysector = "AltFood", subsector = "Algae Coproduction", logit.type="relative-cost-logit") %>%
  write_mi_data("SubsectorLogit_relative-cost-logit","AGLU_LEVEL2_DATA","CoP.SubsectorLogit_relative-cost-logit", "AGLU_XML_BATCH", foodbatch)

tibble(region=character(),supplysector=character(),subsector=character(),logit.type=character()) %>%
  write_mi_data("SubsectorLogit_absolute-cost-logit","AGLU_LEVEL2_DATA","CoP.SubsectorLogit_absolute-cost-logit", "AGLU_XML_BATCH", foodbatch)

algregnames %>% 
  mutate(supplysector = "AltFood", subsector = "Algae Coproduction", logit.year.fillout=1975,
         logit.exponent=-6,year.fillout=2020,share.weight=1, apply.to ="share-weight", 
         from.year=2020, to.year=2100, interpolation.function="fixed") %>%
  write_mi_data("SubsectorAll","AGLU_LEVEL2_DATA","CoP.SubsectorAll", "AGLU_XML_BATCH", foodbatch)

algregnames %>% 
  mutate(supplysector = "AltFood", subsector = "Algae Coproduction", stub.technology = "Algae Coproduction") %>% 
  merge(baseyears) %>%
  mutate(calOutputValue=0,share.weight.year=1975,subs.share.weight=0,tech.share.weight=0) %>%
  write_mi_data("StubTechProd","AGLU_LEVEL2_DATA","CoP.StubTechProd", "AGLU_XML_BATCH", foodbatch)

algregnames %>% 
  mutate(supplysector = "AltFood", subsector="Algae Coproduction", stub.technology="Algae Coproduction") %>%
  write_mi_data("StubTech","AGLU_LEVEL2_DATA","CoP.StubTech", "AGLU_XML_BATCH", foodbatch)

CoPYears %>% 
  mutate(share.weight = AltCoP.shwt) %>%
  select(sector.name,subsector.name,technology,year,share.weight) %>%
  write_mi_data("GlobalTechShrwt","AGLU_LEVEL2_DATA","CoP.GlobalTechShrwt", "AGLU_XML_BATCH", foodbatch)

#Fractional Secondary outputs
# bind_rows(CoPYears %>% 
#             mutate(fractional.secondary.output='refining',
#                    price=0,fraction.produced=0),
#           CoPYears %>% 
#             mutate(fractional.secondary.output='refining',
#                    price=0.1,fraction.produced=1)) %>%
#   select(sector.name,subsector.name,technology,year,fractional.secondary.output,price,fraction.produced) %>%
#   write_mi_data("GlobalTechFractProd","AGLU_LEVEL2_DATA","CoP.GlobalTechFractProd", "AGLU_XML_BATCH", foodbatch)
# 
# CoPYears %>% 
#   mutate(fractional.secondary.output='refining',output.ratio=CoProd.secout) %>%
#   signif_df(4) %>%
#   select(sector.name,subsector.name,technology,year,fractional.secondary.output,output.ratio) %>%
#   write_mi_data("GlobalTechFractSecOut","AGLU_LEVEL2_DATA","CoP.GlobalTechFractSecOut", "AGLU_XML_BATCH", foodbatch)

### Secondary Ouput 
CoPYears %>% 
  mutate(secondary.output='refining',output.ratio=CoProd.secout) %>%
  signif_df(4) %>%
  select(sector.name,subsector.name,technology,year,secondary.output,output.ratio) %>%
  write_mi_data("GlobalTechSecOut","AGLU_LEVEL2_DATA","CoP.GlobalTechSecOut", "AGLU_XML_BATCH", foodbatch)


#Feed
FeedYears<- years %>% mutate(sector.name='FeedCrops',subsector.name='AltFood',technology='AltFood')

Feed.Supplysector_RCL <- algregnames %>% 
  mutate(supplysector = "FeedCrops", logit.type="relative-cost-logit") 
write_mi_data(Feed.Supplysector_RCL,"Supplysector_relative-cost-logit","AGLU_LEVEL2_DATA","Feed.Supplysector_relative-cost-logit", "AGLU_XML_BATCH", foodbatch)

Feed.Supplysector_ACL <- tibble(region=character(),supplysector=character(),logit.type=character())
write_mi_data(Feed.Supplysector_ACL,"Supplysector_absolute-cost-logit","AGLU_LEVEL2_DATA","Feed.Supplysector_absolute-cost-logit", "AGLU_XML_BATCH", foodbatch)

Feed.Subsector_RCL <- algregnames %>% 
  mutate(supplysector = "FeedCrops", subsector = "AltFood", logit.type="relative-cost-logit")
write_mi_data(Feed.Subsector_RCL,"SubsectorLogit_relative-cost-logit","AGLU_LEVEL2_DATA","Feed.SubsectorLogit__relative-cost-logit", "AGLU_XML_BATCH", foodbatch)

Feed.Subsector_ACL <- tibble(region=character(),supplysector=character(),subsector=character(),logit.type=character())
write_mi_data(Feed.Subsector_ACL,"SubsectorLogit_absolute-cost-logit","AGLU_LEVEL2_DATA","Feed.SubsectorLogit_absolute-cost-logit", "AGLU_XML_BATCH", foodbatch)

Feed.StubTechProd <- algregnames %>% 
  mutate(supplysector = "FeedCrops", subsector = "AltFood", stub.technology = "AltFood") %>% 
  merge(tibble(year=c(1975,1990,2005,2010))) %>%
  mutate(calOutputValue=0,share.weight.year=year,subs.share.weight=0,tech.share.weight=0)
write_mi_data(Feed.StubTechProd,"StubTechProd","AGLU_LEVEL2_DATA","Feed.StubTechProd", "AGLU_XML_BATCH", foodbatch)

Feed.GlobalTechShrwt <- FeedYears %>% 
  mutate(share.weight = FeedAlt.shwt) %>%
  select(sector.name,subsector.name,technology,year,share.weight) 
write_mi_data(Feed.GlobalTechShrwt,"GlobalTechShrwt","AGLU_LEVEL2_DATA","Feed.GlobalTechShrwt", "AGLU_XML_BATCH", foodbatch)

Feed.StubTech <- algregnames %>% 
  mutate(supplysector = "FeedCrops", subsector="AltFood", stub.technology="AltFood")
write_mi_data(Feed.StubTech,"StubTech","AGLU_LEVEL2_DATA","Feed.StubTech", "AGLU_XML_BATCH", foodbatch)

Feed.GlobalTechCoef <- FeedYears %>% 
  mutate(sector.name="FeedCrops",minicam.energy.input='AltFood',coefficient=1) %>% 
  select(sector.name,subsector.name,technology,year,minicam.energy.input,coefficient) 
write_mi_data(Feed.GlobalTechCoef,"GlobalTechCoef","AGLU_LEVEL2_DATA","Feed.GlobalTechCoef", "AGLU_XML_BATCH", foodbatch)

Feed.SubsectorAll <- algregnames %>% 
  mutate(supplysector = "FeedCrops", subsector = "AltFood", logit.year.fillout=1975,
         logit.exponent=-6,year.fillout=2020,share.weight=1, apply.to ="share-weight", 
         from.year=2020, to.year=2100, interpolation.function="fixed")
write_mi_data(Feed.SubsectorAll,"SubsectorAll","AGLU_LEVEL2_DATA","Feed.SubsectorAll", "AGLU_XML_BATCH", foodbatch)


###Demand Recycling Demand

#EQUIV_TABLE - Copy
getInput("203.EQUIV_TABLE") %>%
  write_mi_data("EQUIV_TABLE","AGLU_LEVEL2_DATA","DAlt.EQUIV_TABLE", "AGLU_XML_BATCH", demandbatch)


getInput("203.Supplysector_relative-cost-logit",dir="../aglu-data/") %>%
  bind_rows(regnames %>%
              mutate(supplysector='FoodDemand_Crops_Flex',logit.type='relative-cost-logit')) %>%
  write_mi_data("Supplysector_relative-cost-logit","AGLU_LEVEL2_DATA","DAlt.Supplysector_relative-cost-logit", "AGLU_XML_BATCH", demandbatch)
      
getInput("203.Supplysector_absolute-cost-logit",dir="../aglu-data/") %>%
  write_mi_data("Supplysector_absolute-cost-logit","AGLU_LEVEL2_DATA","DAlt.Supplysector_absolute-cost-logit", "AGLU_XML_BATCH", demandbatch)

getInput("203.Supplysector_demand",dir="../aglu-data/") %>%
  bind_rows(regnames %>% 
              mutate(supplysector='FoodDemand_Crops_Flex',output.unit='Pcal',
                     input.unit='Mt',price.unit='1975$/Mcal',logit.year.fillout=1975,logit.exponent=-6)) %>%
  write_mi_data("Supplysector","AGLU_LEVEL2_DATA","DAlt.Supplysector_demand", "AGLU_XML_BATCH", demandbatch)            

getInput("203.SubsectorLogit_relative-cost-logit",dir="../aglu-data/") %>%
  mutate_cond((subsector=="Corn" & supplysector=="FoodDemand_Crops") | (subsector=="OilCrop" & supplysector=="FoodDemand_Crops"),
              supplysector=c("FoodDemand_Crops_Flex")) %>%
  bind_rows(algregnames %>% 
              mutate(supplysector='FoodDemand_Crops_Flex',subsector="AltFood",
                     logit.type='relative-cost-logit')) %>%
  write_mi_data("SubsectorLogit_relative-cost-logit","AGLU_LEVEL2_DATA","DAlt.SubsectorLogit_relative-cost-logit", "AGLU_XML_BATCH", demandbatch)


getInput("203.SubsectorLogit_absolute-cost-logit",dir="../aglu-data/") %>%
  write_mi_data("SubsectorLogit_absolute-cost-logit","AGLU_LEVEL2_DATA","DAlt.SubsectorLogit_absolute-cost-logit", "AGLU_XML_BATCH", demandbatch)

getInput("203.SubsectorAll_demand",dir="../aglu-data/") %>%
  mutate_cond((subsector=="Corn" & supplysector=="FoodDemand_Crops") | (subsector=="OilCrop" & supplysector=="FoodDemand_Crops"),
              supplysector="FoodDemand_Crops_Flex") %>%
  bind_rows(algregnames %>% 
              mutate(supplysector='FoodDemand_Crops_Flex',subsector="AltFood",
                     logit.year.fillout = 1975, logit.exponent = -6, 
                     year.fillout = 1975, share.weight=1, apply.to='share-weight',
                     from.year=2020, to.year=2100, interpolation.function="fixed")) %>%
  write_mi_data("SubsectorAll","AGLU_LEVEL2_DATA","DAlt.SubsectorAll_demand", "AGLU_XML_BATCH", demandbatch)

getInput("203.GlobalTechCoef_demand",dir="../aglu-data/") %>%
  mutate_cond((subsector.name=="Corn" & sector.name=="FoodDemand_Crops") | (subsector.name=="OilCrop" & sector.name=="FoodDemand_Crops"),
              sector.name="FoodDemand_Crops_Flex") %>%
  bind_rows(years %>% 
              mutate(sector.name='FoodDemand_Crops_Flex', subsector.name="AltFood",
                     technology='AltFood',minicam.energy.input="AltFood",coefficient=1)) %>%
  write_mi_data("GlobalTechCoef","AGLU_LEVEL2_DATA","DAlt.GlobalTechCoef_demand", "AGLU_XML_BATCH", demandbatch)


getInput("203.GlobalTechShrwt_demand",dir="../aglu-data/") %>%
  mutate_cond((subsector.name=="Corn" & sector.name=="FoodDemand_Crops") | (subsector.name=="OilCrop" & sector.name=="FoodDemand_Crops"),
              sector.name="FoodDemand_Crops_Flex") %>%
  bind_rows(years %>%
              mutate(sector.name='FoodDemand_Crops_Flex', subsector.name="AltFood",
                     technology='AltFood',share.weight=DemandAlt.shwt)) %>%
  write_mi_data("GlobalTechShrwt","AGLU_LEVEL2_DATA","DAlt.GlobalTechShrwt_demand", "AGLU_XML_BATCH", demandbatch)


getInput("203.StubTech_demand",dir="../aglu-data/") %>% 
  mutate_cond((subsector=="Corn" & supplysector=="FoodDemand_Crops") | (subsector=="OilCrop" & supplysector=="FoodDemand_Crops"),
              supplysector="FoodDemand_Crops_Flex") %>%
  bind_rows(algregnames %>% 
              mutate(supplysector='FoodDemand_Crops_Flex',subsector="AltFood",
                     stub.technology="AltFood")) %>%
  write_mi_data("StubTech","AGLU_LEVEL2_DATA","DAlt.StubTech_demand", "AGLU_XML_BATCH", demandbatch)

getInput("203.StubTechProd_food_crop",dir="../aglu-data/") %>% 
  mutate_cond((subsector=="Corn" & supplysector=="FoodDemand_Crops") | (subsector=="OilCrop" & supplysector=="FoodDemand_Crops"),
              supplysector="FoodDemand_Crops_Flex") %>%
              signif_df(4)%>%
  write_mi_data("StubTechProd","AGLU_LEVEL2_DATA","DAlt.StubTechProd_food_crop", "AGLU_XML_BATCH", demandbatch)

getInput("203.StubTechProd_food_meat",dir="../aglu-data/") %>%
  signif_df(4) %>%
  write_mi_data("StubTechProd","AGLU_LEVEL2_DATA","DAlt.StubTechProd_food_meat", "AGLU_XML_BATCH", demandbatch)

#For some reason oilcrops and corn are being shifted around in the XML output
getInput("203.StubTechProd_nonfood_crop",dir="../aglu-data/") %>%
  signif_df(4) %>%
  write_mi_data("StubTechProd","AGLU_LEVEL2_DATA","DAlt.StubTechProd_nonfood_crop", "AGLU_XML_BATCH", demandbatch)

getInput("203.StubTechProd_nonfood_meat",dir="../aglu-data/") %>%
  signif_df(4) %>%
  write_mi_data("StubTechProd","AGLU_LEVEL2_DATA","DAlt.StubTechProd_nonfood_meat", "AGLU_XML_BATCH", demandbatch)

getInput("203.StubTechProd_For",dir="../aglu-data/") %>%
  signif_df(4) %>%
  write_mi_data("StubTechProd","AGLU_LEVEL2_DATA","DAlt.StubTechProd_For", "AGLU_XML_BATCH", demandbatch)

getInput("203.StubTechFixOut_exp",dir="../aglu-data/") %>%
  signif_df(4) %>%
  write_mi_data("StubTechFixOut","AGLU_LEVEL2_DATA","DAlt.StubTechFixOut_exp", "AGLU_XML_BATCH", demandbatch)

getInput("203.StubCalorieContent_crop",dir="../aglu-data/") %>% 
  mutate_cond((subsector=="Corn" & supplysector=="FoodDemand_Crops") | (subsector=="OilCrop" & supplysector=="FoodDemand_Crops"),
              supplysector="FoodDemand_Crops_Flex") %>%
  bind_rows(algregyears %>% 
              mutate(supplysector = "FoodDemand_Crops_Flex", subsector="AltFood",stub.technology="AltFood",
                minicam.energy.input = "AltFood", efficiency=Calorie.alg,market.name=region)) %>% 
  signif_df(4) %>% 
  write_mi_data("StubCalorieContent","AGLU_LEVEL2_DATA","DAlt.StubCalorieContent_crop", "AGLU_XML_BATCH", demandbatch)

getInput("203.StubCalorieContent_meat",dir="../aglu-data/") %>% 
  signif_df(4) %>% 
  write_mi_data("StubCalorieContent","AGLU_LEVEL2_DATA","DAlt.StubCalorieContent_meat", "AGLU_XML_BATCH", demandbatch)

getInput("203.PerCapitaBased",dir="../aglu-data/") %>% 
  bind_rows(regnames %>% 
              mutate(energy.final.demand='FoodDemand_Crops_Flex',perCapitaBased=1)) %>% 
  write_mi_data("PerCapitaBased","AGLU_LEVEL2_DATA","DAlt.PerCapitaBased", "AGLU_XML_BATCH", demandbatch)

#The recalibrate base year shares for food markets into flex and non-flex
getInput("203.BaseService",dir="../aglu-data/") %>% 
  filter(energy.final.demand!='FoodDemand_Crops') %>%
  bind_rows(getInput("203.StubTechProd_food_crop",dir="../aglu-data/") %>% 
              mutate_cond((subsector=="Corn" & supplysector=="FoodDemand_Crops") | (subsector=="OilCrop" & supplysector=="FoodDemand_Crops"),
                          supplysector="FoodDemand_Crops_Flex") %>% 
              select(-c(stub.technology,share.weight.year,subs.share.weight,tech.share.weight)) %>%
              group_by(supplysector,region,year) %>%
              summarise(base.service=sum(calOutputValue)) %>%
              rename(energy.final.demand=supplysector)) %>% 
  signif_df(4) %>% 
  write_mi_data("BaseService","AGLU_LEVEL2_DATA","DAlt.BaseService", "AGLU_XML_BATCH", demandbatch)


getInput("203.IncomeElasticity",dir="../aglu-data/") %>%
  bind_rows(getInput("203.IncomeElasticity",dir="../aglu-data/") %>% 
              filter(energy.final.demand=='FoodDemand_Crops') %>% 
              mutate(energy.final.demand='FoodDemand_Crops_Flex')) %>%
  write_mi_data("IncomeElasticity","AGLU_LEVEL2_DATA","DAlt.IncomeElasticity", "AGLU_XML_BATCH", demandbatch)

#Price elasticity for food markets. Setting variable FixedFoodDemand<-TRUE will set elasticities of meat markets to 0
getInput("203.PriceElasticity",dir="../aglu-data/") %>% 
  {if(FixedFoodDemand) mutate_cond(.,energy.final.demand=="FoodDemand_Meat",price.elasticity=0) else . } %>%
  bind_rows(regyears %>% 
              mutate(energy.final.demand="FoodDemand_Crops_Flex",price.elasticity=0)) %>%
  filter(year >= 2015) %>% 
  write_mi_data("PriceElasticity","AGLU_LEVEL2_DATA","DAlt.PriceElasticity", "AGLU_XML_BATCH", demandbatch)

insert_file_into_batchxml( "AGLU_XML_BATCH", foodbatch, "AGLU_XML_FINAL", foodXML, "", xml_tag="outFile" )
insert_file_into_batchxml( "AGLU_XML_BATCH", demandbatch, "AGLU_XML_FINAL", demandXML, "", xml_tag="outFile" )

#Example
system(paste0('java -Xmx2g -jar ../_common/ModelInterface/src/CSVToXML.jar ../aglu-processing-code/xml-batch/',foodbatch))
system(paste0('java -Xmx2g -jar ../_common/ModelInterface/src/CSVToXML.jar ../aglu-processing-code/xml-batch/',demandbatch))
  