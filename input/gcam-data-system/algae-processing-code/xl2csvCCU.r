#Script to Convert Premade GCAM excel datatables to individual CSV files with creation of common batch xml file
library(XML)
library(readxl)
setwd("/home/michael/GCAM/gcam-core/algae-processing")
unlink(file.path(getwd(),"CCU"),recursive=TRUE)

#Expiremental Data
#L221.AgYield_Alg -- Yield, set to 0 to shut off
#L222.GlobalTechShrwt_AltF -- altfood share weight, set to 0 to turn off food
#L222.GlobalTechShrwt_AltF -- coproduction share weight, set to 0 to turn off coproduction
#L222.GlobalTechShrwt_Feed -- altfood share weight, set to 0 to turn off food
#
# ag_Demand_Alt.xlsx
# L233.GlobalTechShrwt_demand -- set to 0 to turn off Human consumption of altFood

#helper:
signif_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame
  # digits: number of digits to round
  numeric_columns <- sapply(x, class) == 'numeric'
  x[numeric_columns] <-  signif(x[numeric_columns], digits)
  x
}


#Set Mandatory Inputs
ScenarioName = 'DEFAULT' #readline("NAME THIS SCENARIO: ")
Description = 'DEFAULT' #readline("WHAT ARE YOU TESTING? ")
#TAX = readline("WHAT ARE YOU TESTING? ")

#Genearal Parameters
ScenarioName = "TEST"
xmlDB = "Algae"
TaxType <- "UCT" # UCT, FFCIT or NONE
TaxFile <- "../input/policy/ctax_noAlgae.xml"
PolicyTarget = FALSE
StopPeriod = -1 #-1 default, 2050 = 11

Ccoefs = "../input/gcam-data-system/xml/energy-xml/Ccoef.xml"
PolicyTargetFile = 'forcing_target_2d.xml'
agdemand.Alt = "../input/gcam-data-system/xml/aglu-xml/demand_input_ALT.xml"



#runsummary = c(Name = ScenarioName, Description=Description, db=xmlDB, CCU=CCUfile, tax=TaxType, TaxValues = TaxFile, 
#               PolicyTarget=PolicyTarget, Description=Description, time = Sys.time)

xml.header = c(
  '<ModelInterfaceBatch>',
  '    <class name="ModelInterface.ModelGUI2.InputViewer">',
  '    <command name="CSV file">',
  '        <headerFile>../_common/headers//ModelInterface_headers.txt</headerFile>')

xml.tail = c(
  "    </command>",
  "    </class>",
  "</ModelInterfaceBatch>"
)

#Update from Dropbox 
# file.copy("../../../Dropbox (ScienceandIndustry)/AlgaeGCAM/Model/en_Alg.xlsx",getwd(),overwrite=TRUE)
# file.copy("../../../Dropbox (ScienceandIndustry)/AlgaeGCAM/Model/ag_Alg.xlsx",getwd(),overwrite=TRUE)
# file.copy("../../../Dropbox (ScienceandIndustry)/AlgaeGCAM/Model/demand_input_ALT.xlsx",getwd(),overwrite=TRUE)
# file.copy("../../../Dropbox (ScienceandIndustry)/AlgaeGCAM/Model/CCU",getwd(),recursive=TRUE,overwrite=TRUE)

file.copy("../../../Dropbox (ScienceandIndustry)/AlgaeGCAM/Model/Technology",getwd(),recursive=TRUE,overwrite=TRUE)
file.copy("../../../Dropbox (ScienceandIndustry)/AlgaeGCAM/Model/Geospatial",getwd(),recursive=TRUE,overwrite=TRUE)


AgAlgFile = "ag_Alg"
EnAlgFile = "en_Alg"
AgDemandFile = "demand_input_ALT"


files = c(EnAlgFile,AgAlgFile,AgDemandFile) #
dirs = c("energy","aglu","aglu")
CCUfile = "CCU_GAS" #"CCU_GAS", "CCU_BIO", "CCU_GAS_fso", "CCU_BIO_fso", "" if no CCU market

n = 1
for (f in files){
  filename = paste0("XLSXs/",f,".xlsx")
  f.sheets = excel_sheets(filename)
  xml = xml.header
  for (i in 1:length(f.sheets)){
    ws = read_excel(filename,i,col_names=FALSE)
    #break out input table to get datat to trim
    ws.header = head(ws,5)
    ws.data = read_excel(filename,i,col_names=FALSE,skip=5)
    ws.data = signif_df(ws.data,4)
    ws2 = rbind(ws.header,ws.data)
    #write out
    if (!(nchar(CCUfile)==0 && f.sheets[i] == "L221.AgCoef_FC_Alg")){
      write.table(ws2,paste("../input/gcam-data-system/",dirs[n],"-data/level2/",f.sheets[i],".csv",sep=""),row.names = FALSE,na="",col.names=FALSE,sep=',',quote=FALSE)
      xml=c(xml,paste("        <csvFile>../",dirs[n],"-data/level2//",f.sheets[i],".csv","</csvFile>",sep=""))
    }
  }

  #Add suffix to indicate that agAlg is using feedstock carbon or not
  if (nchar(CCUfile)>0 && f == AgAlgFile){CCUsuffix <- "_CCU"} 
    else if (nchar(CCUfile)==0 && f == AgAlgFile){CCUsuffix <- "_NOCCU"}
    else {CCUsuffix<- ""}
  
  #Add XML Footer
  xml=c(xml,paste0("        <outFile>../xml/",dirs[n],"-xml//",f,CCUsuffix,".xml</outFile>"))
  write(c(xml,xml.tail),paste0("../input/gcam-data-system/",dirs[n],"-processing-code/xml-batch/batch_",f,CCUsuffix,".xml"))
  print(c("Making ",f,CCUsuffix))
  n = n+1
}

#Create CCU Market if one is given
if (nchar(CCUfile)>0){
  print("Applying CCU Model, MAKE SURE TO ADD XML TAG")
  xml = xml.header
  f.sheets = excel_sheets(paste0("XLSXs/",CCUfile,".xlsx"))
  for (i in 1:length(f.sheets)){
    ws = read_excel(paste0("XLSXs/",CCUfile,".xlsx"),i,col_names=FALSE)
    ws.header = head(ws,5)
    ws.data = read_excel(paste0("XLSXs/",CCUfile,".xlsx"),i,col_names=FALSE,skip=5)
    ws.data = signif_df(ws.data,4)
    ws2 = rbind(ws.header,ws.data)
    write.table(ws2,paste0("../input/gcam-data-system/energy-data/level2/",f.sheets[i],".csv"),row.names = FALSE,na="",col.names=FALSE,sep=',',quote=FALSE)
    xml=c(xml,paste0("        <csvFile>../energy-data/level2//",f.sheets[i],".csv","</csvFile>"))
  }
  xml=c(xml,paste0("        <outFile>../xml/energy-xml//",CCUfile,".xml</outFile>"))
  write(c(xml,xml.tail),paste0("../input/gcam-data-system/energy-processing-code/xml-batch/batch_",CCUfile,".xml"))
}

setwd('../input/gcam-data-system/energy-data')
system('make XML_BATCH')
setwd('../aglu-data')
system('make XML_BATCH')
setwd('../../../algae-processing')

