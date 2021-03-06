
if( !exists( "ENERGYPROC_DIR" ) ){
    if( Sys.getenv( "ENERGYPROC" ) != "" ){
        ENERGYPROC_DIR <- Sys.getenv( "ENERGYPROC" )
    } else {
        stop("Could not determine location of energy data system. Please set the R var ENERGYPROC_DIR to the appropriate location")
    }
}

# Universal header file - provides logging, file support, etc.
source(paste(ENERGYPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
source(paste(ENERGYPROC_DIR,"/../_common/headers/ENERGY_header.R",sep=""))
logstart( "LA152.transportation.R" )
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/GCAM_header.R",sep=""))
adddep(paste(ENERGYPROC_DIR,"/../_common/headers/ENERGY_header.R",sep=""))
printlog( "Historical transportation sector energy consumption by region and fuel" )

# -----------------------------------------------------------------------------
# 1. Read files
sourcedata( "COMMON_ASSUMPTIONS", "A_common_data", extension = ".R" )
sourcedata( "ENERGY_ASSUMPTIONS", "A_energy_data", extension = ".R" )
enduse_fuel_aggregation <- readdata( "ENERGY_MAPPINGS", "enduse_fuel_aggregation" )
enduse_sector_aggregation <- readdata( "ENERGY_MAPPINGS", "enduse_sector_aggregation" )
L131.in_EJ_R_Senduse_F_Yh <- readdata( "ENERGY_LEVEL1_DATA", "L131.in_EJ_R_Senduse_F_Yh" )

# -----------------------------------------------------------------------------
# 2. Perform computations
#Calculation of transportation sector energy consumption
L152.in_EJ_R_trn_F_Yh <- subset( L131.in_EJ_R_Senduse_F_Yh, grepl( "trn", sector ) )
L152.in_EJ_R_trn_F_Yh$sector <- enduse_sector_aggregation$sector_agg[ match( L152.in_EJ_R_trn_F_Yh$sector, enduse_sector_aggregation$sector ) ]
L152.in_EJ_R_trn_F_Yh$fuel <- enduse_fuel_aggregation$trn[ match( L152.in_EJ_R_trn_F_Yh$fuel, enduse_fuel_aggregation$fuel ) ]
L152.in_EJ_R_trn_F_Yh <- aggregate( L152.in_EJ_R_trn_F_Yh[ X_historical_years ], by=as.list( L152.in_EJ_R_trn_F_Yh[ R_S_F ] ), sum )
L152.in_EJ_R_trn_F_Yh$sector <- sub( "in_", "", L152.in_EJ_R_trn_F_Yh$sector )

# -----------------------------------------------------------------------------
# 3. Output
#Add comments for each table
comments.L152.in_EJ_R_trn_F_Yh <- c( "Transportation energy consumption by GCAM region / mode / fuel / historical year","Unit = EJ" )

#write tables as CSV files
writedata( L152.in_EJ_R_trn_F_Yh, domain="ENERGY_LEVEL1_DATA", fn="L152.in_EJ_R_trn_F_Yh", comments=comments.L152.in_EJ_R_trn_F_Yh )

# Every script should finish with this line
logstop()
