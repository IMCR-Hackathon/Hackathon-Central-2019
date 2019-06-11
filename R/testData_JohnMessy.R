testData_JohnMessy <- function(localFile){
# Package ID: knb-lter-vcr.247.10 Cataloging System:https://pasta.edirepository.org.
# Data set title: Water Quality Sampling - integrated measurements for the Virginia Coast, 1992-2018.
# Data set creator: Dr. Karen McGlathery -  
# Data set creator: Dr. Robert Christian -  
# Metadata Provider:    - Virginia Coast Reserve Long-Term Ecological Research Project 
# Contact: Dr. Karen McGlathery -    - kjm4k@virginia.edu
# Contact:    - Information manager - Virginia Coast Reserve Long-Term Ecological Research Project   - jporter@lternet.edu
# Stylesheet v2.7 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

if(file.exists(localFile)){
  return(read.csv(localFile))
}
  
infile1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-vcr/247/10/ce44ee0ccaccf489f3ccb04d3cf85e39" 
infile1 <- sub("^https","http",infile1) 
dt1 <-read.csv(infile1,header=F 
          ,skip=22
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "STATION",     
                    "MEASUREDATE",     
                    "STAGRPCODE",     
                    "STATYPE",     
                    "TRANSECT",     
                    "LATITUDE",     
                    "LONGITUDE",     
                    "TIME",     
                    "ANALYST",     
                    "TIDESTAGE",     
                    "SCTTEMP",     
                    "SCTSAL",     
                    "SCTCOND",     
                    "AIRTEMP",     
                    "REFRSAL",     
                    "DOTEMP",     
                    "DO",     
                    "SECCHI",     
                    "DEPTH",     
                    "WINDDIR",     
                    "WINDSP",     
                    "COMMENTOBS",     
                    "CHLA",     
                    "CHLA_SD",     
                    "CHLA_N",     
                    "CHLA_QUAL",     
                    "PHAEOPIGMENTS",     
                    "PHAEOPIGMENTS_SD",     
                    "PHAEOPIGMENTS_N",     
                    "PHAEOPIGMENTS_QUAL",     
                    "NH3",     
                    "NH3_SD",     
                    "NH3_N",     
                    "NH3QUAL",     
                    "PO4",     
                    "PO4_SD",     
                    "PO4_N",     
                    "PO4QUAL",     
                    "NOX",     
                    "NOX_SD",     
                    "NOX_N",     
                    "NOXQUAL",     
                    "NO2",     
                    "NO2_SD",     
                    "NO2_N",     
                    "NO2QUAL",     
                    "DIN",     
                    "COMMENTNUTR",     
                    "NOXTDN",     
                    "NOXTDN_SD",     
                    "NOXTDN_N",     
                    "NOXTDNQUAL",     
                    "NO2TDN",     
                    "NO2TDN_SD",     
                    "NO2TDN_N",     
                    "NO2TDNQUAL",     
                    "COMMENTTDN",     
                    "PROCESSORTDN",     
                    "ORGANIC_CONTENT",     
                    "ORGANIC_CONTENT_SD",     
                    "ORGANIC_CONTENT_N",     
                    "OC_QUAL",     
                    "POROSITY",     
                    "POROSITY_SD",     
                    "POROSITY_N",     
                    "POROSITY_QUAL",     
                    "COMMENTS_ORGANIC_CONTENT",     
                    "CHLOR_A",     
                    "CHLOR_A_SD",     
                    "CHLOR_A_N",     
                    "CHLOR_AQUAL",     
                    "PHAEOPIGMENTS_A",     
                    "PHAEOPIGMENTS_A_SD",     
                    "PHAEOPIGMENTS_A_N",     
                    "PHAEOPIGMENTS_AQUAL",     
                    "COMMENTWATERCHL",     
                    "GRACILARIA_MASS",     
                    "GRACILARIA_SD",     
                    "GRACILARIA_N",     
                    "GRACILARIA_QUAL",     
                    "ULVA_MASS",     
                    "ULVA_SD",     
                    "ULVA_N",     
                    "ULVA_QUAL",     
                    "COMMENTMACROALGAE",     
                    "TSS",     
                    "TSS_SD",     
                    "TSS_N",     
                    "TSSQUAL",     
                    "POM",     
                    "POM_SD",     
                    "POM_N",     
                    "POMQUAL",     
                    "PIM",     
                    "PIM_SD",     
                    "PIM_N",     
                    "PIMQUAL",     
                    "COMMENTTSS",     
                    "SEDMASS",     
                    "SEDMASS_SD",     
                    "SEDMASS_N",     
                    "SEDMASSQUAL",     
                    "SEDPERC_N",     
                    "SEDPERC_N_SD",     
                    "SEDPERC_N_N",     
                    "SEDPERC_NQUAL",     
                    "SEDPERC_C",     
                    "SEDPERC_C_SD",     
                    "SEDPERC_C_N",     
                    "SEDPERC_CQUAL",     
                    "COMMENTSEDCN"    ), check.names=TRUE)
               
  
# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
                
if (class(dt1$STATION)!="factor") dt1$STATION<- as.factor(dt1$STATION)                                   
# attempting to convert dt1$MEASUREDATE dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1MEASUREDATE<-as.Date(dt1$MEASUREDATE,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1MEASUREDATE) == length(tmp1MEASUREDATE[!is.na(tmp1MEASUREDATE)])){dt1$MEASUREDATE <- tmp1MEASUREDATE } else {print("Date conversion failed for dt1$MEASUREDATE. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1MEASUREDATE) 
if (class(dt1$STAGRPCODE)!="factor") dt1$STAGRPCODE<- as.factor(dt1$STAGRPCODE)
if (class(dt1$STATYPE)!="factor") dt1$STATYPE<- as.factor(dt1$STATYPE)
if (class(dt1$TRANSECT)!="factor") dt1$TRANSECT<- as.factor(dt1$TRANSECT)
if (class(dt1$LATITUDE)=="factor") dt1$LATITUDE <-as.numeric(levels(dt1$LATITUDE))[as.integer(dt1$LATITUDE) ]
if (class(dt1$LONGITUDE)=="factor") dt1$LONGITUDE <-as.numeric(levels(dt1$LONGITUDE))[as.integer(dt1$LONGITUDE) ]
if (class(dt1$TIME)=="factor") dt1$TIME <-as.numeric(levels(dt1$TIME))[as.integer(dt1$TIME) ]
if (class(dt1$ANALYST)!="factor") dt1$ANALYST<- as.factor(dt1$ANALYST)
if (class(dt1$TIDESTAGE)!="factor") dt1$TIDESTAGE<- as.factor(dt1$TIDESTAGE)
if (class(dt1$SCTTEMP)=="factor") dt1$SCTTEMP <-as.numeric(levels(dt1$SCTTEMP))[as.integer(dt1$SCTTEMP) ]
if (class(dt1$SCTSAL)=="factor") dt1$SCTSAL <-as.numeric(levels(dt1$SCTSAL))[as.integer(dt1$SCTSAL) ]
if (class(dt1$SCTCOND)=="factor") dt1$SCTCOND <-as.numeric(levels(dt1$SCTCOND))[as.integer(dt1$SCTCOND) ]
if (class(dt1$AIRTEMP)=="factor") dt1$AIRTEMP <-as.numeric(levels(dt1$AIRTEMP))[as.integer(dt1$AIRTEMP) ]
if (class(dt1$REFRSAL)=="factor") dt1$REFRSAL <-as.numeric(levels(dt1$REFRSAL))[as.integer(dt1$REFRSAL) ]
if (class(dt1$DOTEMP)=="factor") dt1$DOTEMP <-as.numeric(levels(dt1$DOTEMP))[as.integer(dt1$DOTEMP) ]
if (class(dt1$DO)=="factor") dt1$DO <-as.numeric(levels(dt1$DO))[as.integer(dt1$DO) ]
if (class(dt1$SECCHI)=="factor") dt1$SECCHI <-as.numeric(levels(dt1$SECCHI))[as.integer(dt1$SECCHI) ]
if (class(dt1$DEPTH)=="factor") dt1$DEPTH <-as.numeric(levels(dt1$DEPTH))[as.integer(dt1$DEPTH) ]
if (class(dt1$WINDDIR)!="factor") dt1$WINDDIR<- as.factor(dt1$WINDDIR)
if (class(dt1$WINDSP)=="factor") dt1$WINDSP <-as.numeric(levels(dt1$WINDSP))[as.integer(dt1$WINDSP) ]
if (class(dt1$COMMENTOBS)!="factor") dt1$COMMENTOBS<- as.factor(dt1$COMMENTOBS)
if (class(dt1$CHLA)=="factor") dt1$CHLA <-as.numeric(levels(dt1$CHLA))[as.integer(dt1$CHLA) ]
if (class(dt1$CHLA_SD)=="factor") dt1$CHLA_SD <-as.numeric(levels(dt1$CHLA_SD))[as.integer(dt1$CHLA_SD) ]
if (class(dt1$CHLA_N)=="factor") dt1$CHLA_N <-as.numeric(levels(dt1$CHLA_N))[as.integer(dt1$CHLA_N) ]
if (class(dt1$CHLA_QUAL)!="factor") dt1$CHLA_QUAL<- as.factor(dt1$CHLA_QUAL)
if (class(dt1$PHAEOPIGMENTS)=="factor") dt1$PHAEOPIGMENTS <-as.numeric(levels(dt1$PHAEOPIGMENTS))[as.integer(dt1$PHAEOPIGMENTS) ]
if (class(dt1$PHAEOPIGMENTS_SD)=="factor") dt1$PHAEOPIGMENTS_SD <-as.numeric(levels(dt1$PHAEOPIGMENTS_SD))[as.integer(dt1$PHAEOPIGMENTS_SD) ]
if (class(dt1$PHAEOPIGMENTS_N)=="factor") dt1$PHAEOPIGMENTS_N <-as.numeric(levels(dt1$PHAEOPIGMENTS_N))[as.integer(dt1$PHAEOPIGMENTS_N) ]
if (class(dt1$PHAEOPIGMENTS_QUAL)!="factor") dt1$PHAEOPIGMENTS_QUAL<- as.factor(dt1$PHAEOPIGMENTS_QUAL)
if (class(dt1$NH3)=="factor") dt1$NH3 <-as.numeric(levels(dt1$NH3))[as.integer(dt1$NH3) ]
if (class(dt1$NH3_SD)=="factor") dt1$NH3_SD <-as.numeric(levels(dt1$NH3_SD))[as.integer(dt1$NH3_SD) ]
if (class(dt1$NH3_N)=="factor") dt1$NH3_N <-as.numeric(levels(dt1$NH3_N))[as.integer(dt1$NH3_N) ]
if (class(dt1$NH3QUAL)!="factor") dt1$NH3QUAL<- as.factor(dt1$NH3QUAL)
if (class(dt1$PO4)=="factor") dt1$PO4 <-as.numeric(levels(dt1$PO4))[as.integer(dt1$PO4) ]
if (class(dt1$PO4_SD)=="factor") dt1$PO4_SD <-as.numeric(levels(dt1$PO4_SD))[as.integer(dt1$PO4_SD) ]
if (class(dt1$PO4_N)=="factor") dt1$PO4_N <-as.numeric(levels(dt1$PO4_N))[as.integer(dt1$PO4_N) ]
if (class(dt1$PO4QUAL)!="factor") dt1$PO4QUAL<- as.factor(dt1$PO4QUAL)
if (class(dt1$NOX)=="factor") dt1$NOX <-as.numeric(levels(dt1$NOX))[as.integer(dt1$NOX) ]
if (class(dt1$NOX_SD)=="factor") dt1$NOX_SD <-as.numeric(levels(dt1$NOX_SD))[as.integer(dt1$NOX_SD) ]
if (class(dt1$NOX_N)=="factor") dt1$NOX_N <-as.numeric(levels(dt1$NOX_N))[as.integer(dt1$NOX_N) ]
if (class(dt1$NOXQUAL)!="factor") dt1$NOXQUAL<- as.factor(dt1$NOXQUAL)
if (class(dt1$NO2)=="factor") dt1$NO2 <-as.numeric(levels(dt1$NO2))[as.integer(dt1$NO2) ]
if (class(dt1$NO2_SD)=="factor") dt1$NO2_SD <-as.numeric(levels(dt1$NO2_SD))[as.integer(dt1$NO2_SD) ]
if (class(dt1$NO2_N)=="factor") dt1$NO2_N <-as.numeric(levels(dt1$NO2_N))[as.integer(dt1$NO2_N) ]
if (class(dt1$NO2QUAL)!="factor") dt1$NO2QUAL<- as.factor(dt1$NO2QUAL)
if (class(dt1$DIN)=="factor") dt1$DIN <-as.numeric(levels(dt1$DIN))[as.integer(dt1$DIN) ]
if (class(dt1$COMMENTNUTR)!="factor") dt1$COMMENTNUTR<- as.factor(dt1$COMMENTNUTR)
if (class(dt1$NOXTDN)=="factor") dt1$NOXTDN <-as.numeric(levels(dt1$NOXTDN))[as.integer(dt1$NOXTDN) ]
if (class(dt1$NOXTDN_SD)=="factor") dt1$NOXTDN_SD <-as.numeric(levels(dt1$NOXTDN_SD))[as.integer(dt1$NOXTDN_SD) ]
if (class(dt1$NOXTDN_N)=="factor") dt1$NOXTDN_N <-as.numeric(levels(dt1$NOXTDN_N))[as.integer(dt1$NOXTDN_N) ]
if (class(dt1$NOXTDNQUAL)!="factor") dt1$NOXTDNQUAL<- as.factor(dt1$NOXTDNQUAL)
if (class(dt1$NO2TDN)=="factor") dt1$NO2TDN <-as.numeric(levels(dt1$NO2TDN))[as.integer(dt1$NO2TDN) ]
if (class(dt1$NO2TDN_SD)=="factor") dt1$NO2TDN_SD <-as.numeric(levels(dt1$NO2TDN_SD))[as.integer(dt1$NO2TDN_SD) ]
if (class(dt1$NO2TDN_N)=="factor") dt1$NO2TDN_N <-as.numeric(levels(dt1$NO2TDN_N))[as.integer(dt1$NO2TDN_N) ]
if (class(dt1$NO2TDNQUAL)!="factor") dt1$NO2TDNQUAL<- as.factor(dt1$NO2TDNQUAL)
if (class(dt1$COMMENTTDN)!="factor") dt1$COMMENTTDN<- as.factor(dt1$COMMENTTDN)
if (class(dt1$PROCESSORTDN)!="factor") dt1$PROCESSORTDN<- as.factor(dt1$PROCESSORTDN)
if (class(dt1$ORGANIC_CONTENT)=="factor") dt1$ORGANIC_CONTENT <-as.numeric(levels(dt1$ORGANIC_CONTENT))[as.integer(dt1$ORGANIC_CONTENT) ]
if (class(dt1$ORGANIC_CONTENT_SD)=="factor") dt1$ORGANIC_CONTENT_SD <-as.numeric(levels(dt1$ORGANIC_CONTENT_SD))[as.integer(dt1$ORGANIC_CONTENT_SD) ]
if (class(dt1$ORGANIC_CONTENT_N)=="factor") dt1$ORGANIC_CONTENT_N <-as.numeric(levels(dt1$ORGANIC_CONTENT_N))[as.integer(dt1$ORGANIC_CONTENT_N) ]
if (class(dt1$OC_QUAL)!="factor") dt1$OC_QUAL<- as.factor(dt1$OC_QUAL)
if (class(dt1$POROSITY)=="factor") dt1$POROSITY <-as.numeric(levels(dt1$POROSITY))[as.integer(dt1$POROSITY) ]
if (class(dt1$POROSITY_SD)=="factor") dt1$POROSITY_SD <-as.numeric(levels(dt1$POROSITY_SD))[as.integer(dt1$POROSITY_SD) ]
if (class(dt1$POROSITY_N)=="factor") dt1$POROSITY_N <-as.numeric(levels(dt1$POROSITY_N))[as.integer(dt1$POROSITY_N) ]
if (class(dt1$POROSITY_QUAL)=="factor") dt1$POROSITY_QUAL <-as.numeric(levels(dt1$POROSITY_QUAL))[as.integer(dt1$POROSITY_QUAL) ]
if (class(dt1$COMMENTS_ORGANIC_CONTENT)!="factor") dt1$COMMENTS_ORGANIC_CONTENT<- as.factor(dt1$COMMENTS_ORGANIC_CONTENT)
if (class(dt1$CHLOR_A)=="factor") dt1$CHLOR_A <-as.numeric(levels(dt1$CHLOR_A))[as.integer(dt1$CHLOR_A) ]
if (class(dt1$CHLOR_A_SD)=="factor") dt1$CHLOR_A_SD <-as.numeric(levels(dt1$CHLOR_A_SD))[as.integer(dt1$CHLOR_A_SD) ]
if (class(dt1$CHLOR_A_N)=="factor") dt1$CHLOR_A_N <-as.numeric(levels(dt1$CHLOR_A_N))[as.integer(dt1$CHLOR_A_N) ]
if (class(dt1$CHLOR_AQUAL)!="factor") dt1$CHLOR_AQUAL<- as.factor(dt1$CHLOR_AQUAL)
if (class(dt1$PHAEOPIGMENTS_A)=="factor") dt1$PHAEOPIGMENTS_A <-as.numeric(levels(dt1$PHAEOPIGMENTS_A))[as.integer(dt1$PHAEOPIGMENTS_A) ]
if (class(dt1$PHAEOPIGMENTS_A_SD)=="factor") dt1$PHAEOPIGMENTS_A_SD <-as.numeric(levels(dt1$PHAEOPIGMENTS_A_SD))[as.integer(dt1$PHAEOPIGMENTS_A_SD) ]
if (class(dt1$PHAEOPIGMENTS_A_N)=="factor") dt1$PHAEOPIGMENTS_A_N <-as.numeric(levels(dt1$PHAEOPIGMENTS_A_N))[as.integer(dt1$PHAEOPIGMENTS_A_N) ]
if (class(dt1$PHAEOPIGMENTS_AQUAL)!="factor") dt1$PHAEOPIGMENTS_AQUAL<- as.factor(dt1$PHAEOPIGMENTS_AQUAL)
if (class(dt1$COMMENTWATERCHL)!="factor") dt1$COMMENTWATERCHL<- as.factor(dt1$COMMENTWATERCHL)
if (class(dt1$GRACILARIA_MASS)=="factor") dt1$GRACILARIA_MASS <-as.numeric(levels(dt1$GRACILARIA_MASS))[as.integer(dt1$GRACILARIA_MASS) ]
if (class(dt1$GRACILARIA_SD)=="factor") dt1$GRACILARIA_SD <-as.numeric(levels(dt1$GRACILARIA_SD))[as.integer(dt1$GRACILARIA_SD) ]
if (class(dt1$GRACILARIA_N)=="factor") dt1$GRACILARIA_N <-as.numeric(levels(dt1$GRACILARIA_N))[as.integer(dt1$GRACILARIA_N) ]
if (class(dt1$GRACILARIA_QUAL)!="factor") dt1$GRACILARIA_QUAL<- as.factor(dt1$GRACILARIA_QUAL)
if (class(dt1$ULVA_MASS)=="factor") dt1$ULVA_MASS <-as.numeric(levels(dt1$ULVA_MASS))[as.integer(dt1$ULVA_MASS) ]
if (class(dt1$ULVA_SD)=="factor") dt1$ULVA_SD <-as.numeric(levels(dt1$ULVA_SD))[as.integer(dt1$ULVA_SD) ]
if (class(dt1$ULVA_N)=="factor") dt1$ULVA_N <-as.numeric(levels(dt1$ULVA_N))[as.integer(dt1$ULVA_N) ]
if (class(dt1$ULVA_QUAL)!="factor") dt1$ULVA_QUAL<- as.factor(dt1$ULVA_QUAL)
if (class(dt1$COMMENTMACROALGAE)!="factor") dt1$COMMENTMACROALGAE<- as.factor(dt1$COMMENTMACROALGAE)
if (class(dt1$TSS)=="factor") dt1$TSS <-as.numeric(levels(dt1$TSS))[as.integer(dt1$TSS) ]
if (class(dt1$TSS_SD)=="factor") dt1$TSS_SD <-as.numeric(levels(dt1$TSS_SD))[as.integer(dt1$TSS_SD) ]
if (class(dt1$TSS_N)=="factor") dt1$TSS_N <-as.numeric(levels(dt1$TSS_N))[as.integer(dt1$TSS_N) ]
if (class(dt1$TSSQUAL)!="factor") dt1$TSSQUAL<- as.factor(dt1$TSSQUAL)
if (class(dt1$POM)=="factor") dt1$POM <-as.numeric(levels(dt1$POM))[as.integer(dt1$POM) ]
if (class(dt1$POM_SD)=="factor") dt1$POM_SD <-as.numeric(levels(dt1$POM_SD))[as.integer(dt1$POM_SD) ]
if (class(dt1$POM_N)=="factor") dt1$POM_N <-as.numeric(levels(dt1$POM_N))[as.integer(dt1$POM_N) ]
if (class(dt1$POMQUAL)!="factor") dt1$POMQUAL<- as.factor(dt1$POMQUAL)
if (class(dt1$PIM)=="factor") dt1$PIM <-as.numeric(levels(dt1$PIM))[as.integer(dt1$PIM) ]
if (class(dt1$PIM_SD)=="factor") dt1$PIM_SD <-as.numeric(levels(dt1$PIM_SD))[as.integer(dt1$PIM_SD) ]
if (class(dt1$PIM_N)=="factor") dt1$PIM_N <-as.numeric(levels(dt1$PIM_N))[as.integer(dt1$PIM_N) ]
if (class(dt1$PIMQUAL)!="factor") dt1$PIMQUAL<- as.factor(dt1$PIMQUAL)
if (class(dt1$COMMENTTSS)!="factor") dt1$COMMENTTSS<- as.factor(dt1$COMMENTTSS)
if (class(dt1$SEDMASS)=="factor") dt1$SEDMASS <-as.numeric(levels(dt1$SEDMASS))[as.integer(dt1$SEDMASS) ]
if (class(dt1$SEDMASS_SD)=="factor") dt1$SEDMASS_SD <-as.numeric(levels(dt1$SEDMASS_SD))[as.integer(dt1$SEDMASS_SD) ]
if (class(dt1$SEDMASS_N)=="factor") dt1$SEDMASS_N <-as.numeric(levels(dt1$SEDMASS_N))[as.integer(dt1$SEDMASS_N) ]
if (class(dt1$SEDMASSQUAL)!="factor") dt1$SEDMASSQUAL<- as.factor(dt1$SEDMASSQUAL)
if (class(dt1$SEDPERC_N)=="factor") dt1$SEDPERC_N <-as.numeric(levels(dt1$SEDPERC_N))[as.integer(dt1$SEDPERC_N) ]
if (class(dt1$SEDPERC_N_SD)=="factor") dt1$SEDPERC_N_SD <-as.numeric(levels(dt1$SEDPERC_N_SD))[as.integer(dt1$SEDPERC_N_SD) ]
if (class(dt1$SEDPERC_N_N)=="factor") dt1$SEDPERC_N_N <-as.numeric(levels(dt1$SEDPERC_N_N))[as.integer(dt1$SEDPERC_N_N) ]
if (class(dt1$SEDPERC_NQUAL)!="factor") dt1$SEDPERC_NQUAL<- as.factor(dt1$SEDPERC_NQUAL)
if (class(dt1$SEDPERC_C)=="factor") dt1$SEDPERC_C <-as.numeric(levels(dt1$SEDPERC_C))[as.integer(dt1$SEDPERC_C) ]
if (class(dt1$SEDPERC_C_SD)=="factor") dt1$SEDPERC_C_SD <-as.numeric(levels(dt1$SEDPERC_C_SD))[as.integer(dt1$SEDPERC_C_SD) ]
if (class(dt1$SEDPERC_C_N)=="factor") dt1$SEDPERC_C_N <-as.numeric(levels(dt1$SEDPERC_C_N))[as.integer(dt1$SEDPERC_C_N) ]
if (class(dt1$SEDPERC_CQUAL)!="factor") dt1$SEDPERC_CQUAL<- as.factor(dt1$SEDPERC_CQUAL)
if (class(dt1$COMMENTSEDCN)!="factor") dt1$COMMENTSEDCN<- as.factor(dt1$COMMENTSEDCN)
                
# Convert Missing Values to NA for non-dates
                
dt1$SCTTEMP <- ifelse((trimws(as.character(dt1$SCTTEMP))==trimws("NA")),NA,dt1$SCTTEMP)
        
write.csv(file=localFile, dt1)

return(dt1)
}