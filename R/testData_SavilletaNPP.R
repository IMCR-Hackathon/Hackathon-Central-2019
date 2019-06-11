testData_SavilletaNPP <- function(localFile){
  
  if(file.exists(localFile)){
    return(read.csv(localFile))
  }
# Package ID: knb-lter-sev.289.239911 Cataloging System:https://pasta.lternet.edu.
# Data set title: Core Site Grid Quadrat Data for the Net Primary Production Study at the Sevilleta National Wildlife Refuge, New Mexico (2013- present).
# Data set creator:  Esteban Muldavin -  
# Metadata Provider:  Information Manager Sevilleta LTER -  
# Contact:    - Information Manager LTER Network Office  - tech-support@lternet.edu
# Contact:  Information Manager Sevilleta LTER -    - data-use@sevilleta.unm.edu
# Metadata Link: https://portal.lternet.edu/nis/metadataviewer?packageid=knb-lter-sev.289.239911
# Stylesheet v2.7 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

infile1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-sev/289/239911/08da2835d94560e099ae3da721cfdaf3" 
infile1 <- sub("^https","http",infile1) 
 dt1 <-read.csv(infile1,header=F 
          ,skip=1
            ,sep=","  
        , col.names=c(
                    "year",     
                    "season",     
                    "date",     
                    "site",     
                    "quad",     
                    "treatment",     
                    "species",     
                    "obs",     
                    "cover",     
                    "height",     
                    "count",     
                    "comment"    ), check.names=TRUE)
               
  
# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
                
if (class(dt1$season)!="factor") dt1$season<- as.factor(dt1$season)                                   
# attempting to convert dt1$date dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%m/%d/%Y"
tmp1date<-as.Date(dt1$date,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1date) == length(tmp1date[!is.na(tmp1date)])){dt1$date <- tmp1date } else {print("Date conversion failed for dt1$date. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1date) 
if (class(dt1$site)!="factor") dt1$site<- as.factor(dt1$site)
if (class(dt1$quad)!="factor") dt1$quad<- as.factor(dt1$quad)
if (class(dt1$treatment)!="factor") dt1$treatment<- as.factor(dt1$treatment)
if (class(dt1$species)!="factor") dt1$species<- as.factor(dt1$species)
if (class(dt1$obs)!="factor") dt1$obs<- as.factor(dt1$obs)
if (class(dt1$cover)=="factor") dt1$cover <-as.numeric(levels(dt1$cover))[as.integer(dt1$cover) ]
if (class(dt1$height)=="factor") dt1$height <-as.numeric(levels(dt1$height))[as.integer(dt1$height) ]
if (class(dt1$count)!="factor") dt1$count<- as.factor(dt1$count)
if (class(dt1$comment)!="factor") dt1$comment<- as.factor(dt1$comment)
                
# Convert Missing Values to NA for non-dates
                
dt1$cover <- ifelse((trimws(as.character(dt1$cover))==trimws("-999")),NA,dt1$cover)
dt1$height <- ifelse((trimws(as.character(dt1$height))==trimws(".")),NA,dt1$height)

write.csv(file=localFile, dt1)

return(dt1)
}



