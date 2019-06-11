# Package ID: knb-lter-vcr.153.24 Cataloging System:https://pasta.edirepository.org.
# Data set title: High and Low Tides of Hog Island Bay, Redbank, VA, and Oyster, VA for the Virginia Coast Reserve 2007-2018.
# Data set creator:  John Porter -  
# Data set creator:  David Krovetz -  
# Data set creator:  James Spitler -  
# Data set creator:  Thomas Williams -  
# Data set creator:  Kathleen Overman -  
# Data set creator:  William Nuttle -  
# Metadata Provider:    - Virginia Coast Reserve Long-Term Ecological Research Project 
# Contact:  John Porter -    - jhp7e@virginia.edu
# Contact:    - Information manager - Virginia Coast Reserve Long-Term Ecological Research Project   - jporter@lternet.edu
# Stylesheet v1.3 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@Virginia.edu 
#
#install package tidyverse if not already installed
if(!require(tidyverse)){ install.packages("tidyverse") }  
library("tidyverse") 
infile1 <- trimws("https://pasta.lternet.edu/package/data/eml/knb-lter-vcr/153/24/e7ab56ded773a2631a2fecaeb88de3f9") 
infile1 <-sub("^https","http",infile1)
# This creates a tibble named: dt1 
	dt1 <-read_delim(infile1  
                ,delim=","   
                ,skip=22 
                    ,quote='"'  
                    , col_names=c( 
                        "STATION",   
                        "TYPE",   
                        "DATE",   
                        "TIME",   
                        "TIDE",   
                        "WTEMP"   ), 
                    col_types=list( 
                        col_character(),  
                        col_character(), 
                        col_character(), 
                        col_number() , 
                        col_number() , 
                        col_number() ), 
                        na=c( " ",".","NA")  )
                    
                
# Observed issues when reading the data. An empty list is good!
problems(dt1) 
# Here is the structure of the input data tibble: 
glimpse(dt1) 
# And some statistical summaries of the data 
summary(dt1) 
# Get more details on character variables
                     
summary(as.factor(dt1$STATION)) 
summary(as.factor(dt1$TYPE))




