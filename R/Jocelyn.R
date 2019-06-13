
#----------------------------------------------------

#Hackathon 6.12.2019 & 6.13.2019

#----------------------------------------------------

#Function code for summary table for general group

#Code authored by Kathy Todd-Brown and expanded by Jocelyn Wardrup
#Summary table to utilize as future code for other datasets obtain a 
#snapshot of numerical and categorical data from various studies....

#Reviewed over by KTB and JW, assistance and input on functions 
#provided by John Porter. 

#----------------------------------------------------

localDir <- '~/Documents/GitHubRepo/Hackathon-Central-2019/temp/'

#install library tidyverse
library(tidyverse)
remotes::install_github('NCEAS/metajam', force=TRUE)
#Rename for your respective computer....
source('/Users/Jocelyn/Documents/GitHub/Hackathon-Central-2019/R/read_data_archived.R')
#import the data from this link...
workingTest <- read_data_archived('https://arcticdata.io/metacat/d1/mn/v2/object/urn%3Auuid%3A9e123f84-ce0d-4094-b898-c9e73680eafa')

#----------------------------------------------------

#Step 1: Data Summary Tables

#----------------------------------------------------

#Step 1a:
#Function for 
#Table to display numerical summary information....

HackathonFunction1<-function(data.df1)  {
  SummaryTable_Numeric <- data.df1 %>% 
    select_if(is.numeric) %>%
    gather(key="column_name", value='value') %>%
    group_by(column_name) %>%
    summarize(min=min(value, na.rm=TRUE),
              quantile_25 = quantile(value, 0.25, na.rm=TRUE),
              quantile_50 = quantile(value, 0.50, na.rm=TRUE),
              quantile_75 = quantile(value, 0.75, na.rm=TRUE),
              max = max(value, na.rm=TRUE), 
              mean = mean(value, na.rm=TRUE),
              Unique_Values = length(unique(value)),
              Total_Values = sum(is.finite(value)),
              NAs= sum(is.na(value))
    )
  return(SummaryTable_Numeric)
}

SummaryTable_Numeric <- HackathonFunction1(data.df1 = workingTest$data)

SummaryTable_Numeric

#----------------------------------------------------

#Step 1b:
#Function for 
#Table to display categorical summary information....

HackathonFunction2<-function(data.df2)  {
  SummaryTable_Categorical <- data.df2 %>% 
    select_if(~!is.numeric(.)) %>%
    gather(key="column_name", value='level') %>%
    group_by(column_name, level) %>%
    tally %>%
    ungroup() %>% group_by(column_name) %>%
    mutate(n_Categories = length(level))
  
  return(SummaryTable_Categorical)
}

SummaryTable_Categorical <- HackathonFunction2(data.df2 = workingTest$data)

SummaryTable_Categorical

#Step 1c:
#Function for 
#Table to display categorical summary information....

HackathonFunction3<-function(data.df3)  {
  SummaryTable_Categorical_Individual <- data.df3 %>% 
    select_if(~!is.numeric(.)) %>%
    gather(key="column_name", value="level") %>%
    group_by(column_name) %>%
    summarize(Total_Values = length(level),
               n_Categories = length(unique(level)),
                missingCount = sum(is.na(level))
    )
  return(SummaryTable_Categorical_Individual)
}

SummaryTable_Categorical_Individual <- HackathonFunction3(data.df3 = 
                                                            workingTest$data)

SummaryTable_Categorical_Individual

#----------------------------------------------------

#Step 1d:
#Function for 
#Combining Tables

#----------------------------------------------------


HackathonSummaryTablesCombined<-bind_rows(SummaryTable_Categorical_Individual, 
                                          SummaryTable_Categorical, 
                                          SummaryTable_Numeric)
HackathonSummaryTablesCombined


#This is a little messy looking above.... I think we should keep them separate 
#(JW)

