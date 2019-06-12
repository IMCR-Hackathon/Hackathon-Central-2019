#Hackathon 6.12.2019, New Mexico
#Environmental Data Iniative 

#Function code for summary table for general group

#code composed by Kathy Todd-Brown and expanded by Jocelyn Wardrup
#summary table to utilize as future code for other datasets obtain a snapshot of data from various studies....

#reviewed over by KTB and JW

localDir <- '~/Documents/GitHubRepo/Hackathon-Central-2019/temp/'

library(tidyverse)
remotes::install_github('NCEAS/metajam', force=TRUE)
source('/Users/Jocelyn/Documents/GitHub/Hackathon-Central-2019/R/read_data_archived.R')
workingTest <- read_data_archived('https://arcticdata.io/metacat/d1/mn/v2/object/urn%3Auuid%3A9e123f84-ce0d-4094-b898-c9e73680eafa')


HackathonFunction<-function(data.df)  {
  SummaryTable <- data.df %>% 
      select_if(is.numeric) %>%
      gather(key="column_name", value='value') %>%
      group_by(column_name) %>%
      summarize(min=min(value, na.rm=TRUE),
                quantile_25 = quantile(value, 0.25, na.rm=TRUE),
                quantile_50 = quantile(value, 0.50, na.rm=TRUE),
                quantile_75 = quantile(value, 0.75, na.rm=TRUE),
                max = max(value, na.rm=TRUE), 
                mean = mean(value, na.rm=TRUE),
                unique_count = length(unique(value)),
                finite_count = sum(is.finite(value)),
                count = length(value))
  
  return(SummaryTable)
  }

SummaryTable <- HackathonFunction(data.df = workingTest$data)

SummaryTable
