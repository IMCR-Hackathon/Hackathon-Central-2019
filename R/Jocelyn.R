#Hackathon 6.12.2019

#Function code for summary table for general group

#code composed by Kathy Brown and expanded by Jocelyn Wardrup
#summary table to utilize as future code for other datasets obtain a snapshot of data from various studies....


localDir <- '~/Documents/GitHubRepo/Hackathon-Central-2019/temp/'

library(tidyverse)
remotes::install_github('NCEAS/metajam', force=TRUE)
source('/Users/Jocelyn/Documents/GitHub/Hackathon-Central-2019/R/read_data_archived.R')
workingTest <- read_data_archived('https://arcticdata.io/metacat/d1/mn/v2/object/urn%3Auuid%3A9e123f84-ce0d-4094-b898-c9e73680eafa')


HackathonFunction<-function(value)  {{
  SummaryTable <- {workingTest$data %>% 
      select_if(is.numeric) %>%
      gather(key="column_name", value='value') %>%
      group_by(column_name) %>%
      summarize(class=class(value),
                min=min(value, na.rm=TRUE),
                quatile_25 = quantile(value, 0.25, na.rm=TRUE),
                quatile_50 = quantile(value, 0.50, na.rm=TRUE),
                quatile_75 = quantile(value, 0.75, na.rm=TRUE),
                max = max(value, na.rm=TRUE), 
                mean = mean(value, na.rm=TRUE), 
                median = median(value, na.rm=TRUE),
                unique_count = length(unique(value)),
                finite_count = sum(is.finite(value)),
                count = length(value))}}
  
  
  return(SummaryTable)}

SummaryTable
