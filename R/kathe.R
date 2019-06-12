localDir <- '~/Documents/GitHubRepo/Hackathon-Central-2019/temp/'

library(tidyverse)
remotes::install_github('NCEAS/metajam')
source('R/read_data_archived.R')
workingTest <- read_data_archived('https://arcticdata.io/metacat/d1/mn/v2/object/urn%3Auuid%3A9e123f84-ce0d-4094-b898-c9e73680eafa')

numeric_sum <- workingTest$data %>% 
  select_if(is.numeric) %>%
  gather(key="column_name", value='value') %>%
  group_by(column_name) %>%
  summarise(min=min(value, na.rm=TRUE), 
            quatile_25 = quantile(value, 0.25, na.rm=TRUE),
            quatile_50 = quantile(value, 0.50, na.rm=TRUE),
            quatile_75 = quantile(value, 0.75, na.rm=TRUE),
            max = max(value, na.rm=TRUE), 
            unique_count = length(unique(value)),
            finite_count = sum(is.finite(value)),
            count = length(value))

nonNumeric_sum <- workingTest$data %>% 
  select_if(list(~!is.numeric(.))) %>%
  gather(key="column_name", value='level') %>%
  group_by(column_name, level) %>%
  tally %>%
  ungroup() %>% group_by(column_name) %>%
  mutate(numLevels = length(level),
         missingCount = ifelse(any(is.na(level)), n[is.na(level)], 0))

####################11 June 2019
source('R/testData_SavilletaNPP.R')
source('R/testData_WaterQualityVirginia.R')

SAV_NPP <- testData_SavilletaNPP(file.path(localDir, 'SAV_NPP.RData'))
H2O_Qual <- testData_WaterQualityVirginia(file.path(localDir, 'H2O_Qual.RData'))
library(tidyverse)
numeric_sum <- SAV_NPP %>% 
  select_if(is.numeric) %>%
  gather(key="column_name", value='value') %>%
  group_by(column_name) %>%
  summarise(min=min(value), 
            quatile_25 = quantile(value, 0.25),
            quatile_50 = quantile(value, 0.50),
            quatile_75 = quantile(value, 0.75),
            max = max(value), 
            unique_count = length(unique(value)),
            finite_count = sum(is.finite(value)),
            count = length(value))

nonNumeric_sum <- SAV_NPP %>% 
  select_if(funs(!is.numeric(.))) %>%
  gather(key="column_name", value='level') %>%
  group_by(column_name, level) %>%
  tally %>%
  ungroup() %>% group_by(column_name) %>%
  mutate(numLevels = length(level),
         missingCount = ifelse(any(is.na(level)), n[is.na(level)], 0))

