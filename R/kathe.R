localDir <- '~/Documents/GitHubRepo/Hackathon-Central-2019/temp/'

SAV_NPP <- testData_SavilletaNPP(file.path(localDir, 'SAV_NPP.csv'))
H2O_Qual <- testData_WaterQualityVirginia(file.path(localDir, 'H2O_Qual.csv'))

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
  mutate(numLevels = length(level))

