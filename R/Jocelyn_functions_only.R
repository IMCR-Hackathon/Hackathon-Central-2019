
#install library tidyverse
library(tidyverse)

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



#Function for 
#Table to display categorical summary information....

HackathonFunction2<-function(data.df2)  {
  SummaryTable_Categorical <- data.df2 %>% 
    select_if(~!is.numeric(.)) %>%
    gather(key="column_name", value='level') %>%
    group_by(column_name, level) %>%
    tally %>%
    ungroup() %>% group_by(column_name) %>%
    mutate(n_MainCategories = length(level),
         missingCount = ifelse(any(is.na(level)), n[is.na(level)], 0)
         )
return(SummaryTable_Categorical)
}




