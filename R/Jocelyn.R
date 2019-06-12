HackathonFunction2<-function(value2)  {{
  SummaryTable2 <- {workingTest$data %>% 
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
  
  
  return(SummaryTable2)}

SummaryTable2
