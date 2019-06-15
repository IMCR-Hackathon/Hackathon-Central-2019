### This file contains a list of functions that Alesia wrote
### These functions are meant to summarize time (POSIX, Date) objects in a data.frame

# libraries that are used in these functions
library(lubridate)
library(ggplot2)
library(tidyr)
library(dplyr)
library(stringr)
library(cowplot)
library(scales)
library(readr)


## make_vector_of_POSIX_variable_names
# this function makes a list of POSIX class columns in a data.frame
make_vector_of_POSIX_variable_names<-function(df){
  POSIX_var_name_vector<-c("")
  attach(df,warn.conflicts = F)
  for(varname in names(df)) {
    var<-get(varname)
    if(is.POSIXt(var)){
      POSIX_var_name_vector<-append(POSIX_var_name_vector, varname, after = length(POSIX_var_name_vector))
    }
  }
  detach(df)
  POSIX_var_name_vector<-tail(POSIX_var_name_vector,length(POSIX_var_name_vector)-1)
  return(POSIX_var_name_vector)
}

## make_vector_of_Date_variable_names
# this function makes a list of POSIX class columns in a data.frame
make_vector_of_Date_variable_names<-function(df){
  Date_var_name_vector<-c("")
  attach(df,warn.conflicts = F)
  for(varname in names(df)) {
    var<-get(varname)
    if(is.Date(var)){
      Date_var_name_vector<-append(Date_var_name_vector, varname, after = length(Date_var_name_vector))
    }
  }
  detach(df)
  Date_var_name_vector<-tail(Date_var_name_vector,length(Date_var_name_vector)-1)
  return(Date_var_name_vector)
}

## make_time_variable_summary_df
# this function writes a summary table of time (POSIX, Date) objects
make_time_variable_summary_df<-function(var){
  var_type<-class(var)[1]
  num_times<-length(levels(as.factor(var)))
  num_valid<-sum(!is.na(var))
  num_missing<-sum(is.na(var))
  first_time <- min(var, na.rm = T)
  last_time <- max(var, na.rm = T)
  mode_time_step <- names(which.max(table(diff(unique(var[order(var)]), lag = 1))))
  #med_time_step <- median(diff(unique(var[order(var)]), lag = 1), na.rm = T)
  df<-data.frame(var_type,first_time,last_time,mode_time_step,num_times,num_valid,num_missing)
  return(df)
}

## freqclocks
# this function creates ggplot clocks of the frequency of data occurring at different time frequencies, if Date type data
freqclocks_forDates <- function(df,var,varname) {
  na.df <- as.data.frame(!is.na(df))
  na.df[,varname] <- df[,varname]
  long.df <- gather(na.df, key = column.name, value = count, -varname)
  
  freq.timeline <- ggplot(long.df[long.df$count %in% TRUE,], aes(x = long.df[long.df$count %in% TRUE,1], group = column.name)) + 
    theme_bw() +
    geom_freqpoly(aes(colour = column.name), bins = 100) + 
    ylab("Data Coverage") + xlab("Time")  +
    guides(colour = guide_legend("Column Names")) +
    #theme(legend.position = "top") + 
    scale_x_date(labels=date_format("%b-%Y"))
  
  # plot how much data occurs in each month of the year
  month.clock <- ggplot(long.df[long.df$count %in% TRUE,], aes(x = months(long.df[long.df$count %in% TRUE,1], T), group = column.name)) + 
    theme_bw() +
    geom_histogram(stat = "count", aes(fill = column.name)) + 
    coord_polar() + xlim(month.abb) +
    ylab("Data Coverage") + xlab("Month of Year") +
    guides(fill = guide_legend("Column Names")) +
    theme(legend.position = "none")
  
  Date.plots <- plot_grid(freq.timeline, month.clock,
                           ncol=1, align = "v", rel_widths = c(3,1))
  return(list(freq.timeline, month.clock))
}


## freqclocks
# this function creates ggplot clocks of the frequency of data occurring at different time frequencies, if POSIX type data
freqclocks_forPOSIX <- function(df,var,varname) {
  na.df <- as.data.frame(!is.na(df))
  na.df[,varname] <- df[,varname]
  long.df <- gather(na.df, key = column.name, value = count, -varname)
  
  freq.timeline <- ggplot(long.df[long.df$count %in% TRUE,], aes(x = long.df[long.df$count %in% TRUE,1], group = column.name)) + 
    theme_bw() +
    geom_freqpoly(aes(colour = column.name), bins = 100) + 
    ylab("Data Coverage") + xlab("Time")  +
    guides(colour = guide_legend("Column Names")) +
    theme(legend.position = "top") + 
    scale_x_datetime(labels=date_format("%b-%Y"))
  
  # plot how much data occus in each hour of the day
  hour.clock <- ggplot(long.df[long.df$count %in% TRUE,], aes(x = hour(long.df[long.df$count %in% TRUE,1]), group = column.name)) + 
    theme_bw() +
    geom_histogram(bins = 24, aes(fill = column.name)) + 
    coord_polar() +
    ylab("Data Coverage") + xlab("Hour of day") +
    guides(fill = guide_legend("Column Names")) +
    theme(legend.position = "none")
  
  # plot how much data occurs in each month of the year
  month.clock <- ggplot(long.df[long.df$count %in% TRUE,], aes(x = months(long.df[long.df$count %in% TRUE,1], T), group = column.name)) + 
    theme_bw() +
    geom_histogram(stat = "count", aes(fill = column.name)) + 
    coord_polar() + xlim(month.abb) +
    ylab("Data Coverage") + xlab("Month of Year") +
    guides(fill = guide_legend("Column Names")) +
    theme(legend.position = "none")
  
  both.clocks <- plot_grid(month.clock, hour.clock,
                           nrow=1, align = "h", rel_widths = c(1,1))
  POSIX.plots <- plot_grid(freq.timeline, both.clocks,
                           ncol=1, align = "v", rel_widths = c(1,1))
  return(three.plots)
}


# this function takes character-class columns and attempts to coerce them into Date-class, if reasonable    
# test whether a column conforms to standard date formats

# if data table does not have a formatted date column, scan each character column and test whether any of them can be coerced to a date




time.detector <- function(df) {
  
  if(all(!as.vector(t(as.data.frame(sapply(df, class))[,1])) %in% c("Date", "POSIXc", "POSIXlt", "POSIXct"))) {
    for(i in 1:dim(df)[2]) {
      if(is.character(df[,i]) | is.factor(df[,i])) {
        
        var <- as.character(unique(df[,i]))
        formatted.date.col <- NA
        
        date.formats <- c("%d%b%Y", "%d%.%m%.%Y", "%m%.%d%.%Y", "%d%.%m%.%y", "%m%.%d%.%y", "%d%.%b%.%Y", "%b%.%d%.%Y", "%Y%.%m%.%d", "%Y%.%d%.%m", "%y%.%m%.%d", "%b %d, %Y")
        
        date.tests <- data.frame(matrix(NA, nrow = length(var), ncol = length(date.formats)), row.names = as.character(var))
        colnames(date.tests) <- date.formats
        
        date.tests2 <- as.data.frame(
          lapply(as.list(date.formats), function(x){
          ifelse(is.na(parse_date(var, x)), NA, 1)
        }))
        names(date.tests2) <- date.formats
        
        max.dates.coerced <- max(colSums(date.tests2), na.rm = T)
        coerced.date <- NA
        
        if(!is.infinite(max.dates.coerced)) {
          coerced.date <- parse_date(df[,i], date.formats[colSums(date.tests2) %in% max.dates.coerced])
        }
        
        df$coerced.date <- coerced.date
        
      } else {next}
    }
  }
  return(df)
}

