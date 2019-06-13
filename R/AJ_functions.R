### This file contains a list of functions that Alesia wrote
### These functions are meant to summarize time (POSIX, Date) objects in a data.frame

## make_vector_of_Date_variable_names
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

## make_vector_of_POSIX_variable_names
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
# this function creates ggplot clocks of the frequency of data occurring at different time frequencies, if POSIX type data
freqclocks_forDates <- function(df,var,varname) {
  na.df <- as.data.frame(!is.na(df))
  na.df[,varname] <- df[,varname]
  long.df <- gather(na.df, key = column.name, value = count, -varname)
  
  freq.timeline <- ggplot(long.df, aes(x = long.df[,1], group = column.name)) + 
    theme_bw() +
    geom_freqpoly(aes(colour = column.name), bins = 100) + 
    ylab("Data Frequency") + xlab("Time")  +
    guides(colour = guide_legend("Column Names")) +
    theme(legend.position = "top") + 
    scale_x_date(labels=date_format("%b-%Y"))
  
  # plot how much data occurs in each month of the year
  month.clock <- ggplot(long.df, aes(x = months(long.df[,1], T), group = column.name)) + 
    theme_bw() +
    geom_histogram(stat = "count", aes(fill = column.name)) + 
    coord_polar() + xlim(month.abb) +
    ylab("Data Coverage") + xlab("Month of Year") +
    guides(fill = guide_legend("Column Names")) +
    theme(legend.position = "none")
  
  Date.plots <- plot_grid(freq.timeline, month.clock,
                           ncol=1, align = "v", rel_widths = c(3,1))
  return(Date.plots)
}


## freqclocks
# this function creates ggplot clocks of the frequency of data occurring at different time frequencies, if POSIX type data
freqclocks_forPOSIX <- function(df,var,varname) {
  na.df <- as.data.frame(!is.na(df))
  na.df[,varname] <- df[,varname]
  long.df <- gather(na.df, key = column.name, value = count, -varname)
  
  freq.timeline <- ggplot(long.df, aes(x = long.df[,1], group = column.name)) + 
    theme_bw() +
    geom_freqpoly(aes(colour = column.name), bins = 100) + 
    ylab("Data Frequency") + xlab("Time")  +
    guides(colour = guide_legend("Column Names")) +
    theme(legend.position = "top") + 
    scale_x_datetime(labels=date_format("%b-%Y")))
  
  # plot how much data occus in each hour of the day
  hour.clock <- ggplot(long.df, aes(x = hour(long.df[,1]), group = column.name)) + 
    theme_bw() +
    geom_histogram(bins = 24, aes(fill = column.name)) + 
    coord_polar() +
    ylab("Data Coverage") + xlab("Hour of day") +
    guides(fill = guide_legend("Column Names")) +
    theme(legend.position = "none")
  
  # plot how much data occurs in each month of the year
  month.clock <- ggplot(long.df, aes(x = months(long.df[,1], T), group = column.name)) + 
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


df <- dt1
varname <- "DATE"
var <- df[,"DATE"]


dt1$DATE <- as.Date(dt1$DATE, "%d%b%Y")
