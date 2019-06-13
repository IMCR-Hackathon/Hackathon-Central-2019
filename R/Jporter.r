make_vector_of_numeric_variable_names<-function(df){
  numeric_var_name_vector<-c("")
  attach(df,warn.conflicts = F)
  for(varname in names(df)) {
    if(is.numeric(get(varname))){
      numeric_var_name_vector<-append(numeric_var_name_vector, varname, after = length(numeric_var_name_vector))
    }
  }
  detach(df)
  numeric_var_name_vector<-tail(numeric_var_name_vector,length(numeric_var_name_vector)-1)
  return(numeric_var_name_vector)
}


make_vector_of_categorical_variable_names<-function(df){
  cat_var_name_vector<-c("")
  attach(df,warn.conflicts = F)
  for(varname in names(df)) {
    var<-get(varname)
    if(is.factor(var) | is_character(var)){
      cat_var_name_vector<-append(cat_var_name_vector, varname, after = length(cat_var_name_vector))
    }
  }
  detach(df)
  cat_var_name_vector<-tail(cat_var_name_vector,length(cat_var_name_vector)-1)
  return(cat_var_name_vector)
}

make_numeric_variable_summary_df<-function(var){
  var_median<-median(var,na.rm=T)
  var_max<-max(var,na.rm=T)
  var_min<-min(var,na.rm=T)
  num_valid<-sum(!is.na(var))
  num_missing<-sum(is.na(var))
  df<-data.frame(var_median,var_max,var_min,num_valid,num_missing)
  return(df)
}

make_cat_variable_summary_df<-function(var){
  var_type<-class(var)
  num_levels<-length(levels(as.factor(var)))
  num_valid<-sum(!is.na(var))
  num_missing<-sum(is.na(var))
  df<-data.frame(var_type,num_levels,num_valid,num_missing)
  return(df)
}

make_numeric_histogram<-function(df,var,varname){
  library(ggplot2)
  #varname=deparse(substitute(var))
  x<-ggplot(df,aes(x=var))+ xlab(varname)+
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
    geom_density(alpha=.2, fill="#FF6666") 
  return(x)
}

make_categorical_histogram<-function(df,var,varname){
  library(ggplot2)
  #varname=deparse(substitute(var))
  x<-ggplot(df,aes(x=var))+ xlab(varname)+
    geom_bar() 
  return(x)
}

# make_missing_plot<-function(var){
#   library(inspectdf)
#   df<-as.data.frame(var)
#   x<-inspect_na(df)
#   show_plot(x)
# }

make_missing_plot<-function(var){
  library(ggplot2)
  is_missing<-ifelse(is.na(var),"Missing","Valid")
  obs_number<-as.numeric(rownames(as.data.frame(is_missing)))
  allones=1
  is_missing_df<-data.frame(obs_number,is_missing,allones)
  x<-ggplot(is_missing_df,aes(obs_number,allones,fill=is_missing))+geom_col(width=1)+
    theme(axis.text.y=element_blank(),
          axis.ticks.y=element_blank())+
    ylab("")+
    scale_fill_manual("legend", values = c("Missing" = "black", "Valid" = "lightgreen"))
#    scale_x_continuous(breaks=c(0, round(length(var)*0.2),round(length(var)*0.4),round(length(var)*0.6),round(length(var)*0.8),length(var)))
  return(x)
}

# make_missing_plot<-function(var){
#   library(ggplot2)
#   is_missing<-ifelse(is.na(var),"Missing","Valid")
#   obs_number<-rownames(as.data.frame(is_missing))
#   allones=1
#   is_missing_df<-data.frame(obs_number,is_missing,allones)
#   x<-ggplot(is_missing_df,aes(obs_number,allones,fill=is_missing))+geom_col()+
#   theme(axis.text.x=element_blank(),axis.text.y=element_blank(),
#          axis.ticks.x=element_blank(),axis.ticks.y=element_blank())+
#          ylab("")+scale_fill_manual("legend", values = c("Missing" = "black", "Valid" = "lightgreen"))
#   return(x)
# }
# make_missing_plot<-function(var){
#   library(ggplot2)
#   is_missing<-ifelse(is.na(var),0,1)
#   obs_number<-rownames(as.data.frame(is_missing))
#   is_missing_df<-data.frame(obs_number,is_missing)
#   x<-ggplot(is_missing_df,aes(obs_number,is_missing))+geom_col()+
#     theme(axis.text.x=element_blank(),
#           axis.ticks.x=element_blank())+ylab("Blank is Missing")
#   return(x)
# }

make_var_report<-function(df){
  library(knitr)
attach(df)
for(varname in names(df)){
  var<-get(varname)
  if (is.numeric(var)){
    print(paste('<a name="',varname[1],'">',sep=''))
    print(varname[1])
    print("</a>")
     x<-make_numeric_variable_summary_df(var)
     print(x)
     #kable(x,format="pandoc")
     x<-make_numeric_histogram(df,var)
     print(x)
  }
  if(is.factor(var) | is_character(var)){
    print(paste('<a name="',varname[1],'">',sep=''))
    print(varname[1])
    print("</a>")
    x<-make_cat_variable_summary_df(var)
    print(x)
    #kable(x,format="pandoc")
    x<-make_categorical_histogram(df,var,varname)
    print(x)
    
  }
}

detach(df)
}
