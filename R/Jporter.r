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

make_numeric_histogram<-function(df,var){
  library(ggplot2)
  varname=deparse(substitute(var))
  x<-ggplot(df,aes(x=var))+ xlab(varname)+
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
    geom_density(alpha=.2, fill="#FF6666") 
  return(x)
}

make_categorical_histogram<-function(df,var){
  library(ggplot2)
  varname=deparse(substitute(var))
  x<-ggplot(df,aes(x=var))+ xlab(varname)+
    geom_bar() 
  return(x)
}
