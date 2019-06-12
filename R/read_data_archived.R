# Read data package from DataONE member node
#
# Steps:
# 1.) Search DataONE and identify a data package of interest
# 2.) Right click "download all" button and copy URL
# 3.) Enter URL as character string in the url argument of this function

# List of data package identifiers for testing --------------------------------

# metajam example 1
data.pkg.url <- 'https://arcticdata.io/metacat/d1/mn/v2/object/urn%3Auuid%3A9e123f84-ce0d-4094-b898-c9e73680eafa'

# Problematic dataset due to column names not displayed properly
# This data package fails to load in metajam ... error message:
#
# Error in check_version(data_url, formatType = "data") : 
# No matching identifiers were found.
data.pkg.url <- 'https://gmn.lternet.edu/mn/v2/packages/application%2Fbagit-097/doi%3A10.6073%2Fpasta%2F08abf45e6ef1585ad7ee1e00fb9d7dc1'

# A typical dataset
# This data package fails to load in metajam ... error message:
#
# Error in check_version(data_url, formatType = "data") : 
# No matching identifiers were found.
data.pkg.url <- 'https://gmn.edirepository.org/mn/v2/packages/application%2Fbagit-097/doi%3A10.6073%2Fpasta%2Fb745934d136ce9ca8de26c5063eee86a'

# A typical dataset
# This data package fails to load in metajam ... error message:
#
# Error in check_version(data_url, formatType = "data") : 
# No matching identifiers were found.
data.pkg.url <- 'https://gmn.lternet.edu/mn/v2/packages/application%2Fbagit-097/doi%3A10.6073%2Fpasta%2F7e48a6e1fb576a5be7b20ffbbaa10503'

# And here is a problematic dataset - too many columns, lots of missing data, some range and unit issues.
# This data package fails to load in metajam ... error message:
#
# Error in check_version(data_url, formatType = "data") : 
# No matching identifiers were found.
data.pkg.url <- 'https://gmn.lternet.edu/mn/v2/packages/application%2Fbagit-097/doi%3A10.6073%2Fpasta%2Fb650b236f092e0fdee0d5d8ccf521cb3'

# Dataset with code generation line (and R code....)
#
# This data package fails to load in metajam ... error message:
#
# Error in check_version(data_url, formatType = "data") : 
# No matching identifiers were found.
data.pkg.url <- 'https://gmn.lternet.edu/mn/v2/packages/application%2Fbagit-097/doi%3A10.6073%2Fpasta%2F9f2f89e48f9e943f7125d1a335d96eb0'

# Non-problematic LTER daily stream water chemistry dataset:
#
# This data package fails to load in metajam ... error message:
#
# Error in check_version(data_url, formatType = "data") : 
# No matching identifiers were found.
data.pkg.url <- 'https://gmn.lternet.edu/mn/v2/packages/application%2Fbagit-097/doi%3A10.6073%2Fpasta%2F451553a669950425a96450f23bd09e07'


# Clean data table with code generation also
#
# This data package fails to load in metajam ... error message:
#
# Error in check_version(data_url, formatType = "data") : 
# No matching identifiers were found.
data.pkg.url <- 'https://gmn.lternet.edu/mn/v2/packages/application%2Fbagit-097/doi%3A10.6073%2Fpasta%2F3c10627e2344d74141b45e70a9144ccb'

# Typical dataset
#
# This data package fails to load in metajam ... error message:
#
# Error in check_version(data_url, formatType = "data") : 
# No matching identifiers were found.
data.pkg.url <- 'https://gmn.lternet.edu/mn/v2/packages/application%2Fbagit-097/doi%3A10.6073%2Fpasta%2Fc964ed49ff284dfcaaf53719651da60f'

# Spatially distributed data, with two data tables, one for data, one for sites:
#
# This data package fails to load in metajam ... error message:
#
# Error in check_version(data_url, formatType = "data") : 
# No matching identifiers were found.
data.pkg.url <- 'https://gmn.lternet.edu/mn/v2/packages/application%2Fbagit-097/doi%3A10.6073%2Fpasta%2F74d30fdbb17e0f76b54548ce74bf27e4'

# datasets with explicit spatial and temporal columns :)
#
# This data package fails to load in metajam ... error message:
#
# Error in check_version(data_url, formatType = "data") : 
# No matching identifiers were found.
data.pkg.url <- 'https://knb.ecoinformatics.org/knb/d1/mn/v2/packages/application%2Fbagit-097/resource_map_urn%3Auuid%3Ac8763ac5-fa04-44e6-b6c2-a2d326bbcabb'

# Three data tables, long time series, multiple measurements
#
# This data package fails to load in metajam ... error message:
#
# Error in check_version(data_url, formatType = "data") : 
# No matching identifiers were found.
data.pkg.url <- 'https://gmn.lternet.edu/mn/v2/packages/application%2Fbagit-097/doi%3A10.6073%2Fpasta%2Fbe54fff488ec4d0d6146956b5fff057e'

# Begin function --------------------------------------------------------------

read_data_archived <- function(data.pkg.url){
  
  # Create directory for data and metadata ------------------------------------
  # Create directory in temporary R environment to download data to. I'm not
  # entirely sure what the ramifications of this are, so this location may
  # need to change.
  
  message('Creating data package directory')
  
  dir.create(paste0(tempdir(), '/data_package'))
  
  # Download data and metadata to directory -----------------------------------
  
  message('Downloading data package to directory')
  
  pkg_dir_name <- suppressMessages(
    metajam::download_d1_data(
      data_url = data.pkg.url,
      path = paste0(tempdir(), '/data_package')
    )
  )
  
  # Read data and metadata ----------------------------------------------------
  
  # Read data and metadata
  
  input <- suppressMessages(
    metajam::read_d1_files(
      folder_path = pkg_dir_name
    )
  )
  
  # Remove data and metadata directory
  
  unlink(
    paste0(tempdir(), '/data_package'), 
    recursive = TRUE, 
    force = TRUE
  )

  # Return data and metadata --------------------------------------------------
  
  input
  
}


