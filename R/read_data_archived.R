# Read data package from DataONE member node
#
# Steps:
# 1.) Search DataONE and identify a data package of interest
# 2.) Right click "download all" button and copy URL
# 3.) Enter URL as character string in the url argument of this function

# List of data package identifiers for testing
data.pkg.url <- 'https://arcticdata.io/metacat/d1/mn/v2/object/urn%3Auuid%3A9e123f84-ce0d-4094-b898-c9e73680eafa'


read_archived_data <- function(data.pkg.url){
  
  # Create download directory -------------------------------------------------
  # Create directory in temporary R environment to download data to. I'm not
  # entirely sure what the ramifications of this are, so this location may
  # need to change.
  
  message('Creating data package directory')
  
  dir.create(paste0(tempdir(), '/data_package'))
  
  # Download data to directory ------------------------------------------------
  
  message('Downloading data package to directory')
  
  pkg_dir_name <- suppressMessages(
    metajam::download_d1_data(
      data_url = data.pkg.url,
      path = paste0(tempdir(), '/data_package')
    )
  )
  
  # Read data and metadata ----------------------------------------------------
  
  input <- suppressMessages(
    metajam::read_d1_files(
      folder_path = paste0(tempdir(), '/data_package/', pkg_dir_name)
    )
  )
  
  # Remove data
  
  unlink(paste0(tempdir(), '/data_package'))

  # Return data and metadata --------------------------------------------------
  
  input
  
}
