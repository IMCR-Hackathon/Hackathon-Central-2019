# Read data package from DataONE member node
#
# Steps:
# 1.) Search DataONE and identify a data package of interest
# 2.) Select and copy the data package DOI, which has the form:
#     "doi:10.6073/pasta/7e48a6e1fb576a5be7b20ffbbaa10503"
# 3.) Enter the DOI to the data.pkg.doi argument as a character string

read_data_archived <- function(data.pkg.doi){
  
  # Create directory for data and metadata ------------------------------------
  
  message('Creating data package directory')
  
  dir.create(paste0(tempdir(), '/data_package'))
  
  # Download data and metadata to directory -----------------------------------
  
  message('Downloading data package to directory')
  
  pkg_dir_name <- suppressMessages(
    metajam::download_d1_data_pkg(
      meta_obj = data.pkg.doi,
      path = paste0(tempdir(), '/data_package')
    )
  )
  
  # Read data and metadata ----------------------------------------------------
  
  # Read data and metadata for each data object of the data package
  
  input <- suppressWarnings(
    suppressMessages(
      lapply(
        pkg_dir_name,
        metajam::read_d1_files
      )
    )
  )
  
  # Set list object names as list names
  
  fname <- paste0(
    stringr::str_extract(
      pkg_dir_name,
      '(?<=__)[:graph:]*(?=__)'
    ),
    '.',
    stringr::str_extract(
      pkg_dir_name,
      '(?<=__)[:alpha:]*$'
    )
  )
  
  names(input) <- fname

  # # Remove data and metadata directory ----------------------------------------
  # 
  # unlink(
  #   paste0(tempdir(), '/data_package'),
  #   recursive = TRUE,
  #   force = TRUE
  # )
  
  # Set class and add directory -----------------------------------------------
  
  class(input) <- 'online_data'

  input$data_package_path <- pkg_dir_name
  
  # Return data, metadata, and directory path(s) ------------------------------
  
  input
  
}


