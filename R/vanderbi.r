install.packages('remotes') # remotes allows you to install R packages from GitHub
remotes::install_github('NCEAS/metajam') # metajam reads data packages from DataONE member repositories
library(remotes)
library(metajam)

download_d1_data("https://pasta.lternet.edu/package/data/eml/knb-lter-fce/1169/6/b75babf9ad4d6e6ebb57e0a5b3f43a88", path = ".")
