# This is a list of package dependencies
# Add your function dependencies here

# Install dependencies --------------------------------------------------------

install.packages('remotes') # remotes allows you to install R packages from GitHub
remotes::install_github('clnsmth/metajam') # metajam reads data packages from DataONE member repositories
install.packages('testhat')

# Load dependencies -----------------------------------------------------------

library(remotes)
library(metajam)
library(testthat)
