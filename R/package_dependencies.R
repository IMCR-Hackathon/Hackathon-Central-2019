# This is a list of package dependencies
# Add your function dependencies here

# Install dependencies --------------------------------------------------------

install.packages('remotes') # remotes allows you to install R packages from GitHub
remotes::install_github('NCEAS/metajam') # metajam reads data packages from DataONE member repositories

# Load dependencies -----------------------------------------------------------

library(remotes)
library(metajam)
