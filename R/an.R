library(metajam)
library(inspectdf)

metajam::download_d1_data("https://cn.dataone.org/cn/v2/resolve/urn%3Auuid%3A43f82ba7-6c5c-4d9d-84f6-b4162537e043", "D:/git repos/hackathon_2019/subsistence")

data <- read_d1_files("D:/git repos/hackathon_2019/subsistence/doi_10.5063_F1TD9VKD__ASFDB_FullPermit__csv")

id_space <- function(df, meta){
  
}

spacetime <- function(df, meta){
  
}