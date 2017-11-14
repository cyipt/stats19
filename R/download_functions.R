library(jsonlite)

stats19.download <- function(type = c("acc","cas","veh"),year){
  #Get List of Datasets from Data.GOV.UK
  link <- "https://data.gov.uk/api/3/action/package_show?id=road-accidents-safety-data"
  dir <- fromJSON(link, simplifyVector = FALSE)

}
