# - # - # - # - # - # - # - # - # - # - # - # - #
##  Global Population Dynamics Databse (GPDD)  ##
# - # - # - # - # - # - # - # - # - # - # - # - #

## hello my name is Joey

## set working directory
getwd()

## load required packages
library(devtools)
library(tidyverse)
# devtools::install_github("ropensci/rgpdd")
library(rgpdd)

## check which version of the GPDD database is being use
?gpdd_version()

## plot an example of the data
ggplot(data = dplyr::filter(gpdd_data, MainID %in% 1:10)) + 
  geom_line(mapping = aes(x = SeriesStep, y = Population, 
                          col = MainID, group = MainID))

## rename some of the columns to differentiate columns with the same name 
## in different tables and to ensure column names match where appropriate
names(gpdd_taxon)[12] <- "Notes_taxon"
names(gpdd_datasource)[9] <- "Notes_datasource"
names(gpdd_main)[3] <- "DatasourceID" ## to match gpdd_datasource

## merge tables from the GPDD database into a single data.frame
gpdd_compiled <- Reduce(
  function(x, y) merge(x, y, all.x = TRUE, all.y = FALSE), 
  list(gpdd_data, gpdd_timeperiod, gpdd_main, gpdd_taxon, 
       gpdd_location, gpdd_biotope, gpdd_datasource)
)

length(unique(gpdd_compiled$MainID))  # [1] 4471

## find datasets with multiple samples per sampling year
gpdd_limited <- data.frame()

for (i in unique(gpdd_compiled$MainID)) {
  data <- subset(gpdd_compiled, MainID == i)
  if(nrow(data) > length(unique(data$SampleYear))) {
    gpdd_limited <- rbind(gpdd_limited, data)
  }
}

rm(data, i, gpdd_taxon, gpdd_datasource, gpdd_main)

length(unique(gpdd_limited$MainID))   # [1] 1042
