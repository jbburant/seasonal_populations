###############################################
## - # - # - # - # - # - # - # - # - # - # - ##
##   Seasonal Population Dynamics Database   ##
## - # - # - # - # - # - # - # - # - # - # - ##
###############################################

## set working directory
getwd()

## load working directory
load("./R/gpdd_data_extraction.RData")

## call required packages
# devtools::install_github("ropensci/rgpdd")
pkgs <- c("tidyverse", "stringr", "ggthemes", "devtools", "rgpdd", "lubridate")
lapply(pkgs, library, character.only = TRUE)
rm(pkgs)

## set plotting theme
theme_set(theme_few())


# explore GPDD database ---------------------------------------------------

## check which version of the GPDD database is being use
?gpdd_version()

## plot an example of the data
ggplot(data = group_by(gpdd_limited, MainID) %>% 
         mutate(Scaled = scale(Population)) %>% 
         filter(., max(SeriesStep) == 10)) + 
  geom_line(mapping = aes(x = SeriesStep, y = Scaled, 
                          colour = as.factor(MainID), group = MainID)) + 
  scale_colour_tableau(name = "MainID") + 
  labs(x = "Series time step", y = "Population size (scaled)")

## explore the duration of datasets in GPDD database
ggplot(data = group_by(gpdd_compiled, MainID) %>% 
         summarize(Duration = length(SampleYear))) + 
  geom_histogram(mapping = aes(x = Duration), 
                 binwidth = 10, colour = "black", fill = "grey70")
## Comment: this doesn't really make sense 
## (what has been sampled for > 200 years??)

# merge database tables ---------------------------------------------------

## rename some of the columns to differentiate columns with the same name 
## in different tables and to ensure column names match where appropriate
gpdd_taxon <- gpdd_taxon %>% 
  rename(Notes_taxon = Notes)
gpdd_datasource <- gpdd_datasource %>% 
  rename(Notes_datasource = Notes)
gpdd_main <- gpdd_main %>% 
  rename(DatasourceID = DataSourceID)

## merge tables from the GPDD database into a single data.frame
gpdd_compiled <- as.tbl(Reduce(
  function(x, y) merge(x, y, by = intersect(names(x), names(y)), 
                       all.x = TRUE, all.y = FALSE), 
  list(gpdd_data, gpdd_timeperiod, gpdd_main, gpdd_taxon, 
       gpdd_location, gpdd_biotope, gpdd_datasource)))

rm(gpdd_main, gpdd_datasource, gpdd_taxon)

length(unique(gpdd_compiled$MainID))  # [1] 4471


# subset database to datasets with multiple samples per year --------------

## use the column SamplingFrequency, which describes how many times a 
## population was sampled within a given year

unique(gpdd_compiled$SamplingFrequency)
## Comment: SamplingFrequency is a factor because in addition to numbers it
## also includes levels for "Generations" and "Variable"

gpdd_limited <- gpdd_compiled %>% 
  group_by(MainID) %>%
  filter(!SamplingFrequency %in% c("0.5", "1", "1.3"))

## drop unused levels
gpdd_limited <- gpdd_limited %>%
  mutate(SamplingFrequency = as.factor(SamplingFrequency))

## explore limited data
with(gpdd_limited, tapply(MainID, SamplingFrequency, FUN = function(x) 
  length(unique(x))))

ggplot(data = group_by(gpdd_limited, MainID) %>% 
         summarize(Duration = length(SampleYear))) + 
  geom_histogram(mapping = aes(x = Duration), 
                 binwidth = 10, colour = "black", fill = "grey70")


## -- ## -- ## -- ## -- ## COMMENTED OUT ## -- ## -- ## -- ## -- ##
# gpdd_limited <- data.frame()
# 
# for (i in unique(gpdd_compiled$MainID)) {
#   data <- subset(gpdd_compiled, MainID == i)
#   if(nrow(data) > length(unique(data$SampleYear))) {
#     gpdd_limited <- rbind(gpdd_limited, data)
#   }
# }
# 
# rm(data, i, gpdd_taxon, gpdd_datasource, gpdd_main)
# 
# length(unique(gpdd_limited$MainID))   # [1] 1042


# define a season for each sampling interval ------------------------------

## use latitude (LatDD) to define a Hemisphere for each dataset
gpdd_limited <- gpdd_limited %>%
  mutate(Hemisphere = case_when(
    LatDD > 0 ~ "Northern", 
    LatDD < 0 ~ "Southern"))

## date on which sampling starts and ends for each sampling interval
gpdd_limited <- gpdd_limited %>%
  mutate(
    SeasonBegin = as.double(format(
      date_decimal(DecimalYearBegin), "%m%d")), 
    SeasonEnd = as.double(format(date_decimal(DecimalYearEnd), "%m%d"))) 

## convert start and end dates to hemisphere-specific seasons
gpdd_limited <- gpdd_limited %>%
  mutate(SeasonBegin = case_when(
    SeasonBegin %in% c(321:620) ~ "Spring", # 21-Mar to 20-Jun
    SeasonBegin %in% c(621:920) ~ "Summer", # 21-Jun to 20-Sep
    SeasonBegin %in% (921:1220) ~ "Autumn", # 21-Sep to 20-Dec
    SeasonBegin %in% c(0:320, 1221:1231) ~ "Winter"), # 21-Dec to 20-Mar
    
    SeasonEnd = case_when(
    SeasonEnd %in% c(321:620) ~ "Spring", 
    SeasonEnd %in% c(621:920) ~ "Summer", 
    SeasonEnd %in% (921:1220) ~ "Autumn", 
    SeasonEnd %in% c(0:320, 1221:1231) ~ "Winter"))

## how many records are there for each season?
table(gpdd_limited$SeasonBegin)
table(gpdd_limited$SeasonEnd)

## how many time series are sampled per season?
with(gpdd_limited, tapply(MainID, SeasonBegin, FUN = function(x) 
  length(unique(x))))


## NOTE: THESE SEASONS ARE NOT HEMISPHERE SPECIFIC! All seasons are 
## defined based on Northern Hemisphere convention. 
## Too many datasets have missing data for Latitude (LatDD). 
## Is there another way to define location?
table(gpdd_limited$Hemisphere, useNA = "ifany")
# Northern Southern     <NA> 
#    15445      637    80618 



## classify records as either breeding or non-breeding
gpdd_limited <- gpdd_limited %>%
  mutate(Season = case_when(
    SeasonBegin %in% c("Autumn", "Winter") ~ "Non-breeding", 
    SeasonBegin %in% c("Spring", "Summer") ~ "Breeding"))

table(gpdd_limited$Season, useNA = "ifany")
with(gpdd_limited, tapply(MainID, Season, FUN = function(x) 
  length(unique(x))))

# reorganize principle columns in limited dataset -------------------------
glimpse(gpdd_limited)

gpdd_sub <- gpdd_limited %>% 
  select(., MainID, ## dataset identifier
         Population, PopulationUntransformed, ## population size
         SampleYear, Generation, SeriesStep, SamplingFrequency, ## time
         DecimalYearBegin, DecimalYearEnd, 
         TimePeriod, TimePeriodGroup, SeasonBegin, SeasonEnd, 
         LocationID, Hemisphere, Continent, Ocean, LongDD, LatDD, ## location
         TaxonID, TaxonomicClass, TaxonomicOrder, TaxonName) # taxonomy


length(unique(gpdd_limited$TaxonName))

boxplot(Population ~ SeasonBegin, data = gpdd_limited)

gpdd_limited %>%
  group_by(MainID, SampleYear, Season) %>%
  summarise(mean = mean(Population)) %>%
  filter(MainID %in% tail(unique(gpdd_limited$MainID), 10)) %>%
  ggplot(aes(x = SampleYear, y = mean, group = MainID)) + 
  geom_point(aes(colour = as.factor(Season))) + 
  geom_line(colour = "grey60", alpha = 0.7) + 
  scale_colour_manual(values = c("steelblue4", "orange3"), name = "Season")

