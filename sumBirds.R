# Load libraries
library(lubridate)
library(tidyverse)
library(janitor)
library(tibble)

# TODO
# consider if any additional criteria for typical birds is missing
# autojoin liturgical season date boundaries (see @kevin below)

# Parameters
nRecent = 11 # Include observations from nRecent last years
yearMax = 2023 # last full year of observations
yearMin = yearMax - nRecent
distance_from = 5 # Maximum distance of observation from point of interest (in km)

# subregion name to read
locSubregion = c("US-VA-171", "US-VA-003")


#TODO read in file of locSubregion(s)
# make loop. for each locSubregion, read in, filter year limits
i = 1
df = NULL
for (i in 1:length(locSubregion)) {
 
  df_individual = read.csv(file = )
   
}
  


# now filter for birds showing up only within N km of the specified coordinates
loc_fin = loc_fin %>%
  group_by(id) %>%
  mutate(
    dist_km = geosphere::distm(x = c(locLng,locLat), y = c(lng,lat), fun = distHaversine)) %>%
  ungroup() %>%
  mutate(dist_km = as.numeric(dist_km/1000),
         within5km = ifelse(dist_km<=distance_from, 1, 0)) %>%
  filter(within5km == 1)


# Order of seasons
seasonOrder = c("Advent",
                "Christmas",
                "Epiphany",
                "Lent",
                "Easter",
                "Ordinary Time (early)",
                "Ordinary Time (late)")

# File containing all observations from eBird
birds = read_csv(paste0(birdFile,".csv"))

# File containing our own observations that aren't included in eBird
# Pull only species codes
seenAtLocation = read_csv(paste0(birdFile,"Obs.csv")) %>%
  pull(speciesCode)

# File containing bird families for species
birdFamily = read_csv("birdFamily.csv") %>%
  select(comName, birdFamily)

# File containing liturgical seasons
# Clean date to get range
liturgicalSeasonLookup = read_csv("liturgicalDateBoundaries.csv") %>%
  mutate(season_start = lubridate::mdy(date),
         liturgicalSeason_final = factor(liturgicalSeason_final, levels = seasonOrder)) %>%
  group_by(year, liturgicalSeason_final) %>%
  summarise(season_start = min(season_start)) %>%
  ungroup() %>%
  arrange(season_start) %>%
  mutate(season_end = lead(season_start) - 1) %>%
  mutate(liturgicalSeason_final = as.character(liturgicalSeason_final))

# Last season (Christmas 2021) ends before Epiphany on Jan 6, 2022
liturgicalSeasonLookup$season_end[nrow(liturgicalSeasonLookup)] = ymd("2022-01-06") - 1

# Make start and end of season into interval
liturgicalSeasonLookup = liturgicalSeasonLookup %>%
  mutate(season = interval(season_start, season_end))

# Filter the hyperlocal birds and make pretty dates
birds_df = birds %>%
  dplyr::filter(dist_km <= distance_from,
                !is.na(howMany)) %>%
  mutate(obsDt = lubridate::ymd(as.Date(obsDt)),
         obsMonth = month(obsDt),
         obsYear = year(obsDt)) %>%
  select(-month)

# join in the bird family information
birds_df = birds_df %>%
  left_join(birdFamily, by = "comName")

# join in the liturgical seasons by closest inclusive date from liturgicalSeasonLookup applied to obsDt
birds_df$liturgicalSeason_final = NA

for (i in 1:nrow(birds_df)) {
  date = birds_df$obsDt[i]
  which_season = liturgicalSeasonLookup %>%
    mutate(this_season = date %within% liturgicalSeasonLookup$season) %>%
    filter(this_season == TRUE) %>%
    pull(liturgicalSeason_final)
  birds_df$liturgicalSeason_final[i] = which_season
  print(i)
}

# Order of seasons
seasonOrder = c("Advent",
                "Christmas",
                "Epiphany",
                "Lent",
                "Easter",
                "Ordinary Time (early)",
                "Ordinary Time (late)")

# Change seasons to a factor so it's ordered
birds_df = birds_df %>%
  mutate(liturgicalSeason_final = factor(liturgicalSeason_final, levels = seasonOrder))

# Get all years in the supplied df
years = birds_df %>%
  pull(obsYear) %>%
  unique()

# Take the most recent N years that appear in the df
recentYears = tail(years, nRecent)

# find the unique birds in a given year of data, then pull into wide format
unique_birds_year = birds_df %>%
  group_by(birdFamily,
           comName,
           speciesCode,
           obsYear) %>%
  dplyr::summarize(totalBirds = sum(howMany)) %>%
  group_by(birdFamily,
           comName,
           speciesCode) %>%
  mutate(grandTotal = sum(totalBirds)) %>%
  pivot_wider(names_from = obsYear,
              values_from = totalBirds,
              values_fill = 0) %>%
  dplyr::select("comName",
                sort(colnames(.))) %>%
  ungroup()

# calculate typical birds in a given year
# see typical_bird for logic of what makes a typical bird
typical_birds_year = unique_birds_year %>%
  mutate(count_0 = rowSums(unique_birds_year == 0, na.rm = T),
         count_0_Nyears = rowSums(unique_birds_year[,which(colnames(unique_birds_year) %in% min(recentYears)):
                                                      which(colnames(unique_birds_year) %in% max(recentYears))] == 0, na.rm = T),
         mean_all = round(grandTotal/length(years),2),
         mean_years = rowMeans(unique_birds_year[,which(colnames(unique_birds_year) %in% min(recentYears)):
                                               which(colnames(unique_birds_year) %in% max(recentYears))]),
         typical_bird = case_when(grandTotal >= length(years)*2~1, # total birds observed anytime in N years is 2*N or more. So in a 10 year window, both scenarios count: 1) 20 birds in 1 year counts and 2) 2 birds each year for 10 years
                                 count_0/length(years) <= .2 | count_0_Nyears == 0~1,
                                 mean_years > nRecent ~1, # the average observed each year is greater than nRecent
                                 mean_years < 1~0, # the average observed each year is not less than 1 bird per year
                                 unique_birds_year[,which(colnames(unique_birds_year) %in% max(recentYears))]/grandTotal >= .8~1, # more than 80% of years as defined by nRecent have observations
                                 TRUE~0)) %>%
  filter(typical_bird == 1 | speciesCode %in% seenAtLocation) # OR ALSO we personally observed the bird while on site at corhaven

# make a list of the typical birds for later lookup
typical_birds_name = typical_birds_year %>%
  pull(comName)

# find unique birds per month to calculate most common liturgical season
# unique_birds_month = birds_df %>%
#   filter(comName %in% typical_birds_name) %>%
#   #mutate(month = as.numeric(as.character(month))) %>%
#   group_by(birdFamily,
#            comName,
#            speciesCode,
#            # obsMonth,
#            liturgicalSeason_collapsed) %>%
#   dplyr::summarize(totalBirds = sum(howMany)) %>%
#   group_by(comName,
#            speciesCode) %>%
#   mutate(grandTotal = sum(totalBirds)) %>%
#   group_by(comName,
#            speciesCode,
#            liturgicalSeason_collapsed) %>%
#   mutate(total_season = sum(totalBirds)) %>%
#   pivot_wider(names_from = liturgicalSeason_collapsed,
#               values_from = totalBirds,
#               values_fill = 0) %>%
#   mutate(percent = round(total_season/grandTotal,2),
#          typicalSeason = case_when(percent>.75~liturgicalSeason_collapsed,
#                                    percent>.7~liturgicalSeason_collapsed,
#                                    percent>.65~liturgicalSeason_collapsed,
#                                    percent>.6~liturgicalSeason_collapsed,
#                                    percent>.55~liturgicalSeason_collapsed,
#                                    percent>.5~liturgicalSeason_collapsed,
#                                    percent>.45~liturgicalSeason_collapsed,
#                                    percent>.4~liturgicalSeason_collapsed,
#                                    percent>.35~liturgicalSeason_collapsed,
#                                    percent>.3~liturgicalSeason_collapsed,
#                                    percent>=.25~liturgicalSeason_collapsed,
#                                    TRUE~"none")) %>%
#   dplyr::select("comName",
#                 "speciesCode",
#                 "typicalSeason",
#                 sort(colnames(.))) %>%
#   ungroup()

# find unique birds per month to calculate most common liturgical season
unique_birds_month = birds_df %>%
  filter(comName %in% typical_birds_name) %>%
  #mutate(month = as.numeric(as.character(month))) %>%
  group_by(birdFamily,
           comName,
           speciesCode,
           liturgicalSeason_final) %>%
  dplyr::summarize(totalBirds = sum(howMany)) %>%
  mutate(grandTotal = sum(totalBirds),
         pct = totalBirds / grandTotal,
         pct_round = round(pct/.05)*.05, # Round percentage to nearest 5%
         is_typical = (pct_round == max(pct_round))) %>% # is_typical if the rounded pct is the max for group
  filter(is_typical) %>%
  select(comName, speciesCode, liturgicalSeason_final) |> 
  ungroup() %>%
  group_by(birdFamily, comName, speciesCode) %>%
  ## For birds with multiple typical seasons, combine those seasons into a single string, then drop duplicated rows
  #mutate(liturgicalSeason_final = paste0(liturgicalSeason_final, collapse = ", ")) %>%
  distinct()

# Write unique birds and their seasons
write_csv(unique_birds_month, file = "unique_birds_season.csv")

# # check that all birds have at least one typical season set
# ifelse(n_distinct(unique_birds_month$comName) != n_distinct(unique_birds_month$comName[unique_birds_month$typicalSeason!="none"]),
#                                                             "ERROR: not all birds have a season",
#                                                             "OK: all birds have a season")
# 
# 
# unique_birds_season = unique_birds_month %>%
#   select(birdFamily,
#          comName,
#          speciesCode,
#          typicalSeason,
#          percent) %>%
#   filter(typicalSeason != "none")
# 
# # check that all directly observed birds are in the dataset
# table(seenAtLocation %in% unique_birds_season$speciesCode)
# 
# # move long to wide to pull seasons
# unique_birds_season = unique_birds_season %>%
#   group_by(birdFamily,
#            comName,
#            speciesCode) %>%
#   pivot_wider(names_from = typicalSeason,
#               values_from = percent) %>%
#   ungroup() %>%
#   select(birdFamily,
#          comName,
#          speciesCode,
#          Advent,
#          Christmas,
#          Epiphany,
#          Lent,
#          Easter,
#          `Ordinary Time`
#          )
# 
# write.csv(unique_birds_season,paste0(birdFile,"_unique_birds_season_",Sys.Date(),".csv"),row.names = F)