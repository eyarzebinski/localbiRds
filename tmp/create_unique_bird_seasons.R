library(lubridate)
library(tidyverse)
library(janitor)
library(tibble)

# TODO
# factor levels for liturgical seasons to appear in order as specified in `seasonOrder`
# consider if any additional criteria for typical birds is missing
# autojoin liturgical season date boundaries (see @kevin below)

birdFile = "corhaven"
nRecent = 11

#setwd("~/Desktop/personal/coracle/")
birds = read.csv(paste0(birdFile,".csv"),stringsAsFactors = F)

# read lookup files
seenAtLocation = read.csv(paste0(birdFile,"Obs.csv"),stringsAsFactors = F)
birdFamily = read.csv("birdFamily.csv",stringsAsFactors = F)
liturgicalSeasonLookup = read.csv("liturgicalDateBoundaries.csv",stringsAsFactors = F)

seasonOrder = c("Advent",
                "Christmas",
                "Epiphany",
                "Lent",
                "Easter",
                "Ordinary Time")

liturgicalSeasonLookup = liturgicalSeasonLookup %>%
    mutate(date = lubridate::mdy(date))

seenAtLocation = seenAtLocation %>%
    select(speciesCode) %>% unlist()

# filter the hyperlocal birds and make pretty dates
birds_df = as_tibble(birds1) %>%
    dplyr::filter(within5km == 1,
                  !is.na(howMany)) %>%
    mutate(obsDt = lubridate::ymd(as.Date(obsDt)),
           obsMonth = month(obsDt),
           obsYear = year(obsDt)) %>%
    select(-month)

# join in the bird family information
birds_df = birds_df %>%
    left_join(birdFamily, by = "comName")

# join in the liturgical seasons by closest inclusive date from liturgicalSeasonLookup applied to obsDt
birds_df = birds_df %>%
    left_join( # TODO @kevin
    )

# get all years in the supplied df
years = unlist(sort(unique(birds_df$obsYear)))

# take the most recent N years that appear in the df
recentYears = tail(years,nRecent)

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
    select(comName) %>% unlist()

# find unique birds per month to calculate most common liturgical season
unique_birds_month = birds_df %>%
    filter(comName %in% typical_birds_name) %>%
    #mutate(month = as.numeric(as.character(month))) %>%
    group_by(birdFamily,
             comName,
             speciesCode,
             obsMonth,
             liturgicalSeason_collapsed) %>%
    dplyr::summarize(totalBirds = sum(howMany)) %>%
    group_by(comName,
             speciesCode) %>%
    mutate(grandTotal = sum(totalBirds)) %>%
    group_by(comName,
             speciesCode,
             liturgicalSeason_collapsed) %>%
    mutate(total_season = sum(totalBirds)) %>%
    pivot_wider(names_from = obsMonth,
                values_from = totalBirds,
                values_fill = 0) %>%
    mutate(percent = round(total_season/grandTotal,2),
           typicalSeason = case_when(percent>.75~liturgicalSeason_collapsed,
                                     percent>.7~liturgicalSeason_collapsed,
                                     percent>.65~liturgicalSeason_collapsed,
                                     percent>.6~liturgicalSeason_collapsed,
                                     percent>.55~liturgicalSeason_collapsed,
                                     percent>.5~liturgicalSeason_collapsed,
                                     percent>.45~liturgicalSeason_collapsed,
                                     percent>.4~liturgicalSeason_collapsed,
                                     percent>.35~liturgicalSeason_collapsed,
                                     percent>.3~liturgicalSeason_collapsed,
                                     percent>=.25~liturgicalSeason_collapsed,
                                     TRUE~"none")) %>%
    dplyr::select("comName",
                  "speciesCode",
                  "typicalSeason",
                  sort(colnames(.))) %>%
    ungroup()


# check that all birds have at least one typical season set
ifelse(n_distinct(unique_birds_month$comName) != n_distinct(unique_birds_month$comName[unique_birds_month$typicalSeason!="none"]),
       "ERROR: not all birds have a season",
       "OK: all birds have a season")


unique_birds_season = unique_birds_month %>%
    select(birdFamily,
           comName,
           speciesCode,
           typicalSeason,
           percent) %>%
    filter(typicalSeason != "none")

# check that all directly observed birds are in the dataset
table(seenAtLocation %in% unique_birds_season$speciesCode)

# move long to wide to pull seasons
unique_birds_season = unique_birds_season %>%
    group_by(birdFamily,
             comName,
             speciesCode) %>%
    pivot_wider(names_from = typicalSeason,
                values_from = percent) %>%
    ungroup() %>%
    select(birdFamily,
           comName,
           speciesCode,
           Advent,
           Christmas,
           Epiphany,
           Lent,
           Easter,
           `Ordinary Time`
    )

write.csv(unique_birds_season,paste0(birdFile,"_unique_birds_season_",Sys.Date(),".csv"),row.names = F)