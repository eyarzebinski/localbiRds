library(lubridate)
library(tidyverse)
library(janitor)
library(tibble)

birdFile = "corhaven"
nRecent = 10

setwd("~/Desktop/personal/coracle/")
birds = read.csv(paste0(birdFile,".csv"),stringsAsFactors = F)
seenAtLocation = read.csv(paste0(birdFile,"Obs.csv"),stringsAsFactors = F)

seasonOrder = c("Advent",
                "Christmas",
                "Epiphany",
                "Lent",
                "Easter",
                "Ordinary Time")

seenAtLocation = seenAtLocation %>%
  select(speciesCode) %>% unlist()

birds_df = as_tibble(birds) %>%
  dplyr::filter(within5km == 1,
                !is.na(howMany)) %>%
  mutate(obsDate = lubridate::mdy(obsDate),
         obsMonth = month(obsDate)) %>%
  select(obsMonth,
         obsDate,
         everything())

# get all years in the supplied df
years = unlist(sort(unique(birds_df$year)))

# take the most recent N years that appear in the df
recentYears = tail(years,nRecent)

unique_birds_year = birds_df %>%
  #mutate(year = as.numeric(as.character(year))) %>%
  group_by(comName,
           speciesCode,
           year) %>%
  dplyr::summarize(totalBirds = sum(howMany)) %>%
  group_by(comName,
           speciesCode) %>%
  mutate(grandTotal = sum(totalBirds)) %>%
  pivot_wider(names_from = year,
              values_from = totalBirds,
              values_fill = 0) %>%
  dplyr::select("comName",
                sort(colnames(.))) %>%
  ungroup()

typical_birds_year = unique_birds_year %>%
  mutate(count_0 = rowSums(unique_birds_year == 0, na.rm = T),
         count_0_Nyears = rowSums(unique_birds_year[,which(colnames(unique_birds_year) %in% min(recentYears)):
                                                      which(colnames(unique_birds_year) %in% max(recentYears))] == 0, na.rm = T),
         mean_all = round(grandTotal/length(years),2),
         mean_years = rowMeans(unique_birds_year[,which(colnames(unique_birds_year) %in% min(recentYears)):
                                               which(colnames(unique_birds_year) %in% max(recentYears))]),
         typical_bird = case_when(grandTotal >= length(years)*2~1,
                                 count_0/length(years) <= .2 | count_0_Nyears == 0~1,
                                 mean_years > 10~1,
                                 mean_years < 1~0,
                                 unique_birds_year[,which(colnames(unique_birds_year) %in% max(recentYears))]/grandTotal >= .8~1,
                                 TRUE~0)) %>%
  filter(typical_bird == 1 | speciesCode %in% seenAtLocation) 

typical_birds_name = typical_birds_year %>%
  select(comName) %>% unlist()


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




