# Pull hyperlocal bird reports
# by: Evelyn Yarzebinski, 2021-11
# TODO - make auto-publish doc

# set up env
library(geosphere)
library(tidyverse)
library(geosphere)
library(rebird)

# set date range to grab data from ebird
dateChoices = seq.Date(as.Date("2015-01-01"), as.Date("2018-12-31"), by = 1)

i = 1
loc = NULL
locName = [input nickname eg 'mtRoyal']
locLat = [input Lat]
locLng = [input Lng]
locCounty = [input county eg 'US-PA-003']

# run loop to pull ebird data by specified information above
for (i in 1:length(dateChoices)) {
  
  dayObs = ebirdhistorical(fieldSet = 'full',
                           loc = locCounty,
                           date = dateChoices[i],
                           sleep = 3) # the delay in seconds between pings - reduce server load
  
  loc = bind_rows(loc,
                  dayObs)
  
  print(paste0(dateChoices[i]," complete"))
}

# process the data and make good date values
loc_fin = loc %>%
  filter(subnational2Code == locCounty) %>% # filter for the county - some error cropped up in older years
  mutate(month = substr(obsDt,6,7),
         year = substr(obsDt,1,4),
         id = row_number()-1)

# now filter for birds showing up only within 5km of the specified coordinates
loc_fin = loc_fin %>%
  group_by(id) %>%
  mutate(
    dist_km = geosphere::distm(x = c(locLng,locLat), y = c(lng,lat), fun = distHaversine)) %>%
  ungroup() %>%
  mutate(dist_km = as.numeric(dist_km/1000),
         within5km = ifelse(dist_km<=5, 1, 0))

# write the file out
write.csv(loc_fin, paste0(locName,"_",min(dateChoices),"_",max(dateChoices),".csv"),row.names = F)
