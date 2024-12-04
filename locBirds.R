# Pull hyperlocal bird reports from ebird
# by: Evelyn Yarzebinski, 2021-11

# set up env
library(geosphere)
library(tidyverse)
library(rebird)

# # check subregions if needed
# checkSubregions = ebirdsubregionlist("country") # find all country-level regions
# checkSubregions = ebirdsubregionlist("subnational1","US") # find all subregions in a country
# checkSubregions = ebirdsubregionlist("subnational2","US-MD") # find all 2nd level subregions in a country (state/region level)


# set date range to grab data from ebird
dateChoices = seq.Date(as.Date("2021-01-01"), as.Date("2021-12-31"), by = 1)

i = 1
loc = NULL
# locName = [input nickname eg 'mtRoyal']
# locLat = [input Lat]
# locLng = [input Lng]
# locSubregion = [input county eg 'US-PA-003']

locName = 'corhaven'
locSubregion = 'US-VA-171'
locLat = 38.70020106084334
locLng = -78.68155380385737

# run loop to pull ebird data by specified information above
for (i in 1:length(dateChoices)) {
  
  dayObs = ebirdhistorical(fieldSet = 'full',
                           loc = locSubregion,
                           date = dateChoices[i],
                           sleep = 3) # the delay in seconds between pings - reduce server load
  
  loc = bind_rows(loc,
                  dayObs)
  
  print(paste0(dateChoices[i]," complete"))
}

# process the data and make good date values
loc_fin = birds %>%
  filter(subnational2Code == locSubregion) %>% # filter to make sure only the specified county appears - some erroneous data appeared in some older years
  mutate(month = substr(obsDt,6,7),
         year = substr(obsDt,1,4),
         id = row_number()-1)

# write the file out
write.csv(loc_fin, paste0(locSubregion,"_",min(dateChoices),"_",max(dateChoices),".csv"),row.names = F)
