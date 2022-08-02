# Scrape to birdFamily.csv

library(tidyverse)
library(glue)
library(rvest)

# df <- read_csv("tmp/corhaven_unique_birds_season_2022-02-08.csv") |> 
df <- read_csv("tmp/unique_birds_season.csv") |> 
    mutate(url = paste0("https://ebird.org/species/", speciesCode)) |> 
    count(birdFamily, comName, speciesCode, url) |> 
    select(-n)
df$description <- NA
df$photo <- NA

# Do this for each bird
for (i in 1:nrow(df)) {
    # Access description, always in 4th node of that class
    description <- read_html(df$url[i]) |> 
        html_nodes(".u-stack-sm") |> 
        html_text() |> 
        nth(4)
    
    # Access photo URL
    photo <- read_html(df$url[i]) |> 
        html_nodes(".ImageResponsive") |> 
        html_attr("data-src") |> 
        nth(1)
    
    # Download photo
    download.file(url = photo, 
                  destfile = glue("photos/{df$speciesCode[i]}.png"))
    
    # Place in dataframe
    df$description[i] <- description
    df$photo[i] <- photo
    
    # Progress
    print(i)
}

# Write dataframe
write_csv(df, file = "ebird_data.csv")
