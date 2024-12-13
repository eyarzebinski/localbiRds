render_fieldguide_table <- function(season) {
    
    # Get birds for a season, add some hacky columns for the table
    season_birds <- df |> 
        filter(typicalSeason == season) |> 
        mutate(photo_path = as.character(photo_path),
               text = paste0("<br><b>", comName, "</b><br><i>", birdFamily, "</i><br>", 
                             description, "<br><br>"),
               image = "",
               buffer = "")
    
    # Pull image paths
    img_paths <- season_birds |> 
        pull(photo_path)
    
    # Generate table
    season_birds |>
        select(image, buffer, text) |>
        kbl(booktabs = TRUE, escape = FALSE, col.names = NULL, longtable = TRUE) |>
        kable_styling(full_width = TRUE, font_size = 12) |>
        column_spec(1, image = img_paths)
        
}

