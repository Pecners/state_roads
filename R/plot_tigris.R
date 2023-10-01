library(tidyverse)
library(MetBrewer)
library(colorspace)
library(glue)
library(sf)
library(crayon)
library(tigris)
library(magick)
library(tigris)

s <- states()

# filter for only contiguous states
skip <- c(
  "Puerto Rico",
  "United States Virgin Islands",
  "Commonwealth of the Northern Mariana Islands",
  "American Samoa",
  "Guam"
)

skinny_s <- s |> 
  filter(!NAME %in% skip) |>
  arrange(NAME) |> 
  pull(NAME)

walk(skinny_s[1:51], function(fs) {
  if (!file.exists(glue("plots/{fs}_done.png"))) {
    st_counties <- counties(state = fs)
    
    roads <- map_df(st_counties$COUNTYFP, function(c) {
      cc <- which(st_counties$COUNTYFP == c)
      cat(cyan(glue("Starting {c} County, {fs} ({cc} of {length(st_counties$NAME)})")), "\n")
      t <- NULL
      # This code chunk is where data is queried from OSM.
      # I am saving the different queries because it kept failing
      # when I tried to do them all at once.
      try(
        t <- tigris::roads(state = fs, county = c)
      )
  
      
      cat(red(glue("Finished {c} County, {fs}")), "\n")
      if (!is.null(t)) {
        return(t)
      }

    })
    
    if (fs == "Florida") {
      missed <- st_read("data/Florida/tl_2021_12075_roads/tl_2021_12075_roads.shp")
      one <- roads |> 
        bind_rows(missed) |> 
        st_as_sf() |> 
        st_union()
    } else {
      one <- roads |> 
        st_as_sf() |> 
        st_union()
    }
    
    s <- states()
    
    this_state <- s |> 
      filter(NAME == fs) |> 
      st_transform(crs = st_crs(one))
    
    bb <- st_bbox(one) %>%
      st_as_sfc() %>%
      st_transform(., crs = st_crs(this_state))
    
    outline <- st_difference(bb, this_state)
    
    if (fs == "Alaska") {
      crs <- 3338
    } else if (fs == "Hawaii") {
      crs <- 6628
    } else {
      crs <- 5070
    }
    
    one_p <- one |> 
      ggplot() +
      geom_sf(color = "white", linewidth = .075) +
      # geom_sf(data = outline, fill = "black", color = NA) +
      theme_void() +
      coord_sf(crs = crs)
    
    ggsave(glue("plots/{fs}.png"), plot = one_p, bg = "black",
           width = 9, height = 13)
    
    img <- image_read(glue("plots/{fs}.png"))
    image_info(img)
    
    image_border(img, color = "black", 
                 geometry = glue("0x750")) |> 
      image_crop(geometry = glue("{image_info(img)$width}x{image_info(img)$height+1000}"),
                 gravity = "north") |> 
      image_annotate(text = str_to_upper(fs), 
                     font = "Poller One",
                     color = "grey90",
                     size = 200, 
                     gravity = "north",
                     location = "+0+375") |> 
      image_annotate(text = "Graphic by Spencer Schien (@MrPecners)",
                     font = "Amarante",
                     color = "grey60",
                     size = 50,
                     gravity = "north",
                     location = "+0+600") |> 
      image_annotate(text = "Data from OpenStreetMap",
                     font = "Amarante",
                     color = "grey60",
                     size = 50,
                     gravity = "north",
                     location = "+0+675") |> 
      image_write(glue("plots/{fs}_done.png"))
  }
  
  
})

