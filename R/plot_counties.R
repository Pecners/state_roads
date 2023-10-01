library(tidyverse)
library(MetBrewer)
library(colorspace)
library(glue)
library(sf)
library(crayon)
library(tigris)
library(magick)
library(tigris)
library(foreach)
library(doParallel)

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

registerDoParallel(10)

do_it <- function(fs) {
  st_counties <- counties(state = fs)
  st_counties <- st_counties |> 
    mutate(ind = as.numeric(COUNTYFP)) |> 
    arrange(ind)
  
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
      return(t |> 
               mutate(county = c))
    }
    
  })
  
  # if (fs == "Florida") {
  #   missed <- st_read("data/Florida/tl_2021_12075_roads/tl_2021_12075_roads.shp")
  #   one <- roads |> 
  #     bind_rows(missed) |> 
  #     st_as_sf() |> 
  #     st_union()
  # } else {
  #   one <- roads |> 
  #     st_as_sf() |> 
  #     st_union()
  # }
  
  # one <- roads |> 
  #   st_as_sf() |> 
  #   st_union()
  
  if (fs == "Alaska") {
    crs <- 3338
  } else if (fs == "Hawaii") {
    crs <- 6628
  } else {
    crs <- 5070
  }
  
  dir <- glue("vid_plots/{fs}")
  if (!dir.exists(dir)) {
    dir.create(dir)
  }
  
  walk(1:length(st_counties$COUNTYFP), function(i) {
    cat(cyan(glue("Starting {fs} plot {i} of {length(st_counties$COUNTYFP)}")), "\n")
    
    if (i == 1) {
      all_black <- roads |> 
        ggplot() +
        geom_sf(color = "black", linewidth = .075) +
        theme_void() +
        coord_sf(crs = crs)
      
      tmp_f <- tempfile(fileext = ".png")
      final_f <- glue("{dir}/000.png")
      
      ggsave(tmp_f, plot = all_black, bg = "black",
             width = 9, height = 13)
      
      img <- image_read(tmp_f)
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
        image_annotate(text = "Data from US Census Bureau",
                       font = "Amarante",
                       color = "grey60",
                       size = 50,
                       gravity = "north",
                       location = "+0+675") |> 
        image_write(final_f)
    }
    
    white_c <- st_counties$COUNTYFP[1:i]
    black_c <- st_counties$COUNTYFP[(i+1):length(st_counties$COUNTYFP)]
    fps <- st_counties$COUNTYFP[i]
    white <- roads |> 
      filter(county %in% white_c)
    black <- roads |> 
      filter(county %in% black_c)
    
    one_p <- black |> 
      ggplot() +
      geom_sf(color = "black", linewidth = .075) +
      geom_sf(data = white, color = "white", linewidth = .075) +
      theme_void() +
      coord_sf(crs = crs)
    
    tmp_f <- tempfile(fileext = ".png")
    final_f <- glue("{dir}/{fps}.png")
    
    ggsave(tmp_f, plot = one_p, bg = "black",
           width = 9, height = 13)
    
    img <- image_read(tmp_f)
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
      image_annotate(text = "Data from US Census Bureau",
                     font = "Amarante",
                     color = "grey60",
                     size = 50,
                     gravity = "north",
                     location = "+0+675") |> 
      image_write(final_f)
  })
  
  
}

which(skinny_s == "Colorado")

foreach(i = skinny_s[6]) %dopar% do_it(fs = i)


