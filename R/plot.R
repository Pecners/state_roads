library(tidyverse)
library(MetBrewer)
library(colorspace)
library(glue)
library(sf)
library(crayon)
library(tigris)
library(magick)

d_files <- list.files("data")


walk(d_files[2], function(fs) {
  if (!file.exists(glue("plots/{fs}_done.png"))) {
    these_files <- list.files(glue("data/{fs}"))
    
    roads <- map_df(these_files, function(tt) {
      cat(cyan(glue("Loading {fs}/{tt}")), "\n")
      t <- read_rds(glue("data/{fs}/{tt}")) 
      
      # county <-  str_extract(tt, "^[:alpha:]*(?=_)")
      # 
      # this_county <- counties |>
      #   filter(NAME == county) 
      
      if (!is.null(t$geometry)) {
        t |>
          select(highway) |>
          as_tibble()
      }
    })
    
    one <- roads |> 
      st_as_sf() |> 
      st_union()
    
    s <- states()
    
    this_state <- s |> 
      filter(NAME == fs) |> 
      st_transform(crs = st_crs(one))
    
    bb <- st_bbox(one)%>%
      st_as_sfc() %>%
      st_transform(., crs = st_crs(this_state))
    
    outline <- st_difference(bb, this_state)
    
    one_p <- one |> 
      ggplot() +
      geom_sf(color = "white", linewidth = .075) +
      geom_sf(data = outline, fill = "black", color = NA) +
      theme_void()
    
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

