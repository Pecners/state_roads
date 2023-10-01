library(tidyverse)
library(glue)
library(osmdata)
library(tigris)
library(crayon)
library(sf)

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
  filter(!NAME %in% skip)

missed <- which(!skinny_s$NAME %in% list.files("data"))


# The `osmdata` package facilitates OSM overpass queries.
# This is a vector of strings that will be passed as the
# value in a key value pair, with the key always being 
# "highway". Additionally, links are queried appending
# "_link" to the value. More info on these features can be found
# here: https://wiki.openstreetmap.org/wiki/Map_features.

queries <- c(
  "motorway",
  "trunk",
  "primary",
  "secondary",
  "tertiary",
  "residential",
  "service",
  "unclassified"
)

q <- c(queries, paste(queries, "_link", sep = ""))

sts <- skinny_s |> 
  arrange(NAME) |> 
  pull(NAME)


counties <- map_df(sts, function(s) {
  counties <- tigris::counties(state = s)
  counties |> 
    select(NAME, geometry) |> 
    mutate(state = s) 
})

while (length(list.dirs("data")) < 51) {
  walk(sts, function(s) {
    cat(magenta(paste("Starting", s, "\n")))
    
    st_counties <- counties |> 
      filter(state == s)
    
    dir <- glue("data/{s}")
    
    if (!dir.exists(dir)) {
      dir.create(dir)
    }
    
    walk(st_counties$NAME, function(c) {
      cc <- which(st_counties$NAME == c)
      cat(cyan(glue("Starting {c} County, {s} ({cc} of {length(st_counties$NAME)})")), "\n")
      
      this_county <- st_counties |> 
        filter(NAME == c)
      # This code chunk is where data is queried from OSM.
      # I am saving the different queries because it kept failing
      # when I tried to do them all at once.
      
      walk(q, function(x) {
        if (!file.exists(glue("data/{s}/{c}_{x}.rda"))) {
          cat(cyan(glue("Starting {c} {x}, {s}")), "\n")
          
          try({
            t <- opq(st_bbox(this_county)) |> 
              add_osm_feature(key = "highway", value = x) |> 
              osmdata_sf()
            
            saveRDS(t$osm_lines, file = glue("data/{s}/{c}_{x}.rda"))
            cat(red(glue("Finished {c} {x}, {s}")), "\n")
          })
        }
        
        
      })
      cat(red(glue("Finished {c} County, {s}")), "\n")
      
    })
    
    
    cat(yellow(glue("Finished {s}")), "\n")
  })
  
}
