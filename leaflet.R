library(leaflet);
library(maptools);
library(maps);
library(stringr);

# Capitalize the first word of every string element in a vector
properCaps <- function(x) {
  words <- str_split(x, "\\s")
  out <- sapply(words, function(word) {
    paste0(
      toupper(str_sub(word, 1, 1)),
      str_sub(word, start = 2),
      collapse = " "
    )
  })
  return(out)
}

world <- rworldmap::getMap() # Get SpatialPolygonsDataFrame of countries
states <- map("state", fill = TRUE, plot = FALSE) # Get maps class of American states
state_names <- sapply(str_split(states$names, ":"), function(x) x[1]) %>% 
  properCaps() # Properly capitalized IDs for states
state_names[state_names == "Georgia"] <- "Georgia state" # Deal with Georgia country vs Georgia, USA
  
states <- map2SpatialPolygons(states, IDs = state_names, proj4string = world@proj4string)
world <- SpatialPolygons(world@polygons, proj4string = world@proj4string)

sp_polys     <- rbind(world, states)

sp_centroids <- read.csv("data/filtered_world_cities.csv", stringsAsFactors = FALSE, row.names = 1) %>% 
  select(long, lat) %>% 
  as.matrix() %>% 
  rbind(coordinates(sp_polys))

leaflet() %>% addPolygons(data = world, fillColor="white", weight = 0.5) %>% 
  addCircles(data = coordinates(world)) %>% 
  addTiles()
