library(dplyr);
library(lubridate);
library(rvest);
library(reshape2);
library(stringr);
library(maps);
#source("keywords.R")
all_keywords <- read.csv("data/keywords.csv", stringsAsFactors = FALSE);
key_pattern <- paste0( unique(all_keywords$Keywords), collapse = "| ");
rm(all_keywords);

drawKeywordConnections <- function(scraped_output, all_keywords, centroids) {
  poly_lines <- scraped_output$Keywords %>% 
    left_join(all_keywords, by = "Keywords") %>% 
    group_by(Country) %>% 
    summarize(Freq = sum(Freq)) %>% 
    left_join(data.frame(sp_centroids, Country = row.names(sp_centroids), stringsAsFactors = FALSE), by = "Country") %>% 
    filter(!is.na(lat)) %>% 
    arrange(desc(Freq))

  # Repeat every row that's not the first row, twice.
  idx <- seq(from = 2, to = nrow(poly_lines)) %>% rep(each = 2) 
  connections <- poly_lines[idx, ]
  # Replace every repeated row with the first row
  connections[duplicated(connections, fromLast = TRUE), ] <- poly_lines[1, ]
  # The idea is we want to keep drawing lines starting from the first row (center of the story)
  # to the extremities (coordinates with less frequency than the center)
}

leaf <- leaflet() %>% addPolygons(data = world, fillColor = "white", weight = 0.5)
for (art in test) {
  
}


library(leaflet);
library(maptools);
world <- rworldmap::getMap()

leaflet() %>% 
  addPolygons(data = world, fillColor = "white", weight = 0.5) %>% 
  #addPolylines(data = coordinates(world)[seq(5),])
  addPolylines(data = matrix(c(coordinates(world)[1,], coordinates(world)[2,], coordinates(world)[1,], coordinates(world)[3,],
                               coordinates(world)[1,], coordinates(world)[4,], coordinates(world)[1,], coordinates(world)[5,]), ncol = 2, byrow = TRUE))
