library(dplyr);
library(lubridate);
library(rvest);
library(stringr);

cnn <- "http://www.cnn.com"

# Africa
africa_selector <- "#africa-zone-2 a, #africa-zone-1 a"
africa <- paste(cnn, "africa", sep = "/") %>% 
  read_html() %>%
  html_nodes(africa_selector)
africa_data <- data_frame(
  Title  = html_text(africa, TRUE),
  href   = html_attr(africa, "href"),
  Source = "Africa"
)
rm(africa)

# Americas
americas_selector <- "#americas-zone-2 a, #americas-zone-1 a"
americas <- paste(cnn, "americas", sep = "/") %>% 
  read_html() %>%
  html_nodes(americas_selector)
americas_data <- data_frame(
  Title  = html_text(americas, TRUE),
  href   = html_attr(americas, "href"),
  Source = "Americas"
)
rm(americas)

# Asia
asia_selector <- "#asia-zone-2 a, #asia-zone-1 a"
asia <- paste(cnn, "asia", sep = "/") %>% 
  read_html() %>%
  html_nodes(asia_selector)
asia_data <- data_frame(
  Title  = html_text(asia, TRUE),
  href   = html_attr(asia, "href"),
  Source = "Asia"
)
rm(asia)

# Europe
europe_selector <- "#europe-zone-2 a, #europe-zone-1 a"
europe <- paste(cnn, "europe", sep = "/") %>% 
  read_html() %>%
  html_nodes(europe_selector)
europe_data <- data_frame(
  Title  = html_text(europe, TRUE),
  href   = html_attr(europe, "href"),
  Source = "Europe"
)
rm(europe)

# Middle East
me_selector <- "#middleeast-zone-2 a, #middleeast-zone-1 a"
me <- paste(cnn, "middle-east", sep = "/") %>% 
  read_html() %>%
  html_nodes(me_selector)
me_data <- data_frame(
  Title  = html_text(me, TRUE),
  href   = html_attr(me, "href"),
  Source = "Middle East"
)
rm(me)

# United States
us_selector <- "#us-zone-1 a, #us-zone-2 a, #us-zone-3 a"
us <- paste(cnn, "/us", sep = "/") %>% 
  read_html() %>% 
  html_nodes(us_selector)
us_data <- data_frame(
  Title  = html_text(us, TRUE), 
  href   = html_attr(us, "href"),
  Source = "U.S."
)
rm(us)

master <- rbind(africa_data, americas_data, asia_data, europe_data, me_data, us_data) %>% 
  filter(Title != "", str_detect(href, "/index.html")) %>% 
  mutate(Last.scene.date = floor_date( now("UTC"), unit = "day" ));