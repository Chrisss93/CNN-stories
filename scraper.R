library(dplyr);
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
us <- paste(cnn, "us", sep = "/") %>% 
  read_html() %>% 
  html_nodes(us_selector)
us_data <- data_frame(
  Title  = html_text(us, TRUE), 
  href   = html_attr(us, "href"),
  Source = "U.S."
)
rm(us)

# # General politics
# # You idiot, you should have figured out how to do this long ago, now you missed all the trump stuff.
# politics_selector <- "#politics-zone-1 a , #politics-zone-2 a"
# So for some reason, most of cnn.com/politics is generated dynamically by delayed javascript rendering so I can't
# exactly use rvest to look into the html structure. Instead I need a headless web-browser to load the url and 
# "trigger" the js. 

# I'd rather do everything in R with Selenium, but I'm having some difficulties there, so I'm just going to write a  
# script in PhantomJS and run it through R with system()
# politics <- paste(cnn, "politics", sep = "/") %>% 
#   read_html() %>% 
#   html_nodes(politics_selector)
# politics_data <- data_frame(
#   Title  = html_text(politics, TRUE),
#   href   = html_attr(politics, "href"),
#   Source = "Politics"
# )

master <- bind_rows(africa_data, americas_data, asia_data, europe_data, me_data, us_data) %>% 
  filter(Title != "", grepl("*/index.html", href), !xml2:::is_url(href) )



scrapeArticle <- function(dta) {
  url <- paste0(cnn, unique(dta$href))
  doc <- read_html(url)
  lt  <- list(
    content  = html_nodes(doc, ".el__leafmedia--sourced-paragraph .zn-body__paragraph , div.zn-body__paragraph") %>% 
                  html_text(TRUE) %>% 
                  paste(collapse = " ") %>% 
                  sub("^\\(CNN\\)", "", .),
    author   = html_nodes(doc, ".metadata__byline__author a") %>% 
                  html_text(TRUE),
    title    = html_nodes(doc, ".pg-headline") %>% 
                  html_text(TRUE),
    subtitle = html_nodes(doc, "#body-text h3") %>% 
                  html_text(TRUE) %>% 
                  grep("^JUST WATCHED$|^Story highlights$", ., value = TRUE, invert = TRUE) %>% 
                  unique(),
    summary  = html_nodes(doc, ".el__storyhighlights--normal") %>% 
                  html_text(TRUE) %>% 
                  paste(collapse = " "),
    links    = html_nodes(doc, "#body-text a") %>% 
                  html_attr("href") %>% 
                  .[!xml2:::is_url(.) & grepl("index\\.html$", .)] %>% 
                  unique(),
    date     = html_nodes(doc, ".update-time") %>% 
                  html_text(TRUE) %>% 
                  sub("^.*?, ", "", .) %>% 
                  strptime("%a %B %d, %Y", tz = "America/New_York"),
    url      = url,
    source   = unique(dta$Source))
  lt$alternate_titles <- setdiff( unique(dta$Title), lt$title)
  rm(doc)
  return(lt)
}