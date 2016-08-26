library(dplyr);
library(rvest);
library(reshape2);
library(stringr);
library(maps);
# Demonyms
# As far as I know, there is no current functionality for rvest/xml2 to remove or alter nodes yet. So we will scrape the url with XML...which is a huge pain in the ass seeing as they are prohibitively noncooperative with each other

# Or...on the otherhand fuck XML and just do dirty regex to get rid of superscript nodes
delete_super <- function(dta) {
  apply(dta, 2, function(a) {
    gsub("\\[[^\\]]*\\]", "", a, perl=TRUE);
  }) %>% as.data.frame(stringsAsFactors = FALSE)
}

# Next find away to explode the data so that each element with "," and "or" gets its own row.
splitMultiples <- function(lt) {
  demonyms <- unlist(str_split(lt$Demonym, "\\, | or |\\/")) %>% 
    str_replace_all("\"", "")
  
  data_frame(Country = lt$Country, Keywords = demonyms)
}

`%nin%` <- function(x, table) {match(x, table, nomatch = 0) == 0} # Copied from ol' Harrell's Hmisc

explodeData <- function(df) {
  df %>% 
    melt(id.vars = "Country", value.name = "Demonym") %>% 
    filter(!is.na(Country), !is.na(Demonym)) %>% 
    rowwise() %>% 
    do(splitMultiples(.)) %>% 
    ungroup() %>% 
    distinct() %>% 
    filter(Keywords != "")
}

countries <- 
  "https://en.wikipedia.org/wiki/List_of_adjectival_and_demonymic_forms_for_countries_and_nations" %>% 
  read_html() %>% 
  html_node("table.wikitable.sortable") %>% 
  html_table(fill = TRUE) %>% 
  delete_super() %>% 
  rename(Country = `Country name`) %>% 
  setNames(make.names(names(.))) %>% 
  slice(-1) %>%  # Junk first row 
  explodeData()

# Clean
# Handling various subdistricts of the United Kingdom
uk_countries <- c("England", "Scotland", "Wales", "Northern Ireland", "Great Britain")
countries$Country[countries$Country %in% uk_countries] <- "United Kingdom"
# Messiness with PRC and ROC
countries <- countries %>% 
  filter(Keywords %nin% c("none", "see Other words for British", "below")) %>% 
  mutate(Level = 1,
         Parent = NA)

cities <- "https://en.wikipedia.org/wiki/List_of_adjectivals_and_demonyms_for_cities" %>% 
  read_html() %>% 
  html_node("table.wikitable.sortable") %>% 
  html_table(fill = TRUE) %>% 
  delete_super() %>% 
  setNames(make.names(names(.))) %>%  # Sometimes the column name is a blank string
  explodeData() %>% 
  mutate(Level = 2,
         Parent = Country)

states <- "https://en.wikipedia.org/wiki/List_of_demonyms_for_U.S._states" %>% 
  read_html() %>% 
  html_node("table.wikitable") %>% 
  html_table(fill = TRUE) %>%
  delete_super() %>% 
  select(-`Official, unofficial, or informal alternates`)

states_as_keywords <- states %>% 
  mutate(Country = "United States") %>% 
  explodeData() %>% 
  mutate(Level = 1,
         Parent = NA)

states_as_countries <- states %>% 
  mutate(Country = `State or District`) %>% 
  explodeData() %>% 
  mutate(Level = 2,
         Parent = "United States")

# Some of these cities have names that are too close to common words. Filter them out.
english <- "http://www.wordfrequency.info/free.asp?s=y" %>% 
  read_html() %>% 
  html_nodes("#table3") %>% 
  html_table() %>% .[[1]] %>% 
  slice(-seq(4)) # Junk rows
english <- english$X2[nchar(english$X2) > 0] %>% tolower()

more_cities <- world.cities %>% 
  # Ignore cities that are common english words, are too short or are too insignificant (low population)
  filter(tolower(name) %nin% english, nchar(name) > 4, pop > 50000) %>% 
  mutate(Keywords = name, Level = 3, Parent = country.etc) %>% 
  group_by(name, country.etc) %>% 
  filter(pop == max(pop)) %>% # If cities in the same country share names, drop the smaller city
  ungroup()
# If same name countries are in different countries, distinguish the names by adding the country
# more_cities$name[duplicated(more_cities$name)] <- paste0( more_cities$name[duplicated(more_cities$name)],
#                                                           " (",
#                                                           more_cities$country.etc[duplicated(more_cities$name)],
#                                                           ")")
more_cities$Parent <- plyr::mapvalues(more_cities$Parent, 
                                      from = c("Korea South", "Korea North", "Netherlands Antilles", 
                                               "Congo Democratic Republic", "USA"), 
                                      to   = c("South Korea", "North Korea", "Antilles", 
                                               "Democratic Republic of Congo", "US"))

  
all_keywords <- rbind(countries, cities, states_as_countries, states_as_keywords, 
                select(more_cities, Country = name, Keywords, Level, Parent)) %>% 
  distinct() %>% 
  filter(Keywords != "none")

# Some changes to make sure the Country names from wikipedia match the Country names of the polygons
all_keywords$Country <- plyr::mapvalues(all_keywords$Country,
    from = c("United States", "Serbia", "Republic of Ireland", "Russian Federation", "New York/United States", 
             "Åland Islands", "Bahamas, The", "Burkina Fasoa", "Burma", "China, People's Republic of", 
             "China, Republic of", "Congo, Democratic Republic of the", "Côte d'Ivoire", "Curaçao", 
             "French Southern Territories", "Gambia, The", "Guinea-Bissau", "Hong Kong", 
             "Korea, Democratic People's Republic of", "Macau", "Macedonia, Republic of", 
             "Micronesia, Federated States of", "Palestine", "Saint Barthélemy", 
             "Saint Helena, Ascension and Tristan da Cunha", "São Tomé and Príncipe", 
             "South Georgia and the South Sandwich Islands", "Surinam", "Tanzania", "Vatican City State", 
             "Virgin Islands, British", "Virgin Islands, United States"),
    to = c("United States of America", "Republic of Serbia", "Ireland", "Russia", "United States of America", "Aland",
           "The Bahamas", "Burkina Faso", "Myanmar", "China", "Taiwan", "Democratic Republic of the Congo", 
           "Ivory Coast", "Curacao","French Southern and Antarctic Lands", "Gambia", "Guinea Bissau", 
           "Hong Kong S.A.R.", "North Korea", "Macau S.A.R", "Macedonia", "Federated States of Micronesia", 
           "Israel", "Saint Barthelemy", "Saint Helena", "Sao Tome and Principe", 
           "South Georgia and South Sandwich Islands", "Suriname", "United Republic of Tanzania", "Vatican", 
           "British Virgin Islands", "United States Virgin Islands")
)
all_keywords <- all_keywords %>% 
  bind_rows(data_frame(Country = all_keywords$Country, Keywords = all_keywords$Country, Level= all_keywords$Level)) %>% 
  distinct() # Make sure the actual country names are also keywords we use to identify these countries

# Now make sure some city names aren't the same as other country names
test <- all_keywords %>% group_by(Country) %>% summarize(n = length(unique(Level))) %>% filter(n > 1)
# The only data where this actually matters is more_cities, so correct that in both places


bad_names <- more_cities$name %in% test$Country
more_cities$name[bad_names] <- paste0(more_cities$name[bad_names], " (", 
                                      more_cities$country.etc[bad_names], ")")


write.csv(all_keywords, "data/keywords.csv", row.names = FALSE)
write.csv(select(more_cities, name, lat, long), "data/filtered_world_cities.csv", row.names = FALSE)
View(thing)
