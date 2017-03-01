library(rvest);
library(dplyr);
library(reshape2);

delete_superscript_node <- function(dta) {
  apply(dta, 2, function(a) {
    gsub("\\[[^\\]]*\\]", "", a, perl=TRUE);
  }) %>% as_data_frame()
}

splitMultiples <- function(lt) {
  demonyms <- unlist(strsplit(lt$Demonym, "\\, | or |\\/")) %>% 
    gsub("\"", "", .)
  as_data_frame(lt) %>% select(-Demonym, -variable) %>% 
    cbind(Keywords = demonyms)
}

explodeData <- function(df, id_vars = c()) {
  df %>% 
    melt(id.vars = id_vars, value.name = "Demonym") %>% 
    na.omit() %>% 
    rowwise() %>% 
    do(splitMultiples(.)) %>% 
    ungroup() %>% 
    distinct() %>% 
    filter(Keywords != "")
}
`%nin%` <- Negate(`%in%`)

english <- "http://www.wordfrequency.info/free.asp?s=y" %>% 
  read_html() %>% 
  html_nodes("#table3") %>% 
  html_table() %>% .[[1]] %>% 
  slice(-seq(4)) # Junk rows
common_english <- english$X2[nchar(english$X2) > 0] %>% tolower()

countries <- 
  "https://en.wikipedia.org/wiki/List_of_adjectival_and_demonymic_forms_for_countries_and_nations" %>% 
  read_html() %>% 
  html_node("table.wikitable.sortable") %>% 
  html_table(fill = TRUE) %>% 
  delete_superscript_node() %>% 
  setNames(make.names(names(.), unique = TRUE))

countries$Country.name <- plyr::mapvalues(countries$Country.name,
  from = c("United States", "Serbia", "Republic of Ireland", "Russian Federation", "New York/United States", 
           "Åland Islands", "Bahamas, The", "Burkina Fasoa", "Burma", "China, People's Republic of", 
           "China, Republic of", "Congo, Democratic Republic of the", "Côte d'Ivoire", "Curaçao", 
           "French Southern Territories", "Gambia, The", "Guinea-Bissau", "Hong Kong", 
           "Korea, Democratic People's Republic of", "Macau", "Macedonia, Republic of", 
           "Micronesia, Federated States of", "Palestine", "Saint Barthélemy", 
           "Saint Helena, Ascension and Tristan da Cunha", "São Tomé and Príncipe", 
           "South Georgia and the South Sandwich Islands", "Surinam", "Tanzania", "Vatican City State", 
           "Virgin Islands, British", "Virgin Islands, United States", "Cabo Verde", "Congo, Republic of the", 
           "Northern Ireland", "Wales", "Scotland"),
  to = c("United States of America", "Republic of Serbia", "Ireland", "Russia", "United States of America", "Aland",
         "The Bahamas", "Burkina Faso", "Myanmar", "China", "Taiwan", "Democratic Republic of the Congo", 
         "Ivory Coast", "Curacao","French Southern and Antarctic Lands", "Gambia", "Guinea Bissau", 
         "Hong Kong S.A.R.", "North Korea", "Macau S.A.R", "Macedonia", "Federated States of Micronesia", 
         "Israel", "Saint Barthelemy", "Saint Helena", "Sao Tome and Principe", 
         "South Georgia and South Sandwich Islands", "Suriname", "United Republic of Tanzania", "Vatican", 
         "British Virgin Islands", "United States Virgin Islands", "Cape Verde", "Republic of the Congo", "Ireland", 
         "United Kingdom", "United Kingdom"))
countries <- countries %>% 
  mutate(Location = Country.name) %>% 
  slice(-1) %>%  # Junk first row 
  explodeData("Location") %>% 
  filter(Keywords %nin% common_english)

cities <- "https://en.wikipedia.org/wiki/List_of_adjectivals_and_demonyms_for_cities" %>% 
  read_html() %>% 
  html_node("table.wikitable.sortable") %>% 
  html_table(fill = TRUE) %>% 
  delete_superscript_node() %>% 
  setNames(make.names(names(.))) %>%  # Sometimes the column name is a blank string
  select_if( function(x) { sum(is.na(x)) < length(x)*0.05 } ) %>% 
  group_by(City) %>% 
  summarize_if( function(x) {!all(is.na(x))}, paste, collapse = ", ") %>% 
  ungroup()

more_cities <- maps::world.cities %>% 
  mutate(name = replace(name, name == "Soul", "Seoul")) %>% 
  left_join(cities, c("name" = "City")) %>% 
  filter( (!is.na(Adjective) & !is.na(Demonym..colloquial.)) | 
            (tolower(name) %nin% common_english & nchar(name) >= 4 & pop > 15000)) %>% 
  group_by(country.etc, name) %>% 
  filter(pop == max(pop)) %>% # If cities in the same country share names, drop the smaller city
  ungroup() %>% 
  mutate(capital  = NULL, 
         Demonym  = paste(replace(Adjective, is.na(Adjective), ""), 
                          replace(Demonym..colloquial., is.na(Demonym..colloquial.), ""),
                          name, sep = ", ")) %>% 
  explodeData(c("name", "country.etc", "pop", "long", "lat")) %>% 
  rename(Location = name, Parent = country.etc)

more_cities$Parent <- plyr::mapvalues(more_cities$Parent, 
  from = c("UK", "USA", "Korea South", "Korea North", "Tanzania", "Congo Democratic Republic", 
           "Serbia and Montenegro", "Guinea-Bissau", "Congo", "Saint Vincent and The Grenadines", 
           "Palestine", "Bahamas"),
  to   = c("United Kingdom", "United States of America", "South Korea", "North Korea", "United Republic of Tanzania", 
           "Democratic Republic of the Congo", "Republic of Serbia", "Guinea Bissau", "Republic of the Congo", 
           "Saint Vincent and the Grenadines", "Israel", "The Bahamas"))

country_coords <- sp::coordinates(rworldmap::getMap()) %>% 
  as.data.frame() %>% 
  setNames(c("long", "lat")) %>% 
  add_rownames("Location")

location_dta <- countries %>% 
  mutate(long      = round(long, 2), 
         lat       = round(lat, 2),
         Location  = replace(Location, Location %in% c("England", "Scotland", "Wales", "Northern Ireland", 
                                                      "Great Britain"), "United Kingdom")) %>% 
  left_join(country_coords, "Location") %>% 
  na.omit() %>% 
  bind_rows(more_cities) %>% distinct()

saveRDS(location_dta, "data/location_key.rds")

stopwords <- append(common_english[seq(150)], tm::stopwords()) %>% 
  append(c("across", "afterwards", "almost", "alone", "along", "already", "although", "always", "among", "amongst", 
           "amount", "anyhow", "anyone", "anything", "anyway", "anywhere", "around", "became", "becomes", "becoming", 
           "beforehand", "behind", "beside", "besides", "beyond", "bill", "bottom", "co", "con", "cry", "de", 
           "describe", "detail", "done", "due", "eg", "eight", "either", "eleven", "else", "elsewhere", "empty", 
           "enough", "etc", "ever", "every", "everyone", "everything", "everywhere", "except", "fifteen", "fill",
           "five", "forty", "found", "full", "hence", "hereafter", "hereby", "herein", "hereupon", "however", 
           "hundred", "ie", "inc", "indeed", "interest", "keep", "latter", "latterly", "least", "less", "ltd", "made", 
           "meanwhile", "might", "mine", "morevover", "mostly", "move", "must", "name", "namely", "neither", 
           "nevertheless", "next", "nine", "nobody", "none", "often", "onto", "others", "otherwise", "part", "per", 
           "perhaps", "please", "put", "rather", "re", "seem", "seemed", "seeming", "seems", "serious", "several", 
           "show", "side", "since", "sincere", "six", "sixty", "somehow", "someone", "sometime", "sometimes", 
           "somewhere", "ten", "thereafter", "thereby", "therefore", "therein", "thereupon", "third", "though",
           "throughout", "thru", "thus", "toward", "towards", "twelve", "twenty", "un", "upon", "via", "whatever", 
           "whenever", "whereafter", "whereas", "whereby", "wherein", "whereupon", "wherever", "whether", "whoever", 
           "whole", "whose", "within", "without", "yet", "cnn", "reporting", "report", "said", "told", "says", 
           "according", "going", "including", "called", "news", "used", "later", "came", "years", "took", "place", 
           "got", "went", "asked")) %>% 
  gsub("[[:punct:]]", "", .) %>% unique()

saveRDS(stopwords, "data/stopwords.rds")
