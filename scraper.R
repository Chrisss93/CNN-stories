library(dplyr, warn.conflicts = FALSE);
library(rvest);
library(text2vec);

cnn <- "http://www.cnn.com"
sources <- list( c(name = "Africa",      selector = "#africa-zone-2 a, #africa-zone-1 a"), 
                 c(name = "Americas",    selector = "#americas-zone-2 a, #americas-zone-1 a"),
                 c(name = "Asia",        selector = "#asia-zone-2 a, #asia-zone-1 a"), 
                 c(name = "Europe",      selector = "#europe-zone-2 a, #europe-zone-1 a"), 
                 c(name = "Middle-East", selector = "#middleeast-zone-2 a, #middleeast-zone-1 a"),
                 c(name = "US",          selector = "#us-zone-1 a, #us-zone-2 a, #us-zone-3 a"))

scrapeStories <- function(source) {
  stories <- paste(cnn, source["name"], sep = "/") %>% 
    read_html() %>% 
    html_nodes(source["selector"])
  
  stories_d <- data_frame(Title  = html_text(stories, TRUE), 
                          href   = html_attr(stories, "href"), 
                          Source = source["name"]) %>% 
    filter(nchar(Title) > 0, grepl("*/index.html$", href), !xml2:::is_url(href) )
  stopifnot(nrow(stories_d) >= 5)
  return(stories_d)
}

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
    source   = first(dta$Source))
  lt$alternate_titles <- setdiff( unique(dta$Title), lt$title)
  rm(doc)
  return(lt)
}

parseLocations <- function(articles, stopwords = readRDS("stopwords.rds"), keywords = read.csv("data/keywords.csv")) {
  if ( is.null(names(articles)) ) {
    names(articles) <- sapply(articles, "[[", "url")
  }
  keywords <- gsub(" ", "_", removePunctuation(keywords$Keywords))
  
  it <- itoken( sapply(articles, "[[", "content"), removePunctuation, word_tokenizer)
  vocab <- create_vocabulary(it, c(1L, 3L), stopwords, sep_ngram = "_")
  dtm <- create_dtm(it, vocab_vectorizer(vocab))
  idx <- which( dimnames(dtm)[[2]] %in% keywords )
  dtm <- as.matrix(dtm[, idx])
  
  locations <- apply(dtm, 1, function(x) { list("locations" = rep(gsub("_", " ", names(x)), x)) })
  articles <- Map(c, articles, locations)
  
  return(articles)
}

removePunctuation <- function(x) {
  x <- gsub("(\\w)-(\\w)", "\\1MYUNIQUEDASHSYMBOL\\2", x)
  x <- gsub("[[:punct:]]+", "", x)
  x <- gsub("MYUNIQUEDASHSYMBOL", "-", x, fixed = TRUE)
  x <- gsub(" +", " ", x)
  return(x)
}