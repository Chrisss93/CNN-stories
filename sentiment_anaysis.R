library(rmongodb)

mongo <- mongo.create(
  host = Sys.getenv("MONGO_CNN_HOST"), 
  username = Sys.getenv("MONGO_CNN_USER"), 
  password = Sys.getenv("MONGO_CNN_PASSWORD"),
  db       = Sys.getenv("MONGO_CNN_DB")
)
cnn_collection <- paste(Sys.getenv("MONGO_CNN_DB"), Sys.getenv("MONGO_CNN_COLLECTION"), sep = ".")

articles <- mongo.find.all(mongo, cnn_collection, fields = list("_id" = 0L, content = 1L)) %>% 
  unlist()

library(text2vec)
STOPWORDS <- readRDS("stopwords.rds")
removePunctuation <- function(x) {
  x <- gsub("(\\w)-(\\w)", "\\1MYUNIQUEDASHSYMBOL\\2", x)
  x <- gsub("[[:punct:]]+", "", x)
  x <- gsub("MYUNIQUEDASHSYMBOL", "-", x, fixed = TRUE)
  x <- gsub(" +", " ", tolower(x))
  return(x)
}

vocab <- itoken(articles, removePunctuation) %>% 
  create_vocabulary(stopwords = STOPWORDS) %>% 
  prune_vocabulary(doc_proportion_min = 0.05)

dtm <- itoken(articles, removePunctuation) %>% 
  create_dtm(vocab_vectorizer(vocab), "lda_c")

lda_model <- LatentDirichletAllocation$new(n_topics = 5, vocabulary = vocab,
                                           doc_topic_prior = 0.1,
                                           topic_word_prior = 0.1)

doc_topic_distr <- lda_model$fit_transform(dtm, n_iter = 20, check_convergence_every_n = 5)

lda_model$plot()
