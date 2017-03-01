source("scraper.R")
library(rmongodb)
mongo <- mongo.create(
  host     = Sys.getenv("MONGO_CNN_HOST"), 
  username = Sys.getenv("MONGO_CNN_USER"), 
  password = Sys.getenv("MONGO_CNN_PASSWORD"), 
  db       = Sys.getenv("MONGO_CNN_DB")
)
cnn_collection <- paste(Sys.getenv("MONGO_CNN_DB"), Sys.getenv("MONGO_CNN_COLLECTION"), sep = ".")

buf <- mongo.bson.buffer.create()
mongo.bson.buffer.start.object(buf, "locations")
mongo.bson.buffer.append.bool(buf, "$exists", 0)
mongo.bson.buffer.finish.object(buf)
criteria <- mongo.bson.from.buffer(buf)
  
articles <- mongo.find.all(mongo, cnn_collection)
length(unlist(sapply(articles, "[[", "locations")))

articles  <- parseLocations(articles)
length(unlist(sapply(articles, "[[", "locations")))

for (article in articles) {
  buf <- mongo.bson.buffer.create();
  mongo.bson.buffer.start.object(buf, "$set")
  mongo.bson.buffer.append(buf, "locations", article$locations)
  mongo.bson.buffer.finish.object(buf)
  updt <- mongo.bson.from.buffer(buf)
  
  a <- mongo.update(mongo, cnn_collection, list("url" = article$url), updt )
  if (a) {
    print(paste("Finished:", article$url))
  } else {print(paste(article$url, "failed"));break}
}

buf <- mongo.bson.buffer.create()
mongo.bson.buffer.start.object(buf, "url")
mongo.bson.buffer.append(buf, "$in", bad_urls)
mongo.bson.buffer.finish.object(buf)
criteria <- mongo.bson.from.buffer(buf)

new_articles <- mongo.find.all(mongo, cnn_collection, criteria)








mongo.update(mongo, "cnn.articles", list(url = articles[[2]]$url), '{"$unset": {"locations": ""}}')


while(TRUE) {
  message("Getting locations of 1000 articles")
  articles <- mongo.find.all(mongo, "cnn.articles", '{"lat": {"$exists": false}}', limit = 1000)
  if(length(articles) < 1) {break}
  articles2 <- parseLocations(articles)
  
  for (article in articles2) {
    updt <- list("$set" = list("lat" = article$lat, "lon" = article$lon))
    invisible( mongo.update(mongo, "cnn.articles", list(url = article$url), updt) )
  }
}

