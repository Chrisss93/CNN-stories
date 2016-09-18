# library(rmongodb);

# I do not want duplicate articles. But how to acheive this? If I created a unique index on the url field, it would
# stop me from inserting duplicated records (which I do in batch). I could then loop over every record I wish to 
# insert and upsert the records, but that means sending over each record individually. I do not want to stres the # server especially since I may be sending 100s of records at a time. Best to do it in batch. I could, retrieve every 
# unique url from the collection and filter out my insert batch beforehand but I'd have to load the entire list of urls
# into memory. 
# So instead, I'll make the index non-unique, allow records with duplicate url indexes to be inserted, then identify 
# the duplicated records and remove them.

# Before aggregation, only look at the urls that were just scraped. Those are the ones we worry are duplicated
buf <- mongo.bson.buffer.create();
invisible( mongo.bson.buffer.start.object(buf, "$match") )
invisible( mongo.bson.buffer.start.object(buf, "url") )
invisible( mongo.bson.buffer.append(buf, "$in", unname( sapply(articles, "[[", "url")) ) )
invisible( mongo.bson.buffer.finish.object(buf) )
invisible( mongo.bson.buffer.finish.object(buf) )
pipe0 <- mongo.bson.from.buffer(buf)
# Group by url field and measure how many times the url is seen and collect all their object ids
pipe1 <- mongo.bson.from.JSON('{"$group": {"_id": "$url", "dups": {"$addToSet": "$_id"}, "total": {"$sum": 1}}}')
# Keep only the urls which appear more than once.
pipe2 <- mongo.bson.from.JSON('{"$match": {"total": {"$gt": 1}}}')

# Run the mongo command: 
#db.MONGO_CNN_DB.MONGO_CNN_COLLECTION.aggregate([
# {$match: url: {$in: [urls]}}, 
# {$group: _id: "$url", dups: {$addToSet: "$_id"}, total: {"$sum": 1}}, 
# {$match: total: {$gt: 1}}
# ])
duplicates <- mongo.aggregation(mongo, cnn_collection, list(pipe0, pipe1, pipe2)) %>% mongo.bson.to.list()
# Extract all the object IDs from records with duplicate urls, except for the original
duplicate_oids <- lapply(duplicates$result, function(record) { (record$dups[-1]) }) %>% 
  do.call(c, args = .) # using unlist would have coerced the oid class into a character which loses crucial information

buf2 <- mongo.bson.buffer.create()
invisible( mongo.bson.buffer.start.object(buf2, "_id") )
invisible( mongo.bson.buffer.append(buf2, "$in", duplicate_oids) )
invisible( mongo.bson.buffer.finish.object(buf2) )
pipe3 <- mongo.bson.from.buffer(buf2)
# Remove the duplicate object IDs from the collection
invisible( mongo.remove(local, cnn_collection, pipe3) )
