setwd("~/R/Module3/edureka-text-clustering-dataset")
tweets <- read.csv("Tweets.csv", header = T)
head(tweets)
library(tm)
tweets$content
a <- VectorSource(tweets$content)
tweets1 <- Corpus(a)
tweets1
inspect(tweets1)
##########
tweets2 <- tm_map(tweets1, content_transformer(tolower))
tweets2[[1]]
##########
tweets3 <- tm_map(tweets2, removeWords, stopwords("english"))
tweets3[[173]]
##########
tweets4 <- tm_map(tweets3, removePunctuation)
tweets4[[173]]
##########
tweets5 <- tm_map(tweets4, removeNumbers)
tweets5[[173]]
##########
tweets6 <- tm_map(tweets5, stripWhitespace)
tweets6[[173]]
##########
dtm <- DocumentTermMatrix(tweets6)

dtm
inspect(dtm[1:5, 1:10])
findFreqTerms(dtm,10)
findFreqTerms(dtm,5)
dtm_tfxidf <- weightTfIdf(dtm)
inspect(dtm_tfxidf[1:5, 100:103])
m <- as.matrix(dtm_tfxidf)
rownames(m) <- 1:nrow(m)
norm_eucl <- function(m) 
        m/apply(m, MARGIN=1, FUN=function(x) sum(x^2)^.5) 
m_norm <- norm_eucl(m)

t1 <- kmeans(m_norm, 10)

t1$cluster
t1$size
commments_out <- cbind(as.character(tweets$content), t1$cluster)
write.csv(commments_out, "Output_cluster.csv")
### For objective 2:
## It is actually a search use case. You can use K-Means clustering for search .
## All you need to do is add a new row in the tweets.csv which contains words 
## "Clydesdale" and "Budweiser".
## Now, run K-Means clustering again. 
## The tweets which go in same cluster as the new row(created as mentioned above), 
## are the ones which actually have reference to words "Clydesdale" and "Budweiser"
head(tweets$content)

