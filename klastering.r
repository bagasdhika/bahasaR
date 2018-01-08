install.packages("twitteR")
install.packages("ROAuth")
install.packages("tm")
install.packages("ggplot2")
install.packages("wordcloud")
install.packages("plyr")
install.packages("RTextTools")
install.packages("e1071")

library(e1071)
library(twitteR)
library(ROAuth)
library(tm)
library(ggplot2)
library(wordcloud)
library(plyr)
library(RTextTools)
library(e1071)

setup_twitter_oauth("JsZqhclFgxd0U1VG1jmeKzLfB","40l9QX8fZOscjgG1UvFmhFOoziedKnw8HJWYO7c5sO7T7fXBcn","861413684467220480-gGYKh6cU87FrKem09cYUvP08iBUvbTv","agaOa07UN9S5xhZUZ7B41tfGdO2qtXl8LHhSTTGpH8ZSn")

tweets <- userTimeline("Banjir", n = 10)
n.tweet <- length(tweets)
# convert tweets to a data frame
tweets.df <- twListToDF(tweets)

myCorpus <- Corpus(VectorSource(tweets.df$text))
# convert to lower case
myCorpus <- tm_map(myCorpus, content_transformer(tolower))
# remove URLs
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeURL))
# remove anything other than English letters or space
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))
# remove stopwords
myStopwords <- c(setdiff(stopwords('english'), c("r", "big")),"use", "see", "used", "via", "amp")
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
# remove extra whitespace
myCorpus <- tm_map(myCorpus, stripWhitespace)
# keep a copy for stem completion later
myCorpusCopy <- myCorpus
myCorpus
term.freq <- rowSums(as.matrix(tdm))
tdm <- TermDocumentMatrix(myCorpus)
tdmat <- as.matrix(removeSparseTerms(tdm, sparse=0.3))
# compute distances
distMatrix <- dist(scale(tdm))
fit <- hclust(distMatrix, method="ward.D2")
plot(fit)
fit <- hclust(distMatrix, method="single")
plot(fit)
