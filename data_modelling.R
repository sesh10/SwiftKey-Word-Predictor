library(ggplot2) 
library(NLP)

library(openNLP)
library(rJava)
library(RWeka) 
library(RWekajars)
library(SnowballC) 
library(RColorBrewer) 
library(qdap)

library(stringi) 
library(tm)

newsdata <- file("en_US.news.txt", open = "rb") 
news <- readLines(newsdata, encoding = "UTF-8", skipNul=TRUE)

blogdata <- file("en_US.blogs.txt", open="rb") 
blogs <- readLines(blogdata, encoding = "UTF-8", skipNul=TRUE)

twitterdata <- file("en_US.twitter.txt", open = "rb") 
twitter <- readLines(twitterdata, encoding = "UTF-8", skipNul=TRUE)

file.info("en_US.blogs.txt")$size / 1024^2 
file.info("en_US.news.txt")$size  / 1024^2 
file.info("en_US.twitter.txt")$size / 1024^2 

set.seed(1234)
SubsetTwitter <- sample(twitter, size = 5000, replace = TRUE)
SubsetBlogs <- sample(blogs, size = 5000, replace = TRUE)
SubsetNews <- sample(news, size = 5000, replace = TRUE)
SubsetTotal <- c(SubsetTwitter, SubsetBlogs, SubsetNews)
length(SubsetTotal)
writeLines(SubsetTotal, "SubsetTotal.txt")

text <- file("SubsetTotal.txt")
corpus <- readLines(text)
corpus <- Corpus(VectorSource(corpus))

corpus <- tm_map(corpus, content_transformer(function(x) iconv(x, to="UTF-8", sub="byte")))
corpus <- tm_map(corpus, content_transformer(tolower)) 
corpus <- tm_map(corpus, content_transformer(removePunctuation), preserve_intra_word_dashes=TRUE)

## Eliminate Profanity
BadWords = readLines('BadWords.txt')
corpus <- tm_map(corpus,removeWords, BadWords)

corpus <- tm_map(corpus, content_transformer(removeNumbers))

## Eliminate URLs
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
corpus <- tm_map(corpus, content_transformer(removeURL))

corpus <- tm_map(corpus, removeWords, stopwords("english")) 

corpus <- tm_map(corpus, stripWhitespace) 

## Final Corpus data frame
FinalCorpusMem <- data.frame(text = get("content", corpus), stringsAsFactors = FALSE)
head(corpus, 50)


# Getting Bigrams
bigram <- NGramTokenizer(FinalCorpusMem, Weka_control(min = 2, max = 2,delimiters = " \\r\\n\\t.,;:\"()?!"))
bigram <- data.frame(table(bigram))
bigram <- bigram[order(bigram$Freq,decreasing = TRUE),]
names(bigram) <- c("words","freq")
head(bigram)

bigram$words <- as.character(bigram$words)
str2 <- strsplit(bigram$words,split=" ")
bigram <- transform(bigram, 
                    one = sapply(str2,"[[",1),   
                    two = sapply(str2,"[[",2))
bigram <- data.frame(word1 = bigram$one,word2 = bigram$two,freq = bigram$freq,stringsAsFactors=FALSE)

names(bigram)[names(bigram) == 'word1'] <- 'w1'
names(bigram)[names(bigram) == 'word2'] <- 'w2'

# Save File
write.csv(bigram[bigram$freq > 1,],"bigram.csv",row.names=F)
bigram <- read.csv("bigram.csv",stringsAsFactors = F)
saveRDS(bigram,"bigram.RData")


# Getting Trigrams
trigram <- NGramTokenizer(FinalCorpusMem, Weka_control(min = 3, max = 3,delimiters = " \\r\\n\\t.,;:\"()?!"))
trigram <- data.frame(table(trigram))
trigram <- trigram[order(trigram$Freq,decreasing = TRUE),]
names(trigram) <- c("words","freq")
head(trigram)

trigram$words <- as.character(trigram$words)
str3 <- strsplit(trigram$words,split=" ")
trigram <- transform(trigram,
                     one = sapply(str3,"[[",1),
                     two = sapply(str3,"[[",2),
                     three = sapply(str3,"[[",3))

trigram <- data.frame(word1 = trigram$one,word2 = trigram$two, 
                      word3 = trigram$three, freq = trigram$freq,stringsAsFactors=FALSE)

names(trigram)[names(trigram) == 'word1'] <- 'w1'
names(trigram)[names(trigram) == 'word2'] <- 'w2'
names(trigram)[names(trigram) == 'word3'] <- 'w3'

# Save File
write.csv(trigram[trigram$freq > 1,],"trigram.csv",row.names=F)
trigram <- read.csv("trigram.csv",stringsAsFactors = F)
saveRDS(trigram,"trigram.RData")


# Getting Quadgrams
quadgram <- NGramTokenizer(FinalCorpusMem, Weka_control(min = 4, max = 4,delimiters = " \\r\\n\\t.,;:\"()?!"))
quadgram <- data.frame(table(quadgram))
quadgram <- quadgram[order(quadgram$Freq,decreasing = TRUE),]

names(quadgram) <- c("words","freq")
head(quadgram)


quadgram$words <- as.character(quadgram$words)

str4 <- strsplit(quadgram$words,split=" ")
quadgram <- transform(quadgram,
                      one = sapply(str4,"[[",1),
                      two = sapply(str4,"[[",2),
                      three = sapply(str4,"[[",3), 
                      four = sapply(str4,"[[",4))

quadgram <- data.frame(word1 = quadgram$one,
                       word2 = quadgram$two, 
                       word3 = quadgram$three, 
                       word4 = quadgram$four, 
                       freq = quadgram$freq, stringsAsFactors=FALSE)

names(quadgram)[names(quadgram) == 'word1'] <- 'w1'
names(quadgram)[names(quadgram) == 'word2'] <- 'w2'
names(quadgram)[names(quadgram) == 'word3'] <- 'w3'
names(quadgram)[names(quadgram) == 'word4'] <- 'w4'

# Save File
write.csv(quadgram[quadgram$freq > 1,],"quadgram.csv",row.names=F)
quadgram <- read.csv("quadgram.csv",stringsAsFactors = F)
saveRDS(quadgram,"quadgram.RData")