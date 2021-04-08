

rm(list=ls(all=TRUE))
library(dplyr)
library(tm)
library(RWeka)
library(NLP)
library(stringi)
library(stringr)
library(tidytext)

# Read and prepare data ####

# Read in data
blogs  <- readLines("train_data/en_US/en_US.blogs.txt", encoding = "UTF-8", skipNul = TRUE, warn = FALSE)
news <- readLines("train_data/en_US/en_US.news.txt", encoding = "UTF-8", skipNul = TRUE, warn = FALSE)
twitter <- readLines("train_data/en_US/en_US.twitter.txt", encoding = "UTF-8", skipNul = TRUE, warn = FALSE)

set.seed(2021)
sample_perc <- 0.01

#Combine samples
sample_blog <- sample(blogs, ceiling(length(blogs) * sample_perc), replace = FALSE)
sample_news <- sample(news, ceiling(length(news) * sample_perc), replace = FALSE)
sample_twitter <- sample(twitter, ceiling(length(twitter) * sample_perc), replace = FALSE)

sample_combined <- c(sample_blog, sample_news, sample_twitter)
rm(blogs,news,twitter,sample_blog,sample_news,sample_twitter)

sample_combined <- iconv(sample_combined, "latin1", "ASCII", sub = "")

sample_combined <-VCorpus(VectorSource(sample_combined))

#Remove white spaces
to_space <- content_transformer(function(x, pattern) gsub(pattern, " ", x))

sample_combined <- tm_map(sample_combined, stripWhitespace)

#Remove URLs and email addresses
sample_combined <- tm_map(sample_combined, to_space, "^https?://.*[\r\n]*")

sample_combined <- tm_map(sample_combined, to_space, "\\b[A-Z a-z 0-9._ - ]*[@](.*?)[.]{1,3} \\b")

# Convert all words to lowercase
sample_combined <- tm_map(sample_combined, content_transformer(tolower))
sample_combined <- tm_map(sample_combined, removePunctuation)

#Remove numbers
sample_combined <- tm_map(sample_combined, removeNumbers)

bigram_tokeniser <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
bigram_matrix <- TermDocumentMatrix(sample_combined, control = list(tokenize = bigram_tokeniser))
bigram <- tidy(bigram_matrix)
bigram <- as.data.frame(bigram)
bigram <- bigram %>% group_by(term) %>% summarise(sum_count=sum(count))
bigram <- bigram %>% arrange(desc(sum_count))
bigram$term_length <- sapply(strsplit(bigram$term, " "), length)
bigram <- bigram[bigram$term_length==2,]
bigram$term1 <- sapply(strsplit(bigram$term, " "), head,1)
bigram$term2 <- sapply(strsplit(bigram$term, " "), head,2)[2,]
save(bigram, file = "bigram.Rdata")
rm(bigram_matrix)
rm(bigram)

#Trigram
trigram_tokeniser <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
trigram_matrix <- TermDocumentMatrix(sample_combined, control = list(tokenize = trigram_tokeniser))
trigram <- tidy(trigram_matrix)
trigram <- as.data.frame(trigram)
trigram <- trigram %>% group_by(term) %>% summarise(sum_count=sum(count))
trigram <- trigram %>% arrange(desc(sum_count))
trigram$term_length <- sapply(strsplit(trigram$term, " "), length)
trigram <- trigram[trigram$term_length==3,]
trigram$term1 <- sapply(strsplit(trigram$term, " "), head,1)
trigram$term2 <- sapply(strsplit(trigram$term, " "), head,2)[2,]
trigram$term3 <- sapply(strsplit(trigram$term, " "), head,3)[3,]
save(trigram, file = "trigram.RData")
rm(trigram_matrix)
rm(trigram)

#Quadgram
quadgram_tokeniser <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
quadgram_matrix <- TermDocumentMatrix(sample_combined, control = list(tokenize = quadgram_tokeniser))
quadgram <- tidy(quadgram_matrix)
quadgram <- as.data.frame(quadgram)
quadgram <- quadgram %>% group_by(term) %>% summarise(sum_count=sum(count))
quadgram <- quadgram %>% arrange(desc(sum_count))
quadgram$term_length <- sapply(strsplit(quadgram$term, " "), length)
quadgram <- quadgram[quadgram$term_length==4,]
quadgram$term1 <- sapply(strsplit(quadgram$term, " "), head,1)
quadgram$term2 <- sapply(strsplit(quadgram$term, " "), head,2)[2,]
quadgram$term3 <- sapply(strsplit(quadgram$term, " "), head,3)[3,]
quadgram$term4 <- sapply(strsplit(quadgram$term, " "), head,4)[4,]
quadgram <- as.data.frame(quadgram)
save(quadgram, file="quadgram.Rdata")
rm(quadgram_matrix)
rm(quadgram)


rm(sample_combined)















saveRDS(dBlog,"dBlog.RData")
saveRDS(dNews,"dNews.RData")
saveRDS(dTwitter,"dTwitter.RData")
rm(dBlog,dNews,dTwitter)

dBlog <- readRDS("dBlog.RData"); dNews <- readRDS("dNews.RData"); dTwitter <- readRDS("dTwitter.RData")
combinedRaw = c(dBlog, dNews, dTwitter)

combinedRaw<-gsub("\\b[A-Z a-z 0-9._ - ]*[@](.*?)[.]{1,3} \\b", "", combinedRaw)
saveRDS(combinedRaw,"combinedRaw.RData")

set.seed(2021)
n = 0.1
combined = sample(combinedRaw, length(combinedRaw) * n)
rm(combinedRaw)
# Split into train and validation sets
split = sample.split(combined, 0.8)
train = subset(combined, split == T)
valid = subset(combined, split == F)

rm(combined)







blogsURL <- file("train_data/en_US/en_US.blogs.txt", open="rb") # open for reading in binary mode
blogs <- readLines(blogsURL, encoding = "UTF-8", skipNul=TRUE)

newsURL <- file("train_data/en_US/en_US.news.txt", open = "rb") # open for reading in binary mode
news <- readLines(newsURL, encoding = "UTF-8", skipNul=TRUE)

twitterURL <- file("train_data/en_US/en_US.twitter.txt", open = "rb") # open for reading in binary mode
twitter <- readLines(twitterURL, encoding = "UTF-8", skipNul=TRUE)

set.seed(2021)

#Second we establish our sample size

samplesz <- 0.2

# sample all three data 
sampleTwitter <- sample(twitter, length(twitter) * samplesz, replace = TRUE)
sampleBlogs <- sample(blogs, length(blogs) * samplesz, replace = TRUE)
sampleNews <- sample(news, length(news) * samplesz, replace = TRUE)

sampleData <- c(sampleTwitter, sampleBlogs, sampleNews)
rm(blogs, news, twitter, sampleBlogs, sampleNews, sampleTwitter)
rm(blogsURL,newsURL,twitterURL)

textCorpus <- Corpus(VectorSource(sampleData))
textCorpus <- tm_map(textCorpus, content_transformer(function(x) iconv(x, to="UTF-8", sub="byte")))
textCorpus <- tm_map(textCorpus, content_transformer(tolower)) # converting to lowercase
textCorpus <- tm_map(textCorpus, content_transformer(removePunctuation), preserve_intra_word_dashes=TRUE) # removing ponctuation

removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
textCorpus <- tm_map(textCorpus, content_transformer(removeURL))

textCorpus <- tm_map(textCorpus, removeWords, stopwords("english")) # removing stop words in English (a, as, at, so, etc.)

textCorpus <- tm_map(textCorpus, stripWhitespace) ## Stripping unnecessary whitespaces from document

textCorpus <- tm_map(textCorpus, PlainTextDocument) 

rm(textCorpus, sampleData)

saveRDS(textCorpus, file = "finalCorpus.RDS")

finalCorpusMem <- readRDS("finalCorpus.RDS")
## data framing finalcorpus
finalCorpus <-data.frame(text=unlist(sapply(finalCorpusMem,`[`, "content")),stringsAsFactors = FALSE)

#unigram
gramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
NgramMatrix <- TermDocumentMatrix(finalCorpus, control = list(tokenize = gramTokenizer))

unigram <- NGramTokenizer(finalCorpus, Weka_control(min = 1, max = 1))
unigram <- data.frame(table(unigram))
unigram <- unigram[order(unigram$Freq,decreasing = TRUE),]
names(unigram) <- c("word1", "freq")
unigram$word1 <- as.character(unigram$word1)
