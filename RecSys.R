# DMKM : Sai Krishna Kalyan
# My Approach - Parts of Speech Tagging
# We know that emotions for a given corpus can be captured by 
# Verb, Noun, Aadjective and Adverb
# Comparing the extactracted POS words with an emotion based lexicon
# Finally by using word to vec to enrich emotional lexicon 


#install.packages('gutenbergr')
#install.packages('openNLP')
#install.packages('lsa')
library(gutenbergr)
library(NLP) 
library(openNLP)
library(stringr)
library(stringr)
library(stringdist)
library(lsa)
rm(list = ls())

#<Enter base path there>
setwd("/Users/krishna/Experiment/Text Mining Assignment")
source("emotion.R")
source("pos.R")

# User Profile
profile.hulk = read.csv("profile.csv",header =T)

# verb, noun, adjective and adverb 
book_ids = c(1,2,3,4,5,6)
books = gutenberg_download(book_ids, meta_fields = "title")

# Create a dataframe that contains emotions for title,book
emotionsdf <- data.frame()
for(id in book_ids){
  # Get data for a particular ID by subsetting
  book.sub <- subset(books, books$gutenberg_id == id)
  # Pull the title for the given ID  
  title =  book.sub$title[1]
  # Extract all emotion related words from the book
  emo_book  = emotions(book.sub)
  # Compute emotional score for a book
  emotionmatrix = emotionbook(emo_book,id,title)
  # Bind the emotional score to a new dataframe with id,title
  emotionsdf <- rbind(emotionsdf,emotionmatrix)
}

# The emotional scores are string fractor convert them to numeric scores
emotionsdf[3:10] =  apply(emotionsdf[3:10],2, function(x){as.numeric(as.character(x))})
emotionsdf[3:10]
# angerp anticipationp disgustp fearp joyp sadp surprisep trustp     score
# 1   0.14          0.13     0.07  0.16 0.10 0.12      0.05   0.22 0.6809741
# 2   0.19          0.13     0.09  0.19 0.09 0.09      0.01   0.19 0.7167757
# 3   0.10          0.16     0.04  0.15 0.14 0.07      0.07   0.25 0.7048215
# 4   0.06          0.17     0.03  0.17 0.17 0.06      0.09   0.26 0.6578104
# 5   0.08          0.15     0.04  0.15 0.15 0.11      0.03   0.30 0.6429006
# 6   0.18          0.12     0.10  0.16 0.12 0.11      0.05   0.16 0.7395309

# Cosine similarity function that will be used to recommend books
cos.sim <- function(A,B) 
{
  return( sum(A*B)/sqrt(sum(A^2)*sum(B^2)) )
}

# Calculate cosine similarity for a user profile and emotion dataframe
emotionsdf$score= apply(emotionsdf[3:10],1, function(x){cos.sim(profile.hulk[3:10],x)})
emotionsdf
# Recommend the book with maximum score
emotionsdf[which.max(emotionsdf$score),]$title

source('word2vec.R')
