rm(list=ls())
getwd()
#downloading and installing the package from CRAN
#install.packages("tm")
#loading tm
library(tm)
library(textcat)
library(rlist)
library(wordcloud)
library(textcat)
library(translateR)
library(NLP)
library(sentimentr)

library(dplyr)

library(tidyr)

library(igraph)

##################################################################################################

full_dataset <-read.csv("preprocessed_reviews_no_stemming_full_data.csv", header = FALSE)
shuffled_dataset <- data.frame(full_dataset[sample(nrow(full_dataset),size = 300000),])

sentence_vector <-  as.vector(shuffled_dataset)

english_reviews_vector <- sentence_vector$full_dataset.sample.nrow.full_dataset...size...3e.05....[textcat(sentence_vector$full_dataset.sample.nrow.full_dataset...size...3e.05....) == "english"]

write.csv2(as.vector(english_reviews_vector), 'shuffled_preprocessed_english_reviews.csv', row.names = FALSE)

#####################################################################################

english_reviews_vector <-read.csv("shuffled_preprocessed_english_reviews.csv", header = TRUE)

english_reviews_vector <- get_sentences(as.vector(english_reviews_vector$x))

english_reviews_vector<-english_reviews_vector[!is.na(english_reviews_vector)]




english_reviews_vector<-get_sentences(as.vector(gsub("[^0-9A-Za-z///' ]","" , english_reviews_vector ,ignore.case = TRUE)))

english_sentiments <- sentiment(english_reviews_vector)

positive_english_sentiments <- english_reviews_vector[english_sentiments$sentiment > 0]

neutral_english_sentiments <- english_reviews_vector[english_sentiments$sentiment == 0]

negative_english_sentiments <- english_reviews_vector[english_sentiments$sentiment < 0]


#Some visualization of the data

positive_rev_string <- 'Positive Reviews\n'+String(length(positive_english_sentiments))+ ' reviews'
neutral_rev_string <- '\n\n\n\nNeutral Reviews\n'+String(length(neutral_english_sentiments))+ ' reviews'
negative_rev_string <- 'Negative Reviews\n'+String(length(negative_english_sentiments))+ ' reviews'


pie(c(length(positive_english_sentiments),length(neutral_english_sentiments), length(negative_english_sentiments))
    ,labels = c(positive_rev_string,neutral_rev_string, negative_rev_string), radius = 1, 
    col = c('blue','green','red'))

english_reviews_string <- '\tReviews in English: '+ String(length(english_reviews_vector))
other_reviews_string <- '\nReviews in Other Languages \n   ' +  String(300000 - length(english_reviews_vector))

pie(c(length(english_reviews_vector),  300000 - length(english_reviews_vector)), labels = c(english_reviews_string,other_reviews_string), col = c('yellow','green'))
################################################################


# 1. Positive Sentiment Analysis

positive_corpus <- Corpus(VectorSource(positive_english_sentiments))
positive_corpus_filtered <- tm_map(positive_corpus, removeWords, c("madrid", "great", "would", "stay",
                                                                   "place","recommend","good", "really","well",
                                                                   "perfect","nice","also","easy","bit",
                                                                   "sol","time","etc","awesome","ana","need","needs","wonderful",
                                                                   "want","sure","its","like","many","del",
                                                                   "thanks","stayed","much","take","anything","small",
                                                                   "made","one","two","especially",
                                                                   "felt","back","left","however","let","thank","airbnb",
                                                                   "located", "excellent","get","even"
                                                                   ,"definitely","amazing","lovely","super"))
positive_dtm <- DocumentTermMatrix(positive_corpus_filtered)

positive_dtm <- removeSparseTerms(positive_dtm,0.99)

inspect(positive_dtm)
positive_cooccurrenceCounts <- t(as.matrix(positive_dtm)) %*% as.matrix(positive_dtm)
positive_cooccurrenceCounts


positive_sums <- colSums(positive_cooccurrenceCounts)



sorted_positive_sums <- sort(positive_sums, decreasing = TRUE)

names(sorted_positive_sums)
wordcloud(names(sorted_positive_sums), sorted_positive_sums, scale = c(3,0.25),max.words=200)



##########################################
# Negative Sentiment Analysis


negative_corpus <- Corpus(VectorSource(negative_english_sentiments))

negative_corpus_filtered <- tm_map(negative_corpus, removeWords, c("could","stay"
                                                                   ,"bit", "one","bit","located","place",
                                                                   "great", "would","also","madrid",
                                                                   "really","airbnb","however",
                                                                   "like","people","two",
                                                                   "much","well","didn't","use","need",
                                                                   "next","back","ever","makes","always"
                                                                   ,"return","seems","theres","gets","suggest",
                                                                   "real","first","didnt","made","couldnt",
                                                                   "get","things","want","although","lot"
                                                                   ,"got","good","left"))
negative_dtm <- DocumentTermMatrix(negative_corpus_filtered)
negative_dtm <- removeSparseTerms(negative_dtm,0.9)
negative_cooccurrenceCounts <- t(as.matrix(negative_dtm)) %*% as.matrix(negative_dtm)


################################################################


negative_sums <- colSums(as.matrix(negative_cooccurrenceCounts))

sorted_negative_sums <- sort(negative_sums, decreasing = TRUE)
sorted_negative_sums

write.csv2(data.frame(as.vector(names(sorted_negative_sums)),as.vector(sorted_negative_sums)), 'negativewordsums.csv', row.names = FALSE)

names(sorted_negative_sums)

wordcloud(names(sorted_negative_sums), sorted_negative_sums, scale = c(3,0.25),max.words=200)


#####################################################################################



neutral_corpus <- Corpus(VectorSource(neutral_english_sentiments))


neutral_dtm <- DocumentTermMatrix(neutral_corpus)
neutral_dtm <- removeSparseTerms(neutral_dtm,0.999)
neutral_cooccurrenceCounts <- t(as.matrix(neutral_dtm)) %*% as.matrix(neutral_dtm)


################################################################


neutral_sums <- colSums(as.matrix(neutral_cooccurrenceCounts))

sorted_neutral_sums <- sort(neutral_sums, decreasing = TRUE)
sorted_neutral_sums

names(sorted_neutral_sums)

wordcloud(names(sorted_neutral_sums), sorted_neutral_sums, scale = c(3,0.25),max.words=200)





data <- read.table(header=T, row.names=1, text=
                       "    the fat cat
 one   1   0.1   0.1
 two   .1   .1   0.1
 three .1   .2   0.1")

total_occurrences <- colSums(data)
data_matrix <- as.matrix(data)
library(igraph)
library(LICORS)
#data_matrix<-normalize(data_matrix)


co_occurrence <- t(data_matrix) %*% data_matrix



