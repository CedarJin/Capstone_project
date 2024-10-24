pacman::p_load(here,
               tidyverse,
               quanteda)

tokens_flat <- readRDS(here("final","en_US","sample_tokens_clean.rds"))

# bag of Words (BoW) and TF-IDF

# Combine all tokens into a single vector for each data type
all_text <- c(tokens_flat$blogs, tokens_flat$news, tokens_flat$twitter)

# Create a corpus from the tokens
tokens_corpus <- corpus(all_text)

# Create a document-feature matrix (BoW representation)
tokens_dfm <- dfm(tokens(tokens_corpus))

# Display a sample of the Document-Feature Matrix
print(tokens_dfm)

# Convert the DFM into a TF-IDF representation
tokens_tfidf <- dfm_tfidf(tokens_dfm)

# Display a sample of the TF-IDF matrix
print(tokens_tfidf)

# 
# Load necessary packages
library(tidytext)
library(dplyr)

# Convert token list into a data frame
tokens_df <- data.frame(word = all_text, stringsAsFactors = FALSE)

# Create bigrams and trigrams
tokens_df_bigram <- tokens_df %>%
    mutate(next_word = lead(word)) %>%
    filter(!is.na(next_word)) %>%
    unite(bigram, word, next_word, sep = " ")

tokens_df_trigram <- tokens_df %>%
    mutate(next_word1 = lead(word, 1), next_word2 = lead(word, 2)) %>%
    filter(!is.na(next_word1), !is.na(next_word2)) %>%
    unite(trigram, word, next_word1, next_word2, sep = " ")

# Calculate trigram frequency
bigram_counts <- tokens_df_bigram %>%
    group_by(bigram) %>%
    summarize(count = n()) %>%
    arrange(desc(count))

trigram_counts <- tokens_df_trigram %>%
    group_by(trigram) %>%
    summarize(count = n()) %>%
    arrange(desc(count))


# Predict the next word based on the previous two words
predict_next_word_ngram <- function(text) {
   # text = "love u love"
    input_text = unlist(str_split(string = text,pattern = " "))
    input_text = input_text[length(input_text)]
    filtered_bigrams <- bigram_counts %>%
        filter(grepl(paste0("^", input_text, " "), bigram)) %>% 
        arrange(desc(count))
    
    if (nrow(filtered_trigrams) == 0) {
        return("No prediction available")
    } else {
        bigrams = str_split_i(filtered_bigrams$bigram, pattern = " ", i = 2)
        predict_text = bigrams[1:20]
    }
    
    return(predict_text)
}

# Example usage
input_text <- "You're the reason why I smile everyday. Can you follow me please? It would mean"
predicted_word <- predict_next_word_ngram(input_text)
print(predicted_word)



# use caret to build the model
library(caret) 


df_bigram <- tokens_df %>%
    mutate(next_word = lead(word)) %>%
    filter(!is.na(next_word)) 

set.seed(2)
in_train <- createDataPartition(df_bigram$next_word, p = 0.8, list = FALSE)
train_set <- df_bigram[in_train,]
test_set <- df_bigram[-in_train,]

train_set_reduced <- train_set %>% sample_n(5000)  # Take a sample of 5000 rows
test_set_reduced <- test_set %>% sample_n(1000) 
test_set_reduced$next_word <- as.factor(test_set_reduced$next_word)

library(e1071)

# Train Naive Bayes model using e1071, which is less memory-intensive
nb_model <- naiveBayes(next_word ~ ., data = train_set_reduced)
# Predict on the test set
predictions <- predict(nb_model, newdata = test_set_reduced)
predictions <- as.factor(predictions)

all_levels <- union(levels(predictions), levels(test_set_reduced$next_word))
predictions <- factor(predictions, levels = all_levels)
test_set_reduced$next_word <- factor(test_set_reduced$next_word, levels = all_levels)

confusion_matrix <- confusionMatrix(predictions, test_set_reduced$next_word)
confusion_matrix$positive

