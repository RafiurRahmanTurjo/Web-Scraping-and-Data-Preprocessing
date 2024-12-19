library("dplyr")
library("rvest")
library("tidyverse")
library("tidytext")
library("tm")
library("textstem")
library("SnowballC")
library("textclean")
library("emoji")
library("hunspell")

url <- "https://en.prothomalo.com/sports/football/m3n2mygcra"
webpage <- read_html(url)


text_data <- webpage %>%
  html_nodes(".story-content-wrapper") %>% 
  html_text()

print(text_data)

text_data <- unique(text_data)
text_data <- tolower(text_data)
text_data <- gsub("[[:punct:]]", "",text_data)
text_data <- emoji::emoji_replace_name(text_data)
text_data <- trimws(text_data)
text_data <- replace_contraction(text_data)
text_data <- gsub("[0-9]+\\.?[0-9]*", "", text_data)
text_data <- gsub("[!@#?$%^&*()_+=-]", "", text_data)
text_data <- gsub("<.*?>", "", text_data)
text_df <- data.frame(text = text_data)
tokens <- text_df %>% unnest_tokens(word,text)
tokens <- tokens %>% anti_join(stop_words)
stemmed_words <-wordStem(tokens)
lemmatized_words <- lemmatize_words(stemmed_words)

clean_words <- unlist(strsplit(as.character(lemmatized_words[[1]]), split = " "))
text_df <- data.frame(text = clean_words)

i <- 1
while(i <= nrow(text_df)){
  text_df[i,1] <- gsub("[[:punct:]]", "",text_df[i,1])
  i <- i+1  
}


i <- 1
while(i <= nrow(text_df)){
  text_df[i,1] <- hunspell_suggest(text_df[i,1])[[1]][1]
  i <- i+1
}

text_df <- na.omit(text_df)
text_df <- distinct(text_df)

