# setup ------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(tidytext)
library(igraph)
library(ggraph)

# download data
download <- tidytuesdayR::tt_load("2025-04-29")

user2025 <- download$user2025

# explore ----------------------------------------------------------------------
glimpse(user2025)

# count by session type
session_type <- user2025 |> 
  group_by(session) |> 
  count()

# count by keywords
keywords_summary <- user2025 |> 
  group_by(keywords) |> 
  count()

# text analysis of keywords ----------------------------------------------------
keywords <- user2025 |> 
  select(keywords) |> 
  unnest_tokens(word, keywords)

# count keyword frequency
freq <- keywords |> 
  group_by(word) |> 
  count() |> 
  arrange(desc(n))

# bigrams - pairs of consecutive words
bigrams <- user2025 |> 
  select(keywords) |> 
  unnest_tokens(bigram, keywords, token = "ngrams", n = 2) |> 
  filter(!is.na(bigram))

# frequency of bigrams
bigram_freq <- bigrams |> 
  count(bigram, sort = TRUE)

# reformat to plot bigrams
bigram_cleaned <- bigram_freq |> 
  separate(bigram, c("word1", "word2"), sep = " ") |> 
  filter(n > 2)

bigram_graph <- bigram_cleaned |> 
  graph_from_data_frame()

ggraph(bigram_graph, layout = "fr") + 
  geom_edge_link() + 
  geom_node_point() + 
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

