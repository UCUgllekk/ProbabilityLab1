list.files(getwd())
list.files("data/4-spam")
test_path <- "data/4-spam/test.csv"
train_path <- "data/4-spam/train.csv"

library(tidytext)
library(tidyverse)
library(readr)
library(ggplot2)
library(tidyr)
library(patchwork)


stop_words <- read_file("stop_words.txt")
# https://stackoverflow.com/questions/27195912/why-does-strsplit-return-a-list
splitted_stop_words <- strsplit(stop_words, split = "\n")
splitted_stop_words <- splitted_stop_words[[1]]

train <- read.csv(file = train_path, stringsAsFactors = FALSE)
test <- read.csv(file = test_path, stringsAsFactors = FALSE)


create_bag_of_words <- function(data, stop_words) {
  # Видаляємо пунктуацію та цифри
  data$Message <- str_replace_all(data$Message, "[[:punct:]]", " ")
  data$Message <- str_replace_all(data$Message, "[[:digit:]]", " ") # Видаляємо цифри
  data$Message <- str_to_lower(data$Message)

  data$Category <- ifelse(data$Category == "ham", 0, 1)

  data <- data %>%
    mutate(row_index = row_number())

  tidy_text <- data %>%
    unnest_tokens(word, Message, token = "words") %>%
    filter(!word %in% stop_words) %>%
    count(row_index, word)

  dtm <- tidy_text %>%
    cast_dtm(document = row_index, term = word, value = n)

  dtm_df <- as.data.frame(as.matrix(dtm))
  word_freq <- tidy_text %>%
    left_join(data %>% select(row_index, Category), by = "row_index") %>%
    group_by(word, Category) %>%
    summarize(count = sum(n), .groups = "drop") %>%
    spread(Category, count, fill = 0) %>%
    rename(ham = `0`, spam = `1`)


  labels <- data$Category

  return(list(bag_of_words_matrix = dtm_df, labels = labels, word_frequency = word_freq))
}

result <- create_bag_of_words(train, splitted_stop_words)




plot_top_5 <- function(word_freq) {
  top_spam <- word_freq %>%
    arrange(desc(spam)) %>%
    slice_head(n = 5)

  top_ham <- word_freq %>%
    arrange(desc(ham)) %>%
    slice_head(n = 5)

  # Plot histogram for Ham
  p1 <- ggplot(top_ham, aes(x = word, y = ham, fill = "Ham")) +
    geom_bar(stat = "identity") +
    labs(
      title = "Top 5 Most Frequent Words in Ham",
      y = "Frequency"
    ) +
    theme_minimal() +
    scale_fill_manual(values = c("Ham" = "#6987dc")) +
    theme(legend.position = "none")

  # Plot histogram for Spam
  p2 <- ggplot(top_spam, aes(x = word, y = spam, fill = "Spam")) +
    geom_bar(stat = "identity") +
    labs(
      title = "Top 5 Most Frequent Words in Spam",
      y = "Frequency"
    ) +
    theme_minimal() +
    scale_fill_manual(values = c("Spam" = "#f75e5e")) +
    theme(legend.position = "none")


  combined_plot <- p1 + p2 + plot_layout(ncol = 2)
  print(combined_plot)
}


word_frequency <- result$word_frequency
plot_top_5(word_frequency)
