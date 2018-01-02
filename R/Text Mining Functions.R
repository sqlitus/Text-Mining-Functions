# Text Mining Functions #

# Reference: http://uc-r.github.io/descriptive

devtools::install_github("bradleyboehmke/harrypotter")

library(tidyverse)      # data manipulation & plotting
library(stringr)        # text cleaning and regular expressions
library(tidytext)       # provides additional text mining functions
library(harrypotter)    # provides the first seven novels of the Harry Potter series


# Sample Data ----

# create dataframe of book, chapter, word, in sequence
titles <- c("Philosopher's Stone", "Chamber of Secrets", "Prisoner of Azkaban",
            "Goblet of Fire", "Order of the Phoenix", "Half-Blood Prince",
            "Deathly Hallows")

books <- list(philosophers_stone, chamber_of_secrets, prisoner_of_azkaban,
              goblet_of_fire, order_of_the_phoenix, half_blood_prince,
              deathly_hallows)


## dataframe with unnested tokens
series <- tibble()
for(i in seq_along(titles)) { # 1 to length(titles). (looping through each book)
  
  # create dataframe w/ chapter & text
  clean <- tibble(chapter = seq_along(books[[i]]), text = books[[i]]) %>% 
    unnest_tokens(word, text) %>%
    mutate(book = titles[i]) %>%
    select(book, everything())
  
  series <- rbind(series, clean) # adding each cleaned book to the series dataframe
}


## dataframe w/ books, chapters, text. Similar to ticket data.
series.2 <- tibble()
for (i in seq_along(titles)){
  clean.2 <- tibble(chapter = seq_along(books[[i]]), text = books[[i]]) %>%
    mutate(book = titles[i]) %>% 
    select(book, everything())
  series.2 <- rbind(series.2, clean.2)
}




# Word Frequency ----


TM.WordFrequency <- function(df, word.col.string){
  # insert: df w/ unnested tokens, & word column. returns: word frequency
  require(dplyr); require(lazyeval); require(tidytext)
  df %>%
    anti_join(stop_words) %>%
    count_(interp(~x, x = as.name(word.col.string)), sort = TRUE)
}
TM.WordFrequency(series, "word")


TM.WordFrequency.Groups <- function(df, group.col.string, word.col.string){
  # insert: df w/ unnested tokens, >= 1 grouping column, and word column. returns: grouped word counts
  require(dplyr); require(lazyeval); require(tidytext)
  df %>%
    anti_join(stop_words) %>%
    group_by_(.dots = group.col.string) %>%
    count_(interp(~x, x = as.name(word.col.string)), sort = TRUE)
}
TM.WordFrequency.Groups(series, "book", "word")
TM.WordFrequency.Groups(series, "chapter", "word")
TM.WordFrequency.Groups(series, c("book","chapter"), "word")


TM.WordFrequency.Vector <- function(v){
  # insert text vector, get word frequency
  data_frame(text = v) %>% unnest_tokens(word, text) %>% anti_join(stop_words) %>% count(word, sort = TRUE)
}
TM.WordFrequency.Vector(chamber_of_secrets[18])
TM.WordFrequency.Vector(chamber_of_secrets[18:20]) # char vector length 3 elements
TM.WordFrequency.Vector(goblet_of_fire[18])
TM.WordFrequency.Vector(c(chamber_of_secrets[18], goblet_of_fire[18])) # char vector length 2 elements
TM.WordFrequency.Vector(philosophers_stone[1]) %>% head(15)





TM.WordFrequency.Unnest <- function(df, text.col.string){
  require(dplyr); require(lazyeval); require(tidytext)
  df %>%
    unnest_tokens_("word", text.col.string) %>% # entire function needs standard eval
    anti_join(stop_words) %>%
    count(word, sort = TRUE)
}
TM.WordFrequency.Unnest(series.2, "text")


TM.WordFrequency.Groups.Unnest <- function(df, group.col.string, text.col.string){
  # insert: df, >= 1 grouping column, and word column. returns: grouped word counts
  require(dplyr); require(lazyeval); require(tidytext)
  df %>%
    unnest_tokens_("word", text.col.string) %>%
    anti_join(stop_words) %>%
    group_by_(.dots = group.col.string) %>%
    count(word, sort = TRUE)
}
TM.WordFrequency.Groups.Unnest(series.2, "book", "text")
TM.WordFrequency.Groups.Unnest(series, c("book","chapter"), "word")


TM.WordFrequency.Groups.Unnest.Plot <- function(df, group.col.string, text.col.string, top.x = 10){
  # insert: df, >= 1 grouping column, and word column. returns: grouped word counts
  require(dplyr); require(lazyeval); require(tidytext); require(ggplot2)
  df[[group.col.string]] <- as.factor(df[[group.col.string]])
  df[[text.col.string]] <- as.character(df[[text.col.string]])
  
  full.data <- df %>%
    unnest_tokens_("word", text.col.string) %>%
    anti_join(stop_words) %>%
    group_by_(.dots = group.col.string) %>%
    count(word, sort = TRUE)

  if (length(group.col.string) > 1) return(full.data)
  
  p <- full.data %>% top_n(top.x) %>% ungroup() %>% mutate(text_order = nrow(.):1) %>%
    ggplot(aes(reorder(word, text_order), n, fill = word)) + geom_bar(stat = "identity") +
    theme_bw() + 
    labs(x = "Word", y = "Frequency") +
    theme(legend.position = "none") +
    facet_wrap(group.col.string, scales = "free_y") +
    coord_flip()
  
  return(list(full.data = full.data, plot = p))
}
TM.WordFrequency.Groups.Unnest.Plot(series.2, "book", "text")
TM.WordFrequency.Groups.Unnest.Plot(series, c("book","chapter"), "word")
TM.WordFrequency.Groups.Unnest.Plot(series.2, "book", "text")
str(TM.WordFrequency.Groups.Unnest.Plot(series.2, "book", "text"))



TM.WordUseByGroup <- function(df, group.col.string, text.col.string){
  # percent of word use within group VS across entire population
  require(dplyr); require(lazyeval); require(tidytext)
  pct.group <- df %>%
    unnest_tokens_("word", text.col.string) %>%
    anti_join(stop_words) %>%
    count(word) %>%
    transmute(word, all_words = n / sum(n))
  df %>%
    unnest_tokens_("word", text.col.string) %>%
    anti_join(stop_words) %>%
    group_by(.dots = group.col.string) %>%
    count(word) %>%
    mutate(group_words = n / sum(n)) %>%
    left_join(pct.group) %>%
    arrange(desc(group_words)) %>%
    ungroup()
}
TM.WordUseByGroup(series.2, "book", "text") %>% top_n(20) %>% View()

TM.WordUseByGroup.Plot <- function(df, group.col.string, text.col.string, top.x = 20){
  # percent of word use within group VS across entire population
  require(dplyr); require(lazyeval); require(tidytext)
  pct.group <- df %>%
    unnest_tokens_("word", text.col.string) %>%
    anti_join(stop_words) %>%
    count(word) %>%
    transmute(word, all_words = n / sum(n))
  pct.all <- df %>%
    unnest_tokens_("word", text.col.string) %>%
    anti_join(stop_words) %>%
    group_by(.dots = group.col.string) %>%
    count(word) %>%
    mutate(group_words = n / sum(n)) %>%
    left_join(pct.group) %>%
    arrange(desc(group_words)) %>%
    ungroup()
  p <- ggplot(pct.all, aes(x = group_words, y = all_words, color = abs(all_words - group_words))) +
    geom_abline(color = "gray40", lty = 2) +
    geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
    geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
    scale_x_log10(labels = scales::percent_format()) +
    scale_y_log10(labels = scales::percent_format()) +
    scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
    facet_wrap(~ group.col.string, ncol = 2) +
    theme(legend.position="none") +
    labs(y = "Population Frequency", x = "Individual Group Frequency")
  
  return(list(data = pct.all, plot = p))
}
TM.WordUseByGroup.Plot(series.2, "book", "text")

