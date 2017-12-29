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






# check
series %>% anti_join(stop_words) %>% group_by(book) %>% count(word, sort = TRUE) %>% top_n(10) %>%
  ggplot(aes(word, n)) + geom_bar(stat = "identity")

# todo: optional stopword argument. unnest token type. list out stopword libraries & descriptions for arguments