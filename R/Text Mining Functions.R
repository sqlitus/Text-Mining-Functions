# Text Mining Functions #

# Reference: http://uc-r.github.io/descriptive

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

series <- tibble()

for(i in seq_along(titles)) { # 1 to length(titles). (looping through each book)
  
  # create dataframe w/ chapter & text
  clean <- tibble(chapter = seq_along(books[[i]]), text = books[[i]]) %>% 
    unnest_tokens(word, text) %>%
    mutate(book = titles[i]) %>%
    select(book, everything())
  
  series <- rbind(series, clean) # adding each cleaned book to the series dataframe
}



# Word Frequency ----

TM.WordFrequency <- function(df, word.col.string){
  df %>% 
    # anti_join(stop_words, by = word.col.string) %>%
    count(df[[word.col.string]], sort = TRUE)
}
TM.WordFrequency(series, "word")