# Sandbox #
print(object.size(series), units = "auto")



## Pass string column arguments to dplyr function ----
library(tidyverse); library(tidytext); library(lazyeval)

# input dataframe, group by column string, summarise column string
# column name strings need interp(). '~' mandatory. returns a function, or just a var
simpleFunction <- function(dataset, group_var, sum_var){
  # standard evaluation: use xx_ for evaluating strings, and interp() for evaluating strings in the function
  # lazy evaluation: use interp() to build an expression from string variables & other
  require(dplyr)
  require(lazyeval)
  dataset %>% 
    group_by_(group_var) %>%
    summarise_(mean_hp = interp(~mean(x), x = as.name(sum_var))) -> dataset
  return(dataset)
}
simpleFunction(mtcars, "cyl", "hp")
simpleFunction(mtcars, "vs", "wt")

# checks
mtcars %>% group_by(cyl) %>% summarise(mean.thing = mean(hp))
mtcars %>% group_by(cyl) %>% summarise(mean.thing = mean(mpg))
mtcars %>% group_by(vs) %>% summarise(mean.thing = mean(wt))

interp(~mean(x), x = as.name(c(1,2,3)))




## Get count of words in a vector ----
library(tidyverse); library(tidytext)

# turn it into data frame, unnest tokens, count
chamber_of_secrets[19]
data_frame(text = chamber_of_secrets[19]) %>% unnest_tokens(word, text)
data_frame(text = chamber_of_secrets[19]) %>% unnest_tokens(word, text) %>% count(word, sort = TRUE)
data_frame(text = chamber_of_secrets[19]) %>% unnest_tokens(word, text) %>% anti_join(stop_words) %>%
  count(word, sort = TRUE)



# char lists & vectors
str(books)
class(books)
length(books)
str(books[1])
str(books[[1]])
length(books[[1]])
seq_along(titles); 1:7


# test ticket data
test.tickets <- data_frame(sg = c("a","a","r10","p","r10"), 
                           title = c("kiosk down again","kiosk tm issue, kiosk broke", "lane down 201","tax refund lane","lane frozen"))
test.tickets
TM.WordFrequency.Unnest(test.tickets, "title") # raw word count
TM.WordFrequency.Groups.Unnest(test.tickets, "sg", "title") # raw word count by group


# top n
TM.WordFrequency.Unnest(series.2, "text") %>% top_n(10)
TM.WordFrequency.Groups.Unnest(series.2, "book", "text") %>% top_n(10) %>% View()
TM.WordFrequency.Groups.Unnest(series.2[series.2$book == titles[2],], "book", "text") %>% top_n(10)


# todo: optional stopword argument. unnest token type. list out stopword libraries & descriptions for arguments
table(stop_words$lexicon)
filter(stop_words, lexicon == "snowball")

# unnest tokens
series.2 %>% unnest_tokens(sentence, text, token = "sentences") %>% View()
series.2 %>% unnest_tokens(split, text, token = "ngrams", n = 2) %>% View()
series.2 %>% unnest_tokens(split, text, token = "regex", pattern = "harry") %>% View()
series.2 %>% unnest_tokens(sentence, text, to_lower = F) %>% View()


# non standard evaluation
x <- seq(0, 2 * pi, length = 100)
sinx <- sin(x)
plot(x, sinx, type = "l")
str(x)
rm(x, sinx)