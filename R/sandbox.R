# Sandbox #



## Pass string column arguments to dplyr function ----
library(tidyverse); library(tidytext); library(lazyeval)

# input dataframe, group by column string, summarise column string
# column name strings need interp(). '~' mandatory. returns a function, or just a var
simpleFunction <- function(dataset, group_var, sum_var){
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
