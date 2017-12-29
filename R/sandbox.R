# Sandbox #

## Pass string column arguments to dplyr function

# input dataframe, group by column string, summarise column string
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