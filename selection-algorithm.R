# Selection algorithm (generalized for any data set)
# Inspired by TidyTuesday cocktail dataset
# 2020-06-21

# this needs to be realized as a package

# end-user function: 
# selection_path()
# data - dataset that shares similar structure as cocktails
# var - individual element (ingredient) that comprises groups (recipe)
# group - column that identifies subset

# Needed improvements:
# Improve bottleneck in n4()
# Scale to multi-dimensional (algorithm has ability to take hypothetical ith+1 result into consideration)

library(dplyr)
library(tidyr)
library(rlang)

n4_0 <- function(data, var, group) {
  var <- enexpr(var)
  group <- enexpr(group)
  
  stopifnot(is.data.frame(data))
  data <- data
  
  new_function(
    exprs(inventory = , gamma = 5),
    expr({
      choices <- setdiff(unique(pull(data, {{var}})), inventory)
      a <- data %>% 
        filter(!{{var}} %in% inventory) %>% # remove elements chosen in previous iterations
        group_by({{group}}) %>% 
        summarise(y = list({{var}}), .groups = "drop") %>% # nest remaining elements as <chr> for each group (bottleneck)
        crossing(c = choices) %>% 
        rowwise() %>% 
        mutate(y = length(y[!y %in% c]), # what is the length of remaining ingredients for each selection?
               y = exp(2 / (1 + gamma * y)) ^ 2) %>% # optimization fn
        ungroup() %>% 
        arrange(c, {{group}}) %>% 
        rename(grp = {{group}})
      
      # below code used to be accomplished with dplyr syntax - significant performance gain using matrices
      with(a, matrix(y, ncol = n_distinct(c), dimnames = list(unique(grp), unique(c)))) %>% 
        colSums() %>% 
        which.max() %>% 
        names()
    }),
    current_env()
  )
}

selection_path <- function(data, var, group, gamma = 5, cutoff = NULL) {
  var <- enexpr(var)
  group <- enexpr(group)
  
  n4 <- n4_0(data, {{var}}, {{group}})
  
  set <- unique(pull(data, {{var}}))
  if (is.null(cutoff)) cutoff <- length(set)
  out <- character(length = cutoff)
  
  for (i in seq_along(out)) {
    message(i)
    
    out[i] <- n4(out, gamma)
  }
  
  out
}


# Example
capability <- function(x, data, group, var) {
  out <- integer(length = length(x))
  
  for (i in seq_along(x)) {
    out[i] <- data %>% 
      with_groups({{group}}, filter, all({{var}} %in% x[1:i])) %>% 
      distinct({{group}}) %>% 
      nrow()
  }
  
  return(out)
}

cocktails <- read_rds("cocktails.rds") # output from data.R

optimal_selection <- tibble(ingredient = selection_path(cocktails, ingredient, drink)) %>% 
  mutate(performance = capability(ingredient, cocktails, drink, ingredient))

optimal_selection %>% 
  write_rds("optimal-selection.rds")

library(ggplot2)
by_count <- cocktails %>% 
  count(ingredient, sort = T) %>% 
  mutate(performance = capability(ingredient, cocktails, drink, ingredient))

tibble(method = c("count", "selection-alg"),
       id = list(seq_len(nrow(by_count))),
       data = list(by_count, optimal_selection)) %>% 
  unnest(cols = id:data) %>% 
  ggplot(aes(id, performance)) +
  geom_line(aes(color = method, group = method)) +
  theme_light()
