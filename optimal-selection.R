library(readr)

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
