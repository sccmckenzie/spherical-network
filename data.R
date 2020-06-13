library(tidyverse)

cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/cocktails.csv') %>% 
  select(drink, ingredient_number, ingredient, measure) %>% 
  mutate(ingredient = str_to_lower(ingredient),
         ingredient = str_remove(ingredient, "'"))

ingredients <- cocktails %>%
  distinct(ingredient)

library(tokenizers)
with_categories <- ingredients %>% 
  with_groups(ingredient,
              summarise,
              category = tokenize_regex(ingredient,
                                    pattern = " ",
                                    simplify = TRUE)) %>% 
  add_count(category, name = "freq") %>% 
  with_groups(ingredient, slice_max, 1, order_by = freq) %>% 
  with_groups(ingredient, filter, n() == 1) %>% 
  filter(freq > 6)

library(fuzzyjoin)
ingredients_similar <- with_categories %>% 
  transmute(category,
            ingredient,
            # e.g. remove "juice" from "lime juice" since we are comparing within categories
            reduced = str_remove(ingredient, category) %>% 
              str_remove_all(" ")) %>% 
  filter(reduced != "") %>% 
  group_nest(category) %>%
  rowwise() %>% 
  summarise(category = category,
            stringdist_left_join(data,
                                 data, 
                                 by = "reduced",
                                 method = "cosine",
                                 q = 1,
                                 distance_col = "dist") %>%
              filter(dist < 0.3,
                     ingredient.x != ingredient.y) %>%
              distinct(dist, .keep_all = TRUE)) %>% 
  arrange(dist) %>% 
  slice(c(3, 6, 8, 9, 10, 11, 13)) %>% 
  rowwise() %>%
  transmute(ingredient = list(c(ingredient.x, ingredient.y)),
            new_ingredient = ingredient[which.min(str_length(ingredient))]) %>% 
  unnest(ingredient)

cocktails %>% 
  left_join(with_categories) %>% 
  left_join(ingredients_similar) %>% 
  mutate(category = coalesce(category, "other"),
         ingredient = if_else(is.na(new_ingredient), ingredient, new_ingredient)) %>% 
  select(-freq, -new_ingredient) %>% 
  write_rds("cocktails.rds")