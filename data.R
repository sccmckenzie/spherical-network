library(tidyverse)

# this script cleans + transforms raw data from TidyTuesday repo for use in spherical network shiny application

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
                                        # essentially word tokenization
                                        pattern = " ",
                                        simplify = TRUE)) %>% 
  add_count(category, name = "freq") %>% 
  # within each ingredient, what are the most common words?
  with_groups(ingredient, slice_max, 1, order_by = freq) %>% 
  with_groups(ingredient, filter, n() == 1) %>% 
  filter(freq > 6)

library(fuzzyjoin)
ingredients_similar <- with_categories %>% 
  transmute(category,
            ingredient,
            # e.g. "fresh lime juice" -> "freshlime"
            reduced = str_remove(ingredient, category) %>% 
              str_remove_all(" ")) %>% 
  # remove ingredients that are synonymous with category
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
                     #identical strings have distance of 0
                     ingredient.x != ingredient.y) %>%
              # trick to remove mirrored rows
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
  rows_insert(tibble(drink = "Screwdriver",
                     ingredient_number = 2,
                     ingredient = "orange juice",
                     measure = "3 oz",
                     category = "other"),
              by = c("drink", "ingredient_number")) %>% 
  rows_update(tibble(drink = "H.D.",
                     ingredient_number = 1,
                     ingredient = "Whiskey",
                     measure = "4 cl",
                     category = "other"),
              by = c("drink", "ingredient_number")) %>% 
  write_rds("cocktails.rds") # dataset ready for shiny app
