sphereFun <- function(r = NULL){
  if (is.null(r)) r <- sample(c(rep(1, 25), rep(2, 50), rep(5, 100)))
  
  tibble(r = r) %>% 
    rowid_to_column() %>% 
    group_nest(r) %>%
    mutate(data = map2(data, r, ~ {
      r <- ..2
      
      goldenRatio <- (1 + sqrt(5)) / 2
      i <- seq(0, nrow(..1) - 1)
      theta <- 2 * pi * i / goldenRatio
      phi <- acos(1 - 2 *(i)/nrow(..1))
      
      ..1 %>%
        mutate(x = cos(theta) * sin(phi),
               y = sin(theta) * sin(phi),
               z = cos(phi)) %>% 
        mutate(across(c(x, y, z), ~ ..1 * r))
    })
    ) %>% 
    unnest(cols = data) %>% 
    arrange(rowid) %>% 
    select(x, y, z) %>% 
    as.matrix()
}

cocktails <- read_rds("cocktails.rds")
set.seed(17)
nodes <- cocktails %>% 
  pairwise_cor(ingredient, drink) %>% 
  rename(ingredient = item1) %>% 
  group_by(ingredient) %>% 
  summarise(cor_mean = mean(abs(correlation))) %>% 
  arrange(desc(cor_mean)) %>% 
  mutate(id = sample(1:nrow(.))) %>% 
  mutate(grp = cut(cor_mean, quantile(cor_mean, probs = c(0, 0.9, 1)), include.lowest = TRUE),
         grp = c(5, 1)[grp])

cocktails <- cocktails %>% 
  left_join(select(nodes, ingredient, grp))

edges <- cocktails %>% 
  left_join(nodes) %>% 
  select(drink, ingredient) %>% 
  left_join(., ., by = "drink") %>% 
  relocate(drink, .after = last_col()) %>% 
  filter(ingredient.x != ingredient.y)

generate_net <- function(highlight = NULL) {
  if (is.null(highlight)) highlight <- FALSE
  
  highlight_ingredients <- cocktails %>% 
    filter(drink == highlight) %>% 
    pull(ingredient)
  
  nodes <- nodes %>% 
    mutate(color = if_else(ingredient %in% highlight_ingredients, "#79D7F7", "#D1DDe6"),
           color = if_else(grp == 1, adjustcolor(color, alpha = 0.8), adjustcolor(color, alpha = 0.3)))
  
  net <- graph_from_data_frame(d = edges %>% filter(drink == highlight), vertices = nodes)
  V(net)$size <- 8
  V(net)$color <- nodes$color
  V(net)$label <- names(V(net))
  E(net)$arrow.mode <- 0
  
  net
}
