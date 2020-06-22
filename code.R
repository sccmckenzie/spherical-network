cocktails <- read_rds("cocktails.rds")
optimal_selection <- read_rds("optimal-selection.rds")

set.seed(17)

nodes <- optimal_selection %>%
  mutate(selection_id = row_number(),
         grp = as.integer(cut(performance, breaks = c(0, 200, 400, max(performance)))))

cocktails <- cocktails %>% 
  left_join(select(nodes, ingredient, grp))

edges <- cocktails %>% 
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
    mutate(color = if_else(ingredient %in% highlight_ingredients, "#79D7F7", "#D1DDe6"))#,
           #color = if_else(grp == 1, adjustcolor(color, alpha = 0.8), adjustcolor(color, alpha = 0.3)))
  
  net <- graph_from_data_frame(d = edges %>% filter(drink == highlight), vertices = nodes)
  V(net)$size <- c(3, 2, 1)[factor(V(net)$grp)]
  V(net)$color <- nodes$color
  V(net)$label <- names(V(net))
  E(net)$arrow.mode <- 0
  
  net
}
