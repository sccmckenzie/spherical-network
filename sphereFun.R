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
