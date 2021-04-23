

c <- tibble(VITA_RAE.x = c("NA", 4.5, 6, "NA"), 
            VITA_RAE.y = c(1, 6, 6, 2), 
            THIA.x =  c("NA", "NA", 6, "NA"),
            THIA.y = c(1,6,"NA" ,2),
            WATER.x = c(89, 79, 99, 78), 
            WATER.y = c(10, 20, 16, 12))

col_vit <- function(x, y) {
  output <- vector("double", length(x))
  for (i in seq_along(df))
}

f1 <- function(x, i) {
  case_when(
    is.na(x) ~ (i*(100- c$WATER.x)/(100-c$WATER.y)), 
    TRUE ~ x)
}

f1(c$VITA_RAE.x, c$VITA_RAE.y)

output


new_col_name <- paste0("lag_result_", lag_size)
grouped_data <- grouped_data %>% 
  mutate(!!sym(new_col_name) := lag(Result, n = lag_size, default = NA))
}


pmap(x = matched2 %>% 
       select(VITA_RAE.x, THIA.x, WATER.x, WATER.y),
     y = matched2 %>%
       select(VITA_RAE.y, THIA.y, WATER.x, WATER.y), 
     new_col = c("VITA_RAE", "THIA", "WAT1", "WAT2"),
     .f = ~{tibble(
       mutate(!!sym(new_col) := ifelse(is.na(x),
                                       y*(100- y$WATER.x)/(100-y$WATER.y), x )))
     })