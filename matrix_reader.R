library(tidyverse)
library(readxl)

sheets <- excel_sheets("capital_market_matrices.xlsx")
jpm <- read_excel(
  "capital_market_matrices.xlsx",
  sheet = sheets[3], 
  col_names = FALSE,
  trim_ws = TRUE,
  skip = 4
  ) %>% 
  as_tibble() %>% 
  select(-1) %>% 
  `colnames<-`(c("rowname", unlist(.[1:4,1]), .[1,6:ncol(.)])) %>%
  slice(5:n()) %>% 
  column_to_rownames(var = colnames(.)[1])

ra <- read_excel(
  "capital_market_matrices.xlsx",
  sheet = sheets[1], 
  col_names = TRUE,
  trim_ws = TRUE,
  skip = 0,
  n_max = 28
  ) %>% 
  as_tibble() %>% 
  select(-1) %>% 
  column_to_rownames(var = colnames(.)[1])

bny <- read_excel(
  "capital_market_matrices.xlsx",
  sheet = sheets[2], 
  col_names = TRUE,
  trim_ws = TRUE,
  skip = 1,
  n_max = 48
) %>% 
  as_tibble() %>% 
  select(-1) %>% 
  column_to_rownames(var = colnames(.)[1])

br <- read_excel(
  "capital_market_matrices.xlsx",
  sheet = sheets[4], 
  col_names = TRUE,
  trim_ws = TRUE,
  skip = 1,
  n_max = 22
) %>% 
  as_tibble() %>% 
  column_to_rownames(var = colnames(.)[1])

hor <- read_excel(
  "capital_market_matrices.xlsx",
  sheet = sheets[5], 
  col_names = TRUE,
  trim_ws = TRUE,
  skip = 1
) %>% 
  as_tibble() %>% 
  select(-7) %>% 
  column_to_rownames(var = colnames(.)[1]) %>% 
  rename_at(vars(1:2), funs(paste0('10_yr_', .))) %>% 
  rename_at(vars(3:4), funs(paste0('20_yr_', .)))
