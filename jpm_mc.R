library(tidyverse)
library(highcharter)
library(tidyquant)
library(timetk)
library(scales)
library(readxl)
library(ggplot2)
library(EnvStats)
set.seed(4)


jpm <- read_excel("JPM.xlsx", sheet = "return") %>% 
  mutate(wv = weight * volatility)

corr_matrix <- read_excel("JPM.xlsx", sheet = "corr_matrix", col_names = FALSE)

mean_port_return <- crossprod(jpm$weight,jpm$mean_return)
port_var <- t(jpm$wv) %*% as.matrix(corr_matrix) %*% jpm$wv
stddev_port_return <- sqrt(port_var)

simulation_returns <- function(init_value, N, mean, stdev) {
  tibble(returns =  c(init_value, rnorm(N, mean, stdev))) %>%
    select(returns)
}

sims <- 1000
sim_years <- 20

simulation_returns(1, sim_years, mean_port_return, stddev_port_return)
####
starts <-
  rep(1, sims) %>%
  set_names(paste("sim", 1:sims, sep = ""))


mc <- map_dfc(
  starts, 
  simulation_returns,
  N = sim_years, 
  mean = mean_port_return,
  stdev = stddev_port_return
) %>%
  mutate(year = seq(1:nrow(.))) %>%
  select(year, everything()) %>%
  `colnames<-`(c("year", names(starts))) %>%
  gather(key = sim, value = returns, -year) %>%
  group_by(sim) 

gm <- mc %>% 
  spread(sim, returns) %>% 
  select(-year) %>% 
  summarize_each(funs(exp(mean(log(.+1)))-1)) %>% 
  gather(key = sim, value = geo_ave_return)

ggplot(gm, aes(geo_ave_return)) +
  geom_histogram(binwidth = 0.005) +
  geom_vline(aes(xintercept = 0.0765))
  


# hchart( mc,
#         type = 'line',
#         hcaes(y = growth,
#               x = year,
#               group = sim)) %>%
#   hc_title(text = paste(sims, "Simulations", sep = " ")) %>%
#   hc_xAxis(title = list(text = "years")) %>%
#   hc_yAxis(title = list(text = "dollar growth"),
#            labels = list(format = "${value}")) %>%
#   hc_add_theme(hc_theme_flat()) %>%
#   hc_exporting(enabled = TRUE) %>%
#   hc_legend(enabled = FALSE)

mc_ranked <- 
  mc %>% 
  filter(year == max(year)) %>% 
  arrange(desc(growth)) %>% 
  as.data.frame() %>% 
  mutate(id = row_number()) %>% 
  filter(row_number() == round(sims * 0.75, 0) |
           row_number() == round(sims * 0.5, 0) |
           row_number() == round(sims * 0.25, 0)) 


sim_summary <- 
  mc %>%
  summarise(final = last(growth)) %>% 
  summarise(
    first = round(quantile(final, 1/4), 2),
    median = round(quantile(final, 1/2), 2),
    third = round(quantile(final, 3/4), 2))

mc_iqr <- 
  mc %>% 
  left_join(mc_ranked, by = c("sim")) %>% 
  filter(id != "NA") %>% 
  mutate(quartile = case_when(id == round(sims * 0.75, 0) ~ "75th Percentile",
                                            id == round(sims * 0.5, 0) ~ "Median",
                                            id == round(sims * 0.25, 0) ~ "25th Percentile")) %>% 
  ungroup() %>% 
  select(Year = year.x, Growth = growth.x, Quartile = quartile) %>% 
  spread(Quartile, Growth)
  

ggplot(mc_iqr, aes(Year)) +
  geom_ribbon(aes(ymin = `25th Percentile`, ymax = `75th Percentile`), fill = "grey70") +
  geom_line(aes(y = Median))
