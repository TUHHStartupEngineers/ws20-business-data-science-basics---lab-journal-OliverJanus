library(tidyverse)
library(data.table)
library(tidyverse)
library(ggplot2)
library(scales)

covid_data_dt <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv") %>% as.data.table()

# Challenge 5 ----

covid_data_cum_dt <- covid_data_dt[, .(dateRep, day, month, countriesAndTerritories, cases)][
  order(month, day)][
    , cumCases := cumsum(cases), by = "countriesAndTerritories"][
      , .(dateRep, countriesAndTerritories, cumCases)] %>% 
  pivot_wider(names_from = countriesAndTerritories, values_from = cumCases) %>%
  .[, c("dateRep", "Germany", "Spain", "United_Kingdom", "United_States_of_America", "France")] %>% 
  na.omit()

ylab <- c(2.5, 5.0, 7.5, 10, 12.5, 15)
covid_data_cum_dt %>%
  ggplot(aes(x = as.Date(dateRep, format="%d/%m/%Y"))) +
  geom_line(aes(y = Germany, color = "Germany"), size = 0.5, linetype = 1) +
  geom_line(aes(y = France, color = "France"), size = 0.5, linetype = 1) +
  geom_line(aes(y = United_Kingdom, color = "UK"), size = 0.5, linetype = 1)  +
  geom_line(aes(y = Spain, color = "Spain"), size = 0.5, linetype = 1) +
  geom_line(aes(y = United_States_of_America, color = "USA"), size = 0.5, linetype = 1) +
  scale_y_continuous(labels = paste0(ylab, "M"),
                     breaks = 10^6 * ylab) +
  scale_x_date(date_breaks = "1 month", 
             labels=date_format("%B"),
             limits = as.Date(c('2020-01-01','2020-12-31'))) + 
  theme(
    legend.position = "bottom",
    legend.background = element_rect(fill = "grey10"),
    legend.text = element_text(color = 'white'),
    legend.key.size = unit(2, "line"),
    legend.key = element_rect(fill = "grey10"),
    axis.text = element_text(color = 'white', size = 7.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 15),
    title = element_text(colour = 'white', size = 9),
    panel.background = element_rect(fill = 'grey10'),
    panel.grid.major = element_line(color = 'grey20', size = 0.5),
    panel.grid.minor = element_line(color = 'grey30', size = 0.5), 
    plot.background = element_rect(fill = 'grey10')) +
  labs(title    = "COVID-19 confirmed cases worldwide",
       subtitle = "As of 06/12/2020 USA is the leading country in cumulated cases",
       x = "Year 2020",
       y = "Cumulative Cases",
       color = "")

# Challenge 6 ----
