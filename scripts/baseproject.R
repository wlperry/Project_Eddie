# install packages ----
# install.packages("readxl")
# install.packages("tidyverse")
# install.packages("lubridate")
# install.packages("scales")

# load libraries ----
library(tidyverse)
library(readxl)
library(lubridate)
library(scales)
library(janitor)
library(plotly)

setwd("~/Documents/R Projects/Project_Eddie")

# read in file and make year numeric ----
avg_temp.df <- read_excel("data/book_tgt_climate_9.xlsx", sheet="Temp (C)") %>%
  clean_names() %>%
  mutate(year = as.numeric(year))

# plot the new temp data -----
current_temp.plot <- avg_temp.df %>%
  ggplot(aes(year, temperature)) + 
  geom_point() +
  geom_line()
ggplotly(current_temp.plot)

# filter out new data that isolates the max -----
max_temp <- avg_temp.df %>%
  filter(year >1964)

# current temp max linear plot -----
max_current_temp.plot <- max_temp %>%
  ggplot(aes(year, temperature)) + 
  geom_point() +
  geom_line() + 
  geom_smooth(method = "lm")
ggplotly(max_current_temp.plot)

# linear model current temp ----
current_temp.model <- lm(temperature ~ year, data = max_temp)
summary(current_temp.model)


# read in current co2----
co2.df <- read_delim("./data/co2_annmean_mlo.txt", delim = " ",
                     skip_empty_rows = TRUE, 
                     comment = "#",
                     col_names = FALSE) %>%
  rename(year= X1, co2 = X2, uncertainty = X3 ) %>%
  mutate(year = as.numeric(year), co2 = as.numeric(co2))

# plot current co2-----
co2.df %>% ggplot(aes(x=year, y = co2)) + 
  geom_point() + 
  geom_smooth(method="lm")


# current co2 linear model ---- 
co2_model = lm(co2~year, data=co2.df)
summary(co2_model)

# ice core data ----
ice_core.df <- read_excel("./data/Vostok Ice Core Data 2018.xls")

# plot ice core temp ----
ice <- ice_core.df %>% ggplot(aes(age, temp_c)) +
  geom_point() + 
  geom_line() + 
  scale_x_reverse()
ggplotly(ice)


# isolate max rate temp ----
old_max.df <- ice_core.df %>%
  filter(age > 9.7 & age < 16.02)


# model max temp linear ----
old_temp.model <- lm(temp_c ~ age, data=old_max.df)
summary(old_temp.model)

# co2 plot ----
co2.plot <- ice_core.df %>% ggplot(aes(age, co2_ppm)) +
  geom_point() + 
  geom_line() + 
  scale_x_reverse()
ggplotly(co2.plot)

# extract max co2  data ----
old_co2_max.df <- ice_core.df %>%
  filter(age > 11.7 & age < 20.65)

# old co2 model
old_co2.model <- lm(co2_ppm ~ age, data=old_co2_max.df)
summary(old_co2.model)

# save cleaned data example
write_csv(avg_temp.df, "./finalized data/avg_temp.csv")
# 
# test.df <- read_csv("https://raw.githubusercontent.com/wlperry/Project_Eddie/master/finalized_data/avg_temp.csv")
