install.packages("readxl")
install.packages("tidyverse")
install.packages("lubridate")
install.packages("scales")


library(tidyverse)
library(readxl)
library(lubridate)
library(scales)
library(janitor)
library(plotly)

avg_temp.df <- read_excel("data/book_tgt_climate_9.xlsx", sheet="Temp (C)") %>%
  clean_names() %>%
  mutate(year = as.numeric(year))


avg_temp.df %>%
  ggplot(aes(year, temperature)) + 
  geom_point() +
  geom_line()

max_temp <- avg_temp.df %>%
  filter(year >1964)

max_temp %>%
  ggplot(aes(year, temperature)) + 
  geom_point() +
  geom_line() + 
  geom_smooth(method = "lm")

lm(temperature ~ year, data = max_temp)

co2.df <- read_delim("data/co2_annmean_mlo.txt", delim = " ",
                     skip_empty_rows = TRUE, 
                     comment = "#",
                     col_names = FALSE) %>%
  rename(year= X1, co2 = X2, uncertainty = X3 ) %>%
  mutate(year = as.numeric(year), co2 = as.numeric(co2))

co2.df %>% ggplot(aes(x=year, y = co2)) + geom_point() + geom_smooth(method="lm")

co2_model = lm(co2~year, data=co2.df)
summary(co2_model)


ice_core.df <- read_excel("data/Vostok Ice Core Data 2018.xls")

ice <- ice_core.df %>% ggplot(aes(age, temp_c)) +
  geom_point() + 
  geom_line() + 
  scale_x_reverse()
ggplotly(ice)

old_max.df <- ice_core.df %>%
  filter(age > 9.7 & age < 16.02)

old_temp.model <- lm(temp_c ~ age, data=old_max.df)
summary(old_temp.model)


co2.plot <- ice_core.df %>% ggplot(aes(age, co2_ppm)) +
  geom_point() + 
  geom_line() + 
  scale_x_reverse()
ggplotly(co2.plot)

old_co2_max.df <- ice_core.df %>%
  filter(age > 11.7 & age < 20.65)

old_co2.model <- lm(co2_ppm ~ age, data=old_co2_max.df)
summary(old_co2.model)

write_csv(avg_temp.df, "finalized data/avg_temp.csv")

test.df <- read.csv("https://github.com/wlperry/Project_Eddie/blob/master/finalized%20data/avg_temp.csv")
