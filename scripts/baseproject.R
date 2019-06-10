#' ---
#' title: "Project Eddie Temperature and CO2 Project"
#' author: "biobill"
#' date: "2019-06-10"
#' always_allow_html: yes
#' output:
#'    word_document:
#'      toc: false
#'      highlight: haddock
#' ---
#' 

setwd("~/Documents/R Projects/Project_Eddie")
# setwd(gsub(pattern = '/scripts', replacement = '', x = getwd()))

# install packages ----
# install.packages("readxl")
# install.packages("tidyverse")
# install.packages("lubridate")
# install.packages("scales")
# devtools::install_github("thomasp85/patchwork")

# load libraries ----
library(tidyverse)
library(readxl)
library(lubridate)
library(scales)
library(janitor)
library(plotly)
library(patchwork)

# read in file and make year numeric ----
modern_temp.df <- read_excel("./data/book_tgt_climate_9.xlsx", 
                          skip = 3,
                          sheet="Temp (C)") 

modern_temp.df <- modern_temp.df %>%
  clean_names() %>%
  rename(temp_c = temperature) 

write_csv(modern_temp.df, "finalized_data/modern_temp.csv")

# read excel file from link - harder -----
# url <- "http://www.earth-policy.org/datacenter/xls/book_tgt_climate_9.xlsx"
# destfile <- "book_tgt_climate_9.xlsx"
# curl::curl_download(url, destfile)
# avg_temp.df <- read_excel(destfile, skip = 2)


# plot the new temp data -----
modern_temp.plot <- modern_temp.df %>%
  ggplot(aes(year, temp_c)) + 
  geom_point() +
  geom_line()
modern_temp.plot
ggplotly(modern_temp.plot)

# filter out new data that isolates the max -----
max_modern_temp.df  <- modern_temp.df %>%
  filter(year > 1964)

# current temp max linear plot -----
max_modern_temp.plot <- max_modern_temp.df %>%
  ggplot(aes(year, temp_c)) + 
  geom_point() +
  geom_line() + 
  geom_smooth(method = "lm")
max_modern_temp.plot
ggplotly(max_modern_temp.plot)

# linear model current temp ----
modern_temp_inc.model <- lm(temp_c ~ year, data = max_modern_temp.df)
summary(modern_temp_inc.model)

# read in current co2---
modern_co2.df <- read_delim("data/co2_annmean_mlo.txt", delim = " ",
                     skip_empty_rows = TRUE, 
                     comment = "#",
                     col_names = FALSE) %>%
  rename(year= X1, co2_ppm = X2, uncertainty = X3 ) %>%
  mutate(year = as.numeric(year), co2_ppm = as.numeric(co2_ppm))

# plot current co2-----
modern_co2.plot <- modern_co2.df %>% ggplot(aes(x=year, y = co2_ppm)) + 
  geom_point() + 
  geom_smooth(method="lm")
modern_co2.plot
ggplotly(modern_co2.plot)

# current co2 linear model ---- 
modern_co2.model = lm(co2_ppm~year, data=modern_co2.df)
summary(modern_co2.model)

# save cleaned modern co2 data 
write_csv(modern_co2.df, "finalized_data/modern_co2.csv")

# ice core data ----
ancient_temp_co2.df <- read_excel("data/Vostok Ice Core Data 2018.xls") 

# clean data
ancient_temp_co2.df <-ancient_temp_co2.df %>%
  rename(years_1000 = age)

# plot ice core temp ----
anc_temp.plot <- ancient_temp_co2.df %>% 
  ggplot(aes(years_1000, temp_c)) +
  geom_point() + 
  geom_line() + 
  scale_x_reverse()
ggplotly(anc_temp.plot)
anc_temp.plot

# isolate max rate temp ----
anc_max_temp.df <- ancient_temp_co2.df %>%
  filter(years_1000 > 9.7 & years_1000 < 16.02)

anc_temp_max.plot <- anc_max_temp.df %>% 
  ggplot(aes(years_1000, temp_c)) +
  geom_point() + 
  geom_line() + 
  scale_x_reverse()
ggplotly(anc_temp_max.plot)
anc_temp_max.plot

# model max temp linear ----
anc_max_temp.model <- lm(temp_c ~ years_1000, data=anc_max_temp.df)
summary(anc_max_temp.model)

# co2 plot ----
anc_co2.plot <- ancient_temp_co2.df %>% 
  ggplot(aes(years_1000, co2_ppm)) +
  geom_point() + 
  geom_line() + 
  scale_x_reverse()
ggplotly(anc_co2.plot)
anc_co2.plot

# extract max co2  data ----
anc_co2_max.df <- ancient_temp_co2.df %>%
  filter(years_1000 > 11.7 & years_1000 < 20.65)

# co2 anc max plot ----
anc_co2_max.plot <- anc_co2_max.df %>% 
  ggplot(aes(years_1000, co2_ppm)) +
  geom_point() + 
  geom_line() + 
  scale_x_reverse()
ggplotly(anc_co2_max.plot)
anc_co2_max.plot


# old co2 model
anc_co2_max.model <- lm(co2_ppm ~ years_1000, data=anc_co2_max.df)
summary(anc_co2_max.model)

# save cleaned ancient data 
write_csv(ancient_temp_co2.df, "finalized_data/ancient_temp_co2.csv")
 
# test.df <- read_csv("https://raw.githubusercontent.com/wlperry/Project_Eddie/master/finalized_data/avg_temp.csv")


all.plots <- modern_temp.plot + 
  max_modern_temp.plot +
  modern_co2.plot + 
  anc_temp.plot +
  anc_temp_max.plot +
  anc_co2.plot + 
  anc_co2_max.plot
all.plots
  
  