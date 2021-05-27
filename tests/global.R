library(shiny)
library(waiter)
library(shinyjs)
library(bs4Dash)
library(echarts4r)
library(shinyWidgets)
library(tidyverse)

data <- readRDS("data/fvehicle.rds")
caracteristicas <- names(data)[2:15]

# txt <- data %>%
#     select(brand, version, v, v2) %>%
#     apply(., 1, paste0, collapse = " ") %>% 
#     unique()

