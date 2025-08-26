
library(readxl)
library(tidyr)
library(dplyr)
library(collapse)
library(janitor)
library(tsibble)
library(fable)
library(ggplot2)
library(readr)

# General data wrangling
library(tidyverse)
library(skimr)

# Modeling packages
library(tidymodels)
library(stacks)

# Visualization
library(corrr)
library(plotly)


sales<- read_csv('sales1_ml.csv')
sales$date<- as.Date(sales$date)
sales<- sales %>% select(date:available_barcode,revenue)
tail(sales)
sales %>% group_by(dept) %>% summarise(total_revenue= sum(revenue,na.rm = TRUE))


sales %>% group_by(dept) %>% mutate(total_revenue= sum(revenue,na.rm = TRUE))


sales_grouped<- sales %>% dplyr::group_by(date,dept,family,subfamily,section,brand) %>% 
  dplyr::summarise(total_revenue= sum(revenue) )%>% dplyr::mutate(yearmonth= yearmonth(date))

sales_grouped<- sales_grouped %>% group_by(yearmonth,dept,family,subfamily,section,brand) %>% 
  dplyr::summarise(total_revenue= sum(total_revenue) )


sales_grouped<- sales_grouped %>% as_tsibble(key=c(dept,family,subfamily,section,brand),index = yearmonth) %>% 
  fill_gaps(.full = TRUE)




