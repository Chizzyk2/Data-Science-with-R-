
retail_clean

install.packages("tidyverse")
install.packages("esquisse")
install.packages("plotly")
install.packages("lattice")
install.packages("rgl")
install.packages("ggvis")
install.packages("leaflet")
install.packages("highcharter")



library(dplyr)
library(tidyr)
library(ggplot2)
library(tidyverse)




countries <- c("Canada", "Denmark", "Brazil", "United Kingdom")

## The performance of 4 countries

first_plot <- retail_clean %>% group_by(Country,date) %>% filter(Country %in% countries) %>%
  summarise(total_sales = sum(Quantity, na.rm = TRUE)) %>%
  arrange(date) %>% ggplot(aes(x = date, y = total_sales,colour = Country)) + 
  geom_line() + facet_wrap(~Country, scales = "free_y")


# The UK

retail_clean %>% filter(Country == "United Kingdom") %>%
  group_by(Country,date) %>% summarise(total_sales = sum(Quantity, na.rm = TRUE)) %>%
  arrange(date) %>% ggplot(aes(x = date, y = total_sales, colour = Country)) +
  geom_line(color = "purple")

### The trend

retail_clean %>% filter(Country == "United Kingdom") %>%
  group_by(Country,date) %>% summarise(total_sales = sum(Quantity, na.rm = TRUE)) %>%
  arrange(date) %>% ggplot(aes(x = date, y = total_sales, colour = Country)) +
  geom_line(color = "purple") + geom_smooth(method = "lm")




retail_clean$revenue <- retail_clean$Price * retail_clean$Quantity

retail_clean %>% group_by(date) %>% summarise(total_sales = sum(Quantity, na.rm = TRUE),total_revenue = sum(revenue, na.rm = TRUE)) %>%
  ggplot(aes(x=date, y=total_sales))+geom_line(color = "blue")+
  geom_line(aes(y=total_revenue),color = "red")


library(readr)

cars <- read_csv("cars.csv")


library(dplyr)
library(tidyr)
library(plotly)
library(esquisse)
library(lattice)
library(rgl)
library(ggvis)
library(leaflet)
library(highcharter)


glimpse(cars)

## Relation between horsepower and city miles

cars %>% ggplot(aes(x= horsepower, y=city_miles_per_galloon, color = Price))+
  geom_point(color = "deeppink")

## color

cars %>% ggplot(aes(x=weight,Price,colour = sports_car, size = horsepower))+
  geom_point()

cars %>% ggplot(aes(x=weight,Price, shape = sports_car))+
  geom_point()+ theme_classic()

cars %>% ggplot(aes(x = Price, y = horsepower))+
  geom_point()

cars %>% ggplot(aes(x = Price, y = horsepower, color = factor(cylenders)))+
  geom_point()


## Bar Plots

retail_clean

top9_country <- retail_clean %>% group_by(Country) %>% summarise(appearances =n()) %>%
  arrange(desc(appearances)) %>% slice(2:10)

top9 <- as.vector(top9_country[["Country"]])

retail_clean %>% filter(Country %in% top9) %>% ggplot(aes(x=Country))+
  geom_bar(color = "cyan", fill = "purple")

install.packages("lubridate")
library(lubridate)

retail_clean$week <- week(retail_clean$date)

retail_clean %>% ggplot(aes(x = week))+ geom_bar(color="blue",fill="yellow")

top9_country %>% ggplot(aes(x=reorder(Country,appearances),y=appearances))+
  geom_col(color="cyan2",fill="red")+coord_flip()+xlab("Top 9")


## Distribution plot

iris

dist1 <- iris %>% ggplot(aes(x = Sepal.Length, fill = Species))+ 
  geom_density(color = "blue", alpha=0.4)

iris %>% ggplot(aes(x=Sepal.Width, fill = Species))+
  geom_density(alpha=0.4)

 cars %>% ggplot(aes(x=horsepower, fill = factor(cylenders)))+
   geom_density(alpha=0.4)

 
 ## Box Plots
 
table(cars$cylenders)

box1 <- cars %>% filter(cylenders %in% c(4,6,8)) %>% 
  ggplot(aes(x= factor(cylenders), y= horsepower))+
  geom_boxplot(color = "brown")

ggplotly(box1)
ggplotly(dist1)


## Histograms

skus_distributions <- read_csv("sku_distributions.csv")

skus_distributions

grape_histo <- skus_distributions %>% ggplot(aes(x=grape_juice))+
  geom_histogram(color = "blue", fill = "cyan", bins = 10, binwidth = 1)+
  ggtitle("Grape Juice")

apple_histo <- skus_distributions %>% ggplot(aes(x=apple_juice))+
  geom_histogram(color = "pink", fill = "purple")+
  ggtitle("Apple Juice")


cantalop_histo <- skus_distributions %>% ggplot(aes(x=cantalop_juice))+
  geom_histogram(color = "yellow", fill ="brown", bins = 7, binwidth = 1)+
  ggtitle("Cantalop Juice")


install.packages("gridExtra")
library(gridExtra)
 
grid.arrange(grape_histo,apple_histo,cantalop_histo,ncol=3)

skus_distributions %>% ggplot(aes(x=grape_juice))+
  geom_histogram(aes(y=after_stat(density)),color="yellow",fill="blue",bins = 10,binwidth = 1)+
  geom_density(color = "black")
 
 
 
 