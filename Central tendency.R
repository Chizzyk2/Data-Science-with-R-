library(readr)

pineapple <- read_csv("Pinapple_juice.csv")

## Median of the juice

head(pineapple)

ncol(pineapple)

median2 <- apply(pineapple[,2:3],2,median)

## Mean of the juice

mean1 <- apply(pineapple[,2:3],2,mean)

## what is the inner quantile

inner_qua <- apply(pineapple[,2:3],2,IQR)

## What is the 65th percentile

quartile <- pineapple[,2:3]

qantile_65th <- quantile(pineapple$price, 0.65)

## what is the standard deviation?

sd2 <- apply(pineapple[,2:3],2,sd)

## what is the variance?

var2 <- apply(pineapple[,2:3],2,var)

## what is the range?

rang2 <- apply(pineapple[,2:3],2,function(x){max(x)-min(x)})