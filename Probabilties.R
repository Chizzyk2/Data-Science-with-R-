#A pair of fair dice is rolled. Let  𝑋denote the sum of the number of dots on the top faces.

#Construct the probability distribution of  𝑋 for a pair of fair dice.

# find the probability that X is bigger than 7
# find the probability of an odd number 

dice1<- 1:6
dice2<- 1:6
dice3 <-1:6

tosses<- expand.grid(dice1,dice2)
X<- tosses$Var1 +tosses$Var2
# find the probability that X is bigger than 7

length(X)
sum(X > 7)/length(X)

# find the probability of an odd number 

sum(X %% 2 !=0)/ length(X)

tosses_3<- expand.grid(dice1,dice2,dice3)

X<- apply(tosses_3, 1, sum)

sum(X >14)/length(X)


hist(X)


####Binomial Distribution
#80% of people who purchase pet insurance are women. 
#If 9 pet insurance owners are randomly selected, 
#find the probability that exactly 6 are women.
n= 9
x= 6
fact_n= factorial(n)
factor_x= factorial(x)
fact_notx= factorial(n-x)
all_possible_outcomes= fact_n/(fact_notx* factor_x)

p_6_women= 0.8^6
p_3_men= (1-0.8)^3
final= all_possible_outcomes*p_6_women*p_3_men





#Flip a coin 20 times , what is the probability that 9 of them will be heads.
#The probability of having a head is 0.5

n= 20
x= 19
p= 0.5
fact_n= factorial(n)
factor_x= factorial(x)
fact_notx= factorial(n-x)
all_possible_outcomes= fact_n/(fact_notx* factor_x)

p_heads= p^x
p_tails= p^(n-x)

prob_9_heads= all_possible_outcomes*p_heads*p_tails


### lees than or equal to 9
prob_vector= c()
for (X in 0:19){
  n= 20
  x= X
  p= 0.5
  fact_n= factorial(n)
  factor_x= factorial(x)
  fact_notx= factorial(n-x)
  all_possible_outcomes= fact_n/(fact_notx* factor_x)
  
  p_heads= p^x
  p_tails= p^(n-x)
  
  prob_heads= all_possible_outcomes*p_heads*p_tails
  
  prob_vector[X]= prob_heads
  
}

sum(prob_vector)

### poisson distribution

#Ugarte and colleagues report that the average number 
#of goals in a World Cup soccer match is approximately
#2.5 and the Poisson model is appropriate. Because the
#average event rate is 2.5 goals per match, λ = 2.5.

# lambda^k * exp(-lambda)

lambda=2.5
k=5
prob_5_goals= (lambda^k *exp(-lambda))/factorial(k)

dpois(k,lambda)

k= c(0:10)
prob= c()

for (i in 0:10){
  prob[i+1]=dpois(i,lambda = 2.5)
}
length(k)
length(prob)
plot(k,prob,type = 'l')



###Normal distribution

#johan is an avid runner and a racer who usually competed in the long distance races. 
#the avrage
#time he takes for the 4 km  is 20 minutes with a standard dviation of 2 minutes.


#1) what is the probability that johan will take more than 25 minutes ?
  
prob_lessthan25<- pnorm(25,20,2)
prob_of_more<- 1- prob_lessthan25
  
#2) what is the probability the johan will take less than 17 minutes ?
  
less_than_17<- pnorm(17,20,2)  

#3) what is the probability that johan will take from 18 to 23 minutes to finsih the race ?
  
less_18= pnorm(18,20,2)  
less_than_23= pnorm(23,20,2)

prob_18_23= less_than_23-less_18


hist(rnorm(700,20,2))



#### uniform distributions 

uniform<-runif(10000,2,9)
spread= max(uniform)- min(uniform)
less_than_4= (4-min(uniform))/spread
  
  

#### relative risk
library(readr)
tiktok<- read_csv('Categorical.csv')

tiktok

prop_table=prop.table(table(tiktok$group,tiktok$tiktok),margin = 1)

rr= prop_table[2,2]/prop_table[1,2]


####correlations
####correlations

cars<-read_csv("cars.csv")

colnames(cars)

head(cars[,9:19])

correlation<-cor(cars[,c(13,14)])
correlation

sum(is.na(cars)==TRUE)

###check city_miles_per galloon

sum(is.na(cars$city_miles_per_galloon)==TRUE)
####CHECKING horsepower
sum(is.na(cars$horsepower)==TRUE)

nrow(cars)

###reemove NAs

cars_clean<- na.omit(cars)

nrow(cars_clean)

sum(is.na(cars_clean)==TRUE)


#### Which rows has NAs

na_rows_city<- which(is.na(cars$city_miles_per_galloon)==TRUE)

na_rows_city

colnames(cars_clean)
cor(cars_clean[,c(13,14)])
cor_matrix<-cor(cars_clean[,c(9,13,14,16,19)])

###Correlation matrix

cor_matrix

install.packages("corrplot")
library(corrplot)

corrplot(cor_matrix,method = "number")


hist(rpois(2000,2.5))


####conditional probabilities

first_child<- c('G','B')
second_child<- c('G','B')
third_child<- c('G','B')

expand.grid(first_child,second_child,third_child)



(1/8) /(3/8)



