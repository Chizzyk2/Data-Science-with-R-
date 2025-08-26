###housing 
#source:https://github.com/jadeyee/r2d3-part-1-data/blob/master/part_1_data.csv
library(readr)
library(stringr)
library(dplyr)
library(ggplot2)
library(gridExtra)
housing<- read_csv('housing.csv',quote = ',')
names(housing)[c(1,8)]<-c('sf','elevation')
housing$sf<- str_remove(housing$sf,'\"')
housing$elevation<- str_remove(housing$elevation,'\"')
unique(housing$beds)
housing$beds<- factor(housing$beds,ordered = FALSE)
housing$bath<- factor(round(housing$bath,digits = 0),ordered = FALSE)
housing$elevation<-as.numeric(housing$elevation)
housing$sf<- factor(housing$sf)
housing_cleaned<- housing %>% dplyr::filter(beds %in% c(0,1,2,3,4,5,6)) %>% 
  dplyr::filter(bath %in% c(1,2,3,4,5,6,7))
housing_cleaned$price<- NULL


logistic_model1<- glm(sf~price_per_sqft,data = housing_cleaned,family = binomial())

predictions<- predict.glm(logistic_model1,data=housing_cleaned,type = 'response')
housing_cleaned$predictions<- predictions
predictions1<- predictions[1]

coef(logistic_model1)





coefficients(logistic_model1)

exp(coefficients(logistic_model1)[1]+coefficients(logistic_model1)[2]
    *housing_cleaned[1,'price_per_sqft'])/(1+exp(coefficients(logistic_model1)[1]+coefficients(logistic_model1)[2]
                                                 *housing_cleaned[1,'price_per_sqft']))



#### what is the logg odds change of of one unit increase in price to being in newyork?

#log(p/1-p)


### what is the percentage   of odds increase/ decrease of one unit increase in price to being in newyork?
1-exp(coef(logistic_model1)[2])


#### what is the expected increase/decrease  in probabilities of one unit increase in price ?

change<-predict(logistic_model1,data.frame(price_per_sqft= c(1000,1001)),type = 'response')

change[2]-change[1]



hist(predictions)
housing_cleaned$predictions<- NULL
model_all<- glm(sf~.,data= housing_cleaned,family = binomial())
summary(model_all)

boxplot(elevation~sf,data = housing_cleaned)
boxplot(price_per_sqft~sf,data = housing_cleaned)

exp(coef(model_all))

names(housing_cleaned)
summary(housing_cleaned$beds)
test_data<- data.frame(beds=factor(c(0,1,2,3,4,5,6)),bath=factor(rep(2,7)),year_built= rep(mean(housing_cleaned$year_built),7),
                                                                 sqft=rep(mean(housing_cleaned$sqft),7),
              price_per_sqft=rep(mean(housing_cleaned$price_per_sqft),7),
              elevation= rep(mean(housing_cleaned$elevation)))

test_beds<-cbind(test_data,bed_prob=predict(model_all,newdata = test_data,type = 'response'))

#### model_interaction

model_interaction_logistic<- glm(sf~ bath+beds+elevation*price_per_sqft+year_built+sqft,data = housing_cleaned)
summary(model_interaction_logistic)

##first_model
prediction_price_per_sqft_prob<- predict(logistic_model1,data= housing_cleaned,type = 'response')
prediction_price<- ifelse(prediction_price_per_sqft_prob >0.5,1,0)

table(housing_cleaned$sf,prediction_price)
mean(housing_cleaned$sf==prediction_price)


##second_model
prediction_all_prob<- predict(model_all,data= housing_cleaned,type = 'response')
prediction_all<- ifelse(prediction_all_prob >0.5,1,0)

table(housing_cleaned$sf,prediction_all)
mean(housing_cleaned$sf==prediction_all)



### third_model
prediction_inter_prob<- predict(model_interaction_logistic,data= housing_cleaned,type = 'response')
prediction_inter<- ifelse(prediction_inter_prob >0.5,1,0)

table(housing_cleaned$sf,prediction_inter)
mean(housing_cleaned$sf==prediction_inter)



### multinomial logistic regression

library(foreign)
library(nnet)
library(ggplot2)
library(reshape2)

ml <- read.dta("https://stats.idre.ucla.edu/stat/data/hsbdemo.dta")

table(ml$female,ml$honors)

###
prop_female<- 35/(74+35)
prop_male<- 18/(73+18)
odds_female<- prop_female/(1-prop_female)
odds_male<- prop_male/(1- prop_male)

odds_female/odds_male


logistic_model<- glm(honors~female,data = ml,family = binomial())

exp(coef(logistic_model))


unique(ml$ses)
unique(ml$prog)

boxplot(read~prog,data = ml)
boxplot(write~prog,data = ml)


ml$factored<- relevel(ml$prog,ref = 'academic')

logit_model<- multinom(factored~ read+write+ses,data= ml)

p/1-p

#p_general/ p_academic
# p_vocation/p_academic


summary(logit_model)

exp(coef(logit_model))

fitted_prob<- data.frame(fitted(logit_model))


head(fitted_prob)
head(ml)

str(fitted_prob)
fitted_prob<-fitted_prob %>% mutate(predictions= case_when(academic > general & vocation ~ 'academic',
                                              general > vocation & academic ~ 'general',
                                              vocation > general & academic ~ 'vocation'))


mean(ml$prog==fitted_prob$predictions)

names(ml)
unique(ml$ses)
test_Data_multi<- data.frame(ses= c('low','middle','high'),read= mean(ml$read),write= mean(ml$write))

proba_multti_ses<-cbind(test_Data_multi,predict(logit_model,newdata = test_Data_multi,'probs'))

diff(proba_multti_ses$vocation)

boxplot(ml$write)
boxplot(ml$read)

names(ml)
head(ml)

more_features_logit<- multinom(factored~ write+read+schtyp+math+science+ses+socst+female,data = ml)
summary(more_features_logit)
summary(logit_model)

fitting<- data.frame(fitted(more_features_logit))

fitting<- fitting %>% mutate(predictions= case_when(academic > general & vocation ~ 'academic',
                                                   general > vocation & academic ~ 'general',
                                                   vocation > general & academic ~ 'vocation'))

table(ml$prog,fitting$predictions)

mean(ml$prog == fitting$predictions)

ml_scaled<- ml 
names(ml)
numeric_col<-c('read','write','math','science','socst')
























