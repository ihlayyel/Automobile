# The following code has been devloped for Automobile Data Set. 


###############################################################################################################
# ######################### Pre-procsseing and cleaning data
##############################################################################################################


library(plyr)
library(ggplot2)
library(dplyr)
library(DT)
library(tidyr)
library(wesanderson)
library(mice)



#loading the dataset 
car_data <- read.csv("Auto1-DS-TestData.csv", header = TRUE,  stringsAsFactors = FALSE)


# descriptive analysis to see the data types and some basic summary for every column in the data 

str(car_data)
summary(car_data)


#after having a look on the obtained results we can see clearly that there is ? on the data
#its better to eliminate the those data and replace them.

colSums(car_data == '?')
# I can see that ? appear in (normalized.losses 41 , num.of.doors 2, bore 4, stroke 4 , 
#horsepower 2, peak.rpm 2 and price 4)
# Well the main challenge is to find the 
# 1- best way to replace the? With some values and there are many techniques (use the mean or remove rows) 
# 2- the ? Appears in numerical and categorical values 
# So let's try to find a pattern from the existing data we might be able to use that. 



#before doing that lets check if there is any missing values as well. 
colSums(is.na(car_data))
#Great there is no missing values appears among the observation. 


#Now, let handle the ? for category (number of door), I will try to plot some graphs to understand better. 
ggplot(data=car_data, aes(x=num.of.doors)) +
  geom_bar(stat="count")

# as we can see that the there is ? in the data. in addition, we can see that the cars with four doors much bigger there is a high chance that ? means four
#lets try to see if we can make use of car brand with the ? values 

ggplot(car_data, aes(x = make, fill = factor(num.of.doors))) +
  geom_bar() +
  xlab("type") +
  ylab("Total Count") +
  labs(fill = "num.of.doors")


# Good we can see that ? appears in two car brand which are the mazda and dodge cars. But, there is alot of cats also having two and four doors 
# lets try to add body style variable it might makes us have better insights. 
ggplot(car_data, aes(x = num.of.doors, fill = body.style)) +
  geom_bar() +
  facet_wrap(~make) + 
  ggtitle("make") +
  xlab("num.of.doors") +
  ylab("Total Count") +
  labs(fill = "body.style")

#we can see clearly that the majority of sedan is four doors which make sence. I will replace the ? with four

car_data$num.of.doors[car_data$num.of.doors=="?"] <- "four"

#lets have a quick look to the data now 

ggplot(car_data, aes(x = num.of.doors, fill = factor(body.style))) +
  geom_bar() +
  xlab("num.of.doors") +
  ylab("Total Count") +
  labs(fill = "body.style") 

# the data looks much better. lets handle the other ? that appears in numerical variables. 
#(normalized.losses 41 ,  bore 4, stroke 4 , horsepower 2, peak.rpm 2 and price 4)


car_data$price[car_data$price == "?"] <- NA
car_data$peak.rpm[car_data$peak.rpm == "?"] <- NA
car_data$horsepower[car_data$horsepower == "?"] <- NA
car_data$stroke[car_data$stroke == "?"] <- NA
car_data$bore[car_data$bore == "?"] <- NA
car_data$normalized.losses[car_data$normalized.losses == "?"] <- NA

str(car_data)

#we need to convert the variables into numerical  
car_data$price<-as.numeric(as.character(car_data$price))
car_data$peak.rpm<-as.numeric(as.character(car_data$peak.rpm))
car_data$horsepower<-as.numeric(as.character(car_data$horsepower))
car_data$stroke<-as.numeric(as.character(car_data$stroke))
car_data$bore<-as.numeric(as.character(car_data$bore))
car_data$normalized.losses<-as.numeric(as.character(car_data$normalized.losses))

# due to the sensitivity  of the price its better we remove all the NA values in the price 
car_data<-subset(car_data, !is.na(price))

# one of the awesome library is MICE which try to impute the missing values. lets try to use that. 

imputed <- mice(car_data,m=1,maxit=5,meth='pmm',seed=500)

#lets see the summary of the data 
summary(imputed)

imputed$imp$horsepower
imputed$imp$stroke
imputed$imp$bore
imputed$imp$peak.rpm
imputed$imp$normalized.losses

#the first col is the number of column on the data set and the second one is the imputed data. 
# replace the missing values with the imputed values
fulldata <- complete(imputed,1)
completedData <-fulldata
# The data is ready for modeling. Lets have some fun 

###############################################################################################################
 ######################### Data analysis 
##############################################################################################################






#############look at the data distribution for each variable to find some relationship between them########
# one of the important variable  in cars is the millage 
#so, let calculate the overall millage for the cars and do some analysis. 


#Miles Per Gallon
completedData$mpg<- (completedData$city.mpg + completedData$highway.mpg)/2
qplot(completedData$mpg, xlab = 'Miles Per Gallon', ylab = 'Count', binwidth = 2, 
      main='Frequency Histogram: Miles per Gallon')

#num.of.cylinders
qplot(completedData$num.of.cylinders, xlab = 'num.of.cylinders', ylab = 'Count', 
      main='Frequency Histogram: Number of Cylinders')

# Horsepower
qplot(completedData$horsepower, xlab = 'Horsepower', ylab = 'Count', binwidth = 10,
      main='Frequency Histogram: Horsepower')

# price 
qplot(completedData$price, xlab = 'price', ylab = 'Count', binwidth = 200,
      main='Frequency Histogram: price')
#make
qplot(completedData$make, xlab = 'make', ylab = 'Count', main='Frequency Histogram: make')




# h<-hist(completedData$price, breaks=12, col="red", xlab="price for car", 
#         main="Histogram with Normal Curve") 
# xfit<-seq(min(completedData$price),max(completedData$price),length=40) 
# yfit<-dnorm(xfit,mean=mean(completedData$price),sd=sd(completedData$price)) 
# yfit <- yfit*diff(h$mids[1:2])*length(completedData$price) 
# lines(xfit, yfit, col="blue", lwd=2)
# 

table(completedData$num.of.doors)
table(completedData$body.style)
table(completedData$make)
table(completedData$fuel.type)
table(completedData$num.of.cylinders)


#The results show us the number of cars with three and twelve cylinders are only 1. Which is very low that will affect the performance of the model. However we will keep them for the analysis.  

#lets see the distribution  of cars considering the body style and fuel type 

ggplot(completedData, aes(x = fuel.type, fill = body.style)) +
  geom_bar() +
  facet_wrap(~make) + 
  ggtitle(" cars distrubution with respect to body style and fuel type") +
  xlab("fuel.type") +
  ylab("Total Count") +
  labs(fill = "body.style")
# ONE important notice here that  the majority of the cars are gas (sedan  and hatchback) are the most common in the market


#lets see if we can use the num.of.cylinders to understand any pattren. 

ggplot(completedData, aes(x = fuel.type, fill = num.of.cylinders)) +
  geom_bar() +
  facet_wrap(~make) + 
  ggtitle("the number cars with respect to number of cylinder and fuel type") +
  xlab("fuel.type") +
  ylab("Total Count") +
  labs(fill = "num.of.cylinders")

#interesting we can see that the majority of the cars are having four cylinder 
#as we can see toyota, Honda and mitsubishi maybe due to there economy in term of fuel consumption   


boxplot(completedData$price ~ completedData$make)

# the boxplot shows us that the majority  of the car prices are below 20000



top_1 <- completedData %>% 
  group_by(make, body.style, mpg) %>% 
  summarize(total = sum(price)) %>%
  top_n(1)



#we can see the car with the highest millage are toyota and vlokswagen 
ggplot(top_1, aes(make, mpg, fill = body.style)) + 
  geom_bar(stat = "identity") +
  ggtitle("the mpg for car brand and body.style") +
  theme(legend.position = "top") 
# the figure indicate that the hatchback and sedan car have the lowest mpg among between the cars 
#the buyers prefer cars with a low millage 



# lets see the overall price for the car 
ggplot(top_1, aes(make, total, fill = body.style)) + 
  geom_bar(stat = "identity") +
  ggtitle("the price of car sales with the brand") +
  theme(legend.position = "top") 
#the buyers also consdier the price of the cars you can see the price of hatchback and sedan lower comparing to others. 
 #to other cars 


#so i assume that the cars with lower millage have high price lets see the relationship of the cars there. 
ggplot(data = completedData, aes(x = price, y = mpg)) +
  geom_point() +
  geom_smooth(method='lm') +
  xlab('MPG') +
  ylab('price') +
  ggtitle('MPG vs. weight: Entire Sample')

#cool then my assumption is correct which is when the mpg is low the price is high 
#then we can say that the most profitable cars to sell in the market are sedan and hatchback.



#lets try to check the distrbution of car and the symboling affect the car price 
ggplot(data = completedData, aes(x = symboling, y = price)) +
  geom_point() +
  geom_smooth(method='lm') +
  xlab('symboling') +
  ylab('price') +
  ggtitle('MPG vs. weight: Entire Sample')

# there is positive relationship between the cars and risk variable, we can see that the majority of the cars are risky. Which is make since because the cars are used so the risk is high.   

     
     
     
    





###########################################################################################################################################
##################################################Data Modeling  
#################################################################################################################################################

# colnames(fulldata) <- c("symboling" , "normalized" , "make" , "fueltype", "aspiration", "numofdoors", "bodystyle",  "drivewheels", " enginelocation" ,	"wheelbase" , "length" , "width"
#                            , "height" , "curbweight","enginetype","numofcylinders", "enginesize", "fuelsystem" , "bore" , "stroke" , "compressionratio" ,"horsepower" , "peakrpm" , "citympg" , "highwaympg" , "price")
# # split the data into training and testing 
set.seed(1234)
ind<- sample(2, nrow(fulldata), replace=T , prob = c(0.8,0.2))
train <- fulldata[ind==1,]
test <- fulldata[ind==2,]


#####################################################################################################################################################3
###############################################################  model  linear regression    #############################################
#####################################################################################################################################################

#let's hypothesize that price are related to other variables  
# null hypothesis (H0). Here, our null hypothesis is that price and pther variables aren't related.
#H1: There is some relationship between price  and other variables. 
lm<-  lm(price~.,data=train)
summary(lm)


# by looking at the summary of the  model we can see that the model have significant value with some parameters. so we have evidence to reject H0
# is Our multiple R2 value is also a little higher than our adjusted R2. 
#R2  and R2adj. The closer these values are to 1, the better the model explains the label. and the  p-values are smaller than 0.05, we reject H0. there to be a relationship between price and some parameters in the model 
#In the other hand, there are many parameters are no significant on the model, it would be better if we attemp to build another model with the parameters that have a significant value to the model 

#####################################################################################################################################################3
###############################################################  model  improvement feautre selection   #############################################
#####################################################################################################################################################

# i have picked up parameters that have a highly correspondent to our model.  
lm2<-  lm(price ~ make + aspiration + body.style + wheel.base + length + width + height + curb.weight + num.of.cylinders + engine.size + peak.rpm + fuel.type + engine.location,data=train)
summary(lm2)
plot(lm2)

#based on the obtained graphs we can see that: 
# graph1 - the distribution of residuals have some changes with the  fitted price values.
# graph2 - the normal Q-Q residuals are almost having stright line and appears to be close to Normally distributed
# graph3 -  fitted smoothing regression line indicates that the distribution of residuals have some changes with the predicted values  
# graph4 - there are outlier on the data which might affect the performance of the model. The errors and outliers in the data can have greater or lesser effect, depending on how extreme they are and their placement with respect to the other data.

#from the figure we can see  there are some oulier points  11, 16, 18  


Predlm <- predict(lm2, test)

summary(Predlm)
plot(Predlm)

#####################################################################################################################################################3
###############################################################Step-wise regression  #############################################
#####################################################################################################################################################


#Cook's distance is measured in units of standard deviation. which is used to compare the differences between the mean without using data points. 
#Step-wise regression is used to tackle this issue. 


lm.step = stepAIC(lm, direction = 'both')
# ANOVA of the result
lm.step$anova 


summary(lm.step)
# the result analysis indicates that there are 18 variables have been used whch are :
#make + aspiration + num.of.doors + body.style +  drive.wheels + wheel.base + length + width + height + curb.weight + engine.type + num.of.cylinders + engine.size + fuel.system +  bore + stroke + compression.ratio + peak.rpm)
#by looking at the P-value of F-statistic  we can see that the result have been improved comparing with the previous  Lm model . also the value of  R2adj  of this model is slightly better than that of the linear model earlier.


plot(lm.step)


#####################################################################################################################################################3
########################################################################### THE END ######################################
#####################################################################################################################################################



