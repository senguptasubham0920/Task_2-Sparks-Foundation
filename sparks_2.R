library(tidyverse)
library(caret)
dataset = read.csv(url("https://raw.githubusercontent.com/AdiPersonalWorks/Random/master/student_scores%20-%20student_scores.csv"))

#Visualizing data (We Observe that the scatter shows a normal trend)
ggplot(dataset)+
  geom_point(aes(x= Hours,y = Scores))
  
#linearity assumption (high linear correlation)
cor.test(dataset$Hours,dataset$Scores)

#splitting data
set.seed(123)
intrain<-createDataPartition(y=dataset$Scores,p=0.800,list=FALSE)
training_set<-dataset[intrain,]
test_set<-dataset[-intrain,]


#fitting model
linear_model = lm(formula = Scores~Hours,data = training_set)
summary(linear_model)

#Visualizing Training data
new = cbind(training_set,linear_model$fitted.values)
 ggplot(new)+
   geom_point(aes(x = Hours,y = Scores))+
   geom_line(aes(x = training_set$Hours, y = linear_model$fitted.values), colour = "blue")
 
 #Visualizing test data with the model
p= predict(linear_model, newdata = test_set)
ggplot(test_set)+
  geom_point(aes(x = Hours,y = Scores))+
  geom_line(aes(x = Hours, y = p), colour = "blue")
  
  
 
#Predicting Hours = 9.25 and interpreting results 
predict(linear_model, data.frame(Hours = 9.25))
 
# adjusted R-squared value is 0.94 and Predicted Score is 91.899
