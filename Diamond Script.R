# QESTIONS:
#(a) Split the data (80-20) for the 10,000 diamonds into a training dataset and a testing 
#dataset.   
#(b) Using only the training dataset, estimate a regression model. Make sure to describe the 
#steps of your modelling approach, and to interpret your model.  
#Please include the following:  
# 1. Create scatterplots of between Price and each of the independent variables.  
#2. If Price has an exponential relationship to Carat, Length, Width, and Depth. Taking 
#this into consideration, transform Price and get its natural logarithm. 
#3. Additionally, add the logarithmic value of Carat1 to keep the linear relationship with 
#the transformed dependant variable 
#4. Create a multivariable regression model and input all the variables. Provide Equation 
#showing the initial model. 
#5. Provide the Table that shows the results of the model outlined in Equation 
#6. Describe any variables that are not significant 
#7. Develop new model/equation with only significant variables.  
#8. Provide new equation and summary table 
#9. Next develop a correlation matrix. Provide this as  Table . Describe the variables that 
#are highly correlated amongst each other, suggesting potential multicollinearity 
#problems. Describe the issue. 
#10. obtain the final model shown as equation and describe the model. 



# # # # # # ANSWERS:
#Let import the data and callm it diamomd
dim(Diamond)
# (a)Spliiting the data
train_index<-sample(1:nrow(Diamond),0.8*nrow(Diamond))
train_data<-Diamond[train_index,]
test_data<-Diamond[-train_index,]
#Check train data dimension, it should be 8000 rows, 10 coumns
dim(train_data)
#(b) Estimating the regression model;
# (1) Create scatterplots of between Price and each of the independent variables. 
par(mfrow=c(2,3))
plot(train_data$Carat, train_data$Price, main='Price vs Carat',col=2)
abline(lm(train_data$Price~train_data$Carat), col='black', lwd=4)
plot(train_data$Table, train_data$Price, main='Price vs Table', col=2)
abline(lm(train_data$Price~train_data$Table),col='black', lwd=4)
plot(train_data$Length, train_data$Price, main='Price vs Length', col=2)
abline(lm(train_data$Price~train_data$Length), col='black', lwd=4)
plot(train_data$Width, train_data$Price, main='Price vs Width', col=2)
abline(lm(train_data$Price~train_data$Width), col='black', lwd=4)
plot(train_data$Depth, train_data$Price, main='Price vs Depth',col=2 )
abline(lm(train_data$Price~train_data$Depth), col='black', lwd=4)
plot(train_data$DepthPercentage, train_data$Price, main='Price vs DepthPercentage',col=2)
abline(lm(train_data$Price~train_data$DepthPercentage), col='black', lwd=4)
#  Price is not linear with the independent variables
#  (2) Natural log of price
LnPrice<-log(train_data$Price)
#  (3) Natural log of carat
LnCarat<-log(train_data$Carat)
#  (4) Creating Mutiple linaer regression including all the variables
Initial_model=lm(log(Price)~log(Carat)+as.factor(Cut)+as.factor(Colour)+as.factor(Clarity)+Table+Length+Width+Depth+DepthPercentage, data=train_data)
summary(Initial_model)
# (5) Only Table is not statisticaly significant
# (6) creating new model with the significantt variables
New_model=lm(log(Price)~log(Carat)+as.factor(Cut)+as.factor(Colour)+as.factor(Clarity)+Length+Width+Depth+DepthPercentage, data=train_data)
summary(New_model)
# (7) write new model equation with significant variable
# (8) write new model equation eqaution and summary stable
Cor=cor(train_data[sapply(train_data, is.numeric)])
Cor
# (9) rounding the result to two decimal place
cor_round=round(Cor, 2)
cor_round
# (9) Pick out the correlated matrix
# (10) Decribe final model
Final_model=lm(log(Price)~log(Carat)+as.factor(Cut)+as.factor(Colour)+as.factor(Clarity)+DepthPercentage, data=train_data)
summary(Final_model)
#DepthPercentage is not statistically significant, removing it from the model
New_Final_Model= lm(log(Price)~log(Carat)+as.factor(Cut)+as.factor(Colour)+as.factor(Clarity),data=train_data)
summary(New_Final_Model)
# Investigaving model fit 
# Let remove cut, colour, and clarity from the model and call it Reduced model
Reduced_Model=Final_model=lm(log(Price)~log(Carat), data=train_data)
summary(Reduced_Model)
# Investigaving model fit with new final model 
AIC(Reduced_Model)
AIC(New_Final_Model)
anova(Reduced_Model,New_Final_Model)
# # # # #  From the result new final model fit better
# New final model is best fit in predicting price with 98% varaiablity

# (11) Investigaving the residual 
par(mfrow=c(1,2))
plot(New_Final_Model)
# (c) Evaluating the accuracy
New_pred=predict(New_Final_Model, newdata = test_data)
New_pred
Reduced_pred=predict(Reduced_Model, newdata = test_data)
Final_pred=predict(Final_model, newdata = test_data)
# Evaluation metric using MAE
Actual=log(test_data$Price)
# Mean Absolute Error, Mean Squared error, Root mean squared error, Gooodness of fit(R^2)
New_MAE=mean(abs(Actual-New_pred))
New_MAE
Reduced_MAE=mean(abs(Actual-Reduced_pred))
Reduced_MAE
Final_MAE=mean(abs(Actual-Final_pred))
Final_MAE


