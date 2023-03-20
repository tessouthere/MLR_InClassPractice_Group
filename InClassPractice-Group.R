
# In Class Practice || EDA and Multiple Linear Regression #
# Tess Anderson - Working with LeighAnn Robinson

library(tidyverse)
heart<-read_csv('heart.data.csv') 
# We have imported the dataset and saved it to the variable 'heart' so we can explore and work with the data.

# Now, we will explore the data #
glimpse(heart)
length(heart)
names(heart)
head(heart)
summary(heart) # Summary shows missing data in each column. 

# Now we must impute the missing values.

# Before imputing, we draw the distribution to see which imputation method we should use.
library(ggplot2)

# Biking Distribution
ggplot(heart,
       aes(biking))+
  geom_density()
# The plot shows that the distribution is not Normal, so we should use median to impute the missing values.
biking_median<-median(heart$biking,na.rm=TRUE)
heart$biking<-ifelse(is.na(heart$biking),
                     biking_median,
                     heart$biking)
# Missing Values in Biking are now imputed with the median of the column.
paste('Sum of missing values in Biking Column: ',sum(is.na(heart$biking)))

# Smoking Distribution
ggplot(heart,
       aes(smoking))+
  geom_density()
# Again, not the Normal bell curve so we use median.
smoking_median<-median(heart$smoking,na.rm=TRUE)
heart$smoking<-ifelse(is.na(heart$smoking),
                      smoking_median,
                      heart$smoking)
paste('Sum of missing values in Smoking Column: ',sum(is.na(heart$smoking)))
# Smoking Imputation completed

# Heart Disease Distribution
ggplot(heart,
       aes(heart.disease))+
  geom_density()
# Not perfectly normal, but approximately Normal so we will use Mean
hd_mean<-mean(heart$heart.disease,na.rm=TRUE)
heart$heart.disease<-ifelse(is.na(heart$heart.disease),
                            hd_mean,
                            heart$heart.disease)
paste('Sum of missing values in Smoking Column: ',sum(is.na(heart$heart.disease)))
# Imputation is finished, all missing values have been filled with appropriate approximation #

# Now we are ready to begin splitting and fitting the model for MLR. #
library(caTools)
# Creating the split variable which will random split the data into 2 groups
set.seed(123)
split=sample.split(heart$heart.disease,SplitRatio = .80)
# Creating the groups for the data
training_set<-subset(heart,split=TRUE)
testing_set<-subset(heart,split=FALSE)

# Now we use the training set to fit the Model #
regressor<-lm(formula=heart.disease~smoking+biking,training_set)
summ<-summary(regressor)
summ
# the formula is y=14.96+0.18*Smoking-0.2*Biking

# RMSE Calculation #
paste("MSE: ",mean(summ$residuals^2))
#This MSE is very low which means there are very little errors between the data and the predicted values.
# The R^2 is .9765 which means almost 98% of the variation in the data is explained by the model. This is very good.

# Testing the Model with Testing Set #
y_pred<-predict(regressor,newdata=testing_set)
data2<-data.frame(testing_set$heart.disease,y_pred)
head(data2)

# Validation
new<-read_csv('Heart_validation.csv')
new_x<-new[c(1,2)]
new_x
validation_results<-data.frame(new[c(3)],predict(regressor,newdata=new_x))
# Save a table
write.csv(validation_results, "ValidationSet_Prediction.csv")
