#Random Forest Assignment
read.csv("/Users/amandeep/Downloads/customer_churn.csv", stringsAsFactors = T) -> customers
names(customers)
library(caTools)
sample.split(customers$gender,SplitRatio = 0.65)-> split_tag
table(split_tag)
subset(customers,split_tag==TRUE) -> training
nrow(training)
subset(customers,split_tag==FALSE) -> testing
nrow(testing)
install.packages("randomForest")
library(randomForest)
randomForest(gender~MonthlyCharges+tenure, data = training,ntree = 35 ,mtry = 2) ->mod_forest1
class(customers$gender)
importance(mod_forest1)
plot(mod_forest1)
varImpPlot(mod_forest1)
#Predicting
predict(mod_forest1,newdata = testing, type ="class")->result_forest1
table(testing$gender,result_forest1) ->ac
#accuracy
sum(diag(ac))/sum(ac)

#QUESTION 2
randomForest(gender~MonthlyCharges+tenure, data = training,ntree = 350 ,mtry = 2) ->mod_forest2
predict(mod_forest2,newdata = testing, type ="class")->result_forest2
importance(mod_forest1)
plot(mod_forest1)
varImpPlot(mod_forest1)
table(testing$gender,result_forest2) ->ac
#accuracy
sum(diag(ac))/sum(ac)
