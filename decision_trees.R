#Decision tress
read.csv("G:/datasets/Customer-Churn.csv",stringsAsFactors = T)->customer_churn
na.omit(customer_churn)->customer_churn
library(caTools)
sample.split(customer_churn$Dependents, SplitRatio = 0.70)->split_tag
subset(customer_churn,split_tag==T)->train
subset(customer_churn,split_tag==F)->test

library(tree)
tree(Dependents~Partner, data=train)->mod_tree1
plot(mod_tree1)
text(mod_tree1)

predict(mod_tree1,newdata = test, type="class")->result_tree1
head(result_tree1)
View(result_tree1)

table(test$Dependents,result_tree1)->accuracy
accuracy
acc=sum(diag(accuracy))/sum(accuracy)
acc

#2
sample.split(customer_churn$Dependents, SplitRatio = 0.70)->split_tag
subset(customer_churn,split_tag==T)->train
subset(customer_churn,split_tag==F)->test

library(tree)
tree(Dependents~Partner+InternetService, data=train)->mod_tree1
plot(mod_tree1)
text(mod_tree1)

predict(mod_tree1,newdata = test, type="class")->result_tree1
head(result_tree1)
View(result_tree1)

table(test$Dependents,result_tree1)->accuracy
accuracy
acc=sum(diag(accuracy))/sum(accuracy)
acc

library(rpart)
library(rpart.plot)
rpart(formula=Churn~tenure+MonthlyCharges,data=train_d,method="class")->a
rpart.plot(x=a,type=5,extra=0,tweak=1.5)







