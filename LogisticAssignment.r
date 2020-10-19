#Logistic Reg
glm(Churn~TechSupport, data =c_churn, family="binomial") ->log_mod_1
summary(log_mod_1)
predict(log_mod_1, data.frame(TechSupport = "Yes") , type = "response" )
predict(log_mod_1, data.frame(TechSupport = "No") , type = "response" )
predict(log_mod_1, data.frame(TechSupport = "No internet service") , type = "response" )

#question 2
glm(Churn~tenure, data =c_churn, family="binomial") ->log_mod_2
summary(log_mod_2)
table(c_churn$tenure)
predict(log_mod_2, data.frame(tenure = 10) , type = "response" )
predict(log_mod_2, data.frame(tenure = 50) , type = "response" )
predict(log_mod_2, data.frame(tenure = 70) , type = "response" )

#Other question Multiple Rea
sample.split(c_churn$gender,SplitRatio = 0.65)->tag
subset(c_churn , tag == T) -> train1
subset(c_churn , tag == F) -> test1
nrow(test1)
nrow(train1)
glm(gender~ Dependents+InternetService+Contract, data = train1, family = "binomial") ->log_mod_multi
predict(log_mod_multi,newdata = test1 ,type = "response") ->result_log_multi
range(result_log_multi)
table(test1$gender,result_log_multi > 0.50)->acc
#checking accuracy
sum(diag(acc)/sum(acc))


#Question 2 for multiple
glm(gender~ tenure+MonthlyCharges+PaymentMethod, data = train1, family = "binomial") ->log_mod_multi2
predict(log_mod_multi2,newdata = test1 ,type = "response") ->result_log_multi2
range(result_log_multi2)
table(test1$gender,result_log_multi2 > 0.49)->acc1
sum(diag(acc1)/sum(acc1))

#ROCR
sample.split(c_churn$Churn,SplitRatio = 0.80)->tag2
View(tag2)
subset(c_churn , tag2 == T) -> train2
subset(c_churn , tag2 == F) -> test2
glm(Churn~ tenure+MonthlyCharges+TechSupport, data = train2, family = "binomial") ->log_mod_roc
predict(log_mod_roc, newdata = test2, type="response")->result_log_roc
range(result_log_roc)
table(test2$Churn,result_log_roc > 0.55)->acc2
acc2
sum(diag(acc1)/sum(acc1))
install.packages("ROCR")
library("ROCR")
prediction(result_log_roc, test2$Churn) -> pridict_log_roc
performance(pridict_log_roc,"acc")->acc
plot(acc)
performance(pridict_log_roc,"auc")->auc
auc@y.values
auc@y.name
auc@x.values
