
library(readr)
creditcard <- read_csv("D:/creditcard.csv.zip")  



str(creditcard)
creditcard$Class=factor(creditcard$Class , levels = c(0,1))
summary(creditcard)
sum(is.na(creditcard))


table(creditcard$Class)
prop.table(table(creditcard$Class))


labels=c("legit","fraud")
labels=paste(labels,round(100*prop.table(table(creditcard$Class)),2))
labels
labels=paste(labels,"%")


pie(table(creditcard$Class),labels,col= c("orange","red"),
    main="pie chart of credut card transaction")


predictions=rep.int(0,nrow(creditcard))
predictions=factor(predictions,levels=c(0,1))
predictions

#install.packages('caret')
library(caret)
confusionMatrix(data=predictions,reference=creditcard$Class)

library(rpart)
library(rpart.plot)
decisionTree_model = rpart(Class ~ . , creditcard, method = 'class')
predicted_val = predict(decisionTree_model, creditcard, type = 'class')
probability = predict(decisionTree_model, creditcard, type = 'prob')
rpart.plot(decisionTree_model)




library(dplyr)

set.seed(1)
creditcard=creditcard %>% sample_frac(0.1)

table(creditcard$Class)



library(ggplot2)

ggplot(data =creditcard, aes(x= V1 , y= V2, col = Class)) +
  geom_point() +
  theme_bw() +
  scale_color_manual(values=c('dodgerblue2','red'))



#install.packages('caTools')
library(caTools)

set.seed(123)
data_sample = sample.split(creditcard$Class,SplitRatio = 0.80)
data_sample

train_data=subset(creditcard,data_sample==TRUE)

test_data=subset(creditcard,data_sample==FALSE)

dim(train_data)
dim(test_data)


table(train_data$Class)

n_legit=22750
new_frac_legit=0.50
new_n_total=n_legit/new_frac_legit
new_n_total


library(neuralnet)
ANN_model =neuralnet (Class~.,train_data,linear.output=FALSE)
plot(ANN_model)
predANN=compute(ANN_model,test_data)
resultANN=predANN$net.result
resultANN=ifelse(resultANN>0.5,1,0)



#install.packages('ROSE')
library(ROSE)

oversampling_result=ovun.sample(Class ~ .,
                                data=train_data,
                                method="over",
                                N=new_n_total,
                                seed=2019)

oversampled_credit=oversampling_result$data

table(oversampled_credit$Class)

ggplot(data=oversampled_credit,aes(x=V1 , y=V2,col=Class)) +
  geom_point(position = position_jitter(width =0.2)) +
  theme_bw() +
  scale_color_manual(values=c('dodgerblue2','red'))



#Random under-sampling

table(train_data$Class)

n_fraud=35
new_frac_fraud=0.50
new_n_total=n_fraud/new_frac_fraud



#library(ROSE)
undersampling_result=ovun.sample(Class ~ .,
                                 data=train_data,
                                 method="under",
                                 N=new_n_total,
                                 seed=2019)

undersampled_credit=undersampling_result$data

table(undersampled_credit$Class)

ggplot(data=undersampled_credit,aes(x=V1 , y=V2 ,col= Class)) +
  geom_point() +
  theme_bw() +
  scale_color_manual(values=c('dodgerblue2' , 'red'))



#ROS and RUS

n_new=nrow(train_data)
fraction_fraud_new=0.50

sampling_result=ovun.sample(Class ~ .,
                            data=train_data,
                            method="both",
                            N=n_new,
                            p= fraction_fraud_new,
                            seed=2019)

sampled_credit=sampling_result$data

table(sampled_credit$Class)

prop.table(table(sampled_credit$Class))

ggplot(data=sampled_credit,aes(x=V1 , y=V2,col=Class)) +
  geom_point(position = position_jitter(width =0.2)) +
  theme_bw() +
  scale_color_manual(values=c('dodgerblue2','red'))




#using SMOTE to balance the dataset

#install.packages("smotefamily")
library(smotefamily)

table(train_data$Class)

n0=22750
n1=35
r0=0.6

ntimes = ((1-r0) / r0) * (n0 / n1)-1

smote_output= SMOTE(X=train_data[ , - c(1,31)],
                    target = train_data$Class,
                    K = 5,
                    dup_size = ntimes)

credit_smote = smote_output$data

colnames(credit_smote)[30]="Class"

prop.table(table(credit_smote$Class))

ggplot(train_data,aes(x=V1 , y=V2 ,col= Class)) +
  geom_point() +
  scale_color_manual(values=c('dodgerblue2' , 'red'))

ggplot(credit_smote,aes(x=V1 , y=V2 ,col= Class)) +
  geom_point() +
  scale_color_manual(values=c('dodgerblue2' , 'red'))


#install.packages('rpart')
#install.packages('rpart.plot')

library(rpart)
library(rpart.plot)

CART_model=rpart(Class ~ . , credit_smote)

rpart.plot(CART_model,extra=0,type=5,tweak=1.2)

predicted_val=predict(CART_model , test_data , type ='class')



library(caret)

confusionMatrix(predicted_val,test_data$Class)

predicted_val=predict(CART_model , creditcard[,-1],type ='class')
confusionMatrix(predicted_val,creditcard$Class)



CART_model=rpart(Class ~ . ,train_data[,-1])

rpart.plot(CART_model , extra=0 ,type=5 ,tweak =1.2)


predicted_val=predict(CART_model , test_data[,-1] ,type='class')

library(caret)

confusionMatrix(predicted_val , test_data$Class)

predicted_val=predict(CART_model , creditcard [, -1] ,type ='class')

confusionMatrix(predicted_val , creditcard$Class)

