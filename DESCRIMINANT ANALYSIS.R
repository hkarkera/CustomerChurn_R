churn_lda <- churn_knn

indices= sample(nrow(churn_lda), 0.6*nrow(churn_lda))
train_lda = churn_lda[indices, ] #60% of the data
val_lda = churn_lda[-indices,] #40% of the data

## normalizing the data
train_label_lda <- train
val_label_lda <- val
churn_lda_label <- churn_lda



norm<-preProcess(train[,-21],method = c("center","scale"))
train_label_lda[,-21] <- predict(norm,train_lda[,-21])
val_label_lda[,-21] <- predict(norm,val_lda[,-21])
churn_lda_label[,-21] <- predict(norm,churn_knn[,-21])

## creatig linear dercriminant analysis model
install.packages("MASS")
library(MASS)
linear <- lda(Churn ~. , data = train_label_lda)
linear

##  calculating accuracy
predictions <- linear %>% predict(val_label_lda)

mean(predictions$class==val_label_lda$Churn) 

## finding the classification using default cuttoff 0.5
sum(predictions$posterior[ ,1] >=.5)

install.packages("KlaR")
library(klaR)
partimat(Churn~ OnlineSecurity + PaperlessBilling + MonthlyCharges,data=train_label_lda,method="lda") 
