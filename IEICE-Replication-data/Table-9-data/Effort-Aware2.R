


data2=NULL
# Read the data as .csv file 
# File contains package level metrics (Sarkar's Metrics+Robert Martin metrics+PkgReuseindex)
data2 <- read.csv("F:\\ER-Modeling\\Eclipse-Package-Metric-3.0.csv")
#Initiolization of variables. 

data2
colnames(data2)
library("dplyr")
attach(data2)


n.folds<-10
folds <- cut(sample(seq_len(nrow(data2))),  breaks=n.folds, labels=FALSE)
all.confusion.tables
TP
FN
FP
TN
accuracy
precission
recall
MFM
Balance 

all.confusion.tables=NULL
TP=NULL
FN=NULL
FP=NULL
TN=NULL
MFM=NULL
Balance=NULL
accuracy=NULL
precission=NULL
recall=NULL
glm.model=NULL


sensitivity
specificity

sensitivity=NULL
specificity=NULL

thresh.pred=NULL
logit.prob=NULL

all.confusion.tables <- list()

# Loop for 10-fold analysis

for(i in seq_len(n.folds))
{
  
  
  train <-filter(data2, folds !=i)
  test <- filter(data2, folds==i)
  
  

  #Logistic regression model with "RM+M" model
  
  glm.model <- glm((post>0)~ N+A+Ce+Ca+I+D+NC+APIUP+BCF+AC+IC+BCF+SAVI+PPI+MII+SLOC+CUM+CUL,  data=train, family= "binomial")
   
  #Logistic regression model with "RM+M+PkgReuse" model
  glm.model <- glm((post>0)~ N+A+Ce+Ca+I+D+NC+APIUP+BCF+AC+IC+BCF+SAVI+PPI+MII+SLOC+CUM+CUL+PkgMod,  data=train, family= "binomial")
  
  
  model.pred<-predict(glm.model, newdata=test[,-2])
  
  thresh.pred <-model.pred>=0.5
  
  all.confusion.tables[[i]] <- table(factor(test$post>0, levels=c(F,T)), factor(thresh.pred, levels=c(F,T)))
  
  TN[i]=all.confusion.tables[[i]][1,1]
  FN[i]=all.confusion.tables[[i]][2,1]
  FP[i]=all.confusion.tables[[i]][1,2]
  TP[i]=all.confusion.tables[[i]][2,2]
  
  
  accuracy[i] <- (TP[i] + TN[i]) / (TN[i] + FN[i] + FP[i] + TP[i])
  precission[i] <-  (TP[i]) / (TP[i] + FP[i])
  recall[i] <- TP[i] / (TP[i] + FN[i])
  MFM[i]<- (2*recall[i]*precission[i])/(recall[i]+precission[i])
  
  sensitivity[i] <-  (TP[i]) / ( FN[i]+ TP[i])
  specificity[i] <- (TN[i]) / (TN[i]+FP[i]) 

  
  
  
}

# 1. Select maximum F-measure
# MFM
# maxFM=max(MFM)
# maxFM
#2. Re-set the threshold as maximum F-measure

#3. Run the experiment again to determine standard deviation and average values of metric
# sdFM=NULL
# sdFM=sd(MFM)
# sdFM
# avgFM=NULL
# avgFM=mean(MFM)
# avgFM



specificity
sensitivity

blance=NULL


balance= 1- sqrt((0-sensitivity)^2+(1-specificity)^2)
balance=balance/sqrt(2)
maxBalance=max(balance)
maxBalance

balance
avgbalance=mean(balance)
avgbalance
sdblance<-sd(balance)
sdblance


