

#F:/PackageBugPredictionDataSets/Jedit-pkgCycl-4.3.csv
#F:\OpenSourceProjects\EclipseSrc3.0.udb

data2=NULL

data2<-read.csv("F:\\PackageBugPredictionDataSets\\Eclipse-Package-Metric-3.0.csv")
colnames(data2)

library("dplyr")

attach(data2)
data2

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

all.confusion.tables=NULL
TP=NULL
FN=NULL
FP=NULL
TN=NULL
accuracy=NULL
precission=NULL
recall=NULL

sensitivity
specificity

sensitivity=NULL
specificity=NULL

thresh.pred=NULL
logit.prob=NULL

all.confusion.tables <- list()

colnames(data2)
#all.response <- all.predictor <- aucs <- c()

for(i in 10)
{
  
  
  train <-filter(data2, folds !=i)
  test <- filter(data2, folds==i)
  
  #H.Abdeen+RM Model
 
  #glm.model <- glm((post>0) ~   N+A+Ce+Ca+I+D+pkgCohesion+pkgCoupling+pkgCyclpkgs+pkgCyclDepss,  data=train, family= "binomial")
  
  #RM Model 
  
 #  glm.model <- glm((post>0) ~   N+A+Ce+Ca+I+D,  data=train, family= "binomial")
 
 glm.model <- glm((post>0) ~   N+A+Ce+Ca+I, family= "binomial")
 #glm.model <- glm((post>0) ~   N+A+Ce+Ca+I+PkgMod, family= "binomial")
  # glm.model <- glm((post>0) ~  IIPUD+IIPED+IPCI+PGF+PSC,  data=train, family= "binomial")
 #glm.model <- glm((post>0) ~   N+A+Ce+Ca+I+D+MII+NC+BCF+IC+AIC+S+PPI+APIUP+IIPUD+IIPED+IPCI+PGF+PSC,  data=train, family= "binomial")
  
  
  
  model.pred<-predict(glm.model, newdata=test[,c(-2)])
  
  thresh.pred <-model.pred>=0.50
  
  all.confusion.tables[[i]] <- table(factor(test$post>0, levels=c(F,T)), factor(thresh.pred, levels=c(F,T)))
  
  TN[i]=all.confusion.tables[[i]][1,1]
  FN[i]=all.confusion.tables[[i]][2,1]
  FP[i]=all.confusion.tables[[i]][1,2]
  TP[i]=all.confusion.tables[[i]][2,2]
  
  
  accuracy[i] <- (TP[i] + TN[i]) / (TN[i] + FN[i] + FP[i] + TP[i])
  precission[i] <-  (TP[i]) / (TP[i] + FP[i])
  recall[i] <- TP[i] / (TP[i] + FN[i])
  
  sensitivity[i] <-  (TP[i]) / ( FN[i]+ TP[i])
  specificity[i] <- (TN[i]) / (TN[i]+FP[i]) 
  
  
  
}


test 
train


accuracy
precission
recall
ac<-mean(accuracy)
pr<-mean(precission)
rec<-mean(recall)


ac

pr
rec




