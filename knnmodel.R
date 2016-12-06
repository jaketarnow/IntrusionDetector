



## K-Nearest Neighbors
library(ISLR)
library(MASS)
library(e1071)

?knn
attach(Smarket)
## 
Xlag=cbind(Lag1,Lag2)
## training set 
train= ...

## we should perform KNN on number of neighbours (k) = 1 first, more flexible fit giving low variance but high bias
knn.pred=knn(Xlag[train,],Xlag[!train,],Direction[train],k=1)
table(knn.pred,Direction[!train])
mean(knn.pred==Direction[!train])

## KNN on k = 9 which is a high k value, provides low flexibility (high variance) but low bias
knn.pred=knn(Xlag[train,],Xlag[!train,],Direction[train],k=9)
table(knn.pred,Direction[!train])
mean(knn.pred==Direction[!train])

## Depending on results of 1 and 9 we should run a k value in between to see if we get better results
knn.pred=knn(Xlag[train,],Xlag[!train,],Direction[train],k=9)
table(knn.pred,Direction[!train])
mean(knn.pred==Direction[!train])



