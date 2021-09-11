# decision tree
library(tidyverse)
pacman::p_load(rpart,rpart.plot)

# let use mtcars for finding decision based on mpg
mt=mtcars
str(mt)


# classification tree two value am and vs are both categorical 


mt$am=factor(mt$am)
factor(mt$am)


# predicting am with other variable(auto/manual)
dt1=rpart(am~.,mt,method="class")
dt=rpart(am~mpg,mt,method="class")
rpart.plot(dt1)
rpart.plot(dt)
?rpart.plot
rpart.plot(dt,type=2,extra=104,nn=T)


# > dt1
# n= 32 
# 
# node), split, n, loss, yval, (yprob)
# * denotes terminal node
# 
# 1) root 32 13 0 (0.5937500 0.4062500)  
# 2) mpg< 19.45 17  2 0 (0.8823529 0.1176471) *
#   3) mpg>=19.45 15  4 1 (0.2666667 0.7333333) *
# 
# means that decision is based on split based on mpg less than
# 19.5 say 17 cars automatic 2 is loss or manual 0 denote=automatic
# mpg< 19.45 17  2 0 (0.8823529 0.1176471) 

rpart.rules(dt1,nn=T)   #rules


# predict the datset.seed(
library(caTools)
sample=sample.split(mt,SplitRatio =0.8)
train=subset(mt,sample==T)
test=subset(mt,sample==F)

mtpredict=rpart.predict(dt,train,rules=T)
mtpredict




p=data("ptitanic")
# predict the survival or not 
pt=rpart(survived~.,ptitanic)
pt
rpart.plot(pt,cex=0.8)
rpart.rules(pt,nn=T)

# create sample dataset
sampl1=sample.split(ptitanic,SplitRatio = 0.7)
trainp=subset(ptitanic,sampl1==T)
testp=subset(ptitanic,sampl1==F)


pt1=rpart(survived~.,trainp)
rpart.plot(pt1,nn=T,extra=104,cex=0.8)

rpart.rules(pt1,nn=T)



# lets prune the dataset

printcp(pt1) #cp is parameter

pt2=prune(pt1,cp=.01)
pt2

rpart.plot(pt2,type=2,extra=104,cex=0.9)

pt2=prune(pt1,cp=.01)

# select another lowe cp value 

pt3=prune(pt1,cp=0.18)
rpart.plot(pt3,type=2,extra=104,cex=0.9)
