

.libPaths("//172.28.130.113/Decision Services Share/Ankur Verma/R Packages")
.libPaths()


#initiate libraries

library(QuantPsyc)
library(glmnet)
library(dplyr)
library(sqldf)
library(MASS)
library(data.table)
library(plyr)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rpart.utils)
library(partykit)


getwd()
setwd("~/Elasticity/Meat")

seed = 0
set.seed(seed)

train = fread("Meat_Model_30.csv")

train$avg_price_gap = as.character(train$avg_price_gap)
train$avg_price_gap = ifelse(train$avg_price_gap == 'NULL' , -9999999 ,train$avg_price_gap )
train$avg_price_gap = (as.numeric(train$avg_price_gap))

func = function(x) length(x)
train = ddply(train ,"cid", transform , cnt = func(cid) )
train = subset(train , cnt > 5 )


map = unique(train$cid)
map = data.frame(map)
map1 = data.frame(row = 1:nrow(map) ,cid = map$map )
train = merge(train,map1)

train1 = subset(train, row == 1855 )

train_1 = subset(train1, cal_week < 11601)
test = subset(train1,cal_week < 11601)

train1 = subset(train1 , volume_per_daystore1 > 0 )
test = subset(test,volume_per_daystore1 > 0 )

train2  = train1

test1 = subset(test,cal_week < 11601)
test2  = test1

trainx = train2[c(3,5,8:20,41)]			

fita <- rpart(volume_per_daystore1 ~ ., data = trainx , method="anova" , control=rpart.control(minsplit=2, cp=0.005))
rpart.plot(fita)

MyID <- row.names(fita$frame)
Node_Label1 = data.frame(MyID ,  fita$frame)
Node_Label1 = subset(Node_Label1 , var != "<leaf>")
rule_1 = rpart.subrules.table(fita)
rule_1$Subc =  gsub('[[:digit:]]+', '', rule_1$Subrule)
rule_1Subn =  gsub('[[:alpha:]]+', '', rule_1$Subrule)
rule_1d = subset(rule_1 , Subc == "L" )
rule_1d = data.frame(rule_1d , Node_Label1)
rule_1d1 = subset(rule_1d, Variable == "unique_retail_price")
rule_1d1$Greater = as.character(rule_1d1$Greater)
rule_1d1$Greater = as.numeric((rule_1d1$Greater))
rule_1d1$Greater[is.na(rule_1d1$Greater)] = 999999
rule_1d1 = subset(rule_1d1, Greater != 999999 )
rule_1d1$MyID = as.character(rule_1d1$MyID)
rule_1d1$MyID = as.numeric(rule_1d1$MyID)
Node_Label = subset(rule_1d1, MyID > 2)
Node_Label_d = subset(rule_1d1, MyID <= 2) 
Node_Label = Node_Label[c(7)]
Node_Label_d = Node_Label_d[c(7)]

get_node_date <- function(tree = a, node = b ){
  rule <- path.rpart(tree, node)
  rule_2 <- sapply(rule[[1]][-1], function(x) strsplit(x, '(?<=[><=])(?=[^><=])|(?<=[^><=])(?=[><=])', perl = TRUE))
  flag = 1
  ind <- apply(do.call(cbind, lapply(rule_2, function(x) eval(call(x[2], trainx[,x[1]], as.numeric(x[3]))))), 1, all)
  trainx$flag = flag
  if(length(ind[(which(ind==TRUE))])>0)
  {
    trainx[ind,] 
  }else
    trainx
}

if ( nrow(Node_Label) >0)
{
  Base_Node  = get_node_date(tree = fita , node = Node_Label[1,1] )
  Base_Node$Index = Node_Label[1,1] 
}else
{
  Base_Node = trainx
  Base_Node$Index = 0
  Base_Node$flag = 0
}
Base_Node = subset(Base_Node , flag == 10) 

if ( nrow(Node_Label) >= 1 )
{
  for ( i in 1 : nrow(Node_Label) )
  {
    j = Node_Label[i,1]
    Int_Node  = get_node_date(tree = fita , node = j )
    Int_Node$Index = j
    Base_Node = rbind(Base_Node,Int_Node)
  }
}else
{
  Base_Node = trainx
  Base_Node$flag = 0
  Base_Node$Index = 0
}

Base_Node = ddply(Base_Node ,"Index", transform , cnt = func(Index) )


if (nrow(Node_Label_d) > 0)
{
  Node_F = trainx
  Node_F$flag = 1
}else
{
  Node_F = trainx
  Node_F$flag = 0
}

Node_F$Index = 1
Node_F$cnt = nrow(Node_F)

dummy = Node_F[1:5,]
dummy$unique_retail_price = 1
dummy$Index = 0 
dummy$cnt = nrow(dummy) 

Base_Node = rbind(Base_Node , Node_F)
Base_Node_P = subset(Base_Node , flag > 0 )


Base_Node_P1 = subset(Base_Node_P , cnt == max(Base_Node_P$cnt))
Base_Node_D = rbind(dummy , Base_Node_P1)
Base_Node_D1 = subset(Base_Node_D , cnt == max(Base_Node_D$cnt))

#Modelling

train2 = Base_Node_D1[c(1:16)]			

trainx <- as.matrix(data.frame(log(train2$unique_retail_price) ,
                               train2$weekend_days,	train2$jan_days,	train2$feb_days,	train2$mar_days,	train2$apr_days,	train2$may_days,	train2$jun_days,
                               train2$jul_days,	train2$aug_days,	train2$sep_days,	train2$oct_days,	train2$nov_days,	train2$dec_days,
                               train2$significant_snow_ind))


cv.glmmod <- cv.glmnet(trainx,y =  log(train2$volume_per_daystore1) ,alpha=0)
plot(cv.glmmod)
best_lambda <- cv.glmmod$lambda.min

fit <- glmnet( trainx ,y =  log(train2$volume_per_daystore1) 
               , family="gaussian", alpha=0, lambda=best_lambda)

coef(fit)
coef1 = data.frame(Variable = dimnames(coef(fit))[[1]], Beta = matrix(coef(fit)))
coef1 = subset(coef1 , Beta != 0)

final = data.frame(cbind(upc = train1[1,1]  ,coef1 , best_lambda))

for (i in 1856:2074) {
  print(i)
  train1 = subset(train, row == i )
  train_1 = subset(train1, cal_week < 11601)
  test = subset(train1,cal_week < 11601)
  
  train1 = subset(train1 , volume_per_daystore1 > 0 )
  test = subset(test,volume_per_daystore1 > 0 )
  
  train2  = train1
  
  test1 = subset(test,cal_week < 11601)
  test2  = test1
  
  trainx = train2[c(3,5,8:20,41)]			
  
  fita <- rpart(volume_per_daystore1 ~ ., data = trainx , method="anova" , control=rpart.control(minsplit=2, cp=0.005))
  
  
  MyID <- row.names(fita$frame)
  Node_Label1 = data.frame(MyID ,  fita$frame)
  Node_Label1 = subset(Node_Label1 , var != "<leaf>")
  rule_1 = rpart.subrules.table(fita)
  rule_1$Subc =  gsub('[[:digit:]]+', '', rule_1$Subrule)
  rule_1Subn =  gsub('[[:alpha:]]+', '', rule_1$Subrule)
  rule_1d = subset(rule_1 , Subc == "L" )
  rule_1d = data.frame(rule_1d , Node_Label1)
  rule_1d1 = subset(rule_1d, Variable == "unique_retail_price")
  rule_1d1$Greater = as.character(rule_1d1$Greater)
  rule_1d1$Greater = as.numeric((rule_1d1$Greater))
  rule_1d1$Greater[is.na(rule_1d1$Greater)] = 999999
  rule_1d1 = subset(rule_1d1, Greater != 999999 )
  rule_1d1$MyID = as.character(rule_1d1$MyID)
  rule_1d1$MyID = as.numeric(rule_1d1$MyID)
  Node_Label = subset(rule_1d1, MyID > 2)
  Node_Label_d = subset(rule_1d1, MyID <= 2) 
  Node_Label = Node_Label[c(7)]
  Node_Label_d = Node_Label_d[c(7)]
  
  get_node_date <- function(tree = a, node = b ){
    rule <- path.rpart(tree, node)
    rule_2 <- sapply(rule[[1]][-1], function(x) strsplit(x, '(?<=[><=])(?=[^><=])|(?<=[^><=])(?=[><=])', perl = TRUE))
    flag = 1
    ind <- apply(do.call(cbind, lapply(rule_2, function(x) eval(call(x[2], trainx[,x[1]], as.numeric(x[3]))))), 1, all)
    trainx$flag = flag
    if(length(ind[(which(ind==TRUE))])>0)
    {
      trainx[ind,] 
    }else
      trainx
  }
  
  if ( nrow(Node_Label) >0)
  {
    Base_Node  = get_node_date(tree = fita , node = Node_Label[1,1] )
    Base_Node$Index = Node_Label[1,1] 
  }else
  {
    Base_Node = trainx
    Base_Node$Index = 0
    Base_Node$flag = 0
  }
  Base_Node = subset(Base_Node , flag == 10) 
  
  if ( nrow(Node_Label) >= 1 )
  {
    for ( i in 1 : nrow(Node_Label) )
    {
      j = Node_Label[i,1]
      Int_Node  = get_node_date(tree = fita , node = j )
      Int_Node$Index = j
      Base_Node = rbind(Base_Node,Int_Node)
    }
  }else
  {
    Base_Node = trainx
    Base_Node$flag = 0
    Base_Node$Index = 0
  }
  
  Base_Node = ddply(Base_Node ,"Index", transform , cnt = func(Index) )
  
  
  if (nrow(Node_Label_d) > 0)
  {
    Node_F = trainx
    Node_F$flag = 1
  }else
  {
    Node_F = trainx
    Node_F$flag = 0
  }
  
  Node_F$Index = 1
  Node_F$cnt = nrow(Node_F)
  
  dummy = Node_F[1:5,]
  dummy$unique_retail_price = 1
  dummy$Index = 0 
  dummy$cnt = nrow(dummy) 
  
  Base_Node = rbind(Base_Node , Node_F)
  Base_Node_P = subset(Base_Node , flag > 0 )
  
  
  Base_Node_P1 = subset(Base_Node_P , cnt == max(Base_Node_P$cnt))
  Base_Node_D = rbind(dummy , Base_Node_P1)
  Base_Node_D1 = subset(Base_Node_D , cnt == max(Base_Node_D$cnt))
  
  #Modelling
  
  train2 = Base_Node_D1[c(1:16)]			
  
  trainx <- as.matrix(data.frame(log(train2$unique_retail_price) ,
                                 train2$weekend_days,	train2$jan_days,	train2$feb_days,	train2$mar_days,	train2$apr_days,	train2$may_days,	train2$jun_days,
                                 train2$jul_days,	train2$aug_days,	train2$sep_days,	train2$oct_days,	train2$nov_days,	train2$dec_days,
                                 train2$significant_snow_ind))
  
  
  cv.glmmod <- cv.glmnet(trainx,y =  log(train2$volume_per_daystore1) ,alpha=0)
  plot(cv.glmmod)
  best_lambda <- cv.glmmod$lambda.min
  
  fit <- glmnet( trainx ,y =  log(train2$volume_per_daystore1) 
                 , family="gaussian", alpha=0, lambda=best_lambda)
  
  coef(fit)
  coef1 = data.frame(Variable = dimnames(coef(fit))[[1]], Beta = matrix(coef(fit)))
  coef1 = subset(coef1 , Beta != 0)
  
  result = data.frame(cbind(upc = train1[1,1]  ,coef1 ,  best_lambda ))
  final = rbind(final,result)
}

write.csv(final , "meat_rpart_test5.csv" , row.names = FALSE)

