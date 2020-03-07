


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
library(rattle)


getwd()
setwd("/Users/ankur/Documents/Walmart/Elasticity/Meat")

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


train1 = subset(train, row == 494 )

train_1 = subset(train1, cal_week < 11601)
test = subset(train1,cal_week < 11601)

train1 = subset(train1 , volume_per_daystore1 > 0 )
test = subset(test,volume_per_daystore1 > 0 )

train2  = train1

test1 = subset(test,cal_week < 11601)
test2  = test1

trainx = train2[c(3,5,8:55)]			

fita <- rpart(volume_per_daystore1 ~ ., data = trainx , method="anova" , control=rpart.control(minsplit=2, cp=0.005))
#rpart.plot(fita)
fancyRpartPlot(fita)


MyID <- row.names(fita$frame)
Node_Label = data.frame(MyID)
Node_Label$MyID = as.character(Node_Label$MyID)
Node_Label$MyID = as.numeric(Node_Label$MyID)
Node_Label = subset(Node_Label, MyID > 3)


get_node_date <- function(tree = a, node = b ){
  rule <- path.rpart(tree, node)
  rule_2 <- sapply(rule[[1]][-1], function(x) strsplit(x, '(?<=[><=])(?=[^><=])|(?<=[^><=])(?=[><=])', perl = TRUE))
  rule_3 <- rpart.rules.table(fita)
  rule_3 = data.frame(rule_3)
  rule_3 = subset(rule_3 , Rule == node )
  rule_3$Subrule = gsub('[[:digit:]]+', '', rule_3$Subrule)
  rule_3$Leaf = as.character(rule_3$Leaf)
  rule_3$F1 = ifelse(rule_3$Subrule == "R" & rule_3$Leaf == "TRUE" , 1,0)
  rule_3$F2 = ifelse(rule_3$Subrule == "L" & rule_3$Leaf == "FALSE" , 1,0)
  rule_3$F3 = rule_3$F1 + rule_3$F2
  pre = as.numeric(length(rule_2))
  for (i in length(rule_2) :length(rule_2)) {
    if (rule_2[[length(rule_2)]][1] == "unique_retail_price" && rule_2[[length(rule_2)]][2] == ">=" && rule_3[nrow(rule_3) , 6] == 1 )
    {
      rule_2[[length(rule_2)]] = NULL
    }
  } 
  pos = as.numeric(length(rule_2))
  flag = pre - pos
  ind <- apply(do.call(cbind, lapply(rule_2, function(x) eval(call(x[2], trainx[,x[1]], as.numeric(x[3]))))), 1, all)
  trainx$flag = flag
  trainx[ind,]
}

Base_Node  = get_node_date(tree = fita , node = Node_Label[1,1] )
Base_Node$Index = Node_Label[1,1] 

for ( i in 2 : nrow(Node_Label) )
{
  j = Node_Label[i,1]
  Int_Node  = get_node_date(tree = fita , node = j )
  Int_Node$Index = j
  Base_Node = rbind(Base_Node,Int_Node)
}

Base_Node = ddply(Base_Node ,"Index", transform , cnt = func(Index) )
Base_Node$Flag1 = Base_Node$flag
Base_Node$flag = NULL

Node_F = trainx
Node_F$Index = 2
Node_F$cnt = nrow(Node_F)

rule <- path.rpart(fita, 2)
rule_2 <- sapply(rule[[1]][-1], function(x) strsplit(x, '(?<=[><=])(?=[^><=])|(?<=[^><=])(?=[><=])', perl = TRUE))
rule_3 <- rpart.rules.table(fita)
rule_3 = data.frame(rule_3)
rule_3 = subset(rule_3 , Rule == 2 )
rule_3$Subrule = gsub('[[:digit:]]+', '', rule_3$Subrule)
rule_3$Leaf = as.character(rule_3$Leaf)
rule_3$F1 = ifelse(rule_3$Subrule == "R" & rule_3$Leaf == "TRUE" , 1,0)
rule_3$F2 = ifelse(rule_3$Subrule == "L" & rule_3$Leaf == "FALSE" , 1,0)
rule_3$F3 = rule_3$F1 + rule_3$F2
for (i in length(rule_2) :length(rule_2)) {
  if (rule_2[[length(rule_2)]][1] == "unique_retail_price" && rule_2[[length(rule_2)]][2] == ">=" && rule_3[nrow(rule_3) , 6] == 1 )
  {
    Node_F$Flag1 = 1
  }else
    Node_F$Flag1 = 0
}

Base_Node = rbind(Base_Node , Node_F)
Base_Node_P = subset(Base_Node , Flag1 > 0 )

Base_Node_P$Reg_Type = ifelse(Base_Node_P$cnt > 30 , "Regularized" , "Heuristic")
Base_Node_P_Reg = subset(Base_Node_P , cnt > 30)
Base_Node_P_Heu = subset(Base_Node_P , cnt <= 30)

#Regularized
mapp = unique(Base_Node_P_Reg$Index)
mapp = data.frame(mapp)
mapp1 = data.frame(rower = 1:nrow(mapp) ,Index = mapp$mapp )
Base_Node_P_Reg = merge(Base_Node_P_Reg,mapp1)

Base_Node_P_Reg1 = subset(Base_Node_P_Reg, rower == 3 )

train2 = Base_Node_P_Reg1[c(2:51)]			

trainx <- as.matrix(data.frame(log(train2$unique_retail_price) ,
                               train2$weekend_days,	train2$jan_days,	train2$feb_days,	train2$mar_days,	train2$apr_days,	train2$may_days,	train2$jun_days,
                               train2$jul_days,	train2$aug_days,	train2$sep_days,	train2$oct_days,	train2$nov_days,	train2$dec_days,
                               train2$significant_snow_ind))


cv.glmmod <- cv.glmnet(trainx,y =  log(train2$volume_per_daystore1) ,alpha=0.5)
plot(cv.glmmod)
best_lambda <- cv.glmmod$lambda.min

fit <- glmnet( trainx ,y =  log(train2$volume_per_daystore1) 
               , family="gaussian", alpha=0.5, lambda=best_lambda)

coef(fit)

#Heuristic

mapp = unique(Base_Node_P_Heu$Index)
mapp = data.frame(mapp)
mapp1 = data.frame(rower = 1:nrow(mapp) ,Index = mapp$mapp )
Base_Node_P_Heu = merge(Base_Node_P_Heu,mapp1)

Base_Node_P_Heu1 = subset(Base_Node_P_Heu, rower == 1 )

train2 = Base_Node_P_Heu1[c(2:51)]	

trainx <- as.matrix(data.frame(log(train2$unique_retail_price) ,
                               train2$weekend_days,	train2$jan_days,	train2$feb_days,	train2$mar_days,	train2$apr_days,	train2$may_days,	train2$jun_days,
                               train2$jul_days,	train2$aug_days,	train2$sep_days,	train2$oct_days,	train2$nov_days,	train2$dec_days,
                               train2$significant_snow_ind))


cv.glmmod <- cv.glmnet(trainx,y =  log(train2$volume_per_daystore1) ,alpha=0.5)
plot(cv.glmmod)
best_lambda <- cv.glmmod$lambda.min

fit <- glmnet( trainx ,y =  log(train2$volume_per_daystore1) 
               , family="gaussian", alpha=0.5, lambda=best_lambda)

coef(fit)

