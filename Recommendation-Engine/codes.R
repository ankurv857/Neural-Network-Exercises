#mahadev

getwd()
setwd("/Users/ankur/Documents/Competitions/Receng")

Cust_data = 'CID.csv'
Task_data = 'TID.csv'
func = function(Cust_data,Task_data,TaskID)
  
{
# calling libraries

library(data.table)
library(plyr)
library("arules")
library(reshape)

#import data

CID = fread(Cust_data)
TID = fread(Task_data)

#create a pivot table to form matrix for recommendation engine

piv_cid = cast(CID, CID ~ SKILLID,value = "RATING",fun.aggregate = mean)
piv_tid = cast(TID, SKILLID ~ TASKID,value = "IMPORTANCE",fun.aggregate = mean)

# reindex the importance value as  it has inverse proportion

for ( i in 2:25 )
{
 piv_tid[,i] = (1/piv_tid[,i])
}

# perform the null value treatment

piv_cid[is.na(piv_cid)] = 0
piv_tid[is.na(piv_tid)] = 0

# convert to matrix

a = as.matrix(piv_cid)
b= as.matrix(piv_tid)

# multiply both the matrix

cum = a%*%b

# get the customer with highest index from the final matrix

CustID = which.max(cum[,TaskID])
return(CustID)
}

func('CID.csv','TID.csv',7)
