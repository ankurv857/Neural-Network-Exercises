
#https://en.wikibooks.org/wiki/R_Programming/Text_Processing#Regular_Expressions

setwd("/Users/ankur/Documents/Competitions/Reg")
getwd()

library(stringr)
library(dplyr)

train = read.csv("train.csv")
test = read.csv("test.csv")

test$X = NULL

train = as.data.frame(train)
test = as.data.frame(test)

train$var = 1
test$var = 0

train$mpn_qs = NULL

tt = rbind.data.frame(train,test)

head(tt)
data = tt

data$consol = paste(data$title ,data$product_description , sep =  "  ")

FE = setDT(data)[, tstrsplit(data$consol, ' ')]

FE = data.frame(FE)
str(FE)

FE[1,2]

qwe = FE

for (i in 1:nrow(qwe))
{
  for (j in 1:ncol(qwe))
  {
    qwe[i,j] = ifelse(grepl("^[[:digit:]]+$", qwe[i,j] ) == FALSE , NA, qwe[i,j])
  }
  
}

qwe$MAX <- apply(qwe,1,max,na.rm=TRUE) 




qwe[1,2] = ifelse(grepl("^[[:digit:]]+$", qwe[1,2] ) == FALSE , NA, qwe[1,2])

head(qwe[2])

asf = data$consol %>% str_match_all("[0-9]+")
asf[1]

data$asd = strsplit(data$consol, " ")
head(data)

FE$numbers <- grepl("^[[:digit:]]+$", FE$V2) 
head(FE$numbers)

records_sep <- strsplit(x=data$consol, " ")
as = as.vector(records_sep[1])
as

as = c(FE[1,])
as
numbers <- grepl("^[[:digit:]]+$", as) 
letters <- grepl("^[[:alpha:]]+$", as) 
both <- grepl("^[[:digit:][:alpha:]]+$", as) 
as[xor((letters | numbers), both)]
asd


mywords = "123-AWE"
mywords
numbers <- grepl("[[:digit:]]", mywords) 

numbers <- !grepl("^[.*[:digit:].*]+$", mywords) 
letters <- grepl("^[[:alpha:]]+$", mywords) 
both <- agrepl("^[[:digit:][:alpha:]]+$", mywords) 
numbers

mywords<- c("harry","met","sally","subway 10","1800Movies","12345")
mywords
numbers <- grepl("^[[1:9]]+$", mywords) 
letters <- grepl("^[[:alpha:]]+$", mywords) 
both <- grepl("^[[:digit:][:alpha:]]+$", mywords) 
mywords[xor((letters | numbers), both)]

wer = (gsub("([0-9]+).*$", "\\1", records_sep))
wer

wer1 = regmatches(records_sep, gregexpr("[[:digit:]]+", records_sep))
wer1[1]
records1_L <- strsplit(as, ",")

nrow(asd)

alphanum = grep(pattern = "[:digit:]",asd[1],value = TRUE)
alphanum
asd[1]


data$extract = gsub("[:punct:]" ," ", data$consol)

regmatches(data$consol, gregexpr("((?![0-9]+)[A-Za-z0-9]+)", perl = TRUE)[[1L]]
           
         
           
