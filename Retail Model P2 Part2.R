

options(scipen=999) # Scientific penalty to show numbers not in scientific format. 999 is the maximum precision in R
options(stringsAsFactors = FALSE,scipen = 999)

library(tidyverse)
library(data.table)
library(caret)
library(ggplot2)
library(e1071)
library(forecast)
library(broom)
library(devtools)
library(vegan)
library(stringr)
library(stringi)
library(psych)
library(vcd)
library(dplyr)
library(randomForest) 
library(tidyr)
library(tree)
library(pROC)
library(cvTools)
library(car)

#Setting working directory
setwd("D:/Data_Science/Edvancer/Data")

#Load train and test data
store_train=read.csv("store_train.csv",stringsAsFactors = F)
store_test=read.csv("store_test.csv",stringsAsFactors = F)

#Data prep
store_test$store=NA
store_train$data='train'
store_test$data='test'
store_all=rbind(store_train,store_test)
glimpse(store_all)

sum(unique(table(store_all$state_alpha)))

store_all$population[is.na(store_all$population)]=round(mean(store_all$population,na.rm=T),0)
store_all$country[is.na(store_all$country)]=round(mean(store_all$country,na.rm=T),0)

CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub("\\/","_",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}

char_logical=sapply(store_all,is.character)
cat_cols=names(store_all)[char_logical]
cat_cols=cat_cols[!(cat_cols %in% c('data','store'))]
cat_cols

for(col in cat_cols){
  store_all=CreateDummies(store_all,col,50)
}

glimpse(store_all)

#Checking missing
store_all=store_all[!((is.na(store_all$store)) & store_all$data=='train'), ]
for(col in names(store_all)){
  if(sum(is.na(store_all[,col]))>0 & !(col %in% c("data","store"))){
    store_all[is.na(store_all[,col]),col]=mean(store_all[store_all$data=='train',col],na.rm=T)
  }
}

any(is.na(store_all))
sum(is.na(store_all))
colSums(is.na(store_all))

store_train=store_all %>% filter(data=='train') %>% select(-data)
store_test=store_all %>% filter(data=='test') %>% select(-data,-store)

#Model Logistic 
set.seed(2)
s=sample(1:nrow(store_train),0.75*nrow(store_train))
train_75=store_train[s,] 
test_25=store_train[-s,]  

for_vif=lm(store~.-Id-sales0-sales2-sales3-sales1,data=train_75)
sort(vif(for_vif),decreasing = T)[1:3]

summary(for_vif)

##Build Logistic Model
fit=glm(store~.-Id-sales0-sales2-sales3-sales1,data=train_75) #32 predictor var
fit=step(fit)

summary(fit)
formula(fit)

fit=glm(store ~ sales4 + CouSub + population + storecode_METRO12620N23019 + 
          storecode_METRO14460MM1120,data=train_75) #32 predictor var

library(pROC)
scoreLG=predict(fit,newdata =test_25,type = "response")
roccurve=roc(test_25$store,scoreLG) 
auc(roccurve)

#Model Decision Tree
train_75_1 <- copy(train_75)
train_75_1$store <- as.factor(train_75_1$store)
train_75_1$Id <- NULL
rf.model3= randomForest(store~.,data=train_75_1)
test.score3=predict(rf.model3,newdata=test_25,type="prob")[,2]
auc(roc(test_25$store,test.score3))

library(cvTools)
store_train$store=as.factor(store_train$store)
glimpse(store_train)

param=list(mtry=c(3,4,6,8,10),
           ntree=c(50,100,200,500,700,800,900), 
           maxnodes=c(5,10,15,20,30,50,100,300,500,600,700),
           nodesize=c(1,2,5,10,20,30,40)       
)

mycost_auc=function(store,yhat){  #Real #Predicted
  roccurve=pROC::roc(store,yhat)
  score=pROC::auc(roccurve)
  return(score)
}  

subset_paras=function(full_list_para,n=10){  #n=10 is default, you can give higher value
  
  all_comb=expand.grid(full_list_para)
  
  s=sample(1:nrow(all_comb),n)
  
  subset_para=all_comb[s,]
  
  return(subset_para)
}

num_trial=50
my_params=subset_paras(param,num_trial)
my_params

myauc=0

for(i in 1:num_trial){  
  #print(paste('starting iteration :',i))
  # uncomment the line above to keep track of progress
  params=my_params[i,]
  
  k=cvTuning(randomForest,
             store~.-Id, 
             data =store_train,
             tuning =params,
             folds = cvFolds(nrow(store_train), K=15, type ="random"),
             cost =mycost_auc, 
             seed =2,
             predictArgs = list(type="prob"))
  
  score.this=k$cv[,2]


  if(score.this>myauc){
    #print(params)
    #uncomment the line above to keep track of progress
    myauc=score.this
    #print(myauc)
    #uncomment the line above to keep track of progress
    #print(myauc)
    best_params=params
  }
  #print('DONE')
}

best_params
##      mtry ntree maxnodes nodesize
## 1076    3   800      500        5

ci.rf.final=randomForest(store~.-Id,
                         mtry=3,
                         ntree=800,
                         maxnodes=500,
                         nodesize=5,
                         data=store_train
)

myauc

test.score_final=predict(ci.rf.final,newdata=store_test, type="prob")[,2]
write.csv(test.score_final,'P2_part2.csv',row.names = F)
