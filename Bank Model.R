options(scipen = 999,stringsAsFactors = F)

getwd()


#####################

b_train = read.csv("C:\\Users\\Vikas Sharma\\Downloads\\R Projects\\R Projects\\bank-full_train.csv", stringsAsFactors = FALSE)

b_train

head(b_train)


####################

b_test = read.csv("C:\\Users\\Vikas Sharma\\Downloads\\R Projects\\R Projects\\bank-full_test.csv", stringsAsFactors = FALSE)

b_test

head(b_test)

###########################

str(b_train)

str(b_test)


###### for showing the how many rows and coloumn are present

dim(b_train)

dim(b_test)


names(b_train)
names(b_test)


##################################

setdiff(names(b_train) , names(b_test))

#######################



##### adding the one more coloumn b_test$y=NA cz this is dependant vairable
#####          ( adding in test coloumn )

b_test$y=NA


names(b_train)

names(b_test)



#### adding the one more coloumn in the train data set as name data

b_train$data = 'Train'


### for adding the one more coloumn in the test data set as the name data

b_test$data = 'Test'

#### now checking the dimensions again

dim(b_train)

dim(b_test)



##### showing the content of y and their unique values


table(b_train$y)

prop.table(table(b_train$y))


##### for combining both the data set we use rbind function


df = rbind(b_train , b_test)

df

head(df)


dim(df)


######### use str()

str(df)


##### calculating the how many missing values are present in entire data set

lapply(df , function(x) sum(is.na(x)))



table(b_train$y)

table(df$data)


####### now i am replacing the target variables with yes with 1 and with no with 0

table(df$y)

df$y=ifelse(df$y=="yes",1,0)

df$y=as.numeric(df$y)

table(df$y)

str(df)



###### calculating how many variables are in character format

lapply(df , function(x) sum(is.character(x)))


########### using sapply


names(df)[sapply(df , function(x) is.character(x))]


############################

library(dplyr)


quantile(df$age)


df=df %>% 
  mutate(age_LT_EQ_33=as.numeric(age<=33),
         age_33_39=as.numeric(age>33 & age <=39),
         age_39_48=as.numeric(age>39 & age <=48),
         age_GT_48=as.numeric(age>48 & age <=95)) %>% 
  select(-age) 



table(df$age_LT_EQ_33)
table(df$age_33_39)
table(df$age_39_48)
table(df$age_GT_48)


str(df)


########################

quantile(df$duration)


df=df %>% 
  mutate( duration_LT_EQ_103=as.numeric(duration <= 103),
          duration_103_180=as.numeric(duration>103 & duration <=180),
          duration_200_300=as.numeric(duration>200 & duration <=300),
          duration_180_319=as.numeric(duration>180 & duration <=319),
          duration_GT_319=as.numeric(duration>319 & duration <=4918)) %>% 
  select(-duration)


table(df$duration_LT_EQ_103)
table(df$duration_103_180)
table(df$duration_200_300)
table(df$duration_180_319)
table(df$duration_GT_319)


str(df)

###################################

table(df$day)

table(df$pdays)

table(df$previous)



quantile(df$day)

quantile(df$pdays)

quantile(df$previous)

##################################

quantile(df$pdays)

df=df %>%
  mutate(pdays_cnpc = ifelse(pdays == -1, 1, 0),
         pdays_cnpc = as.numeric(pdays_cnpc)) %>% 
  select(-pdays,-day,-previous) 


table(df$pdays_cnpc)


str(df)



###### calculating how many variables are in character format

names(df)[sapply(df , function(x) is.character(x))]


table(df$campaign)


##########################


cat_cols = c(
  "job",
  "marital",
  "education",
  "default",
  "housing",
  "loan",
  "contact",
  "poutcome",
  "month",
  "campaign"

)



############# creating the dummies function


CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var]) ## getting the table for the variable(any categorical variable)
  t=t[t>freq_cutoff] ## cutoff is the frequency of occurance of a variable default is 0 , but ideally it should be atleast 15-20% of actual data,
  ## so here whatever categories which are less than that cut off frequencies are dropped(no dummes are created for them)
  t=sort(t) ## sort the data
  categories=names(t)[-1] ## pick everything but exclude the first as it has lowest frequency: REmember its n-1
  
  for( cat in categories){
    name=paste(var,cat,sep="_") ## Inside the for loop create a name separated by name of variable and category separeted by "_" underscore
    name=gsub(" ","",name) ## replace any spaces if there is found in categoreis of variables
    name=gsub("-","_",name) ## replace any dash if found in categories to underscropes: e.g. 'Cat-1', 'Cat-2' will be 'Cat_1', 'Cat_2'
    name=gsub("\\?","Q",name) ## any question mark is converted to 'Q'
    name=gsub("<","LT_",name) ## Less than sign is converted to LT_
    name=gsub("\\+","",name) ## + sign is removed
    name=gsub("\\/","_",name) ## "/" is replaced with "_"
    name=gsub(">","GT_",name) ## ">" is replaced with 'GT_'
    name=gsub("=","EQ_",name) ## '=' is replaced with 'EQ_'
    name=gsub(",","",name) ##  ',' is replaced with ''
    name=gsub("default","On_LOAD",name) 
    data[,name]=as.numeric(data[,var]==cat) ## changing to numeric type
  }
  
  data[,var]=NULL
  return(data)
}




## picking all the character columns and creating dummies


for(cat in cat_cols){
  df=CreateDummies(df,cat,700)
}


str(df)

glimpse(df)


################################

# as this data was coming in alias hence removed

df=df %>% 
  select(-age_GT_48,-duration_GT_319) 


glimpse(df)


##############################


######## again splitting for the modeling

b_train=df %>% 
  filter(data=='Train') %>% 
  select(-data)


names(b_train)

head(b_train)




b_test=df %>% 
  filter(data=='Test') %>% 
  select (-data,-y)

names(b_test)


head(b_test)

############################

###### splitting the data 80%-20%


set.seed(2)

s=sample(1:nrow(b_train),0.75*nrow(b_train))

b_train1=b_train[s,]
b_train1


b_train2=b_train[-s,]
b_train2


#######################

glimpse(b_train1)

############################

dim(b_train)

dim(b_train1)

dim(b_train2)

###############################


##### creating the formula for whole model

##### taking all the value as independent variable except y and ID

form =
  as.formula(paste0('y ~ ', paste0(setdiff(
    names(b_train1), c('y', 'ID')
  ), collapse = ' + ')))



######################


library(car)


## For VIF calculation; we create a basic linear model

for_vif=lm(form,data=b_train1)

for_vif


## find the first value of vif, if you found it to be greater than 4 then use the below code

vif(for_vif)

sort(vif(for_vif),decreasing = T)


###############################

alias(lm(form,data=b_train1))$Complete

#######################


## k is the vif value initialized to a very high value
k = 100000000
## appended_dropped is a sentinal value which will change with k so that we don't run into infinite loop
appended_dropped = c('y')

## loop will run untill all the values have vif lower than 4
while(k > 4){
  for_vif=lm(form,data=b_train1) ## first a linear model for understanding the linear combination
  k <- sort(vif(for_vif),decreasing = T)[1] ## get the value of vif for highest value
  if (k <= 4){
    break
  }
  var_dropped <- names(k) ## get the name of the variable which has highest value
  print(k)
  appended_dropped <- c(var_dropped, appended_dropped) ## update the sentinal value with the new variable which needs to be dropped
  form <-
    as.formula(paste0('y ~ ', paste0(setdiff(
      names(b_train1), c('y', 'ID', appended_dropped)
    ), collapse = ' + '))) ## update the formula everytime
}


#####   poutcome_unknown 
##          878.4464 
##        job_management 
#          8.11334 
#        education_secondary 
#            6.86213 
#        month_may 
#          6.843654 
#        contact_unknown 
#            5.217955 
#        pdays_cnpc 
#            4.971353 
#        campaign_1 
#          4.447453


## Always remember VIF is an iterative process for variable removal. Never ever use it at once
## VIF doesn't depend on dependent variable, Even if you change the dependent variable the information will remain the same 


## chanign the dependent variable to factor

b_train1$y = as.factor(b_train1$y)


nchar(form)


##### creating the logistic model

log_fit=glm(form,data=b_train1,family = "binomial")

log_fit


####### run the stepwise

log_fit=step(log_fit)

##### this might take 5-6 minutes to finish 

formula(log_fit)


############


glm(y ~ balance + age_33_39 + age_39_48 + duration_LT_EQ_103 + duration_103_180 + 
      duration_180_319 + job_housemaid + job_entrepreneur + job_retired + 
      job_services + job_technician + job_blue_collar + marital_married + 
      education_primary + education_tertiary + On_LOAD_no + housing_yes + 
      loan_no + contact_cellular + poutcome_failure + month_jan + 
      month_feb + month_nov + month_aug + month_jul + campaign_6 + 
      campaign_5 + campaign_4 + campaign_3 + campaign_2 ,
    data= b_train1, family = 'binomial')



################



form =
  as.formula("y ~ balance + age_33_39 + age_39_48 + duration_LT_EQ_103 + 
    duration_103_180 + duration_180_319 + job_housemaid + job_entrepreneur + 
             job_retired + job_services + job_technician + job_blue_collar + 
             marital_married + education_primary + education_tertiary + 
             On_LOAD_no + housing_yes + loan_no + contact_cellular + poutcome_failure + 
             month_jan + month_feb + month_nov + month_aug + month_jul + 
             campaign_6 + campaign_5 + campaign_4 + campaign_3 + campaign_2")

form


#####################


log_fit=glm(form,data=b_train1,family='binomial')

log_fit

summary(log_fit)

###############################

## Run the logistic
## if you find any warning of 'fitted probabilites of 1/0' 
## It is due to complete/quasi separation
## you need to remove the variable which is behaving similar to your dependent variable
## there is no shortcut, but a basic relationship using table can be gathered to remove them
## There is another way to find out(mentioned below):
## alternatively, with the given set of variable run it by picking one variable at a time and adding it to the equation,and running the logistic regression
## so an example would be y ~ x1 in first step, then y ~ x1 + x2 in second step, then y ~ x1 + x2 + x3, if adding x3 results to 1/0 fitted probabilites then x3 
## is not right variable, hence you drop it.
## drop that variable. Carry the above approach untill you are finished with all the variables


saveRDS(file='log_fit_class.RDS', log_fit)


log_fit = readRDS(file='log_fit_class.RDS')



######################


## caTools to get the ROC:
## Run it to determine the ROC Plot
## Install caTools library

install.packages("caTools")
caTools::colAUC(predict(log_fit, b_train1, type = 'response'), 
                b_train1$y, plotROC = TRUE)

caTools::colAUC(predict(log_fit, b_train2, type = 'response'), 
                b_train2$y, plotROC = TRUE)


round( 0.8517467 - 0.8497878 , 3 )

########################


#### performance of score model on validation data
library(pROC)

# install.packages('pROC')



## scoring the test(validation) data

val.score=predict(log_fit,newdata = b_train2,type='response')

val.score


## scoring the train

train.score = predict(log_fit, newdata=b_train1, type='response')

train.score


#comparing the auc for train and test
library(pROC)
auc(roc(b_train2$y,val.score))


auc(roc(b_train1$y,train.score))


# so the tentative score performance of logistic regression is going to be around 0.85
# now lets build the model on entire training data

# code given below is result of multiple iterations
## final model for glm with given set of variables,
## the final variables are result of step wise
## bivariate analysis of data(relationship b/w dependent variable an indepndent variable)

# now if we needed to submit probability scores for the test data we can do at this point




##### final score to be submitted

test.prob.score= predict(log_fit,newdata = b_test,type='response')

test.prob.score


write.csv(test.prob.score,"proper_submission_file_name.csv",row.names = F)


#######################


# however if we need to submit hard classes, we'll need to determine cutoff score

## scoring the b_train2 and b_train1

b_train1$score = predict(log_fit, newdata=b_train1,type = 'response')

b_train1$score


b_train2$score = predict(log_fit, newdata=b_train2, type = 'response')

b_train2$score


##############


library(ggplot2)

ggplot(data=b_train1,aes(x = score, y = y, col=y)) + geom_point() + geom_jitter()


head(b_train1)

## dplyr::ntile
## Decile wise information for doing decile wise analysis

b_train1$decile = ntile(b_train1$score, 10)

b_train1$decile


########################



y = b_train1 %>% 
  group_by(decile) %>% 
  summarise(counts = n(), event_sum = sum(as.numeric(as.character(y))),'min'= min(score),'max' = max(score)) %>%
  arrange(desc(decile)) %>% 
  data.frame()


y



## This output to be paste on excel sheet, only two columns are required here 
## the counts and event_sum
## you need to reverse sort the data using decile and copy and paste these columns
## in the given excel sheet
write.csv(file='Y.csv',y, row.names = F)



## For K-S we use below code
## determine the score using predict
train.score = predict(log_fit, newdata = b_train1, type = 'response')

train.score


## get the real value using y of b_train1

real=b_train1$y

real

length(real)


## get 999 values of probabilities score for which you want to test TP, FP, FN and TN

cutoffs=seq(0.0001,0.9999,0.0001)

length(cutoffs)


##### Create a data frame with initialised garbage values

cutoff_data=data.frame(cutoff=99,Sn=99,Sp=99,KS=99,F5=99,F.1=99,M=99)

cutoff_data


#################################

## iterating the loop for all the 999 probabilities

for(cutoff in cutoffs){
  ## determine the prediction for each cut off here
  predicted=as.numeric(train.score>cutoff) ## Here hard classess of 0 and 1 are generated for every iteration
  
  ## fill the value of TP, FP, FN and TN
  TP=sum(real==1 & predicted==1) #True Positive Rate 
  TN=sum(real==0 & predicted==0) # True Negative Rate
  
  FP=sum(real==0 & predicted==1) # False Positive Rate
  FN=sum(real==1 & predicted==0) # False Negative Rate
  
  P=TP+FN # Total Positives
  N=TN+FP # Total Negatives
  
  Sn=TP/P # True Positive Rate : Sensitivity
  Sp=TN/N # True Negative Rate : Specificity
  precision=TP/(TP+FP) ## Precision
  recall=Sn ## Sensitivity and REcall is same, also called as hit
  ## KS is the cutoff
  KS=(TP/P)-(FP/N) ## KS cutoff 
  
  F5=(26*precision*recall)/((25*precision)+recall)
  ## F.1 score is maximum at 1 and min at 0
  ## A value of F.1 closer to 1 is good
  ## In case of low event rate model, F.1 closer to 1 is great
  ## F.1 score captures both precision and recall hence it is very useful in case of low event rate model
  F.1=(1.01*precision*recall)/((.01*precision)+recall)
  
  M=(4*FP+FN)/(5*(P+N))
  
  ## Binding the data
  cutoff_data=rbind(cutoff_data,c(cutoff,Sn,Sp,KS,F5,F.1,M))
}



View(cutoff_data)


##### removing the garbage column

cutoff_data=cutoff_data[-1,]


##### getting the row where maximum value of KS is there

cutoff_data[cutoff_data$KS == max(cutoff_data$KS),]


View(cutoff_data)



##### use the cut off to get the hard class value


b_train1$predicted_Class = (b_train1$score > 0.1154)+0



table(b_train1$predicted_Class)

install.packages('e1071')

library(caret)



## Draw the confusion matrix:
## Draw it for both train and test, compare them to see if they both behave similarly


confusionMatrix(factor(b_train1$predicted_Class),
                b_train1$y,
                positive = '1')



#######################


my_cutoff=cutoff_data$cutoff[which.max(cutoff_data$KS)]

my_cutoff


##### now that we have our cutoff we can convert score to hard classes


##### my_cutoff = 0.1154


##### In case you want to submit hard class probability

test.predicted=as.numeric(test.prob.score>my_cutoff)

test.predicted


write.csv(test.predicted,"Vikas_Sharma_P5_part2.csv",row.names = F)


