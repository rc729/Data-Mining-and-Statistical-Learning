Authors: Roger Cheng, Eunnyoung Kim, Ling Zhang
#=======library=========

library(readxl)
library(ggplot2)
library(dplyr)



#========read table============





#=======Data Manipulation========

#### Function to tokenize a string ####
Clean_String <- function(string){
  # Lowercase
  temp <- tolower(string)
  # Remove everything that is not a number or letter (may want to keep more 
  # stuff in your actual analyses). 
  temp <- str_replace_all(temp,"[^a-zA-Z\\s|^0-9.]", " ")
  # Shrink down to just one white space
  temp <- str_replace_all(temp,"[\\s]+", " ")
  # Split it
  temp <- str_split(temp, " ")[[1]]
  # Get rid of trailing "" if necessary
  indexes <- which(temp == "")
  if(length(indexes) > 0){
    temp <- temp[-indexes]
  } 
  return(temp)
}

### Clean minimum qualifications to get Years ###
string <- data[,7]
string[1]
years <- c()
for(i in 1:dim(data)[1]) {
  temp_tokens <- Clean_String(string[i])
  index <- grep("years|year",temp_tokens)[1]
  years <- c(years, as.numeric(temp_tokens[index-1]))
}
#### Get number of sentences in Responsibilities column ####
responsibilities <- (data[,6])
sentence <- c()
for(i in 1:dim(data)[1]) {
  sentence <- c(sentence, str_count(responsibilities[i], "\\.")
                - str_count(responsibilities[i], "etc"))
}


mean(sentence) 
sentence[sentence == 0] <- 5 # assign any entry that has 0 sentences to the avg sentences (5)


data1=data[,c(2,3,6,7,8,9)]

#delete the blank cells
datablank=filter(data1,Degree=="NA")

#get the data without blank cells
data2=anti_join(data1,datablank)

#mutate new columns
data3=data2%>%
  mutate(year1=ifelse(Years=="NA",0,1),degree1=ifelse(Degree=="null",0,1)) #0 is for not having that variable

#change major null to both

data3$Major[data3$Major=="Null"]="Both"

#deal with category variable

##E&T
data3$Category[data3$Category=="Hardware Engineering"]="E&T"
data3$Category[data3$Category=="Technical Solutions"]="E&T"
data3$Category[data3$Category=="Hardware Engineering"]="E&T"
data3$Category[data3$Category=="Software Engineering"]="E&T"
data3$Category[data3$Category=="Data Center & Network"]="E&T"
data3$Category[data3$Category=="Technical Writing"]="E&T"
data3$Category[data3$Category=="Technical Infrastructure"]="E&T"
data3$Category[data3$Category=="IT & Data Management"]="E&T"
data3$Category[data3$Category=="Network Engineering" ]="E&T"
data3$Category[data3$Category=="IT & Data Management"]="E&T"

##Sale
data3$Category[data3$Category=="Product & Customer Support"]="Sales"
data3$Category[data3$Category=="Sales & Account Management"]="Sales"
data3$Category[data3$Category=="Sales Operations" ]="Sales"

##Marketing
data3$Category[data3$Category=="Marketing & Communications"]="Marketing"

##Business
data3$Category[data3$Category=="Manufacturing & Supply Chain" ]="Business"
data3$Category[data3$Category=="Partnerships" ]="Business"
data3$Category[data3$Category=="Business Strategy" ]="Business"
data3$Category[data3$Category=="Partnerships" ]="Business"
data3$Category[data3$Category=="Administrative" ]="Business"
data3$Category[data3$Category=="Real Estate & Workplace Services"]="Business"


##People
data3$Category[data3$Category=="Program Management"]="People"
data3$Category[data3$Category=="Developer Relations"]="People"
data3$Category[data3$Category=="People Operations"]="People"

##Legal
data3$Category[data3$Category=="Legal & Government Relations" ]="Legal"

##Design
data3$Category[data3$Category=="User Experience & Design"]="Design"

#edit location
data3$Location[data3$Location %in% c( "United States","Canada" )]="North America"
data3$Location[data3$Location %in% c("Brazil" , "Mexico" ,"Colombia" ,"Argentina"  )]="South America"
data3$Location[data3$Location %in% c("Ireland", "Netherlands","Germany","Switzerland" , "United Kingdom" ,
                                     "Italy","Poland","France"  ,"Romania" ,"Sweden" ,"Norway",
                                     "Finland", "Denmark","Belgium","Austria","Czechia","Russia",
                                     "Croatia","Greece" ,"Hungary" ,"Spain" ,"Slovakia" ,"Lithuania" ,
                                     "Ukraine","Portugal" )]="Europe"
data3$Location[data3$Location %in% c("Singapore" ,"China","Taiwan" ,"India","United Arab Emirates",
                                     "South Korea"  ,"Japan" ,"Turkey" ,"Philippines", "Israel",
                                     "Hong Kong","Indonesia" ,"Thailand")]="Asia"

data3$Location[data3$Location %in% c( "South Africa" , "Kenya","Nigeria" )]="Africa"

#=======Descriptive statistics========

#individual variables
ggplot(data3,aes(Category))+geom_bar()+theme_bw()+labs(y="Count")
ggplot(data3,aes(Location))+geom_bar()+theme_bw()+labs(y="Count")
ggplot(data3,aes(Degree))+geom_bar()+theme_bw()+labs(y="Count")
ggplot(data3,aes(Major))+geom_bar()+theme_bw()+labs(y="Count")
ggplot(data3,aes(Years))+geom_histogram()+theme_bw()
ggplot(data3,aes(Sentences))+geom_histogram()+theme_bw()


#relationship between two
ggplot(data3,aes(Location,fill=Category))+geom_bar()+theme_bw()+labs(y="Count")
ggplot(data3,aes(Category,fill=Degree))+geom_bar()+theme_bw()+labs(y="Count")
ggplot(data3,aes(Category,fill=Major))+geom_bar()+theme_bw()+labs(y="Count")

data3%>%
  group_by(Category)%>%
  summarise(mean=mean(Years,na.rm = T))%>%
  ggplot(aes(Category,mean))+geom_bar(stat = "identity")+ylab("Years")+theme_bw()

data3%>%
  group_by(Category)%>%
  summarise(mean=mean(Sentences,na.rm = T))%>%
  ggplot(aes(Category,mean))+geom_bar(stat = "identity")+ylab("Sentenses")+theme_bw()




#=======Models========
#--------(KNN)---------

data=read.csv("job3.csv")
data=data[,-1]
attach(data)


set.seed(1) 
train = sample(1:1212,606)
test = (-train)

Category_test=Category[test]


library(class)
train.X=cbind(Location,Major,Sentences, year1,degree1)[train,] #X for training data
test.X=cbind(Location,Major,Sentences, year1,degree1)[test,] #X for testing data
train.Category=Category[train] #Y for training data
knn.pred=knn(train.X,test.X,train.Category,k=1)

mean(knn.pred!=Category_test)

test_err = rep(0,50)
for (i in 1:50){
  set.seed(1)
  knn.pred=knn(train.X,test.X,train.Category,k=i)
  test_err[i] = mean(knn.pred!=Category_test)
}
test_err


#---------------classification tree--------------

library(tree)
library(ISLR)

#change data type
data$Years=as.numeric(data$Years,na.rm=T)
data$year1=as.factor(data$year1)
data$degree1=as.factor(data$degree1)
data$Category=as.factor(data$Category)
data$Degree=as.factor(data$Degree)
data$Major=as.factor(data$Major)
data$Location=as.factor(data$Location)

test.tree=tree(Category~.-year1-degree1,data)

plot(test.tree)
text(test.tree,pretty=0)
test.tree

#calculate error

set.seed(3)
trainct=sample(1:nrow(data), 606)#ct means classification tree
trainct=data[train,]
testct=data[-train,]

tree1=tree(Category~.,trainct)
tree.pred=predict(tree1,testct,type="class")
table1=table(tree.pred,testct$Category)
#error
1-sum(diag(table1))/sum(table1)


#---------random forest-------------
library(randomForest)

set.seed(1)

trainrf=data[train,]
testrf=data[-train,]

bag1=randomForest(Category~Sentences+year1+Degree+Location+Major,trainrf,mtry=sqrt(5),importance=TRUE)

yhat.bag = predict(bag1,testrf)
plot(yhat.bag, testrf$Category)
table2=table(yhat.bag, testrf$Category)

1-sum(diag(table2))/sum(table2)


#---------Multinominal-------------
library(nnet)
set.seed(3)
train=sample(1:nrow(data), 606)#ct means classification tree
trainmn=data[train,]
testmn=data[-train,]

m=multinom(Category~Sentences*year1+Degree+Location+Major,data=trainmn)
summary(m)

predm=predict(m,testmn)
table3=table(predm,testmn$Category)
1-sum(diag(table3))/sum(table3)

#----------- Naive Bayes --------------
library(readxl)
library(e1071)

set.seed(2)
train=sample(1:nrow(data), .5*nrow(data))
data.train = data[train,]
data.test=data[-train,]
Category.test=Category[-train]

nb.data <- naiveBayes(Category~., data = data.train)
print(nb.data)

predictions <- predict(nb.data, data.test, type = "class")
predtable <- table(Category.test, predictions)
predtable

1-sum(diag(predtable))/sum(predtable)

