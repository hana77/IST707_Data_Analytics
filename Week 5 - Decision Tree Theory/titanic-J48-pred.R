library("RWeka")
trainset <- read.csv("/Users/hana/Documents/IST 707 Data Analytics/Week 5 - Decision Tree Theory/titanic-train.csv")
testset <- read.csv("/Users/hana/Documents/IST 707 Data Analytics/Week 5 - Decision Tree Theory/titanic-test.csv")
str(trainset)
str(testset)

#Transform data type Numeric -> Nominal
NN <- make_Weka_filter("weka/filters/unsupervised/attribute/NumericToNominal")

#Apply the filter to both datasets
trainset <-NN(data=trainset,control=Weka_control(R="1-3"),na.action = NULL)
testset <-NN(data=testset,control=Weka_control(R="1,3"),na.action = NULL)

#deal with mssing values
MS <- make_Weka_filter("weka/filters/unsupervised/attribute/ReplaceMissingValues")
#APlly the filter function to both datasets
trainset <- MS(data=trainset, na.action = NULL)
testset <- MS(data=testset, na.action = NULL)

str(trainset)

#Apply J48 algorithm, build decision tree model
m=J48(Survived~.,data=trainset)
m=J48(Survived~.,data=trainset,control = Weka_control(U=FALSE,M=2,C=0.5))
m1=J48(Survived~.,data=trainset,control = Weka_control(U=TRUE,M=2))
WOW("J48")

#Use 10 fold cross-validation to evaluate the model
e<- evaluate_Weka_classifier(m, numFolds = 10,seed=1,class=TRUE)
e
e1<- evaluate_Weka_classifier(m1, numFolds = 10,seed=1,class=TRUE)
e1

#apply the model with test dataset
pred=predict(m,newdata=testset, type=c("class"))
pred
write.csv(pred,file=("predict_titnaic.csv"))

#Removing irrelevant attributes
myVars =c("Pclass","Sex","Age","SibSp","Parch","Fare","Survived")
newtrain=trainset[myVars]
newtest=testset[myVars]
m=J48(Survived~.,data=newtrain)
m=J48(Survived~.,data=newtrain, control=Weka_control(U=FALSE,M=2,C=0.5))
e=evaluate_Weka_classifier(m,seed=1,numFolds = 10)
pred=predict(m,newdata=newtest,type=c("class"))
myids=c("PassengerId")
id_col=testset[myids]
newpred=cbind(id_col,pred)
colnames(newpred)=c("Passengeid","Survived")
View(newpred)
write.csv(newpred,("titanic-J48-pred.csv"),row.names=FALSE)