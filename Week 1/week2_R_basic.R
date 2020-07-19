a = c(150.0, 150.4, 150.6)
mean(a)
signif(mean(a),digit=4)

titanic <- read.csv('/Users/hana/Documents/IST 707 Data Analytics/titanic/train.csv',na.string=c(""))
str(titanic)
summary(titanic)
titanic$Survived = factor(titanic$Survived)
str(titanic)
titanic$Pclass=ordered(titanic$Pclass)
str(titanic)
mons=c("Jan","Jan","Feb","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Oct","Nov","Dec","Dec")
table(mons)
mons_factor=factor(mons,levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE)        
table(mons_factor)
titanic$PassengerId=factor(titanic$PassengerId)
str(titanic)
titanic[complete.cases(titanic),]
nrow(titanic[!complete.cases(titanic),])
nrow(titanic[complete.cases(titanic),])
length(which(is.na(titanic$Age)))
titanic$Age[is.na(titanic$Age)] <- mean(titanic$Age, na.rm=TRUE)
titanic_new <- titanic[complete.cases(titanic),]
nrow(titanic_new)
myVars=c("Pclass","Sex","Age","SibSp","Fare","Survived")
titanic_new3 <- titanic[myVars]
str(titanic_new3)
$ PassengerId: int  1 2 3 4 5 6 7 8 9 10 ...
$ Survived   : int  0 1 1 1 0 0 0 0 1 1 ...
$ Pclass     : int  3 1 3 1 3 3 1 3 3 2 ...
$ Name       : Factor w/ 891 levels "Abbing, Mr. Anthony",..: 109 191 358 277 16 559 520 629 417 581 ...
$ Sex        : Factor w/ 2 levels "female","male": 2 1 1 1 2 2 2 2 1 1 ...
$ Age        : num  22 38 26 35 35 NA 54 2 27 14 ...
$ SibSp      : int  1 1 0 1 0 0 0 3 0 1 ...
$ Parch      : int  0 0 0 0 0 0 0 1 2 0 ...
$ Ticket     : Factor w/ 681 levels "110152","110413",..: 524 597 670 50 473 276 86 396 345 133 ...
$ Fare       : num  7.25 71.28 7.92 53.1 8.05 ...
$ Cabin      : Factor w/ 147 levels "A10","A14","A16",..: NA 82 NA 56 NA NA 130 NA NA NA ...
$ Embarked 
titanic <- read.csv('/Users/hana/Documents/IST 707 Data Analytics/titanic/train.csv',na.string=c(""))
length(which(is.na(titanic$Age)))
length(which(is.na(titanic$PassengerId)))
length(which(is.na(titanic$Survived)))
length(which(is.na(titanic$Pclass)))
length(which(is.na(titanic$Name)))
length(which(is.na(titanic$Sex)))
length(which(is.na(titanic$SibSp)))
length(which(is.na(titanic$Parch)))
length(which(is.na(titanic$Ticket)))
length(which(is.na(titanic$Fare)))
length(which(is.na(titanic$Cabin)))
length(which(is.na(titanic$Embarked)))

titanic_new <- titanic[complete.cases(titanic),]
length(which(is.na(titanic_new)))

table(titanic$Sex)
table(titanic$Sex)[which.max(table(titanic$Sex))] 
hist(titanic$Age)
boxplot(titanic$Age)
plot(titanic$Age, titanic$Fare)
table(titanic$Sex, titanic$Survived)
male=titanic[titanic$Sex=='male',]
mean(male$Fare)
female=titanic[titanic$Sex=='female',]
mean(female$Fare)
boxplot(titanic$Fare)
hist(titanic$Fare)
table(titanic$Embarked,titanic$Survived)
sales <- read.csv('/Users/hana/Documents/IST 707 Data Analytics/Week 1/sales.csv')
salesByRegion <- aggregate(cbind(Mon,Tue,Wed,Thu,Fri,Sat,Sun),by=list(Group.region=Region),FUN=sum)
str(sales)
InWeekend <- rowSums(sales[,c('Sat','Sun')])
salesNew <- data.frame(sales,InWeekend)
salesNew
salesInWeekend <- aggregate(InWeekend,by=list(Region),FUN=mean)
salesInWeekend
menandwomen <- aggregate((cbind()))

age <- cut(titanic$Age, breaks=c(0,10,20,30,40,50,60,Inf),labels = c("Child","teens","twenties","thirties","fourties","fifties","old"))
age
plot(titanic$Age,log(titanic$Age))
Min_max <- (titanic$Age-min(titanic$Age,na.rm=TRUE))/(max(titanic$Age,na.rm=TRUE)-min(titanic$Age,na.rm=TRUE))
Min_max
plot(Min_max,titanic$Age)

class <- cut(titanic$Pclass, breaks = c(0,1,2,3),label=c('first','second','thrid'))
class

#Random sampling
#sample, first to the last row in titanic, sample 100, replacement is false
sample <- titanic[sample(1:nrow(titanic),100,replace = FALSE),]
table(titanic$Survived)
table(sample$Survived)

##Systematic Sampling
#every tenth row..
ss=titanic[seq(1,nrow(titanic),9),]
nrow(ss)
