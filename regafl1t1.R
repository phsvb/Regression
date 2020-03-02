titanic <- read.csv(
  "http://www.math.ku.dk/~susanne/titanic.txt",
  header = TRUE,
  colClasses = c("factor", "factor","factor", "numeric", "integer", "integer"))



summary(titanic)
plot(titanic)


MF=glm(survived~., family=binomial(link=logit),data=titanic)

summary(MF)
drop1(MF)
drop1(MF, test="Chisq")
drop1(MF, test="LRT")
?drop1
plot(titanic$sibsp)

titanic$sibsp



head(titanic)

titanic

library(mice)
library(tidyverse)
library(ggplot2)
library(ggcorrplot)

miceMod <-mice(titanic, m = 50, maxit = 20, method = "pmm")
densityplot(miceMod)

summary(miceMod)

# Make a data set of only the missing values in our dataset

testdata <- titanic %>%
  filter(is.na(age))
# predict for the missing value using our mice model

newpred<- predict(pooled_lm, newdata = testdata)
NewTitanic <- Titanic
NewTitanic$age[is.na(NewTitanic$age) == TRUE] <- newpred
summary(Titanic$age)
summary(NewTitanic$age)


dim(testdata)
dim(titanic)

t1=addmargins(table(titanic$survived,titanic$sex),1)
t1
margin.table(t1)
t2=addmargins(table(titanic$survived,titanic$sex),1)

prop.table(t2,2)

addmargins(prop.table(table(titanic$survived,titanic$sex)))

addmargins(table(titanic$survived,titanic$sex))

table(testdata$survived,testdata$sex)
127*161/(339*682)
(127*161/(339*682))^-1

31*26/(47*159)

table(titanic$survived,titanic$sibsp)
table(titanic$survived,titanic$parch)

dim(titanic)
head(titanic)


sib1=rep(NA,dim(titanic)[1])
parch1=rep(NA,dim(titanic)[1])
for (i in 1:dim(titanic)[1]){
  if(titanic[i,5]==0){ #sibsp
    sib1[i]=0
  }
  else{
    sib1[i]=1
  }
  if(titanic[i,6]==0){ #parch
     parch1[i]=0
  }
  else{
    parch1[i]=1
  }
}
T2=cbind(titanic,sib1,parch1)
head(T2)


dim(subset(titanic,titanic$parch!=0))

table(titanic$survived,titanic$parch)
table(T2$survived,T2$sib1)
table(T2$survived,T2$parch1)


addmargins(table(titanic$survived,titanic$parch))


matrix(c(666,809-666,336,500-336),nrow=2,ncol=2)

666*164/(143*336)


(asp3= array(data=c(666,809-666,336,500-336),
             dim=c(2,2),
             dimnames=list(
               survival=c("0","1"),
               parents/children=c("0","more than 0")
             )))

addmargins(asp3)



margin.table(asp3,margin=1)

###

#add the margins
asp=table(titanic$survived,titanic$sex)
addmargins(asp)

#marginal table for suviv
margin.table(asp,margin=1)

#sample proportions,joint and marginal distributions
addmargins(prop.table(asp))

#sample proportions, conditionally on treatment
addmargins(prop.table(asp,margin=1),margin=2) # so P(Surv=0|Sex=M)=0.84

#try to do one for sex/pclass or pclass/surv
#addmargins(prop.table(asp,margin=1),margin=2) # so P(Surv=0|Sex=M)=0.84


addmargins(table(titanic$sex,titanic$pclass),margin=2) 


ftable(titanic$survived,titanic$sex,titanic$pclass) #this one is nice

prop.table(ftable(titanic$survived,titanic$pclass,titanic$sex) #this one is nice
           )

addmargins(prop.table(ftable(titanic$survived,titanic$sex,titanic$pclass))) #this one is nice


