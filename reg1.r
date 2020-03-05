titanic <-read.csv("http://www.math.ku.dk/~susanne/titanic.txt",header =TRUE,
                   colClasses =c("factor","factor","factor","numeric","integer","integer"))

table(titanic$survived,titanic$pclass)

table(titanic$survived,titanic$sex)

#include all variables by the below:
addmargins(prop.table(table(titanic$survived,titanic$sex),margin=2),margin=1)
addmargins(prop.table(table(titanic$survived,titanic$pclass),margin=2),margin=1)
addmargins(prop.table(table(titanic$survived,titanic$parch),margin=2),margin=1)
addmargins(prop.table(table(titanic$survived,titanic$sibsp),margin=2),margin=1)

table(titanic$survived,titanic$parch)
table(titanic$survived,titanic$sibsp)
#table(titanic$survived,titanic$sibsp)



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
Tdata=cbind(titanic,sib1,parch1)


addmargins(prop.table(table(Tdata$survived,Tdata$parch1),margin=2),margin=1)
addmargins(prop.table(table(Tdata$survived,Tdata$sib1),margin=2),margin=1)
(0.664*0.5342)/(0.3353*0.4657) #
(0.6531987*0.4569378)/(0.3468013*0.5430622) #1.15

#initial model below
M4=glm(survived~pclass*age+sex*parch1+sib1*parch1,family=binomial(link=logit),data=Tdata)
drop1(M4,test="LRT")
#proceed iteratively...
#aic  989
M4a=glm(survived~pclass*age+sex*parch1+sib1,family=binomial(link=logit),data=Tdata)
#anova(M4,M4a,test="Chisq")
drop1(M4a,test="LRT")

#final model below. aic 989.42
M4b=glm(survived~pclass*age+sex*parch1+sib1,family=binomial(link=logit),data=Tdata)
drop1(M4b,test="LRT")

#fine
M4c=glm(survived~pclass+age+sex*parch1+sib1,family=binomial(link=logit),data=Tdata)
drop1(M4c,test="LRT") #final model. 989.96 aic

########### #final model below
M4b=glm(survived~pclass+age+sex*parch1+sib1,family=binomial(link=logit),data=Tdata)
drop1(M4b,test="LRT") #989.96
plot(M4b)
M4b


#"residual" plots":
N4b=transform(
  Tdata,
  .fitted=predict(M4b),
  .deviance=residuals(M4b),
  .pearson=residuals(M4b,type="pearson")
)

p1= qplot(.fitted,.deviance, data=N4b)+ 
  geom_smooth(size=1)
p1


p2= qplot(.fitted,.pearson, data=N4b)+ 
  geom_smooth(size=1)
p2

p3= qplot(.fitted,sqrt(abs(.pearson)), data=N4b)+ 
  geom_smooth(size=1)
p3

plot(M4b)
