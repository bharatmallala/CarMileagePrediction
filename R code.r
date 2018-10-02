auto<-read.csv(file.choose())

# displaying missing values
sapply(auto, function(x) sum(is.na(x)))

#Treating missing values


auto$ï..Â.mpg[is.na(auto$ï..Â.mpg)] <- names(which.max(table(auto$ï..Â.mpg)))
auto$horsepower[is.na(auto$horsepower)] <- names(which.max(table(auto$horsepower)))
colnames(auto)<-c("mpg","cylinders", "displacement","horsepower","weight", "accelaration","modelyear", "origin")


#converting to numeric
auto$mpg<-as.numeric(auto$mpg)
auto$cylinders<-as.numeric(auto$cylinders)
auto$displacement<-as.numeric(auto$displacement)
auto$horsepower<-as.numeric(auto$horsepower)
auto$weight<-as.numeric(auto$weight)
auto$accelaration<-as.numeric(auto$accelaration)
auto$modelyear<-as.numeric(auto$modelyear)



#converting mpg to ordinal varibale

auto<-  transform(auto, group=cut(as.numeric(mpg),breaks=10))

#reloading the data with mpg ratings
autompg<-read.csv(file.choose())

#renaming the coloumn names

colnames(autompg)<-c( "cylinders", "displacement","horsepower","weight", "accelaration","modelyear", "origin","mpgrating")
colnames(autompg)<-c( "c", "d","h","w", "a","y", "o","m")


#fitting the complete independence model

library(MASS)

indmodel<- polr(factor(m)~c+d+h+a+y+o+w ,data = autompg ,Hess = TRUE )

summary(indmodel)

step(indmodel ,direction = "backward")

#from the step ward backward elimination we get y+w+h+o as the best model. Now fititng this model

bestmodel<- polr(factor(m)~h+y+o+w ,data = autompg ,Hess = TRUE )
summary(bestmodel)

#finding p -value
1-pchisq(bestmodel$deviance,df = bestmodel$df.residual,lower.tail = F)




#fitting ordinal logistic regression with interactions
library(glmnet)

autofinal <-subset(autompg, select = c(y,h,o,m,w))

autofinal$w = (autofinal$w-min(autofinal$w))/(max(autofinal$w)-min(autofinal$w))

interactionmodel<- polr(factor(m)~.^3 ,data = autofinal)

summary(res<-stepAIC(interactionmodel, scope= list(lower = ~ w+y+h+o), direction="backward"))

#fitting other models
library(MASS)

model1<-polr(factor(m)~ y:h:w + y:o:w + h:o:w +y:h+ y:o+ y:w+ h:o+ o:w +h:w, data = autofinal, Hess = TRUE)
summary(model1)


model2<-polr(factor(m)~y+h+w+o, data = autofinal, Hess = TRUE)
summary(model2)

#best model
bestmodel<-polr(factor(m)~ y+h+w+o + o:y + y:w + o:h +y:h, data = autofinal, Hess = TRUE)
summary(model3)

#goodness of fit
1-pchisq(model3$deviance,df = model3$df.residual,lower.tail = F)



#graphs
library("ggplot2")
ggplot(data = auto, aes(x = mpg, y = modelyear)) + geom_point() + geom_smooth(method.args = list(degree=1))