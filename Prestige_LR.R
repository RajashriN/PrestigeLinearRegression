library(car)
mydat<-Prestige

dim(mydat)

str(mydat)

summary(mydat)

head(mydat,5)##top 5 values



library(ggplot2)

result<-ggplot(data = mydat, mapping = aes(x = income, y = education)) +
  geom_point()
##Average years of education is proportional to the income 

ggplot(data = mydat, mapping = aes(x = type, y = women)) +
  geom_point()


##ggplot(data = mydat, mapping = aes(x = education, y = women)) +
  ##geom_rug(sides = "tr") 

##Type of job - Blue collar workers are higher compared to white collared workers.

ggplot(mydat) + 
  geom_bar(aes(type))






##Model building

##Linear model


##using rmse function
rmse_fun = function() { return(c(mean( (dtrain$pred - dtrain$income)^2,na.rm = TRUE) ^0.5,          
                                 mean( (dtest$pred - dtest$income)^2 ,na.rm = TRUE) ^0.5))
}
mydat$rowno <- 1:nrow(mydat)
dtrain<-subset(mydat,rowno <= nrow(mydat)*0.7)
dtest<-subset(mydat,rowno > nrow(mydat)*0.7)
model_lm <- lm(income ~ education, data=dtrain)
summary(model_lm)
dtrain$pred <- predict(model_lm, newdata = dtrain)
dtest$pred <- predict(model_lm, newdata = dtest)
rmse_fun()

cor(mydat$income,mydat$education) ##0.5 shows a moderate positive relationship
cor(dtest$income,dtest$education)##test data
cor(dtrain$income,dtrain$education)##train data

##visualising the train and test model fit lines
plot(dtrain$income ~ dtrain$education,data = dtrain)+
  abline(model_lm)

plot(dtest$income ~ dtest$education,data = dtest)+
  abline(model_lm)





