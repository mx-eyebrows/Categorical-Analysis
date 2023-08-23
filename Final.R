#### (1)
Data1 <- read.table("Data1.dat",header = TRUE)
Data1$Level <- factor(Data1$Level)
Tab1 <- aggregate(Data1$Count, 
                  by=list(Level=Data1$Level, Male=Data1$Male, LD=Data1$LD),
                  FUN=sum)
names(Tab1)[4]="Count"

# (a)
ftable(xtabs(Count ~ Level + Male + LD,  data =Tab1),
       col.vars = "LD")

# (b)
Tab1.1 <- tapply(Tab1$Count,Tab1[,c(2,3,1)],sum)
library(vcd)
or <- oddsratio(Tab1.1,log=F)
or

Tab1.2<- tapply(Tab1$Count,Tab1[,c(2,3)],sum)
oddsratio(Tab1.2,log=F)

# (c)
cmh.test <- mantelhaen.test(Tab1.1, correct = F)
cmh.test$stat
cmh.test$p.value



# (d)
cmh.test$est
cmh.test$conf.int[1:2]

#### (2)
# (a)
Tab2=cbind(Data1[1:24,c(1,2,3,4,6)], Data1[25:48,6])
names(Tab2)[5:6]=c("No", "Yes")
Tab2$Level <- as.factor(Tab2$Level)

# (b)
obj1=glm(cbind(Yes, No)~Level*Male*Night*Wet,  family=binomial(), data=Tab2)
summary(obj1)
step(obj1)

# (c)
obj.select=step(obj1)
resid(obj.select, type="pearson")/sqrt(1 -lm.influence(obj.select)$hat)
############ The standardized Pearson residuals fluctuate around 0, suggesting the model with Level, Male, Night, Wet as predictors fits well.
# (d)
obj3=glm(cbind(Yes, No)~Level+Male*Night+Wet,  family=binomial(), data=Tab2)
anova(obj3, obj.select, test="Chisq")


# (e)
obj2=glm(cbind(Yes, No)~Male*Night+Wet,  family=binomial(), data=Tab2)
anova(obj2, obj.select, test="Chisq")

# (f)

Tab3.1=(Tab2[rep(seq_len(nrow(Tab2)), Tab2$No),])[,-c(5,6)]
Tab3.1$y = 0
Tab3.2=(Tab2[rep(seq_len(nrow(Tab2)), Tab2$Yes),])[,-c(5,6)]
Tab3.2$y = 1
Tab3=rbind(Tab3.1, Tab3.2)

model.fit <- glm(y~ Level + Male + Night + Wet + Male:Night + Level:Wet,
             family=binomial(), data=Tab3) 

pihat <- predict(model.fit,type="response")


pi0 <- seq(0.05,0.95,by=.05)
fun <- function(x,y) ifelse(x>y,1,0)
sensfun <-function(ypred) sum(ypred[Tab3$y==1]==1)/sum(Tab3$y==1)
specfun <-function(ypred) sum(ypred[Tab3$y==0]==0)/sum(Tab3$y==0)
roc <- function(arg1) {
  yhat <- outer(arg1,pi0,fun)
  sens <- apply(yhat,2,sensfun)
  spec <- apply(yhat,2,specfun)
  data.frame(sens=sens,spec=spec)
}

x <- roc(pihat)
f <- approxfun(1-x$spec,x$sens)
area <- integrate(f,0.05,0.95)$value
plot(1-x$spec,x$sens,xlab="1 - specificity",
     ylab="sensitivity",pch=19)



#### (3)
Data2 <- read.table("Data2.dat",header = TRUE)
Data2$Level <- factor(Data2$Level)
# (a)
library(nnet)
obj4.1 <- multinom(Distraction ~ Level+Male, data = Data2,
                 weights = Count, trace = F)

obj4.2 <- multinom(Distraction ~ Level*Male, data = Data2,
                   weights = Count, trace = F)
anova( obj4.2, obj4.1)

# (b)
newdata <- expand.grid(Level=levels(Data1$Level),
                       Male=c(0,1))
marg.count <- as.vector(tapply(Data2$Count,list(Data2$Level,Data2$Male),sum))
pred.count <- predict(obj4.2,newdata,type="probs")*marg.count
round(pred.count,1)




#### (4)
library(rms)
library(VGAM)
fit.vglm1 <- vglm(Distraction ~ Level+Male, 
                 cumulative(link = logitlink, parallel = T), 
                 weights = Count, data = Data2)

fit.vglm2 <- vglm(Distraction ~ Level*Male, 
                  cumulative(link = logitlink, parallel = T), 
                  weights = Count, data = Data2)
summary(fit.vglm2)
predictvglm(fit.vglm1, 
            type = c( "response"))



############ level 1 Male
predict(fit.vglm2, newdata = data.frame(Level=factor(1), Male = 1),type="response")
sum(predict(fit.vglm2, newdata = data.frame(Level=factor(1), Male = 1),type="response")[1:4])

############ level 2 Male
predict(fit.vglm2, newdata = data.frame(Level=factor(2), Male = 1),type="response")
sum(predict(fit.vglm2, newdata = data.frame(Level=factor(2), Male = 1),type="response")[1:4])



#### (5)
Data3 <- read.table("Data3.dat",header = TRUE)
Data3$Distraction <- factor(Data3$Distraction)
Data3$Level <- factor(Data3$Level)
##(a)
fit.loglinear <- glm(Count~Level*Male*Night*Wet+LD+Distraction, 
                     family=poisson, data=Data3)
fit.main <- glm(Count~Level+Male+Night+Wet+LD+Distraction, 
                family=poisson, data=Data3)
anova(fit.main,fit.loglinear,test="Chisq")
############ p-value < 0.05, indicating the two models are significantly different
##(b)
fit.ll.select <- step(fit.loglinear, 
                  scope = list(upper = ~.^6), 
     direction = "forward")

## (d)
fit.temp <- update(fit.ll.select, ~. - LD:Distraction)
anova(fit.temp, fit.ll.select,test="Chisq")

## (f)
1-pchisq(fit.temp$deviance, fit.temp$df.residual)
