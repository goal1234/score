 
# the German Credit Data

# read comma separated file into memory
data<-read.csv("C:/Documents and Settings/MyDocuments/GermanCredit.csv")

#code to convert variable to factor
data$property <-as.factor(data$ property)

#code to convert to numeric
data$age <-as.numeric(data$age)

#code to convert to decimal
data$amount<-as.double(data$amount)

data$amount<-
    as.factor(ifelse(data$amount<=2500,'0-2500',
        ifelse(data$amount<=5000,'2600-5000','5000+')))

d = sort(sample(nrow(data), nrow(data)*.6))
#select training sample
train<-data[d,]
test<-data[-d,]
train<-subset(train,select=-default)

# Traditional Credit Scoring Using Logistic Regression in R
m<-glm(good_bad~.,data=train,family=binomial())

# for those interested in the step function one can use m<-
# step(m) for it
# I recommend against step due to well known issues with it
# choosing the optimal #variables out of sample

# load library
library(ROCR)

# score test data set
test$score<-predict(m,type='response',test)
pred<-prediction(test$score,test$good_bad)
perf <- performance(pred,"tpr","fpr")
plot(perf)

# Calculating KS Statistic
#this code builds on ROCR library by taking the max delt
#between cumulative bad and good rates being plotted by
#ROCR
max(attr(perf,'y.values')[[1]]-attr(perf,'x.values')[[1]])

# top3 variable affecting Credit Score Function

#get results of terms in regression
g<-predict(m,type='terms',test)

ftopk<- function(x,top=3){
res=names(x)[order(x, decreasing = TRUE)][1:top]
paste(res,collapse=";",sep="")
}
# Application of the function using the top 3 rows
topk=apply(g,1,ftopk,top=3)

#add reason list to scored tets sample
test<-cbind(test, topk)

# Cutting Edge techniques Available in R
#load tree package
library(rpart)
fit1<-rpart(good_bad~.,data=train)

plot(fit1)
text(fit1)
#test$t<-predict(fit1,type='class',test)

#score test data
test$tscore1<-predict(fit1,type='prob',test)
pred5<-prediction(test$tscore1[,2],test$good_bad)
perf5 <- performance(pred5,"tpr","fpr")
#build model using 90% 10% priors
#with smaller complexity parameter to allow more complex trees
# for tuning complexity vs. pruning see Thernau 1997

fit2<-
rpart(good_bad~.,data=train,parms=list(prior=c(.9,.1)),cp=.0002)
plot(fit2);text(fit2);

test$tscore2<-predict(fit2,type='prob',test)

pred6 <- prediction(test$tscore2[,2], test$good_bad)
perf6 <- performance(pred6, 'tpr', 'fpr')

#prints complexity and out of sample error
printcp(fit1)

#plots complexity vs. error
plotcp(fit1)

#prints complexity and out of sample error
printcp(fit2)

#plots complexity vs. error
plotcp(fit2)

plot(perf5,col='red',lty=1,
    main='Tree vs Tree with Prior Prob');
plot(perf4, col='green',add=TRUE,lty=2);
legend(0.6,0.6,c('simple tree','tree with 90/10
    prior'),col=c('red','green'),lwd=3)

#print rules for all classes
list.rules.rpart(fit1)
list.rules.rpart(fit2)

#custom function to only print rules for bad loans
listrules(fit1)
listrules(fit2)


# Bayesian Networks in Credit Scoring
#load library
library(deal)
#make copy of train
ksl<-train

#discrete cannot inherit from continuous so binary
#good/bad must be converted to numeric for deal package
ksl$good_bad<-as.numeric(train$good_bad)

#no missing values allowed so set any missing to 0
# ksl$history[is.na(ksl$history1)] <- 0

#drops empty factors
# ksl$property<-ksl$property[drop=TRUE]

ksl.nw <- network(ksl)
ksl.prior <- jointprior(ks.nw)

# the ban list is a matrix with two columns 
#banlist <- matrix(c(5,5,6,6,7,7,9,8,9,8,9,8,9,8),ncol=2)
## ban arrows towards Sex and Year

# note this a computationally intensive procuredure and if
you know that certain variables should have not
relationships you should specify
# the arcs between variables to exclude in the banlist
ksl.nw <- learn(ksl.nw,ksl,ksl.prior)$nw
#this step appears expensive so reset restart from 2 to 1
and degree from 10 to 1
result <-
heuristic(ksl.nw,ksl,ksl.prior,restart=1,degree=1,trace=TRU
E)
thebest <- result$nw[[1]]
savenet(thebest, "ksl.net")
print(ksl.nw,condposterior=TRUE



# In particular trying 80/20,
# 90/10, 60/40, 50/50 type priors seems to be a quick and effective hueristic approach to
# getting high performing trees.

#load tree package
library(rpart)

fit1<-rpart(good_bad~.,data=train)
plot(fit1);text(fit1);
#test$t<-predict(fit1,type='class',test)

#score test data
test$tscore1<-predict(fit1,type='prob',test)
pred5<-prediction(test$tscore1[,2],test$good_bad)
perf5 <- performance(pred5,"tpr","fpr")

#build model using 90% 10% priors
#with smaller complexity parameter to allow more complex trees
# for tuning complexity vs. pruning see Thernau 1997
fit2<-rpart(good_bad~.,data=train,parms=list(prior=c(.9,.1)),cp=.0002)
plot(fit2);text(fit2);


test$tscore2<-predict(fit2,type='prob',test)
pred6<-prediction(test$tscore2[,2],test$good_bad)
perf6<- performance(pred6,"tpr","fpr")

#prints complexity and out of sample error
printcp(fit1)

#plots complexity vs. error
plotcp(fit1)
#prints complexity and out of sample error
printcp(fit2)
#plots complexity vs. error
plotcp(fit2)

# Compare ROC Performance of Trees
plot(perf5,col='red',lty=1,main='Tree vs Tree with Prior Prob');
plot(perf4, col='green',add=TRUE,lty=2);
legend(0.6,0.6,c('simple tree','tree with 90/10 prior'),col=c('red','green'),lwd=3)

# Converting Trees to Rules
#print rules for all classes
list.rules.rpart(fit1)
list.rules.rpart(fit2)
#custom function to only print rules for bad loans
listrules(fit1)
listrules(fit2)

# Conditional inference Trees
#conditional inference trees corrects for known biases in chaid and cart
library(party)
cfit1<-ctree(good_bad~.,data=train)
plot(cfit1)

resultdfr <- as.data.frame(do.call("rbind", treeresponse(cfit1, newdata = test)))
test$tscore3<-resultdfr[,2]
pred9<-prediction(test$tscore3,test$good_bad)
perf9 <- performance(pred9,"tpr","fpr")

plot(perf5,col='red',lty=1,main='Tree vs Tree with Prior Prob vs Ctree');
plot(perf6, col='green',add=TRUE,lty=2);
plot(perf9, col='blue',add=TRUE,lty=3);
legend(0.6,0.6,c('simple tree','tree with 90/10
prior','Ctree'),col=c('red','green','blue'),lwd=3)

# Using Random Forests
library(randomForest)
arf<-
randomForest(good_bad~.,data=train,importance=TRUE,proximit
y=TRUE,ntree=500, keep.forest=TRUE)
#plot variable importance
varImpPlot(arf)
testp4<-predict(arf,test,type='prob')[,2]
pred4<-prediction(testp4,test$good_bad)
perf4 <- performance(pred4,"tpr","fpr")
#plotting logistic results vs. random forest ROC
#plotting logistic results vs. random forest ROC
plot(perf,col='red',lty=1, main='ROC Logistic Vs. RF');
plot(perf4, col='blue',lty=2,add=TRUE);
legend(0.6,0.6,c('simple','RF'),col=c('red','blue'),lwd=3)

#plot variable importance
varImpPlot(arf)

library(party)
set.seed(42)
crf<-cforest(good_bad~.,control = cforest_unbiased(mtry = 2, ntree = 50), data=train)
varimp(crf)

model based on trial and error based on random forest
variable importance
m2<-glm(good_bad~.+history:other+history:employed
+checking:employed+checking:purpose,data=train,family=binom
ial())
test$score2<-predict(m2,type='response',test)
pred2<-prediction(test$score2,test$good_bad)
perf2 <- performance(pred2,"tpr","fpr")
plot(perf2)
#plotting logistic results vs. random forest ROC
plot(perf,col='red',lty=1, main='ROC Logistic Vs. RF');
plot(perf2, col='orange',lty=2,add=TRUE);
plot(perf4, col='blue',lty=3,add=TRUE);
legend(0.6,0.6,c('simple','logit w
interac','RF'),col=c('red','orange','blue'),lwd=3)

# Calculating Area under the Curve
# the following line computes the area under the curve for
# models
#simple model
performance(pred,"auc")
#random forest
performance(pred2,"auc")
#logit plus random forest interaction of affordability term
performance(pred4,"auc")

# Cross Validation
#load Data Analysis And Graphics Package for R (DAAG)
library(DAAG)
#calculate accuracy over 100 random folds of data for
simple logit
h<-CVbinary(obj=m, rand=NULL, nfolds=100,
print.details=TRUE)
#calculate accuracy over 100 random folds of data for logit
+affordability interactions
g<-CVbinary(obj=m2, rand=NULL, nfolds=100,
print.details=TRUE)

# Cutting Edge techniques: Party Package
#model based recursive paritioning
library(party)
model<-mob(good_bad~afford |
amount+other+checking+duration+savings+marital+coapp+proper
ty+resident+amount,data=train,
model=glinearModel,family=binomial())
plot(model)

test$mobscore<-predict(model, newdata = test, type =
c("response"))

pred7<-prediction(test$mobscore,test$good_bad)
perf7 <- performance(pred7,"tpr","fpr")
plot(perf5,col='red',lty=1,main='Tree vs Tree with Prior
Prob vs. Model Based Tree with Glm');
plot(perf4, col='green',add=TRUE,lty=2);
plot(perf7, col='orange',add=TRUE,lty=3);
legend(0.6,0.6,c('simple tree','tree with 90/10 prior',
'Model based tree with logit'),col=c('red','green','orange'),lwd=3)


# Appendix of Useful Functions
#function to divide to create interaction terms without divide by zero
div<-function(a,b) ifelse(b == 0, b, a/b)

list.rules.rpart <- function(model)
{
if (!inherits(model, "rpart")) stop("Not a legitimate rpart tree")
#
# Get some information.
#
frm <- model$frame
names <- row.names(frm)
ylevels <- attr(model, "ylevels")
ds.size <- model$frame[1,]$n
#
# Print each leaf node as a rule.
#
for (i in 1:nrow(frm))
{
if (frm[i,1] == "<leaf>")
{
# The following [,5] is hardwired - needs work!
cat("\n")
cat(sprintf(" Rule number: %s ", names[i]))
cat(sprintf("[yval=%s cover=%d (%.0f%%) prob=%0.2f]\n",
ylevels[frm[i,]$yval], frm[i,]$n,
round(100*frm[i,]$n/ds.size), frm[i,]$yval2[,5]))
pth <- path.rpart(model, nodes=as.numeric(names[i]),
print.it=FALSE)
cat(sprintf(" %s\n", unlist(pth)[-1]), sep="")
}
}
}

listrules<-function(model)
{
if (!inherits(model, "rpart")) stop("Not a legitimate
rpart tree")
#
# Get some information.
#
frm <- model$frame
names <- row.names(frm)
ylevels <- attr(model, "ylevels")
ds.size <- model$frame[1,]$n
#
# Print each leaf node as a rule.
#
for (i in 1:nrow(frm))
{
if (frm[i,1] == "<leaf>" & ylevels[frm[i,]$yval]=='bad')
{
# The following [,5] is hardwired - needs work!
cat("\n")
cat(sprintf(" Rule number: %s ", names[i]))
cat(sprintf("[yval=%s cover=%d N=%.0f Y=%.0f (%.0f%%)
prob=%0.2f]\n",
ylevels[frm[i,]$yval], frm[i,]$n,
formatC(frm[i,]$yval2[,2], format = "f", digits = 2),
formatC(frm[i,]$n-frm[i,]$yval2[,2], format = "f", digits
= 2),
round(100*frm[i,]$n/ds.size), frm[i,]
$yval2[,5]))
pth <- path.rpart(model, nodes=as.numeric(names[i]),
print.it=FALSE)
cat(sprintf(" %s\n", unlist(pth)[-1]), sep="")
}
}
}

