setwd("E:/order/data")
library(sqldf)
library(tcltk)
library(dplyr)
library(MASS)
library(pROC)

###################################################
#                   ��ʽ                          #
###################################################

#how_z��������0ֵռ��
#development_sample_fun:����ķ������
#ks_data:����KSֵ
#woe:��WOE��ֵ

how_z <- function(x){
  data_nrow <- nrow(x)
  per_zero <- vector()
  long <- ncol(x)
  for(i in 4:long){
    a = sum(x[,i]==0)/data_nrow
    per_zero <- c(per_zero,a)
  }
  label <- colnames(x)[4:long]
  per_result <<- data.frame(name = label,per = per_zero)
  per_result <<-per_result[order(per_result$per,decreasing = T),]
  rm(data_nrow,per_zero,label)
}

##------------------------------------------------------------------
development_sample_fun <-function(x,y,g){
  require(Hmisc)
  cuts <- cut2(x=x,g=g,onlycuts=TRUE)
  
  dpsres1 <- data.frame(x=x,cut = cut2(x, cuts = cuts),score = as.numeric(cut2(x, cuts = cuts)))
  dpsres2 <- data.frame(y=y)
  dpsres3 <- cbind(dpsres1,dpsres2)
  dpsres4 <- (dpsres3[,c('cut','score','y')])
  dpsres5 <- as.data.frame(table(dpsres4$cut,dpsres4$y))
  
  length1 <- length(dpsres5$Var1)
  length2 <- length(dpsres5$Var1)/2
  length3 <- length(dpsres5$Var1)/2+1
  
  df1 <- dpsres5[1:length2,]
  df2 <- dpsres5[length3:length1,]
  id <- 1:length2
  df3 <- data.frame(id,
                    cut=df1$Var1,
                    goodmun=df1$Freq,
                    badmun=df2$Freq)
  
  df3$goodacc <- cumsum(df3$goodmun)
  df3$goodratio <- df3$goodmun/sum(df3$goodmun)
  df3$goodaccratio <- df3$goodacc/sum(df3$goodmun)
  
  df3$badacc <- cumsum(df3$badmun)
  df3$badratio <- df3$badmun/sum(df3$badmun)
  df3$badaccratio <- df3$badacc/sum(df3$badmun)
  
  df3$user <- df1$Freq+df2$Freq
  df3$baddebtrate <- df3$badmun/df3$user
  df3$goodratiobad <- df3$goodmun/df3$badmun
  df3$useracc <- cumsum(df3$user)
  df3$userratio <- df3$user/sum(df3$user)
  df3$useraccratio <- df3$useracc/sum(df3$user)
  
  df3$KSgroup <- abs(df3$goodaccratio-df3$badaccratio)
  x <- (df3$goodmun/sum(df3$goodmun))/(df3$badmun/sum(df3$badmun))
  df3$IVgroup <- (df3$goodmun/sum(df3$goodmun)-df3$badmun/sum(df3$badmun))*(log(x))
  W <- df3$goodratio/df3$badratio
  df3$WOE <- -log(W)
  
  dfend1 <- data.frame(���=id,
                         ���=df1$Var1,
                         
                         δ��ϵ��=df1$Freq,
                         �ۼ�δ��ϵ��= df3$goodacc,
                         δ��ϵռ��=df3$goodratio*100,
                         �ۼ�δ��ϵռ��=df3$goodaccratio*100,
                         
                         ��ϵ��=df2$Freq,
                         �ۼ���ϵ��=df3$badacc,
                         ��ϵ��ռ��=df3$badratio*100,
                         �ۼ���ϵ��ռ��=df3$badaccratio*100,
                         
                         �ۼ��û���=df3$useracc,
                         ��ϵ����=df3$baddebtrate*100,
                         KS����Դ=df3$KSgroup*100,
                         
                         �����û���=df3$goodratiobad,
                         �û���=df3$user,
                         �û���ռ��=df3$userratio*100,
                         �ۼ��û�ռ��=df3$useraccratio*100,
                         WOE=df3$WOE,
                         IV����Դ=df3$IVgroup)
  #--------------------------------------------------------------
  dfend2 <- round(dfend1[,c(3:19)],3)
  M1 <- function(a){
    a1 <- as.vector(a)
    a2 <- gsub("\\[","",a1)
    a3 <- gsub("\\)","",a2)
    a4 <- gsub("\\]","",a3)
    a5 <- gsub("\\(","",a4)
    a6 <- strsplit(a5,split = ',')
    a7 <- as.data.frame(a6)
    a8 <- t(a7)
    rownames(a8)<-NULL
    a8
  }
  MixMax <- M1(dfend1$���)
  m <- as.data.frame(MixMax)
  m1 <- data.frame(����=dfend1$���,������Сֵ=m$V1,�������ֵ=m$V2)
  m2 <- cbind(m1,dfend2)
  
}

ks_data <- function(x,y,z){
  #����KSֵ������xΪ����,y(0,1)����
  dlt <- data.frame(V1 = x,V2 = y)
  dlt <- dlt[order(dlt$V1),]
  dlt <- round(dlt,4)
  dltfun <<- development_sample_fun(dlt$V1,dlt$V2,z)
  KSdlt <- abs(max(dltfun$KS����Դ))
  IVdlt <- sum(dltfun$IV����Դ)
}

woe <- function(x,y){
  #����x = 0,1������y = ʵ�ʵ�ֵ,z = ��ı�ǩ����
  #�������ks��ֵ
  ##�γ���Ҫ�����ݼ�
  gaga <- data.frame(what = x,kniff = y)
  gaga <- na.omit(gaga)
  rows_bad <- sum(gaga$what==0);rows_good <- sum(gaga$what == 1)
  
  #ksֵwoe��ivֵ
  ks <- NULL
  woe <- NULL
  iv_kniff <- NULL
  good <-  NULL
  bad <- NULL
  kniff <<- unique(y)[order(unique(y))]
  for(i in kniff){
    #gogo <- filter(gaga,kniff==i)
    gogo <- gaga[gaga$kniff==i,]
    gogo <- na.omit(gogo)
    
    #ksֵ
    good1 <- sum(gogo$what==1)/rows_good
    bad1 <- sum(gogo$what==0)/rows_bad
    good <- c(good,good1)
    bad <- c(bad,bad1)
    
    #woe
    woe1 <- (sum(gogo$what==1)/rows_good)/(sum(gogo$what==0)/rows_bad)
    woe1 <- log(woe1)
    woe <- c(woe,woe1)
    
    #ivδ�ۼ�
    iv_kniff1 <- (sum(gogo$what==0)/rows_bad) - (sum(gogo$what==1)/rows_good)
    iv_kniff1 <- iv_kniff1*woe1
    iv_kniff <- c(iv_kniff,iv_kniff1)
  }
  good_cum <- cumsum(good)
  bad_cum <- cumsum(bad)
  ks_cum <- cummax((abs(good -bad)))*100
  iv <- cumsum(iv_kniff)
  result <<- data.frame(good_cum = good_cum,bad_cum = bad_cum,ks=ks_cum,iv = iv,woe = woe)
}

#######################################################################################
#                                   ���ݶ�����ɸѡ                                    #
#######################################################################################


file_name <- 'full_info.csv'
glm_sort <- read.csv(file_name,stringsAsFactors = F)
sum(glm_sort$sort != 500)/nrow(glm_sort)

age_sex <- read.csv('age_sex.csv',stringsAsFactors = F)

all_data <- merge(glm_sort,age_sex,by.x = "customer_code",by.y = "CUSTOMER_CODE",all.x = T)
sum(all_data$sort != 500)/nrow(all_data)
rm(age_sex,glm_sort);gc()

info_model_two <- read.csv('info_model_two.csv',stringsAsFactors = F)
all_data <- merge(all_data,info_model_two,by.x = "customer_code",by.y = "customer_code",all.x = T)
maxrepay <- read.csv('maxrepaydate.csv',stringsAsFactors = F)

rol_for_pre <- c("customer_code","contractno","sort","dpd","USER_NAME", "enter_date2","due_amount","to_rtp_days", "days_last_p","if_new_case",
                 "sv_due_rate","sx_eff_rate","return_to_loan_term","if_jyd","if_seek","age0","acdamic_type1","province" )
all_data <- all_data[,rol_for_pre]

all_data <- sqldf('select a.*,
                  b.lastpay_tonow
                  from all_data as a
                  left join maxrepay as b
                  on a.contractno = b.contractno and
                  a.enter_date2 = b.enter_date2')
rm(maxrepay);gc()
#�ͷ��ڴ�
rm(info_model_two);gc()
seven_days_log <- read.csv('seven_days_log.csv',stringsAsFactors = T)
all_data <- sqldf('select a.*,
                  b.seven_days_logs,
                  b.seven_days_eff_logs
                  from all_data as a
                  left join seven_days_log as b
                  on a.customer_code = b.customer_code and
                  a.enter_date2 = b.enter_date2')
rm(seven_days_log,file_name,rol_for_pre);gc()
all_data <- na.omit(all_data)
#---------------------------------------------------------------------------------------
sum(all_data$sort != 500)/nrow(all_data)

#��ϵ�Ŀͻ�
connect <- all_data
sum(connect$sort != 500)/nrow(connect)

result <- aggregate(connect$sort,list(name = connect$USER_NAME,time = connect$enter_date2),length)
user_connect_rank <- filter(result, x > 200)

#result  sortֵ�����˵�0.089
sort_120 <- sqldf('select a.* from all_data as a 
                  inner join user_connect_rank as b on a.USER_NAME = b.name and a.enter_date2 = b.time')

connect <- sort_120
result <- aggregate(connect$sort,list(name = connect$USER_NAME,time = connect$enter_date2),length)

#����user_name ��enter_date2 ���оۺ�
connect <- sort_120[sort_120$sort != 500,]
result <- aggregate(connect$sort,list(name = connect$USER_NAME,time = connect$enter_date2),length)

#��ϵ�ͻ����������
summary(result$x);boxplot(result$x);plot(density(result$x))

#����һЩ��Ϣ
user_connect_rank <- filter(result, x %in% c(15:35))

#result 
sort_15_25 <- sqldf('select a.* from sort_120 as a 
                    inner join user_connect_rank as b on a.USER_NAME = b.name and a.enter_date2 = b.time')

#�Ƴ������ͷ��ڴ�
rm(connect,result);gc()
sort_15_25 <- sort_15_25[sort_15_25$days_last_p>0,]
################################################################################
#                                                                              #
#                           ��ģ����                                           #
#                                                                              #
################################################################################


#ϣ��������Ŀ�����������Ƶ�ʳ��ֳ����Թ�ϵ
#��Ϊ��ѡ�������и���ǳ��ֵ�Ƶ�ʻ��г��ϲ���������һЩ���ҵ�����
#����IV,��KSֵ
#dltfun���ݼ�Ϊ��������


###your target-----------------must be (0,1)
sort_15_25$sort <- ifelse(sort_15_25$sort < 36 ,1,0)
sort_woe <- sort_15_25

md1 <- glm(sort~return_to_loan_term  + lastpay_tonow+days_last_p+
             seven_days_eff_logs + sx_eff_rate + seven_days_logs,data = sort_15_25,
           family = binomial(link = 'logit'))
summary(md1)

pred2 <- predict(md1,sort_15_25,type='response')
pred2 <- as.numeric(as.matrix(pred2))
plot(pred2[order(pred2)],type='l',main = "glm_2------------logistic")

#roc 
modelroc <- roc(sort_woe$sort,pred2)
plot(modelroc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="skyblue", print.thres=TRUE)

#seven_days_eff_logs
ks_data(sort_15_25$seven_days_eff_logs,sort_15_25$sort,20)
(IVdlt <- sum(dltfun$IV����Դ))
(KSdlt <- abs(max(dltfun$KS����Դ)));plot(dltfun$������,type='l') #0.16  10.738 �����Ϳ�����ԭ����ֵ
sort_woe$seven_days_eff_logs <- ifelse(sort_woe$seven_days_eff_logs == 0,0,
                                       ifelse(sort_woe$seven_days_eff_logs <=1,1,2))
md <- glm(sort~seven_days_eff_logs,data = sort_woe,family = binomial(link = 'logit'))
summary(md)

#seven_days_logs
ks_data(sort_15_25$seven_days_logs,sort_15_25$sort,25)
(IVdlt <- sum(dltfun$IV����Դ))
(KSdlt <- abs(max(dltfun$KS����Դ)));plot(dltfun$������,type='l') #0.007   11.53 �����Ϳ�����ԭ����ֵ
sort_woe$seven_days_logs <- ifelse(sort_woe$seven_days_logs == 0,0,
                                   ifelse(sort_woe$seven_days_logs <= 2,1,
                                          ifelse(sort_woe$seven_days_logs <= 5,2,3)))
md <- glm(sort~seven_days_logs,data = sort_woe,family = binomial(link = 'logit'))
summary(md)

#sx_eff_rate
ks_data(sort_15_25$sx_eff_rate,sort_15_25$sort,20)
(IVdlt <- sum(dltfun$IV����Դ))
(KSdlt <- abs(max(dltfun$KS����Դ)));plot(dltfun$������,type='l') #0.097  14.325 ��Ϊ0,2,
breaks <- as.numeric(as.matrix(dltfun$�������ֵ))
sort_woe$sx_eff_rate <- ifelse(sort_15_25$sx_eff_rate == 0,0,1)
md <- glm(sort~sx_eff_rate,data = sort_woe,family = binomial(link = 'logit'))
summary(md)

#sv_due_rate
ks_data(sort_15_25$sv_due_rate,sort_15_25$sort,20)
(IVdlt <- sum(dltfun$IV����Դ))
(KSdlt <- abs(max(dltfun$KS����Դ)));plot(dltfun$������,type='l') #0.007  2.918 ��Ϊ0,4,
breaks <- as.numeric(as.matrix(dltfun$�������ֵ))
sort_woe$sv_due_rate <- ifelse(sort_15_25$sv_due_rate==0,0,1)
md <- glm(sort~sv_due_rate,data = sort_woe,family = binomial(link = 'logit'))
summary(md)

#days_last_p
ks_data(sort_15_25$days_last_p,sort_15_25$sort,20)
(IVdlt <- sum(dltfun$IV����Դ))
(KSdlt <- abs(max(dltfun$KS����Դ)));plot(dltfun$��ϵ����,type='l') #0.039   5.274 ��0,2,4
breaks <- as.numeric(as.matrix(dltfun$�������ֵ))
sort_woe$days_last_p <- ifelse(sort_15_25$days_last_p <= breaks[1],0,
                                 ifelse(sort_15_25$days_last_p <= breaks[3],1,
                                        ifelse(sort_15_25$days_last_p <= breaks[5],2,3)))
md <- glm(sort~days_last_p,data = sort_woe,family = binomial(link = 'logit'))
summary(md)

#return_to_loan_term
ks_data(sort_15_25$return_to_loan_term,sort_15_25$sort,20)
(IVdlt <- sum(dltfun$IV����Դ))
(KSdlt <- abs(max(dltfun$KS����Դ)));plot(dltfun$������,type='l')    #0.007  3.385
breaks <- as.numeric(as.matrix(dltfun$�������ֵ))
sort_woe$return_to_loan_term <- ifelse(sort_15_25$return_to_loan_term <= breaks[1],0,
                                         ifelse(sort_15_25$return_to_loan_term <= breaks[5],1,
                                                ifelse(sort_15_25$return_to_loan_term <= breaks[10],2,3)))
md <- glm(sort~return_to_loan_term,data = sort_woe,family = binomial(link = 'logit'))
summary(md)

#due_amount
ks_data(sort_15_25$due_amount,sort_15_25$sort,5)
(IVdlt <- sum(dltfun$IV����Դ))
(KSdlt <- abs(max(dltfun$KS����Դ)));plot(dltfun$������,type='l')    #0.011   3.219
breaks <- as.numeric(as.matrix(dltfun$�������ֵ))
sort_woe$due_amount <- ifelse(sort_15_25$due_amount <= breaks[2],0,
                                ifelse(sort_15_25$due_amount <= breaks[3],1,2))
md <- glm(sort~due_amount,data = sort_woe,family = binomial(link = 'logit'))
summary(md)

#to_rtp_days
ks_data(sort_15_25$to_rtp_days,sort_15_25$sort,20)
(IVdlt <- sum(dltfun$IV����Դ))
(KSdlt <- abs(max(dltfun$KS����Դ)));plot(dltfun$������,type='l')    #0.011   4.273

#age0
ks_data(sort_15_25$age0,sort_15_25$sort,20)
(IVdlt <- sum(dltfun$IV����Դ))
(KSdlt <- abs(max(dltfun$KS����Դ)));plot(dltfun$������,type='l')    #0  0.602

#lastpay_tonow
ks_data(sort_15_25$lastpay_tonow,sort_15_25$sort,20)
(IVdlt <- sum(dltfun$IV����Դ))
(KSdlt <- abs(max(dltfun$KS����Դ)));plot(dltfun$������,type='l')    #0.186  18.172
breaks <- as.numeric(as.matrix(dltfun$�������ֵ))
sort_woe$lastpay_tonow <- ifelse(sort_15_25$lastpay_tonow <= breaks[5],0,1)
md <- glm(sort~lastpay_tonow,data = sort_woe,family = binomial(link = 'logit'))
summary(md)

#acdamic_type1
ks_data(sort_15_25$acdamic_type1,sort_15_25$sort,20)
(IVdlt <- sum(dltfun$IV����Դ))
(KSdlt <- abs(max(dltfun$KS����Դ)));plot(dltfun$������,type='l')    #0.001  0.945


##############################################################################
#                        �Է��ࣨ���ǩ����Ľ�ģ                            #
#############################################################################

#���ڿ��Է���ı���  lastpay_tonow  due_amount  return_to_loan_term
#���ӵ��ı���  to_rtp_days age0 acdamic_type1

#���б���seven_days_eff_logs  seven_days_logs  sx_eff_rate sv_due_rate
#         days_last_p lastpay_tonow due_amount return_to_loan_term if_jyd

md1 <- glm(sort~if_jyd +return_to_loan_term + due_amount + lastpay_tonow+days_last_p+
             seven_days_eff_logs + sx_eff_rate + seven_days_logs+sv_due_rate,data = sort_woe,
           family = binomial(link = 'logit'))
summary(md1)

pred2 <- predict(md1,sort_15_25,type='response')
pred2 <- as.numeric(as.matrix(pred2))
plot(pred2[order(pred2)],type='l',main = "glm_2------------logistic")

#roc 
modelroc <- roc(sort_woe$sort,pred2)
plot(modelroc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="skyblue", print.thres=TRUE)

#��������
classify <- function(probability)ifelse(probability<0.5,'0','1')

classified <- classify(pred2)
sort <- sort_15_25$sort
confusion_matrix <- table(sort,classified,dnn=c('real','prediction'))
a <- as.data.frame(confusion_matrix)
a1 <-as.numeric(as.character(a$real))
a2<-as.numeric(as.character(a$prediction))
accuracy <- sum(a$Freq[a1==a2])/sum(a$Freq)
Sensitivity <- sum(a$Freq[((a1==1)&(a2==1))])/sum(a$Freq[a1==1])
Specificity <- sum(a$Freq[((a1==0)&(a2==0))])/sum(a$Freq[a1==0])
real_in_prediction <- sum(a$Freq[(a1==1)&(a2==1)])/sum(a$Freq[a2==1])
predict_rate <- sum(a$Freq[a2==1])/sum(a$Freq)  #���ֵ�ı�����0.05
(call_back <- sum(a$Freq[(a1==1)&(a2==1)])/sum(a$Freq[a1==1]))

accuracy;Sensitivity;Specificity;real_in_prediction;predict_rate;call_back
confusion_matrix

##########################################################################################
#                                     woe�ı�׼��                                        #
##########################################################################################

#�õ�ÿ��������woe,Ϊ�����׼��

a <- c('sort','if_jyd','return_to_loan_term','due_amount','lastpay_tonow','days_last_p','sx_eff_rate','seven_days_logs',
       'seven_days_eff_logs','sv_due_rate')
data_woe <- sort_woe[,a]

#if_jyd
woe(data_woe$sort,data_woe$if_jyd);print(result)
result$woe[1]
kniff
data_woe$if_jyd <- ifelse(data_woe$if_jyd == 0, result$woe[1],result$woe[2])

#return_to_loan_term
woe(data_woe$sort,data_woe$return_to_loan_term);print(result)
kniff
data_woe$return_to_loan_term <- ifelse(data_woe$return_to_loan_term == 0,result$woe[1],
                                       ifelse(data_woe$return_to_loan_term == 1,result$woe[2],
                                       ifelse(data_woe$return_to_loan_term == 2,result$woe[3],result$woe[4])))

#due_amount
woe(data_woe$sort,data_woe$due_amount);print(result)
kniff
data_woe$due_amount <- ifelse(data_woe$due_amount == 0,result$woe[1],
                              ifelse(data_woe$due_amount == 1,result$woe[2],result$woe[3]))

#lastpay_tonow
woe(data_woe$sort,data_woe$lastpay_tonow);print(result)
kniff
data_woe$lastpay_tonow <- ifelse(data_woe$lastpay_tonow == 0,result$woe[1],result$woe[2])

#days_last_p
woe(data_woe$sort,data_woe$days_last_p);print(result)
kniff
data_woe$days_last_p <- ifelse(data_woe$days_last_p ==0,result$woe[1],
                               ifelse(data_woe$days_last_p ==1,result$woe[2],
                                      ifelse(data_woe$days_last_p ==2,result$woe[3],result$woe[4])))

#sx_eff_rate
woe(data_woe$sort,data_woe$sx_eff_rate);print(result)
kniff
data_woe$sx_eff_rate <- ifelse(data_woe$sx_eff_rate == 0,result$woe[1],result$woe[2])


#seven_days_logs
woe(data_woe$sort,data_woe$seven_days_logs);print(result)
kniff
data_woe$seven_days_logs <-ifelse(data_woe$seven_days_logs==0,result$woe[1],
                                  ifelse(data_woe$seven_days_logs==1,result$woe[2],
                                         ifelse(data_woe$seven_days_logs==2,result$woe[3],result$woe[4])))
                                                              
#seven_days_eff_logs
woe(data_woe$sort,data_woe$seven_days_eff_logs);print(result)
kniff
data_woe$seven_days_eff_logs <- ifelse(data_woe$seven_days_eff_logs ==0 ,result$woe[1],
                                       ifelse(data_woe$seven_days_eff_logs == 1 ,result$woe[2],result$woe[3]))
#sv_due_rate
woe(data_woe$sort,data_woe$sv_due_rate);print(result)
kniff
data_woe$sv_due_rate <- ifelse(data_woe$sv_due_rate == 0,result$woe[1],result$woe[2])

###################################################################################################
#                                      ��Ҫ�ı������н�ģ                                         #
###################################################################################################

#���������IV��KS��WOE�ĵ��������������ģ���е����������
#��ѡ��ı������д��
#��������û�п��������Բ��ܷ�����������

#return_to_loan_term lastpay_tonow  days_last_p  sx_eff_rate  seven_days_logs  seven_days_eff_logs

need <- c('sort','return_to_loan_term','lastpay_tonow','days_last_p','sx_eff_rate','seven_days_logs','seven_days_eff_logs')
data_woe <- data_woe[,need]

md_woe <-glm(sort~.,data=data_woe,family = binomial(link = 'logit'))
summary(md_woe)

est <-md_woe$coefficients

#1:1�Ǻû��Ⱦ����Ļ�����ֵ,����һ��ʤ�ʸ�50��
p <- 50/log(2)

q <- 500-50*log(1)/log(2)
base <- q + p*est[[1]]
base

getscore<-function(i,x){
  score = round(p*est[[i]]*x,0)
  return(score)
}

odds <- est[[1]] + data_woe$return_to_loan_term*est[[2]]+
                   data_woe$lastpay_tonow*est[[3]]+data_woe$days_last_p*est[[4]] + 
                   data_woe$sx_eff_rate*est[[5]] + data_woe$seven_days_logs*est[[6]]+
                   data_woe$seven_days_eff_logs*est[[7]]

score <- q + p*odds
hist(score,breaks = c(100),xlab = "����", ylab = 'Ƶ�����',main = '������Ƶ��ͼ',col = 'lightblue')

unique(data_woe$return_to_loan_term)
getscore(2,unique(data_woe$return_to_loan_term))

unique(data_woe$lastpay_tonow)
getscore(3,unique(data_woe$lastpay_tonow))

unique(data_woe$days_last_p)
getscore(4,unique(data_woe$days_last_p))

unique(data_woe$sx_eff_rate)
getscore(5,unique(data_woe$sx_eff_rate))

unique(data_woe$seven_days_logs)
getscore(6,unique(data_woe$seven_days_logs))

unique(data_woe$seven_days_eff_logs)
getscore(7,unique(data_woe$seven_days_eff_logs))


##############################���ڽ��������ӹ۲��������ı仯*************************************

data_woe$score <- score
maybe <- data_woe[order(data_woe$score,decreasing = T),]

auc <- NULL
cb <- NULL
fr <- NULL
rp<- NULL
goin <- NULL
for(i in seq(0.01,1,0.01)){
  len <- nrow(maybe)
  eggs <- rep(0,len)
  eggs[1:(len*i)] <-1
  sort <- maybe$sort
  confusion_matrix <- table(sort,eggs,dnn=c('real','prediction'))
  a <- as.data.frame(confusion_matrix)
  a1 <-as.numeric(as.character(a$real))
  a2<-as.numeric(as.character(a$prediction))
  accuracy <- sum(a$Freq[a1==a2])/sum(a$Freq)
  Sensitivity <- sum(a$Freq[((a1==1)&(a2==1))])/sum(a$Freq[a1==1])
  Specificity <- sum(a$Freq[((a1==0)&(a2==0))])/sum(a$Freq[a1==0])
  (real_in_prediction <- sum(a$Freq[(a1==1)&(a2==1)])/sum(a$Freq[a2==1]))
  predict_rate <- sum(a$Freq[a2==1])/sum(a$Freq)  #���ֵ�ı�����0.05
  call_back <- sum(a$Freq[(a1==1)&(a2==1)])/sum(a$Freq[a1==1])
  accuracy;Sensitivity;Specificity;real_in_prediction;predict_rate;call_back
  print(confusion_matrix)
  auc <- c(auc,accuracy)
  cb <- c(cb,call_back)
  fr <- c(fr,Specificity)
  rp <- c(rp,real_in_prediction)
  goin <- c(goin,predict_rate)
}

index <- data.frame(auc = auc*100, cb = cb*100, fr = fr*100,rp = rp*100)
matplot(index,type = 'l',lwd=3,col=c(1:4),xlab = '����ٷֱ�',ylab = '�ٷֱ�')
title(main = '�������Է����Ӱ��')
legend(83,80, inset=.05, legend=c('׼ȷ��','��ȷʶ��(�ٻ�)','����ʶ��','��Ϊ��(��ȷ�ȣ�'), pch=10, col=c(1:4), horiz=FALSE) 

ks_data(data_woe$score,data_woe$sort,10)
