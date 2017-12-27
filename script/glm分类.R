#----------------------------------------------------------读入数据-----------------------------------------
setwd("E:/order/data")
library(magrittr)
library(Hmisc)
library(ggplot2)
library(corrplot)
library(sqldf)
library(tcltk)
library(ipred)
library(randomForest)
#-----------------------------------------------------------------------------------------------------------
file_name_bad <- 'full_info_bad.csv'
info_bad_0 <- read.csv(file_name_bad,stringsAsFactors = FALSE)
info_bad <- info_bad_0
file_name_good <- 'full_info_good.csv'
info_good_0<- read.csv(file_name_good,stringsAsFactors = FALSE)
info_good <- info_good_0
info_bad$what =0
info_good$what=1
all_0 <- rbind(info_bad,info_good)
age_sex <- read.csv('age_sex.csv',stringsAsFactors = F)
maxrepay <- read.csv('maxrepaydate.csv',stringsAsFactors = F)
all_1 <- sqldf('select a.*,
               b.age0,
               b.sex
               from all_0 as a
               left join age_sex as b
               on a.customer_code = b.customer_code')

all_1 <- sqldf('select a.*,
               b.lastpay_tonow
               from all_1 as a
               left join maxrepay as b
               on a.contractno = b.contractno and
               a.enter_date2 = b.enter_date2')
rm(info_good,info_bad,all_0,age_sex,maxrepay)
#------------------------------------------------------------筛选数据--------------------------------------
#用好的组模拟一个情况
#好业绩中的产生联系部分数据框
#data_model <- all_1[all_1$what==1,]
#data_model <- data_model[data_model$sort <500,]
#data_model<- data_model[data_model$dpd %in% c(2:3),]

#好的催收业绩组中发生了联系的-最终输出为data_good
data_good <- all_1[all_1$what==1,]                          #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<可调节参数
data_bad <- data_good[data_good$sort==500,]
data_good <- data_good[data_good$sort<500,]

#bad组中抽样为没有联系的,抽样比例为1:1
#data_bad <- all_1[all_1$what==0,]
#data_bad[is.na(data_bad)] <-0
#data_bad <-data_bad[data_bad$sort==500,]

#没有随机种子，每一次运行都发生变化
data_bad <- data_bad[sample(1*nrow(data_bad),1*nrow(data_good)),]

#合成数据集最终进入模型中
data_model <- rbind(data_good,data_bad)
data_model<- data_model[data_model$dpd %in% c(4:6),]       #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<账期不同调整

#模型需要的字段选择
#----------------------------------------------进入选择的数据集为data_model数据框--------------------------------------------------------------------     
x <- c("sort","dpd",'days_last_p','if_jyd','if_new_case','if_seek','sv_due_rate',"lastpay_tonow" ,'return_to_loan_term','due_amount','sxdays_eff_logs')
data_fit <- data_model[,x]

#-----------------------------------------------------将变量变为分类变量----------------------------------------------------------------
#数据集中均为分类变量，其中sort为多分类的y,其余字段为0,1分类变量
#TRUE为1,值小为0
data_fit$days_last_p <- ifelse(is.na(data_fit$days_last_p)==TRUE,0,1)
data_fit$sv_due_rate <-ifelse(is.na(data_fit$sv_due_rate)==TRUE,0,1)
data_fit$return_to_loan_term <- with(data_fit,ifelse(return_to_loan_term <0.7,0,1))
data_fit$sxdays_eff_logs <- ifelse(is.na(data_fit$sxdays_eff_logs)==TRUE,0,1)
#DUE_AMOUNT 由dpd不同水平来判断
dpd_amount_level <- list()
for(i in 2:35){
  a <- all_1$due_amount[all_1$dpd==i]
  dpd_amount_level[i-1] <-(median(a,na.rm = T))
}
for(i in 2:35){
  data_fit[data_fit$dpd==i,]$due_amount <- ifelse((data_fit[data_fit$dpd==i,]$due_amount <= dpd_amount_level[i-1]),0,1)
}

#lastpay_tonow 由dpd的不同水平决定,将其中NA值变为1
dpd_tonow_level <- list()
for(i in 2:35){
  a <- all_1$lastpay_tonow[all_1$dpd==i]
  dpd_tonow_level[i-1] <-(median(a,na.rm = T))
}
for(i in 2:35){
  data_fit[data_fit$dpd==i,]$lastpay_tonow <- ifelse((data_fit[data_fit$dpd==i,]$lastpay_tonow <= dpd_tonow_level[i-1]),0,1)
}
data_fit$lastpay_tonow[is.na(data_fit$lastpay_tonow)]=1



########################################################################################################### 
                                 ##########形成的表的模型##############
                                      ###################
data_sort <- data_fit                                                           ##########################
data_sort$sort <- with(data_sort,ifelse(sort<=490,1,0))                         ###########################

random <- sample(1:nrow(data_sort),0.8*nrow(data_sort))
data_model_use <- data_sort[random,]
enter_model_one <- data_model_use %>% randomForest(sort~.-dpd,data = .,ntree=30,replace=T,type=classification)
  #glm(sort~.-dpd,family=binomial(link = "logit"),data=.)
###########################################################################################################
#----------------------------------------------加入prd的变量进行选择--------------------------------------#
data_prd <- data_model
#仅在联系的里面模拟产品的不同
data_prd <- subset(data_prd,sort !=500)  

#################################################选择需要的变量############################################
#sort 为因变量，同时,'age0',last_visit_days,prd为需要分类的变量，
x <- c('sort','prd','age0','last_visit_days')

data_prd <- data_prd[,x]
data_prd$prd <-with(data_prd,
                    ifelse(prd %in% c('精英贷','精英贷（银行合作）'),3,
                           ifelse(prd %in% c('新薪贷','薪薪贷（银行合作）','新薪宜楼贷'),2,
                                  ifelse(prd %in% c('助业贷','助业宜楼贷'),1,0))))
#lastpay_tonow 由dpd的不同水平决定,将其中NA值变为1
prd_age_level <- list()
for(i in 0:3){
  a <- data_prd$age0[data_prd$prd==i]
  prd_age_level[i+1] <-(median(a,na.rm = T))
}
for(i in 0:3){
  data_prd[data_prd$prd==i,]$age0 <- ifelse((data_prd[data_prd$prd==i,]$age0 <= prd_age_level[i+1]),1,0)
}
data_prd$age0[is.na(data_prd$age0)]=1

data_prd$last_visit_days <- ifelse(data_prd$last_visit_days<90,1,0)
data_prd$last_visit_days[is.na(data_prd$last_visit_days)] <-0

##################################################################################################################################################
                                                         #########表选顺序择的模型###########
                                                              ###################
data_sort <- data_prd
data_sort$sort <- with(data_sort,ifelse(sort<=10,1,0))

random <- sample(1:nrow(data_sort),0.8*nrow(data_sort))
data_model_use <- data_sort[random,]
enter_model_two <- data_model_use %>% glm(sort~.,family=binomial(link = "logit"),data=.)
###################################################################################################################################################
#---------------------------------------------------------------------------------------------------------------------------------------------------


#--------------------------------------------------------------------开始第一个模型-----------------------------------------------------------------
x <- c("sort","dpd",'days_last_p','if_jyd','if_new_case','if_seek','sv_due_rate',"lastpay_tonow" ,'return_to_loan_term','due_amount','sxdays_eff_logs',
       'prd','age0','last_visit_days')

#---------------------------------------------------------------------数据分类处理--------------------------------------
data_new <-all_1[,x]
x <- c("sort","dpd",'days_last_p','if_jyd','if_new_case','if_seek','sv_due_rate',"lastpay_tonow" ,'return_to_loan_term','due_amount','sxdays_eff_logs')
data_fit <-data_new[,x]
data_fit$days_last_p <- ifelse(is.na(data_fit$days_last_p)==TRUE,0,1)
data_fit$sv_due_rate <-ifelse(is.na(data_fit$sv_due_rate)==TRUE,0,1)
data_fit$return_to_loan_term <- with(data_fit,ifelse(return_to_loan_term <0.7,0,1))
data_fit$sxdays_eff_logs <- ifelse(is.na(data_fit$sxdays_eff_logs)==TRUE,0,1)
#DUE_AMOUNT 由dpd不同水平来判断
dpd_amount_level <- list()
data_fit$dpd[is.na(data_fit$dpd)]<-0
for(i in 2:35){
  a <- all_1$due_amount[all_1$dpd==i]
  dpd_amount_level[i-1] <-(median(a,na.rm = T))}

for(i in 2:35){
  data_fit[data_fit$dpd==i,]$due_amount <- 
    ifelse((data_fit[data_fit$dpd==i,]$due_amount <= dpd_amount_level[i-1]),0,1)}

#lastpay_tonow 由dpd的不同水平决定,将其中NA值变为1
dpd_tonow_level <- list()
for(i in 2:35){
  a <- all_1$lastpay_tonow[all_1$dpd==i]
  dpd_tonow_level[i-1] <-(median(a,na.rm = T))}

for(i in 2:35){
  data_fit[data_fit$dpd==i,]$lastpay_tonow <- ifelse((data_fit[data_fit$dpd==i,]$lastpay_tonow <= dpd_tonow_level[i-1]),0,1)
}
data_fit$lastpay_tonow[is.na(data_fit$lastpay_tonow)]=1
#------------------------------------------------------------------------------------------------------------------------

########################################################################进行分类################################################
data_new$pred <- predict(enter_model_one,data_fit,type='response')
data_from_model1 <- data_new[data_new$pred >0.5,]


#-------------------------------------------------------------------开始第二个模型--------------------------------------
x <- c('sort','prd','age0','last_visit_days')

data_prd <- data_from_model1[,x]
data_prd$prd <-with(data_prd,
                    ifelse(prd %in% c('精英贷','精英贷（银行合作）'),3,
                           ifelse(prd %in% c('新薪贷','薪薪贷（银行合作）','新薪宜楼贷'),2,
                                  ifelse(prd %in% c('助业贷','助业宜楼贷'),1,0))))
#lastpay_tonow 由dpd的不同水平决定,将其中NA值变为1
prd_age_level <- list()
for(i in 0:3){
  a <- data_prd$age0[data_prd$prd==i]
  prd_age_level[i+1] <-(median(a,na.rm = T))
}
for(i in 0:3){
  data_prd[data_prd$prd==i,]$age0 <- ifelse((data_prd[data_prd$prd==i,]$age0 <= prd_age_level[i+1]),1,0)
}
data_prd$age0[is.na(data_prd$age0)]=1

data_prd$last_visit_days <- ifelse(data_prd$last_visit_days<90,1,0)
data_prd$last_visit_days[is.na(data_prd$last_visit_days)] <-0

###############################################################进行分类##########################################
data_from_model1$pred <-predict(enter_model_two,data_prd,type='response')
data_from_model2 <-data_from_model1[data_from_model1$pred>0.5,]
sum(data_from_model2$sort==500)/nrow(data_from_model2)

model_1_num <- nrow(data_from_model1)/nrow(data_fit)
model_2_num <- nrow(data_from_model2)/nrow(data_from_model1)

paste('第一个模型的筛选的概率为：',round(model_1_num*100,2),"%")

paste('第二个模型筛选出的概率为：',round(model_2_num*100,2),'%')

