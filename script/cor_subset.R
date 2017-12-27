#-------------------------------工作路径----------------------------#
setwd("E:/order/data")
library(magrittr)
library(Hmisc)
library(ggplot2)
library(corrplot)
library(sqldf)
library(parallel)
file_name_bad <- 'full_info_bad.csv'
connect <- 'connect.csv'
#-----------------------------------------
info_bad_0 <- read.csv(file_name_bad)
connected<- read.csv(connect,stringsAsFactors = FALSE)
info_bad_0<- sqldf('select a.*,
                 b.order_connected
                 from info_bad_0 as a 
                 left join connected as b 
                 on a.user_name = b.user_name and
                 a.customer_code = b.customer_code and
                 a.enter_date2 = b.oper_time and 
                 a.score = b.score')
info_bad <- subset(info_bad_0,order_connected == '2')                       # 1表示前30 2为30-70 3为70-100 4为不联系的

#缺失值填补
info_bad$th_due_rate[is.na(info_bad$th_due_rate)]=0
info_bad$sv_due_rate[is.na(info_bad$sv_due_rate)]=0
info_bad$th_kp_rate[is.na(info_bad$th_kp_rate)]=0
info_bad$sx_kp_rate[is.na(info_bad$sx_kp_rate)]=0
info_bad$sx_eff_rate[is.na(info_bad$sx_eff_rate)]=0
info_bad$th_eff_rate[is.na(info_bad$th_eff_rate)]=0
info_bad$return_to_loan_term[is.na(info_bad$return_to_loan_term)]=0
info_bad$bal_prin2 <-impute(info_bad$bal_prin2,median)
info_bad$return_to_loan_term <-impute(info_bad$return_to_loan_term,mean)
info_bad$bal_percent <- impute(info_bad$bal_percent,mean)

info_bad$last_visit_days[is.na(info_bad$last_visit_days)]=max(info_bad$last_visit_days)
info_bad$days_last_p[is.na(info_bad$days_last_p)]=max(info_bad$days_last_p)

info_bad$no_call_days[is.na(info_bad$no_call_days)]=0


##########################################################################
########################--------numerics--------##########################
##########################################################################
#将因变量放在第一个

num <- info_bad[,c('bal_prin2','due_amount','list_bal_prin2','th_due_rate','sv_due_rate',
                   'th_kp_rate','sx_kp_rate','return_to_loan_term','bal_percent','th_eff_rate','sx_eff_rate')]

#------------------------------------some feature in num_money------------------------------------#
head(num)
num<- summary(num)

#密度图
ggplot(num,aes(bal_prin2)) +geom_density()

#相关性

num_cor <-scale(num)
num_cor <-cor(num_cor)
num_cov <- cov(num_cor)
#---------------------------------画图----------------------#
corrplot(num_cor)
corrplot(num_cor, method="shade", shade.col=NA, tl.col="black", tl.srt=45)
# Generate a lighter palette
#col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
#corrplot(money_bad_cor, method="shade", shade.col=NA, tl.col="black", tl.srt=45,
#         col=col(200), addCoef.col="black", addcolorlabel="no", order="AOE")
#######



###############################################################################
#########################    定性变量的相关                        ############
###############################################################################
####将要运算的数据赋值给data_go，其类型应该是data.frame其中第一个为因变量
something <- c('order_connected','if_rec','thdays_logs','sxdays_logs','thdays_eff_logs','sxdays_eff_logs','count_th_ptp',
               'count_th_kp','count_sx_ptp','count_sx_kp','thdays_ptp','thdays_kp','sxdays_ptp','sxdays_kp',
               'last_code','last_activitive','last_visit_days','if_new_case','if_new_rtp','if_jyd','if_seek')

if_what <- c('if_rec','if_new_case','if_new_rtp','if_jyd','if_seek','on_ptp')

withtime <-c('order_connected','LOANTERM','rtp','returned_term','tim','dpd','hisdue_times',
             'maxdue_days','to_rtp_days','no_call_days','on_ptp','days_last_p',
             'thdays_payoff_times','svdays_payoff_times')

data_go <-info_bad[,withtime]

#------------------------------phi系数CRAMER系数以及P值--------------------------#
relation <- vector()  # 相依系数
phi <-vector()  #phi系数 适用于2*2的变量系数_无序分类的相关指标
v <- vector()  # cramer系数适用于2*2的变量系数也适用于高于2*2的相关系数_无序分类的相关指标
chisq_p <- vector() #卡方的ｐ值

times <- ncol(data_go)

  for(i in 2:times){
    table2 <-table(data_go[,c(1,i)])
    x <- as.numeric(chisq.test(table2)$statistic[1])
    n <- nrow(table2)
    chisq_p <- cbind(chisq_p,chisq.test(table2,simulate.p.value = T,B = 200)$p.value)
    relation <- cbind(relation,c(sqrt(x/(x+n))))
    phi <- cbind(phi,c(sqrt(x/(n))))
    #cramer's phi 的调整 在4格列联表中没有上限制
    a<-(min(nrow(table2),ncol(table2)))
    v <- cbind(v,c(sqrt(phi[i-1]^2/a)))
  }

#colnames(relation)<-colnames(data_go)[2:times]
#colnames(phi)<-colnames(data_go)[2:times]
#colnames(v)<-colnames(data_go)[2:times]
#colnames(chisq_p)<- colnames(data_go)[2:times]

#---------------------------------------绘图------------------------------------------#
#生成数据框
relation_rate<-data.frame(name=colnames(data_go)[2:times],re=relation[1:(times-1)])
phi_rate <- data.frame(name=colnames(data_go)[2:times],phi = phi[1:(times-1)])
cramer_rate <- data.frame(name=colnames(data_go)[2:times],cramer = v[1:(times-1)])
chisq_pvalue <- data.frame(name=colnames(data_go)[2:times],chisq_p = chisq_p[1:(times-1)])

#
ggplot(relation_rate, aes(x=re, y=reorder(name, re))) +
  geom_point(size=3) + # Use a larger dot
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour="grey60", linetype="dashed"))+
  geom_vline(xintercept = median(relation[1:(times-1)]))+labs(title = "相依系数无序分类的相关指标")

ggplot(phi_rate, aes(x=phi, y=reorder(name, phi))) +
  geom_point(size=3) + # Use a larger dot
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour="grey60", linetype="dashed"))+
  geom_vline(xintercept = median(phi[1:(times-1)]))+labs(title = "phi系数无序分类的相关指标")

ggplot(cramer_rate, aes(x=cramer, y=reorder(name, cramer))) +
  geom_point(size=3) + # Use a larger dot
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour="grey60", linetype="dashed"))+
  geom_vline(xintercept = median(v[1:(times-1)]))+labs(title = "cramer系数无序分类的相关指标")

ggplot(chisq_pvalue, aes(x=chisq_p, y=reorder(name, chisq_p))) +
  geom_point(size=3) + # Use a larger dot
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour="grey60", linetype="dashed"))+
  labs(title = "卡方检验的P-value") +xlim(c(0,0.5))

