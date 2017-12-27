setwd("D:/daily file/M2-M3/gz0922")
gc()
base_m<-read.table("development_0309.csv",,header=TRUE,sep=",",stringsAsFactors=FALSE)
base<-na.omit(base_m)

base$result<-base$next_dpd
base$result[base$next_dpd ==3] <- 0 #0表示未催回
base$result[base$next_dpd %in% c(0,1,2)] <- 1 #1表示催回

base$rtratio<-round(base$returned_1/base$mob,4)*100
base$panaratio2<-round(base$apenalty/base$CONTRACTAMT,4)*100
base$amtratio<-round((base$sa_dpay+base$sa_dint)/(base$sp_dpay+base$sp_dint),4)*100
base$mlratio<-round(base$mob/base$LOANTERM,4)*100
b_base<-data.frame(base[,c("CONTRACTNO","max_od","dif_e_mtran","month_return","dpay_p1","dint_p1","acc11","acc21","rtratio","panaratio2","amtratio","mlratio","result","enter_date","bal","yq_amt")])#选取有用变量及目标变量



#验证单变量是否单调
group<-b_base[,c("mlratio","result")]##选取单变量与目标变量
names(group)<-c("V1","result")
group_order<- group[order(group[,1]),] #按照单变量从小到大排序
group_freq<-table(group_order[,1]) #去重求频数
group_freq<-data.frame(group_freq) #转换为数据框，方便操作
group_freq$frequency <- round(group_freq$Freq/sum(group_freq$Freq),8) ##增加频率，并将展示格式规定为小数点后8位
group_freq$flag_0.05<-0
group_freq$flag_0.05[group_freq$frequency>0.05] <-1 #增加单独频率是否超过5%标识。0表示未超过，1表示超过
group_freq$sum_freq<-group_freq$frequency #增加累计频率字段
num_for<-length(group_freq[,1]) #计算for循环次数
#for循环逻辑：如果单独频率超过5%，则累计频率为其单独频率，否则为与上面所有为超过5%的频率的累计频率
for (i in 2:num_for) 
{if(group_freq$flag_0.05[i]==0 & group_freq$flag_0.05[i-1]==0)
  (group_freq$sum_freq[i]<-group_freq$frequency[i]+group_freq$sum_freq[i-1])
 else (group_freq$sum_freq[i]<-group_freq$frequency[i])}
#增加上面for循环的结果sum_freq值与5%的倍数关系标识,向上取整数
for (i in 1:num_for) 
{group_freq$flag_m[i]<-ceiling(group_freq$sum_freq[i]/0.05)}
#for循环逻辑：如果倍数标识与下一个一致，那么两个计数标识1一致，否则下一个计数标识1要加1
j<-1
for (i in 1: num_for)
{ if (group_freq$flag_m[i]==group_freq$flag_m[i+1])
  (group_freq$flag_c1[i]<-j) & (group_freq$flag_c1[i+1]<-j)
  if (group_freq$flag_m[i]!=group_freq$flag_m[i+1])
    (group_freq$flag_c1[i]<-j) & (group_freq$flag_c1[i+1]<-(j+1)) & (j<-(j+1))}
#特殊情况，如果超过5%的倍数标识刚好与上一个相同或者下一个或者同时相同
#for循环逻辑：单独频率超过5%的计数标识赋值为上下两个计数标识最大值+1，其他不变
group_freq$flag_c2<-group_freq$flag_c1
for (i in 2:num_for)
{if ((group_freq$flag_0.05[i]==1) & ((group_freq$flag_c1[i]==group_freq$flag_c1[i-1])|(group_freq$flag_c1[i]==group_freq$flag_c1[i+1])))
{if (group_freq$flag_c1[i-1]>=group_freq$flag_c1[i+1])
  (group_freq$flag_c2[i]<-group_freq$flag_c1[i-1]+1)
 else (group_freq$flag_c2[i]<-group_freq$flag_c1[i+1]+1)}}

group_merge<-merge(group,group_freq[,c("Var1","flag_c2")],by.x = "V1",by.y = "Var1",all.x = TRUE)#将分组结果返回给原文件
group_freq1<-data.frame(table(group_merge$flag_c2))#对计数标识去重求频数
names(group_freq1)[1]<-'flag_c2' #将频数表里的默认字段名Var1修改为原始字段名

group_freq1$flag_0.5<-0
group_freq1$flag_0.5[group_freq1$Freq<num_for/40]<-1#将频数表里单独频数小于平均频数1/2的标识为1
num_for1<-length(group_freq1[,1])#计算for循环次数
group_freq1$flag_hz<-group_freq1[,1]
#for循环逻辑：如果1/2标识为1，那么判断上下两个频数，将小于1/2的汇入频数更小的一组
for (i in 2:num_for1)
{if ((group_freq1$flag_0.5[i]==1) & (group_freq1$Freq[i-1]<group_freq1$Freq[i+1]))
  (group_freq1$flag_hz[i]<-group_freq1$flag_c2[i-1])}
for (i in 2:num_for1)
{ if ((group_freq1$flag_0.5[i]==2) & (group_freq1$Freq[i-1]>group_freq1$Freq[i+1]))
  (group_freq1$flag_hz[i]<-group_freq1$flag_c2[i+1])}
for (i in 2:num_for1)
{ if ((group_freq1$flag_0.5[i]==2) & (i=num_for1))
  (group_freq1$flag_hz[i]<-group_freq1$flag_c2[i-1])}
group_merge_end<-merge(group_merge,group_freq1[,c("flag_c2","flag_hz")],by.x = "flag_c2",by.y = "flag_c2",all.x = TRUE)#将最终分组结果返回给原文件
group_merge_end$flag_end<-as.numeric(group_merge_end$flag_hz)
#hist(group_merge_end$flag_end,breaks=40)
write.csv(group_merge_end,"mlratio_0309.csv")

b_base$MO<-b_base$max_od
b_base$MO[b_base$max_od<=30]<-1
b_base$MO[b_base$max_od>30 & b_base$max_od<=37]<-2
b_base$MO[b_base$max_od>37]<-3


b_base$DEM<-b_base$dif_e_mtran
b_base$DEM[b_base$dif_e_mtran<=31]<-1
b_base$DEM[b_base$dif_e_mtran>31 & b_base$dif_e_mtran<=48]<-2
b_base$DEM[b_base$dif_e_mtran>48 & b_base$dif_e_mtran<=56]<-3
b_base$DEM[b_base$dif_e_mtran>56 & b_base$dif_e_mtran<=60]<-4
b_base$DEM[b_base$dif_e_mtran>60 & b_base$dif_e_mtran<=61]<-5
b_base$DEM[b_base$dif_e_mtran>61]<-6

b_base$MR<-b_base$month_return
b_base$MR[b_base$month_return<=1300]<-1
b_base$MR[b_base$month_return>1300 & b_base$month_return<=1400]<-2
b_base$MR[b_base$month_return>1400 & b_base$month_return<=2000]<-3
b_base$MR[b_base$month_return>2000 & b_base$month_return<=3000]<-4
b_base$MR[b_base$month_return>3000]<-5

b_base$dpp1<-b_base$dpay_p1
b_base$dpp1[b_base$dpay_p1<=0]<-1
b_base$dpp1[b_base$dpay_p1>0 & b_base$dpay_p1<=370]<-2
b_base$dpp1[b_base$dpay_p1>370 & b_base$dpay_p1<=780]<-3
b_base$dpp1[b_base$dpay_p1>780 & b_base$dpay_p1<=1700]<-4
b_base$dpp1[b_base$dpay_p1>1700]<-5


b_base$dip1<-b_base$dint_p1
b_base$dip1[b_base$dint_p1<=0]<-1
b_base$dip1[b_base$dint_p1>0 & b_base$dint_p1<=140]<-2
b_base$dip1[b_base$dint_p1>140 & b_base$dint_p1<=515]<-3
b_base$dip1[b_base$dint_p1>515 & b_base$dint_p1<=1550]<-4
b_base$dip1[b_base$dint_p1>1550]<-5


b_base$AC1<-b_base$acc1
b_base$AC1[b_base$acc1<=3]<-1
b_base$AC1[b_base$acc1>3 & b_base$acc1<=10]<-2
b_base$AC1[b_base$acc1>10 & b_base$acc1<=13]<-3
b_base$AC1[b_base$acc1>13]<-4


b_base$AC2<-b_base$acc2
b_base$AC2[b_base$acc2<=1]<-1
b_base$AC2[b_base$acc2>1 & b_base$acc2<=2]<-2
b_base$AC2[b_base$acc2>2 ]<-3

b_base$rtr<-b_base$rtratio
b_base$rtr[b_base$rtratio<=29.17]<-1
b_base$rtr[b_base$rtratio>29.17 & b_base$rtratio<=41.94]<-2
b_base$rtr[b_base$rtratio>41.94 & b_base$rtratio<=55.26]<-3
b_base$rtr[b_base$rtratio>55.26 & b_base$rtratio<=73.53]<-4
b_base$rtr[b_base$rtratio>73.53]<-5


b_base$pnr2<-b_base$panaratio2
b_base$pnr2[b_base$panaratio2<=0]<-1
b_base$pnr2[b_base$panaratio2>0 & b_base$panaratio2<=1.22]<-2
b_base$pnr2[b_base$panaratio2>1.22 & b_base$panaratio2<=3.02]<-3
b_base$pnr2[b_base$panaratio2>3.02 & b_base$panaratio2<=6.28]<-4
b_base$pnr2[b_base$panaratio2>6.28 ]<-5


b_base$amr<-b_base$amtratio
b_base$amr[b_base$amtratio<=0]<-1
b_base$amr[b_base$amtratio>0 & b_base$amtratio<=42.19]<-2
b_base$amr[b_base$amtratio>42.19 & b_base$amtratio<=67.05]<-3
b_base$amr[b_base$amtratio>67.05 & b_base$amtratio<=83.86]<-4
b_base$amr[b_base$amtratio>83.86 ]<-5


b_base$mlr<-b_base$mlratio
b_base$mlr[b_base$mlratio<=29.17]<-1
b_base$mlr[b_base$mlratio>29.17 & b_base$mlratio<=47.92]<-2
b_base$mlr[b_base$mlratio>47.92 & b_base$mlratio<=64.58]<-3
b_base$mlr[b_base$mlratio>64.58 & b_base$mlratio<=86.11]<-4
b_base$mlr[b_base$mlratio>86.11 ]<-5


base_DS<-b_base[,c("MO","DEM","MR","dpp1","dip1","AC1","AC2","rtr","pnr2","amr","mlr","result","CONTRACTNO","enter_date","bal","yq_amt")]

#计算分组woe，ks，iv
test<-base_DS[,c("mlr","result")]
names(test)<-c("V1","result")
test$good_flag[test$result==0]<-1
test$bad_flag[test$result==1]<-1
test1 <- table(test[,c("V1","good_flag")])
test1<-data.frame(test1)
test2 <- table(test[,c("V1","bad_flag")])
test2<-data.frame(test2)

test_end<-merge(test1[,c("V1","Freq")],test2[,c("V1","Freq")],by.x = "V1",by.y = "V1",all.x = TRUE) #??????????????????????????
num_test<-length(test_end$V1)
for (i in 1:num_test)
{
  if (i==1)
     {test_end$Freq.x_sum[i]=test_end$Freq.x[i]
      test_end$Freq.y_sum[i]=test_end$Freq.y[i]
      test_end$Freq_sum[i]=test_end$Freq.x_sum[i]+test_end$Freq.y_sum[i]}
  else
     {test_end$Freq.x_sum[i]=test_end$Freq.x[i]+test_end$Freq.x_sum[i-1]
      test_end$Freq.y_sum[i]=test_end$Freq.y[i]+test_end$Freq.y_sum[i-1]
      test_end$Freq_sum[i]=test_end$Freq.x_sum[i]+test_end$Freq.y_sum[i]}
}
test_fun<-function(x,y,z,y_sum,z_sum){
  id<-x
  max_y_sum<-max(y_sum)
  max_z_sum<-max(z_sum)
  group_sum<-y+z
  sum=y_sum+z_sum
  G_per_row=round(y/group_sum,4)
  B_per_row=round(z/group_sum,4)
  G_per_col=round(y_sum/max_y_sum,4)
  B_per_col=round(z_sum/max_z_sum,4)
  GVSB=round(y/z,4)
  woe<-(y/max_y_sum)/(z/max_z_sum)
  ks<-(y_sum/max_y_sum)-(z_sum/max_z_sum) 
  iv<-((y/max_y_sum)-(z/max_z_sum))*log(woe)
  test_g<-data.frame(x,G_num=y,B_num=z,G_sum=y_sum,B_sum=z_sum,group_sum,sum,sum_pre=round(group_sum/max(sum),4),sum_pre_total=round(sum/max(sum),4),Bad_rate=B_per_row,G_per_col,B_per_col,GVSB,WOE=log(woe),KS=round(abs(ks)*100,2),max_KS=round(max(abs(ks)*100,4)),IV=round(iv,2),sum_IV=round(sum(iv),4))
}
test_mlr<-test_fun(test_end$V1,test_end$Freq.x,test_end$Freq.y,test_end$Freq.x_sum,test_end$Freq.y_sum)
#write.csv(test_DEM,"kb_DEM_0309.csv")


#创建in_time_validation
base_m_i<-read.table("intime_201701.csv",,header=TRUE,sep=",",stringsAsFactors=FALSE)
base_i<-na.omit(base_m_i)

base_i$result<-base_i$next_dpd
base_i$result[base_i$next_dpd ==3] <- 0 #0表示未催回
base_i$result[base_i$next_dpd %in% c(0,1,2)] <- 1 #1表示催回

base_i$rtratio<-round(base_i$returned_1/base_i$mob,4)*100
base_i$panaratio2<-round(base_i$apenalty/base_i$CONTRACTAMT,4)*100
base_i$amtratio<-round((base_i$sa_dpay+base_i$sa_dint)/(base_i$sp_dpay+base_i$sp_dint),4)*100
base_i$mlratio<-round(base_i$mob/base_i$LOANTERM,4)*100
b_base_i<-data.frame(base_i[,c("CONTRACTNO","max_od","dif_e_mtran","month_return","dpay_p1","dint_p1","acc11","acc21","rtratio","panaratio2","amtratio","mlratio","result","enter_date","bal","yq_amt")])#选取有用变量及目标变量

b_base_i$MO<-b_base_i$max_od
b_base_i$MO[b_base_i$max_od<=30]<-1
b_base_i$MO[b_base_i$max_od>30 & b_base_i$max_od<=37]<-2
b_base_i$MO[b_base_i$max_od>37]<-3


b_base_i$DEM<-b_base_i$dif_e_mtran
b_base_i$DEM[b_base_i$dif_e_mtran<=31]<-1
b_base_i$DEM[b_base_i$dif_e_mtran>31 & b_base_i$dif_e_mtran<=48]<-2
b_base_i$DEM[b_base_i$dif_e_mtran>48 & b_base_i$dif_e_mtran<=56]<-3
b_base_i$DEM[b_base_i$dif_e_mtran>56 & b_base_i$dif_e_mtran<=60]<-4
b_base_i$DEM[b_base_i$dif_e_mtran>60 & b_base_i$dif_e_mtran<=61]<-5
b_base_i$DEM[b_base_i$dif_e_mtran>61]<-6

b_base_i$MR<-b_base_i$month_return
b_base_i$MR[b_base_i$month_return<=1300]<-1
b_base_i$MR[b_base_i$month_return>1300 & b_base_i$month_return<=1400]<-2
b_base_i$MR[b_base_i$month_return>1400 & b_base_i$month_return<=2000]<-3
b_base_i$MR[b_base_i$month_return>2000 & b_base_i$month_return<=3000]<-4
b_base_i$MR[b_base_i$month_return>3000]<-5

b_base_i$dpp1<-b_base_i$dpay_p1
b_base_i$dpp1[b_base_i$dpay_p1<=0]<-1
b_base_i$dpp1[b_base_i$dpay_p1>0 & b_base_i$dpay_p1<=370]<-2
b_base_i$dpp1[b_base_i$dpay_p1>370 & b_base_i$dpay_p1<=780]<-3
b_base_i$dpp1[b_base_i$dpay_p1>780 & b_base_i$dpay_p1<=1700]<-4
b_base_i$dpp1[b_base_i$dpay_p1>1700]<-5


b_base_i$dip1<-b_base_i$dint_p1
b_base_i$dip1[b_base_i$dint_p1<=0]<-1
b_base_i$dip1[b_base_i$dint_p1>0 & b_base_i$dint_p1<=140]<-2
b_base_i$dip1[b_base_i$dint_p1>140 & b_base_i$dint_p1<=515]<-3
b_base_i$dip1[b_base_i$dint_p1>515 & b_base_i$dint_p1<=1550]<-4
b_base_i$dip1[b_base_i$dint_p1>1550]<-5


b_base_i$AC1<-b_base_i$acc1
b_base_i$AC1[b_base_i$acc1<=3]<-1
b_base_i$AC1[b_base_i$acc1>3 & b_base_i$acc1<=10]<-2
b_base_i$AC1[b_base_i$acc1>10 & b_base_i$acc1<=13]<-3
b_base_i$AC1[b_base_i$acc1>13]<-4


b_base_i$AC2<-b_base_i$acc2
b_base_i$AC2[b_base_i$acc2<=1]<-1
b_base_i$AC2[b_base_i$acc2>1 & b_base_i$acc2<=2]<-2
b_base_i$AC2[b_base_i$acc2>2 ]<-3

b_base_i$rtr<-b_base_i$rtratio
b_base_i$rtr[b_base_i$rtratio<=29.17]<-1
b_base_i$rtr[b_base_i$rtratio>29.17 & b_base_i$rtratio<=41.94]<-2
b_base_i$rtr[b_base_i$rtratio>41.94 & b_base_i$rtratio<=55.26]<-3
b_base_i$rtr[b_base_i$rtratio>55.26 & b_base_i$rtratio<=73.53]<-4
b_base_i$rtr[b_base_i$rtratio>73.53]<-5


b_base_i$pnr2<-b_base_i$panaratio2
b_base_i$pnr2[b_base_i$panaratio2<=0]<-1
b_base_i$pnr2[b_base_i$panaratio2>0 & b_base_i$panaratio2<=1.22]<-2
b_base_i$pnr2[b_base_i$panaratio2>1.22 & b_base_i$panaratio2<=3.02]<-3
b_base_i$pnr2[b_base_i$panaratio2>3.02 & b_base_i$panaratio2<=6.28]<-4
b_base_i$pnr2[b_base_i$panaratio2>6.28 ]<-5


b_base_i$amr<-b_base_i$amtratio
b_base_i$amr[b_base_i$amtratio<=0]<-1
b_base_i$amr[b_base_i$amtratio>0 & b_base_i$amtratio<=42.19]<-2
b_base_i$amr[b_base_i$amtratio>42.19 & b_base_i$amtratio<=67.05]<-3
b_base_i$amr[b_base_i$amtratio>67.05 & b_base_i$amtratio<=83.86]<-4
b_base_i$amr[b_base_i$amtratio>83.86 ]<-5


b_base_i$mlr<-b_base_i$mlratio
b_base_i$mlr[b_base_i$mlratio<=29.17]<-1
b_base_i$mlr[b_base_i$mlratio>29.17 & b_base_i$mlratio<=47.92]<-2
b_base_i$mlr[b_base_i$mlratio>47.92 & b_base_i$mlratio<=64.58]<-3
b_base_i$mlr[b_base_i$mlratio>64.58 & b_base_i$mlratio<=86.11]<-4
b_base_i$mlr[b_base_i$mlratio>86.11 ]<-5

base_DS_i<-b_base_i[,c("MO","DEM","MR","dpp1","dip1","AC1","AC2","rtr","pnr2","amr","mlr","result","CONTRACTNO","enter_date","bal","yq_amt")]


#创建逻辑回归模型
num<-length(base_DS$result)
num_1<-round(0.9*num)

set.seed(12345)
rand<-base_DS[order(runif(num)),]

train<-rand[1:num_1,]
test<-rand[(num_1+1):num,]     
glm_model<-glm(formula=result ~. , 
                  data=train[,c(-13,-14,-15,-16)], 
                  family=binomial)         
print(summary(glm_model))

#lm_model<-lm(formula=result ~. , 
 #              data=train[,c(-13,-14,-15,-16)] 
  #             )         
#print(summary(lm_model))



pre_train <- predict(glm_model,train[,c(-13,-14,-15,-16)])
p=exp(pre_train)/(1+exp(pre_train))#计算因变量的值
train$predicted=1*(p>0.5)#给train数据增加一列，也就是预测，当p>0.5时，预测值为1

TP<-length(train$result[train$result==1 & train$predicted==1])
FP<-length(train$result[train$result==1 & train$predicted==0])
FN<-length(train$result[train$result==0 & train$predicted==1])
TN<-length(train$result[train$result==0 & train$predicted==0])
precision<-TP/(TP+FP)
recall<-TP/(TP+FN)
accuracy<-(TP+TN)/(TP+FP+FN+TN)
F_score<-precision*recall#计算Recall，Precision和F-measure
print(precision)
print(recall)
print(accuracy)
print(F_score)


pre_test <- predict(glm_model,test[,c(-13,-14,-15,-16)])
p=exp(pre_test)/(1+exp(pre_test))##计算因变量的值
test$predicted=1*(p>0.5)#给train数据增加一列，也就是预测，当p>0.5时，预测值为1

TP<-length(test$result[test$result==1 & test$predicted==1])
FP<-length(test$result[test$result==1 & test$predicted==0])
FN<-length(test$result[test$result==0 & test$predicted==1])
TN<-length(test$result[test$result==0 & test$predicted==0])
precision_t<-TP/(TP+FP)
recall_t<-TP/(TP+FN)
accuracy_t<-(TP+TN)/(TP+FP+FN+TN)
F_score_t<-precision_t*recall_t##计算Recall，Precision和F-measure
print(precision_t)
print(recall_t)
print(accuracy_t)
print(F_score_t)

pre_intime <- predict(glm_model,base_DS_i[,c(-13,-14,-15,-16)])
p=exp(pre_intime)/(1+exp(pre_intime))##计算因变量的值
base_DS_i$predicted=1*(p>0.5)#给train数据增加一列，也就是预测，当p>0.5时，预测值为1

TP<-length(base_DS_i$result[base_DS_i$result==1 & base_DS_i$predicted==1])
FP<-length(base_DS_i$result[base_DS_i$result==1 & base_DS_i$predicted==0])
FN<-length(base_DS_i$result[base_DS_i$result==0 & base_DS_i$predicted==1])
TN<-length(base_DS_i$result[base_DS_i$result==0 & base_DS_i$predicted==0])
precision_intime<-TP/(TP+FP)
recall_intime<-TP/(TP+FN)
accuracy_intime<-(TP+TN)/(TP+FP+FN+TN)
F_score_intime<-precision_intime*recall_intime##计算Recall，Precision和F-measure
print(precision_intime)
print(recall_intime)
print(accuracy_intime)
print(F_score_intime)


library(pROC)
roc_train<-roc(train$result,pre_train)
roc_test <- roc(test$result,pre_test)
roc_intime <- roc(base_DS_i$result,pre_intime)

plot(roc_train, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="skyblue", print.thres=TRUE,main="development_sample ROC")

plot(roc_test, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="skyblue", print.thres=TRUE,main="in_time ROC")

plot(roc_intime, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="skyblue", print.thres=TRUE,main="out_time ROC")
#计算评分


base_DS_merge<-merge(base_DS,test_MO[,c("x","WOE")],by.x = "MO",by.y = "x",all.x =FALSE) 
names(base_DS_merge)[17]<-"WOE_MO"
base_DS_merge<-merge(base_DS_merge,test_DEM[,c("x","WOE")],by.x = "DEM",by.y = "x",all.x =FALSE) 
names(base_DS_merge)[18]<-"WOE_DEM"
base_DS_merge<-merge(base_DS_merge,test_MR[,c("x","WOE")],by.x = "MR",by.y = "x",all.x =FALSE) 
names(base_DS_merge)[19]<-"WOE_MR"
base_DS_merge<-merge(base_DS_merge,test_dpp1[,c("x","WOE")],by.x = "dpp1",by.y = "x",all.x =FALSE) 
names(base_DS_merge)[20]<-"WOE_dpp1"
base_DS_merge<-merge(base_DS_merge,test_dip1[,c("x","WOE")],by.x = "dip1",by.y = "x",all.x =FALSE) 
names(base_DS_merge)[21]<-"WOE_dip1"
base_DS_merge<-merge(base_DS_merge,test_AC1[,c("x","WOE")],by.x = "AC1",by.y = "x",all.x =FALSE) 
names(base_DS_merge)[22]<-"WOE_AC1"
base_DS_merge<-merge(base_DS_merge,test_AC2[,c("x","WOE")],by.x = "AC2",by.y = "x",all.x =FALSE) 
names(base_DS_merge)[23]<-"WOE_AC2"
base_DS_merge<-merge(base_DS_merge,test_rtr[,c("x","WOE")],by.x = "rtr",by.y = "x",all.x =FALSE) 
names(base_DS_merge)[24]<-"WOE_rtr"
base_DS_merge<-merge(base_DS_merge,test_pnr2[,c("x","WOE")],by.x = "pnr2",by.y = "x",all.x =FALSE) 
names(base_DS_merge)[25]<-"WOE_pnr2"
base_DS_merge<-merge(base_DS_merge,test_amr[,c("x","WOE")],by.x = "amr",by.y = "x",all.x =FALSE) 
names(base_DS_merge)[26]<-"WOE_amr"
base_DS_merge<-merge(base_DS_merge,test_mlr[,c("x","WOE")],by.x = "mlr",by.y = "x",all.x =FALSE) 
names(base_DS_merge)[27]<-"WOE_mlr"

base_DS_merge_end2<-base_DS_merge[c("WOE_MO","WOE_DEM","WOE_MR","WOE_dpp1","WOE_dip1","WOE_AC1","WOE_AC2","WOE_rtr","WOE_pnr2","WOE_amr","WOE_mlr")]

#对WOE值进行主成分分析
library(psych)
c<-cor(base_DS_merge_end2)
c
e<-eigen(c)
e
pc<-principal(base_DS_merge_end2,nfactors=4,score=TRUE)
head(pc)

base_DS_merge$RC1<-0.186*base_DS_merge$WOE_MO+0.327*base_DS_merge$WOE_DEM+0.934*base_DS_merge$WOE_dpp1+0.877*base_DS_merge$WOE_dip1+0.894*base_DS_merge$WOE_AC1+0.268*base_DS_merge$WOE_AC2+0.466*base_DS_merge$WOE_rtr+0.922*base_DS_merge$WOE_pnr2+0.889*base_DS_merge$WOE_amr+0.520*base_DS_merge$WOE_mlr

base_DS_merge$RC2<-0.908*base_DS_merge$WOE_MO+0.615*base_DS_merge$WOE_DEM+0.188*base_DS_merge$WOE_dpp1+0.293*base_DS_merge$WOE_dip1+0.225*base_DS_merge$WOE_AC1+0.899*base_DS_merge$WOE_AC2+0.168*base_DS_merge$WOE_rtr+0.306*base_DS_merge$WOE_pnr2+0.342*base_DS_merge$WOE_amr+0.108*base_DS_merge$WOE_mlr

base_DS_merge$RC3<-0.112*base_DS_merge$WOE_DEM+0.809*base_DS_merge$WOE_rtr+(-0.791)*base_DS_merge$WOE_mlr

base_DS_merge$RC4<-0.988*base_DS_merge$WOE_MR+0.175*base_DS_merge$WOE_dpp1+0.144*base_DS_merge$WOE_dip1+(-0.127)*base_DS_merge$WOE_rtr+(-0.120)*base_DS_merge$WOE_mlr

base_DS_merge_end3<-base_DS_merge[,c("RC1","RC2","RC3","RC4","result")]

glm_model_woe<-glm(formula=result ~. , 
                   data=base_DS_merge_end3, 
                   family=binomial)         
print(summary(glm_model_woe))

factor<-50/log(2)
offset<-500-factor*log(1)
base_DS_merge$score<-(-0.869184+((-0.133671)*base_DS_merge$RC1)+((-0.168799)*base_DS_merge$RC2)+((0.028083)*base_DS_merge$RC3)+((-0.349943)*base_DS_merge$RC4))*factor+offset
write.csv(base_DS_merge,"base_DS_merge_0309.csv")

#计算模型ks,分组

#重新赋值-score
base_DS_merge$s<-base_DS_merge$score
base_DS_merge$s[base_DS_merge$score<=361]<-1
base_DS_merge$s[base_DS_merge$score>361 & base_DS_merge$score<=371]<-2
base_DS_merge$s[base_DS_merge$score>371 & base_DS_merge$score<=402]<-3
base_DS_merge$s[base_DS_merge$score>402 & base_DS_merge$score<=416]<-4
base_DS_merge$s[base_DS_merge$score>416 & base_DS_merge$score<=431]<-5
base_DS_merge$s[base_DS_merge$score>431 & base_DS_merge$score<=445]<-6
base_DS_merge$s[base_DS_merge$score>445 & base_DS_merge$score<=460]<-7
base_DS_merge$s[base_DS_merge$score>460 & base_DS_merge$score<=478]<-8
base_DS_merge$s[base_DS_merge$score>478 & base_DS_merge$score<=497]<-9
base_DS_merge$s[base_DS_merge$score>497 ]<-10


base_DS_merge$sg<-base_DS_merge$score
base_DS_merge$sg[base_DS_merge$score<=407]<-'CR3'
base_DS_merge$sg[base_DS_merge$score>407 & base_DS_merge$score<=455]<-'CR2'
base_DS_merge$sg[base_DS_merge$score>455 ]<-'CR1'

#base_DS_merge$sg2<-base_DS_merge$sg<-'CR1'
#base_DS_merge$sg2[base_DS_merge$sg<='CR3' & base_DS_merge$bal>100000 & base_DS_merge$yq_amt>8400]<-'CR2'


write.csv(base_DS_merge,"base_DS_merge_end_0309.csv")



test<-base_DS_merge[,c("s","result")]
names(test)<-c("V1","result")
test$good_flag[test$result==0]<-1
test$bad_flag[test$result==1]<-1
test1 <- table(test[,c("V1","good_flag")])
test1<-data.frame(test1)
test2 <- table(test[,c("V1","bad_flag")])
test2<-data.frame(test2)

test_end<-merge(test1[,c("V1","Freq")],test2[,c("V1","Freq")],by.x = "V1",by.y = "V1",all.x = TRUE) #??????????????????????????
num_test<-length(test_end$V1)
for (i in 1:num_test)
{
  if (i==1)
  {test_end$Freq.x_sum[i]=test_end$Freq.x[i]
   test_end$Freq.y_sum[i]=test_end$Freq.y[i]
   test_end$Freq_sum[i]=test_end$Freq.x_sum[i]+test_end$Freq.y_sum[i]}
  else
  {test_end$Freq.x_sum[i]=test_end$Freq.x[i]+test_end$Freq.x_sum[i-1]
   test_end$Freq.y_sum[i]=test_end$Freq.y[i]+test_end$Freq.y_sum[i-1]
   test_end$Freq_sum[i]=test_end$Freq.x_sum[i]+test_end$Freq.y_sum[i]}
}
test_fun<-function(x,y,z,y_sum,z_sum){
  id<-x
  max_y_sum<-max(y_sum)
  max_z_sum<-max(z_sum)
  group_sum<-y+z
  sum=y_sum+z_sum
  G_per_row=round(y/group_sum,4)
  B_per_row=round(z/group_sum,4)
  G_per_col=round(y_sum/max_y_sum,4)
  B_per_col=round(z_sum/max_z_sum,4)
  GVSB=round(y/z,4)
  woe<-(y/max_y_sum)/(z/max_z_sum)
  ks<-(y_sum/max_y_sum)-(z_sum/max_z_sum) 
  iv<-((y/max_y_sum)-(z/max_z_sum))*log(woe)
  test_g<-data.frame(x,G_num=y,B_num=z,G_sum=y_sum,B_sum=z_sum,group_sum,sum,sum_pre=round(group_sum/max(sum),4),sum_pre_total=round(sum/max(sum),4),Bad_rate=B_per_row,G_per_col,B_per_col,GVSB,WOE=log(woe),KS=round(abs(ks)*100,2),max_KS=round(max(abs(ks)*100,4)),IV=round(iv,2),sum_IV=round(sum(iv),4))
}
test_s<-test_fun(test_end$V1,test_end$Freq.x,test_end$Freq.y,test_end$Freq.x_sum,test_end$Freq.y_sum)
write.csv(test_s,"kb_s.csv")


base_intime_merge<-merge(base_DS_i,test_MO[,c("x","WOE")],by.x = "MO",by.y = "x",all.x =FALSE) 
names(base_intime_merge)[17]<-"WOE_MO"
base_intime_merge<-merge(base_intime_merge,test_DEM[,c("x","WOE")],by.x = "DEM",by.y = "x",all.x =FALSE) 
names(base_intime_merge)[18]<-"WOE_DEM"
base_intime_merge<-merge(base_intime_merge,test_MR[,c("x","WOE")],by.x = "MR",by.y = "x",all.x =FALSE) 
names(base_intime_merge)[19]<-"WOE_MR"
base_intime_merge<-merge(base_intime_merge,test_dpp1[,c("x","WOE")],by.x = "dpp1",by.y = "x",all.x =FALSE) 
names(base_intime_merge)[20]<-"WOE_dpp1"
base_intime_merge<-merge(base_intime_merge,test_dip1[,c("x","WOE")],by.x = "dip1",by.y = "x",all.x =FALSE) 
names(base_intime_merge)[21]<-"WOE_dip1"
base_intime_merge<-merge(base_intime_merge,test_AC1[,c("x","WOE")],by.x = "AC1",by.y = "x",all.x =FALSE) 
names(base_intime_merge)[22]<-"WOE_AC1"
base_intime_merge<-merge(base_intime_merge,test_AC2[,c("x","WOE")],by.x = "AC2",by.y = "x",all.x =FALSE) 
names(base_intime_merge)[23]<-"WOE_AC2"
base_intime_merge<-merge(base_intime_merge,test_rtr[,c("x","WOE")],by.x = "rtr",by.y = "x",all.x =FALSE) 
names(base_intime_merge)[24]<-"WOE_rtr"
base_intime_merge<-merge(base_intime_merge,test_pnr2[,c("x","WOE")],by.x = "pnr2",by.y = "x",all.x =FALSE) 
names(base_intime_merge)[25]<-"WOE_pnr2"
base_intime_merge<-merge(base_intime_merge,test_amr[,c("x","WOE")],by.x = "amr",by.y = "x",all.x =FALSE) 
names(base_intime_merge)[26]<-"WOE_amr"
base_intime_merge<-merge(base_intime_merge,test_mlr[,c("x","WOE")],by.x = "mlr",by.y = "x",all.x =FALSE) 
names(base_intime_merge)[27]<-"WOE_mlr"


base_intime_merge$RC1<-0.186*base_intime_merge$WOE_MO+0.327*base_intime_merge$WOE_DEM+0.934*base_intime_merge$WOE_dpp1+0.877*base_intime_merge$WOE_dip1+0.894*base_intime_merge$WOE_AC1+0.268*base_intime_merge$WOE_AC2+0.466*base_intime_merge$WOE_rtr+0.922*base_intime_merge$WOE_pnr2+0.889*base_intime_merge$WOE_amr+0.520*base_intime_merge$WOE_mlr

base_intime_merge$RC2<-0.908*base_intime_merge$WOE_MO+0.615*base_intime_merge$WOE_DEM+0.188*base_intime_merge$WOE_dpp1+0.293*base_intime_merge$WOE_dip1+0.225*base_intime_merge$WOE_AC1+0.899*base_intime_merge$WOE_AC2+0.168*base_intime_merge$WOE_rtr+0.306*base_intime_merge$WOE_pnr2+0.342*base_intime_merge$WOE_amr+0.108*base_intime_merge$WOE_mlr

base_intime_merge$RC3<-0.112*base_intime_merge$WOE_DEM+0.809*base_intime_merge$WOE_rtr+(-0.791)*base_intime_merge$WOE_mlr

base_intime_merge$RC4<-0.988*base_intime_merge$WOE_MR+0.175*base_intime_merge$WOE_dpp1+0.144*base_intime_merge$WOE_dip1+(-0.127)*base_intime_merge$WOE_rtr+(-0.120)*base_intime_merge$WOE_mlr

factor<-50/log(2)
offset<-500-factor*log(1)
base_intime_merge$score<-(-0.869184+((-0.133671)*base_intime_merge$RC1)+((-0.168799)*base_intime_merge$RC2)+((0.028083)*base_intime_merge$RC3)+((-0.349943)*base_intime_merge$RC4))*factor+offset


#重新赋值-score
base_intime_merge$s<-base_intime_merge$score
base_intime_merge$s[base_intime_merge$score<=361]<-1
base_intime_merge$s[base_intime_merge$score>361 & base_intime_merge$score<=371]<-2
base_intime_merge$s[base_intime_merge$score>371 & base_intime_merge$score<=402]<-3
base_intime_merge$s[base_intime_merge$score>402 & base_intime_merge$score<=416]<-4
base_intime_merge$s[base_intime_merge$score>416 & base_intime_merge$score<=431]<-5
base_intime_merge$s[base_intime_merge$score>431 & base_intime_merge$score<=445]<-6
base_intime_merge$s[base_intime_merge$score>445 & base_intime_merge$score<=460]<-7
base_intime_merge$s[base_intime_merge$score>460 & base_intime_merge$score<=478]<-8
base_intime_merge$s[base_intime_merge$score>478 & base_intime_merge$score<=497]<-9
base_intime_merge$s[base_intime_merge$score>497 ]<-10


base_intime_merge$sg<-base_intime_merge$score
base_intime_merge$sg[base_intime_merge$score<=407]<-'CR3'
base_intime_merge$sg[base_intime_merge$score>407 & base_intime_merge$score<=455]<-'CR2'
base_intime_merge$sg[base_intime_merge$score>455 ]<-'CR1'


write.csv(base_intime_merge,"base_intime_merge_end01_0309.csv")



#新出评分部分

rating<-read.table("m2_20170315.csv",,header=TRUE,sep=",",stringsAsFactors=FALSE)
rating$rtratio<-round(rating$returned_1/rating$mob,4)*100
rating$panaratio2<-round(rating$apenalty/rating$CONTRACTAMT,4)*100
rating$amtratio<-round((rating$sa_dpay+rating$sa_dint)/(rating$sp_dpay+rating$sp_dint),4)*100
rating$mlratio<-round(rating$mob/rating$LOANTERM,4)*100

rating$MO<-rating$max_od
rating$MO[rating$max_od<=30]<-1
rating$MO[rating$max_od>30 & rating$max_od<=37]<-2
rating$MO[rating$max_od>37]<-3


rating$DEM<-rating$dif_e_mtran
rating$DEM[rating$dif_e_mtran<=31]<-1
rating$DEM[rating$dif_e_mtran>31 & rating$dif_e_mtran<=48]<-2
rating$DEM[rating$dif_e_mtran>48 & rating$dif_e_mtran<=56]<-3
rating$DEM[rating$dif_e_mtran>56 & rating$dif_e_mtran<=60]<-4
rating$DEM[rating$dif_e_mtran>60 & rating$dif_e_mtran<=61]<-5
rating$DEM[rating$dif_e_mtran>61]<-6

rating$MR<-rating$month_return
rating$MR[rating$month_return<=1300]<-1
rating$MR[rating$month_return>1300 & rating$month_return<=1400]<-2
rating$MR[rating$month_return>1400 & rating$month_return<=2000]<-3
rating$MR[rating$month_return>2000 & rating$month_return<=3000]<-4
rating$MR[rating$month_return>3000]<-5

rating$dpp1<-rating$dpay_p1
rating$dpp1[rating$dpay_p1<=0]<-1
rating$dpp1[rating$dpay_p1>0 & rating$dpay_p1<=370]<-2
rating$dpp1[rating$dpay_p1>370 & rating$dpay_p1<=780]<-3
rating$dpp1[rating$dpay_p1>780 & rating$dpay_p1<=1700]<-4
rating$dpp1[rating$dpay_p1>1700]<-5


rating$dip1<-rating$dint_p1
rating$dip1[rating$dint_p1<=0]<-1
rating$dip1[rating$dint_p1>0 & rating$dint_p1<=140]<-2
rating$dip1[rating$dint_p1>140 & rating$dint_p1<=515]<-3
rating$dip1[rating$dint_p1>515 & rating$dint_p1<=1550]<-4
rating$dip1[rating$dint_p1>1550]<-5


rating$AC1<-rating$acc1
rating$AC1[rating$acc1<=3]<-1
rating$AC1[rating$acc1>3 & rating$acc1<=10]<-2
rating$AC1[rating$acc1>10 & rating$acc1<=13]<-3
rating$AC1[rating$acc1>13]<-4


rating$AC2<-rating$acc2
rating$AC2[rating$acc2<=1]<-1
rating$AC2[rating$acc2>1 & rating$acc2<=2]<-2
rating$AC2[rating$acc2>2 ]<-3

rating$rtr<-rating$rtratio
rating$rtr[rating$rtratio<=29.17]<-1
rating$rtr[rating$rtratio>29.17 & rating$rtratio<=41.94]<-2
rating$rtr[rating$rtratio>41.94 & rating$rtratio<=55.26]<-3
rating$rtr[rating$rtratio>55.26 & rating$rtratio<=73.53]<-4
rating$rtr[rating$rtratio>73.53]<-5


rating$pnr2<-rating$panaratio2
rating$pnr2[rating$panaratio2<=0]<-1
rating$pnr2[rating$panaratio2>0 & rating$panaratio2<=1.22]<-2
rating$pnr2[rating$panaratio2>1.22 & rating$panaratio2<=3.02]<-3
rating$pnr2[rating$panaratio2>3.02 & rating$panaratio2<=6.28]<-4
rating$pnr2[rating$panaratio2>6.28 ]<-5


rating$amr<-rating$amtratio
rating$amr[rating$amtratio<=0]<-1
rating$amr[rating$amtratio>0 & rating$amtratio<=42.19]<-2
rating$amr[rating$amtratio>42.19 & rating$amtratio<=67.05]<-3
rating$amr[rating$amtratio>67.05 & rating$amtratio<=83.86]<-4
rating$amr[rating$amtratio>83.86 ]<-5


rating$mlr<-rating$mlratio
rating$mlr[rating$mlratio<=29.17]<-1
rating$mlr[rating$mlratio>29.17 & rating$mlratio<=47.92]<-2
rating$mlr[rating$mlratio>47.92 & rating$mlratio<=64.58]<-3
rating$mlr[rating$mlratio>64.58 & rating$mlratio<=86.11]<-4
rating$mlr[rating$mlratio>86.11 ]<-5
write.csv(rating,"rating.csv")

rating_DS<-rating[,c("MO","DEM","MR","dpp1","dip1","AC1","AC2","rtr","pnr2","amr","mlr","CONTRACTNO","enter_date","yq_amt","bal","dpd")]
rating_merge<-merge(rating_DS,test_MO[,c("x","WOE")],by.x = "MO",by.y = "x",all.x =FALSE) 
names(rating_merge)[17]<-"WOE_MO"
rating_merge<-merge(rating_merge,test_DEM[,c("x","WOE")],by.x = "DEM",by.y = "x",all.x =FALSE) 
names(rating_merge)[18]<-"WOE_DEM"
rating_merge<-merge(rating_merge,test_MR[,c("x","WOE")],by.x = "MR",by.y = "x",all.x =FALSE) 
names(rating_merge)[19]<-"WOE_MR"
rating_merge<-merge(rating_merge,test_dpp1[,c("x","WOE")],by.x = "dpp1",by.y = "x",all.x =FALSE) 
names(rating_merge)[20]<-"WOE_dpp1"
rating_merge<-merge(rating_merge,test_dip1[,c("x","WOE")],by.x = "dip1",by.y = "x",all.x =FALSE) 
names(rating_merge)[21]<-"WOE_dip1"
rating_merge<-merge(rating_merge,test_AC1[,c("x","WOE")],by.x = "AC1",by.y = "x",all.x =FALSE) 
names(rating_merge)[22]<-"WOE_AC1"
rating_merge<-merge(rating_merge,test_AC2[,c("x","WOE")],by.x = "AC2",by.y = "x",all.x =FALSE) 
names(rating_merge)[23]<-"WOE_AC2"
rating_merge<-merge(rating_merge,test_rtr[,c("x","WOE")],by.x = "rtr",by.y = "x",all.x =FALSE) 
names(rating_merge)[24]<-"WOE_rtr"
rating_merge<-merge(rating_merge,test_pnr2[,c("x","WOE")],by.x = "pnr2",by.y = "x",all.x =FALSE) 
names(rating_merge)[25]<-"WOE_pnr2"
rating_merge<-merge(rating_merge,test_amr[,c("x","WOE")],by.x = "amr",by.y = "x",all.x =FALSE) 
names(rating_merge)[26]<-"WOE_amr"
rating_merge<-merge(rating_merge,test_mlr[,c("x","WOE")],by.x = "mlr",by.y = "x",all.x =FALSE) 
names(rating_merge)[27]<-"WOE_mlr"


rating_merge$RC1<-0.186*rating_merge$WOE_MO+0.327*rating_merge$WOE_DEM+0.934*rating_merge$WOE_dpp1+0.877*rating_merge$WOE_dip1+0.894*rating_merge$WOE_AC1+0.268*rating_merge$WOE_AC2+0.466*rating_merge$WOE_rtr+0.922*rating_merge$WOE_pnr2+0.889*rating_merge$WOE_amr+0.520*rating_merge$WOE_mlr

rating_merge$RC2<-0.908*rating_merge$WOE_MO+0.615*rating_merge$WOE_DEM+0.188*rating_merge$WOE_dpp1+0.293*rating_merge$WOE_dip1+0.225*rating_merge$WOE_AC1+0.899*rating_merge$WOE_AC2+0.168*rating_merge$WOE_rtr+0.306*rating_merge$WOE_pnr2+0.342*rating_merge$WOE_amr+0.108*rating_merge$WOE_mlr

rating_merge$RC3<-0.112*rating_merge$WOE_DEM+0.809*rating_merge$WOE_rtr+(-0.791)*rating_merge$WOE_mlr

rating_merge$RC4<-0.988*rating_merge$WOE_MR+0.175*rating_merge$WOE_dpp1+0.144*rating_merge$WOE_dip1+(-0.127)*rating_merge$WOE_rtr+(-0.120)*rating_merge$WOE_mlr


factor<-50/log(2)
offset<-500-factor*log(1)
rating_merge$score<-(-0.869184+((-0.133671)*rating_merge$RC1)+((-0.168799)*rating_merge$RC2)+((0.028083)*rating_merge$RC3)+((-0.349943)*rating_merge$RC4))*factor+offset
#重新赋值-score
rating_merge$s<-rating_merge$score
rating_merge$s[rating_merge$score<=361]<-1
rating_merge$s[rating_merge$score>361 & rating_merge$score<=371]<-2
rating_merge$s[rating_merge$score>371 & rating_merge$score<=402]<-3
rating_merge$s[rating_merge$score>402 & rating_merge$score<=416]<-4
rating_merge$s[rating_merge$score>416 & rating_merge$score<=431]<-5
rating_merge$s[rating_merge$score>431 & rating_merge$score<=445]<-6
rating_merge$s[rating_merge$score>445 & rating_merge$score<=460]<-7
rating_merge$s[rating_merge$score>460 & rating_merge$score<=478]<-8
rating_merge$s[rating_merge$score>478 & rating_merge$score<=497]<-9
rating_merge$s[rating_merge$score>497 ]<-10


rating_merge$sg<-rating_merge$score
rating_merge$sg[rating_merge$score<=407]<-'CR3'
rating_merge$sg[rating_merge$score>407 & rating_merge$score<=455]<-'CR2'
rating_merge$sg[rating_merge$score>455 ]<-'CR1'


rating_merge$sg2<-'CR1'
#----------------------------------------------中心化后排名----------------------------------------#
rating_merge$sg2<-'CR1'
a <- rating_merge$sg %in% c('CR2','CR3')
b<-rating_merge[a,]
bal_normal <- scale(rating_merge$bal[a], center=F,scale=sd(rating_merge$bal))
amt_normal <- scale(rating_merge$yq_amt[a],center=F,scale=sd(rating_merge$yq_amt))

b$new <- bal_normal + amt_normal
b <-b[order(b$new,decreasing = T),]
b$sg2[1:(0.03*nrow(b))]  <- 'CR2' 
rating_merge$sg2[rating_merge$CONTRACTNO %in% b$CONTRACTNO[1:(0.03*nrow(b))]] <- 'CR2' 
write.csv(rating_merge,"rating_merge_0315.csv")


