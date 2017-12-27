setwd("E:/order/data")
library(magrittr)
library(Hmisc)
library(ggplot2)
library(corrplot)
library(sqldf)
library(tcltk)
library(e1071)
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
  #-------------------第二个模型
data_good <- data_good[order(data_good$enter_date2),]
plot(1:nrow(data_good),data_good$due_amount)  #时序特征

  #多分类的变量模型，还是从logit模型开始
  info_model_two <- read.csv('info_model_two.csv',stringsAsFactors = F)
  data_good <- all_1[all_1$what==1,]                          #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<可调节参数为业绩好的                                                    #
  data_bad <- data_good[data_good$sort==500,]                                                                                                                  #
  data_good <- data_good[data_good$sort<500,]
  #合并需要的数据
  data_good1 <-merge(data_good,info_model_two,by.x= "customer_code",by.y ="CUSTOMER_CODE" , all.x = T)
  
  #---------------------EDAdata--------------------------------------
  x <- c('sort','marry_staus1','acdamic_type1','province','age0','due_amount')
  #需要用acdamic_type和age来代表一个新的东东，或者直接放在x中
  data_good1<- data_good1[data_good1$dpd %in% c(4:6),]  
  explore_data <- data_good1[,x]
  summary(explore_data)
  explore_data <- na.omit(explore_data)
  cor(explore_data)
  cov(explore_data)
    table(explore_data$marry_staus1)
    table(explore_data$acdamic_type1)
    table(explore_data$province)
    
  explore_data <-explore_data[order(explore_data$sort),]
  
  
  #模型出来
  for(i in c(9,18,27)){
    data_model_two <- explore_data
    data_model_two$sort <- ifelse(data_model_two$sort<18,1,0)
    data_model_two$due_amount <-ifelse(data_model_two$due_amount<8000,0,1)
    data_model_two$age0 <- ifelse(data_model_two$age0<35,0,
                                  ifelse(data_model_two$age0<45,1,2))
    model_two <- glm(sort~I(due_amount^2)+age0+marry_staus1+acdamic_type1,data=data_model_two,family = binomial(link='logit'))
    print(summary(model_two))
    data_model_two$sort <- as.factor(data_model_two$sort)
    data_model_two$marry_staus1 <- as.factor(data_model_two$marry_staus1)
    data_model_two$acdamic_type1 <- as.factor(data_model_two$acdamic_type1)
    data_model_two$province <- as.factor(data_model_two$province)
    data_model_two$due_amount <-as.factor(data_model_two$due_amount)
    data_model_two$age0 <- as.factor(data_model_two$age0)
    tree <- rpart(sort~.,data = data_model_two,control = rpart.control(cp = 0.000))
    rpart.plot::rpart.plot(tree)
  }
  
  enter_model_two <-model_two
  

  