x <- c(3,5,4,6,3,2,1,4,3,2)  
y <- c('c','c','d','b','a','b','d','e','e','d')  
z <- c(1,2,3,4,5,6,7,8,9,10)  
testData <- data.frame(x=x,y=y,z=z)  
o <- order(testData[,"x"],TestData[,"y"])  
testData[o,] 
## example with character variables and NAs
testDF <- data.frame(v1 = c(1,3,5,7,8,3,5,NA,4,5,7,9),
                     v2 = c(11,33,55,77,88,33,55,NA,44,55,77,99) )
by1 <- c("red", "blue", 1, 2, NA, "big", 1, 2, "red", 1, NA, 12)
by2 <- c("wet", "dry", 99, 95, NA, "damp", 95, 99, "red", 99, NA, NA)
aggregate(x = testDF, by = list(by1, by2), FUN = "mean")

#-------------------------------------------------------04WiEEPr
a<-data.frame(x=c("A","B","A","B","C","C"),y=c("a","a","b","a","b","c"),z=c(6,2,3,4,5,6))
library(dplyr)
a %>% group_by(x, y) %>% mutate(rank_z = rank(z)) 

vDates <- as.Date(c("2013-06-01", "2013-07-08", "2013-09-01", "2013-09-15"))
vDates.bymonth <- cut(vDates, breaks = "month") 
Dates <- data.frame(vDates, vDates.bymonth)
filter(Hdma_dat,pclass == 1)
select(Hdma_dat,pclass,survived) ##Q!Tqpclass1dA?  

result1<-aggregate(orders$AMOUNT, orders[,c("SELLERID","CLIENT")],sum)  
result2<-aggregate(orders$AMOUNT, orders[,c("SELLERID","CLIENT")],max)  
result<-cbind(result1,result2$x)

#----------------------------------------------------------------7VWiEEPr
data <- data.frame(
  class=c(1, 1, 1, 2, 2, 2, 3, 3, 3),
  height=c(12, 16, 15, 24, 17, 20, 11, 15, 20),
  weight=c(3.6, 4.3, 4.7, 6.3, 2.4, 3.8, 5.5, 4.4, 2.2)
)

data %>%
  group_by(class) %>%
  arrange(height)
#---------------------------------------------I>3}VX84PP----------  
test<- data.frame(x = c(1:10,1:3), y=1:13)

test[!duplicated(test$x), ]
  
odd <- function(x){
  n = length(x)
  k = 1
  odd <- c()
    for(i in 1:n){
      odd <-c(k)
      if(x[i+1] > x[i]){
        k = k + 1
        odd <-c(k,k)
      }
    }
}
  #-----------------------------------------------gagaga---------------
  data <-aggregate(data_test$pred,data_test[,c(2,3)],FUN=rank,ties.method=c('first'))
  data$max <-lapply(data$x,max)
  
  sort <- vector()
  for(i in 1:nrow(data)){
    b <-data$max[[i]]-data$x[[i]] +1
    sort <- c(sort,b)
  }
  sort1 <-as.numeric(as.matrix(unlist(data$x)))
  data_test$sort_pred <- sort
  
  #--------------------------shijianxuanze---------------
  data_model_pre$enter_date2 <- as.Date(data_model_pre$enter_date2)
  data_for_fit <- filter(data_model_pre,enter_date2 < '2016/6/30')
  data_for_test <- filter(data_model_pre,enter_date2 >= '2016/6/30')
  
  #------------------------------------------------------------------------
  ##=;2f<lQiD#PM
  library(boot)
  data_cross <- data_fit
  data_cross$sort <- ifelse(data_cross$sort != 500, 1, 0)
  cv.glm(data_cross,md_glm,K =10)$delta
  
  cost <- function(r, pi = 0) mean(abs(r-pi) > 0.5) 
  cv.glm(data_cross,md_glm2,cost,K =10)$delta
  
  
  data_nnet <- as.matrix(data_fit)
  md_nnet <- nnet(sort~.-if_seek-seven_days_eff_logs-age0-sv_due_rate,data = data_fit,size=3)
  sum(predict(md_nnet,data_test[,-c(1,2,3)]) == 1)
  
  library(e1071)
  md_svm <- svm(sort~.-if_seek-seven_days_eff_logs-age0-sv_due_rate,data = data_fit)
  
  
  
  ##--------------------------------
  data <- add_data
  b <- vector()
  k <- 1                    #TZD#PMVP8C
  l <- 8                    #Wn:sR;8v?*J<5D6+6+#,C?4N6<8|8D
  for(j in 1:2){            #Q-;74NJ}C?4N<uIY
    a1 <- vector()
    k <- k+1
    l <- l+1
    for(i in l:10){
      md <- glm(data[,1]~data[,k]+data[,k+1]+data[,k+2]+data[,k+3]+data[,k+4]+data[,k+5]+data[,k+6]+data[,i],data=data,family = binomial(link = 'logit'))
      a <- summary(md)$coefficients[-1,]
      a1 <- rbind(a1,a)
    }
    b <- rbind(b,a1)
  }
  
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
  
  appear_times <- function(x,y){
    #FdVPxN*APC{,yN*GP8n5D5c#,2;0|:,WnP!:MWn4sV5
    #3vOVR;8vkniff1dA?
  low <- min(x)
  high <- max(x)
  kniff <<- as.numeric(cut(x,c(low,y,high),include.lowest = T))
  kniff[is.na(kniff)] = 0
  a <- unique(kniff)
    for(i in a){
    if(sum(kniff == i) < 10){
      print("4fTZP!SZ105D7VWi")}
    else{
      print("7VWi6<4sSZ108v")
    }
  }
  }
  
  ##------------------------------------------------------------------ksV5
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
    
    dfend1 <- data.frame(组别=id,
                           组距=df1$Var1,
                           坏用户数量=df1$Freq,
                           累计坏用户数= df3$goodacc,
                           坏用户占比=df3$goodratio*100,
                           累计坏用户数占比=df3$goodaccratio*100,
                           好用户数=df2$Freq,
                           累计好用户数=df3$badacc,
                           好用户占比=df3$badratio*100,
                           累计好用户数占比=df3$badaccratio*100,
                           累计用户数=df3$useracc,
                           回收率=df3$baddebtrate*100,
                           KS数据源=df3$KSgroup*100,
                           坏好用户比=df3$goodratiobad,
                           用户数=df3$user,
                           用户数占比=df3$userratio*100,
                           累计用户占比=df3$useraccratio*100,
                           WOE=df3$WOE,
                           IV数据源=df3$IVgroup)
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
    MixMax <- M1(dfend1$组距)
    m <- as.data.frame(MixMax)
    m1 <- data.frame(分组=dfend1$组别,当组最小值=m$V1,当组最大值=m$V2)
    m2 <- cbind(m1,dfend2)
    
  }
  
  
  ks_data <- function(x,y,z){
    #xJG4}GP8n5DV5#,yN*sortV5
    dlt <- data.frame(V1 = x,V2 = y)
    dlt <- dlt[order(dlt$V1),]
    dlt <- round(dlt,4)
    dltfun <<- development_sample_fun(dlt$V1,dlt$V2,z)
    KSdlt <- abs(max(dltfun$KS数据源))
    IVdlt <- sum(dltfun$IV数据源)
  }
  
  
  
  
  #------------------------------------ks值和iv值----------------------------------------
  #输入x = 0,1变量；y = 实际的值,z = 打的标签分组
  #计算输出ks的值
  woe <- function(x,y){
    
    ##形成需要的数据集
    gaga <- data.frame(what = x,kniff = y)
    gaga <- na.omit(gaga)
    rows_bad <- sum(gaga$what==0);rows_good <- sum(gaga$what == 1)
    
    #ks值woe和iv值
    ks <- NULL
    woe <- NULL
    iv_kniff <- NULL
    good <-  NULL
    bad <- NULL
    kniff <<- unique(y)[order(unique(y))]
    for(i in kniff){
      gogo <- filter(gaga,kniff==i)
      gogo <- na.omit(gogo)
      
      #ks值
      good1 <- sum(gogo$what==1)/rows_good
      bad1 <- sum(gogo$what==0)/rows_bad
      good <- c(good,good1)
      bad <- c(bad,bad1)
      
      #woe
      woe1 <- (sum(gogo$what==1)/rows_good)/(sum(gogo$what==0)/rows_bad)
      woe1 <- log(woe1)
      woe <- c(woe,woe1)
      
      #iv未累加
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
  
  