#单变量测试KS用-----------------------------------------------------------


dlt <- data.frame(V1 = data_p_mon[,4],V2 = data_p_mon[,1])
dlt <- dlt[order(dlt$V1),]
dlt <- round(dlt,4)


#----------------read.data/updata---------------------------------
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
  df3$WOE <- log(W)
  
  dfend1 <- data.frame(组别=id,
                       组距=df1$Var1,
                       好用户数量=df1$Freq,
                       累计好用户数= df3$goodacc,
                       好用户占比=df3$goodratio*100,
                       累计好用户数占比=df3$goodaccratio*100,
                       坏用户数=df2$Freq,
                       累计坏用户数=df3$badacc,
                       坏用户占比=df3$badratio*100,
                       累计坏用户数占比=df3$badaccratio*100,
                       累计用户数=df3$useracc,
                       坏账率=df3$baddebtrate*100,
                       KS数据源=df3$KSgroup*100,
                       好坏用户比=df3$goodratiobad,
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
#-------------------------------------------------------------------
#各列的ks值大小
ks <-NULL
iv <- NULL
for(j in 2:11){
dlt <- data.frame(V1 = data_p_mon[,8],V2 = data_p_mon[,1])
dlt <- dlt[order(dlt$V1),]
dlt <- round(dlt,4)
分组数量 <- 50
dltfun <- development_sample_fun(dlt$V1,dlt$V2,分组数量)
KSdlt <- abs(max(dltfun$KS数据源))
IVdlt <- sum(dltfun$IV数据源)
ks <- c(ks,KSdlt)
iv <- c(iv,IVdlt)
}
print(rbind(colnames(data_p_mon)[2:11],ks))
print(rbind(colnames(data_p_mon)[2:11],iv))



