#---------------------------------------------Optimal Binning for Scoring Modeling

if(FALSE){
  "自动分箱，给变量在不同区间打标签，让X和Y的拟合时候根据的敏感，这种和特征工程很像"
  "包里提供的方法, 感谢让我学习一下(o???▽???)o  "
  
}

# Package loading and data exploration
library(smbinning) # Load package and its data
data(chileancredit) # Load smbinning sample dataset (Chilean Credit)

# Quick description of the data
str(chileancredit) 

# Tabulate target variable
table(chileancredit$FlagGB) 

# Training and testing samples (Just some basic formality for Modeling) 
chileancredit.train=subset(chileancredit,FlagSample==1)
chileancredit.test=subset(chileancredit,FlagSample==0)

# Package application
result=smbinning(df=chileancredit.train,y="FlagGB",x="TOB",p=0.05) # Run and save result
result$ivtable # Tabulation and Information Value
result$iv # Information value
result$bands # Bins or bands
result$ctree # Decision tree from partykit


#------------------------------------Customized Binning
# Package loading and data exploration
library(smbinning) # Load package and its data
data(chileancredit) # Load smbinning sample dataset (Chilean Credit)
str(chileancredit) # Quick description of the data
table(chileancredit$FlagGB) # Tabulate target variable

# Training and testing samples (Just some basic formality for Modeling) 
chileancredit.train=subset(chileancredit,FlagSample==1)
chileancredit.test=subset(chileancredit,FlagSample==0)

# Remove exclusions from chileancredit dataset
TOB.train=
  subset(chileancredit,(FlagSample==1 & (FlagGB==1 | FlagGB==0)), select=TOB)
TOB.test=
  subset(chileancredit,(FlagSample==0 & (FlagGB==1 | FlagGB==0)), select=TOB)

# Custom cutpoints using percentiles (20% each)
TOB.Pct20=quantile(TOB.train, probs=seq(0,1,0.2), na.rm=TRUE)
TOB.Pct20.Breaks=as.vector(quantile(TOB.train, probs=seq(0,1,0.2), na.rm=TRUE))
Cuts.TOB.Pct20=TOB.Pct20.Breaks[2:(length(TOB.Pct20.Breaks)-1)]

# Package application and results
result=
  smbinning.custom(df=chileancredit.train,
                   y="FlagGB",x="TOB",cuts=Cuts.TOB.Pct20) # Run and save
result$ivtable # Tabulation and Information Value


#-------------------------------------------------Exploratory Data Analysis (EDA)
# Package loading and data exploration
library(smbinning) # Load package and its data
data(chileancredit) # Load smbinning sample dataset (Chilean Credit)

# Training and testing samples (Just some basic formality for Modeling) 
chileancredit.train=subset(chileancredit,FlagSample==1)
chileancredit.test=subset(chileancredit,FlagSample==0)

# EDA application
smbinning.eda(chileancredit.train,rounding=3)$eda # Table with basic statistics.
smbinning.eda(chileancredit.train,rounding=3)$edapct # Table with basic percentages.

# Package loading and data exploration
library(smbinning) # Load package and its data
data(chileancredit) # Load smbinning sample dataset (Chilean Credit)
chileancredit.train=subset(chileancredit,FlagSample==1)
chileancredit.test=subset(chileancredit,FlagSample==0)
result=smbinning(df=chileancredit.train,y="FlagGB",x="TOB",p=0.05) # Run and save result

#----------------------Binning on Factor Variables
# Package loading and data exploration
library(smbinning) # Load package and its data
data(chileancredit) # Load smbinning sample dataset (Chilean Credit)
str(chileancredit) # Quick description of the data
table(chileancredit$FlagGB) # Tabulate target variable

# Training and testing samples (Just some basic formality for Modeling) 
chileancredit.train=subset(chileancredit,FlagSample==1)
chileancredit.test=subset(chileancredit,FlagSample==0)

# Package application and results
result.train=smbinning.factor(df=chileancredit.train,
                              y="FlagGB",x="IncomeLevel")
result.train$ivtable
result.test=smbinning.factor(df=chileancredit.test,
                             y="FlagGB",x="IncomeLevel")
result.test$ivtable

# Plots
par(mfrow=c(2,2))
smbinning.plot(result.train,option="dist",sub="Income Level (Tranining Sample)")
smbinning.plot(result.train,option="badrate",sub="Income Level (Tranining Sample)")
smbinning.plot(result.test,option="dist",sub="Income Level (Test Sample)")
smbinning.plot(result.test,option="badrate",sub="Income Level (Test Sample)")

#------------------------------------------------------Plots after binning
# Plots
par(mfrow=c(2,2))
boxplot(chileancredit.train$TOB~chileancredit.train$FlagGB,
        horizontal=TRUE, frame=FALSE, col="lightgray",main="Distribution")
mtext("Time on Books (Months)",3)
smbinning.plot(result,option="dist",sub="Time on Books (Months)")
smbinning.plot(result,option="badrate",sub="Time on Books (Months)")
smbinning.plot(result,option="WoE",sub="Time on Books (Months)")

#------------------------------------------------information value Summary
# Package loading and data exploration
library(smbinning) # Load package and its data
data(chileancredit) # Load smbinning sample dataset (Chilean Credit)

# Training and testing samples (Just some basic formality for Modeling) 
chileancredit.train=subset(chileancredit,FlagSample==1)
chileancredit.test=subset(chileancredit,FlagSample==0)

# Summary IV application
sumivt=smbinning.sumiv(chileancredit.train,y="FlagGB")
sumivt # Display table with IV by characteristic
smbinning.sumiv.plot(sumivt,cex=0.8) # Plot IV summary table

#----------------------------------------------------Plot Information Value Summary
# Package loading and data exploration
library(smbinning) # Load package and its data
data(chileancredit) # Load smbinning sample dataset (Chilean Credit)

# Training and testing samples (Just some basic formality for Modeling) 
chileancredit.train=subset(chileancredit,FlagSample==1)
chileancredit.test=subset(chileancredit,FlagSample==0)

# Plotting smbinning.sumiv
sumivt=smbinning.sumiv(chileancredit.train,y="FlagGB")
sumivt # Display table with IV by characteristic
smbinning.sumiv.plot(sumivt,cex=0.8) # Plot IV summary table


