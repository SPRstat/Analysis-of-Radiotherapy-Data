#  EDA [EXPLORATORY DATA ANALYSIS]
library("lattice") 
library("heplots") 
library("MASS")
library("rpart") 
library("rgl") 
library("RVAideMemoire") 
library("MVN") 
library("ggplot2")
library("psych")
library("mda")
library("biotools")
D = read.table("C:\\Users\\Dell\\OneDrive\\Desktop\\Radio-Therapy\\radiotherapy.txt",header = FALSE,sep = ",") 

## Univariate EDA

## Average number of symptoms
summary(D$V1)
plot(D$V1,main = "Avg Number of Symptoms")
hist(D$V1,main = "Avg Number of Symptoms")
boxplot(D$V1,main = "Avg Number of Symptoms")
qqmath(D$V1,D,main = "Avg Number of Symptoms")

#Group-Wise
histogram(~V1|V6,D,main = "Avg Number of Symptoms")
densityplot(~V1|V6,D,main = "Avg Number of Symptoms")
qqmath(~V1|V6,D,main = "Avg Number of Symptoms")
boxplot(V1~V6,D,main = "Avg Number of Symptoms")

#AVG AMOUNT OF ACTIVITY
summary(D$V2)
plot(D$V2, main = "Avg Amount of Activity")
hist(D$V2, main = "Avg Amount of Activity")
boxplot(D$V2,  main = "Avg Amount of Activity")
qqmath(D$V2,D, main = "Avg Amount of Activity")

#Group-Wise
histogram(~V2|V6,D, main = "Avg Amount of Activity")
densityplot(~V2|V6,D, main = "Avg Amount of Activity")
qqmath(~V2|V6,D, main = "Avg Amount of Activity")
boxplot(V2~V6,D, main = "Avg Amount of Activity")


#AVG AMOUNT OF SLEEP
summary(D$V3)
plot(D$V3, main = "Avg Amount of Sleep")
hist(D$V3, main = "Avg Amount of Sleep")
boxplot(D$V3, main = "Avg mount of Sleep")
qqmath(D$V3,D, main = "Avg Amount of Sleep")

#Group-Wise
histogram(~V3|V6,D,main = "Avg Amount of Sleep")
densityplot(~V3|V6,D,main = "Avg Amount of Sleep")
qqmath(~V3|V6,D,main = "Avg Amount of Sleep")
boxplot(V3~V6,D,main = "Avg Amount of Sleep")

#AVG AMOUNT OF FOOD CONSUMED
summary(D$V4)
plot(D$V4,main = "Avg Amount of Food Consumption")
hist(D$V4,main = "Avg Amount of Food Consumption")
boxplot(D$V4,main = "Avg Amount of Food Consumption")
qqmath(D$V4,D,main = "Avg Amount of Food Consumption")


#Group-Wise
histogram(~V4|V6,D,main = "Avg Amount of Food Consumption")
densityplot(~V4|V6,D,main = "Avg Amount of Food Consumption")
qqmath(~V4|V6,D,main = "Avg Amount of Food Consumption")
boxplot(V4~V6,D,main = "Avg Amount of Food Consumption")

#AVG AMOUNT OF APPETITE
summary(D$V5)
plot(D$V5,main = "Avg Amount of Appetite")
hist(D$V5,main = "Avg Amount of Appetite")
boxplot(D$V5,main = "Avg Amount of Appetite")
qqmath(D$V5,D,main = "Avg Amount of Appetite")

#Group-Wise
histogram(~V5|V6,D,main = "Avg Amount of Appetite")
densityplot(~V5|V6,D,main = "Avg Amount of Appetite")
qqmath(~V5|V6,D,main = "Avg Amount of Appetite")
boxplot(V5~V6,D,main = "Avg Amount of Appetite")

#BIVARIATE EDA

scatterplot(D$V1,D$V2,data = D,regline = FALSE,smooth = FALSE,groups = D$V6,xlab = "Avg Number of Symptoms",ylab = "Avg Amount of Activity")
scatterplot(D$V1,D$V3,data = D,regline = FALSE,smooth = FALSE,groups = D$V6,xlab = "Avg Number of Symptoms",ylab = "Avg Amount of Sleep")
scatterplot(D$V1,D$V4,data = D,regline = FALSE,smooth = FALSE,groups = D$V6,xlab = "Avg Number of Symptoms",ylab = "Avg Amount of Food Consumed")
scatterplot(D$V1,D$V5,data = D,regline = FALSE,smooth = FALSE,groups = D$V6,xlab = "Avg Number of Symptoms",ylab = "Avg Appetite")
scatterplot(D$V2,D$V3,data = D,regline = FALSE,smooth = FALSE,groups = D$V6,xlab = "Avg Amount of Activiy",ylab = "Avg Amount of Sleep")
scatterplot(D$V2,D$V4,data = D,regline = FALSE,smooth = FALSE,groups = D$V6,xlab = "Avg Amount of Activiy",ylab = "Avg Amount of Food Consumed")
scatterplot(D$V2,D$V5,data = D,regline = FALSE,smooth = FALSE,groups = D$V6,xlab = "Avg Amount of Activiy",ylab = "Avg Amount of Appetite")
scatterplot(D$V3,D$V4,data = D,regline = FALSE,smooth = FALSE,groups = D$V6,xlab = "Avg Amount of Sleep",ylab = "Avg Amount of Food Consumed")
scatterplot(D$V3,D$V5,data = D,regline = FALSE,smooth = FALSE,groups = D$V6,xlab = "Avg Amount of Sleep",ylab = "Avg Amount of Appetite")
scatterplot(D$V4,D$V5,data = D,regline = FALSE,smooth = FALSE,groups = D$V6,xlab = "Avg Amount of Food Consumed",ylab = "Avg Amount of Appetite")



## Trivariate EDA

cloud(D$V1~D$V2*D$V3,D,groups = D$V6) 


cloud(D$V1~D$V2*D$V4,D,groups = D$V6) 


cloud(D$V1~D$V2*D$V5,D,groups = D$V6) 


cloud(D$V1~D$V3*D$V4,D,groups = D$V6) 


cloud(D$V1~D$V3*D$V5,D,groups = D$V6) 


cloud(D$V1~D$V4*D$V5,D,groups = D$V6) 


cloud(D$V2~D$V3*D$V4,D,groups = D$V6) 


cloud(D$V2~D$V3*D$V5,D,groups = D$V6) 


cloud(D$V2~D$V4*D$V5,D,groups = D$V6) 


cloud(D$V3~D$V4*D$V5,D,groups = D$V6) 

## Test Of (Univariate) Normality 

library("lattice") 
library("heplots") 
library("MASS")
library("rpart") 
library("rgl") 
library("RVAideMemoire") 
library("MVN") 
library("ggplot2")
D = read.table("C:\\Users\\Dell\\OneDrive\\Desktop\\Radio-Therapy\\radiotherapy.txt",header = FALSE,sep = ",") 
head(D)

Y1 = sqrt(D$V1)
Y2 = D$V3 
Y3 = sqrt(D$V4) 
Y4 = D$V5
D.Y = cbind(Y1,Y2,Y3,Y4)


d_0=D[D$V6==0,c(-2,-6)]## Table for the variables when X6=0 
d_1=D[D$V6==1,c(-2,-6)]## Table for the variables when X6=1 
d_2=D[D$V6==2,c(-2,-6)]## Table for the variables when X6=2 
d_3=D[D$V6==3,c(-2,-6)]## Table for the variables when X6=3 


## Kolmogorov-Smirnoff Test 

## Testing Normality For V1

ks.test(D$V1,"pnorm")
# Not Normal Null hyopthesis is Rejected 


## Testing Normality For V2

ks.test(D$V2,"pnorm")


## Testing Normality For V3

ks.test(D$V3,"pnorm")


## Testing Normality For V4

ks.test(D$V4,"pnorm")

## Testing Normality For V5

ks.test(D$V5,"pnorm")


## Shapiro Wilk's Test

## Testing Normality For V1

shapiro.test(D$V1)

## Testing Normality For V2

shapiro.test(D$V2)

## Testing Normality For V3

shapiro.test(D$V3)

## Testing Normality For V4

shapiro.test(D$V4)

## Testing Normality For V5

shapiro.test(D$V5)


##  Transformation to Convert Non-Normal Variables To Normal

##  V1 ( Average number of symptoms throughout the treatment )

shapiro.test(sqrt(D$V1))    ## Checking Normality After doing sqrt transformation over V1

##  V4 ( Average amount of through Food consumed throughout the treatment )

shapiro.test(sqrt(D$V4))    ## Checking Normality After doing sqrt transformation over V4

##  V2 ( Average amount of ???Activity??? throughout the treatment )

shapiro.test(sqrt(D$V2))   ## Checking Normality After doing sqrt transformation over V2

## we apply the Box-Cox transformation and check how it performs.

boxcox(lm(D$V2 ~ 1))

shapiro.test((1-(D$V2)^(-0.5))/0.5)  ## Checking Normality after doing Boxcox Transformation over V2

## we apply the Johnson transformation and check how it performs.

power_trans=function(x,lamda){ 
  if (x>=0) 
  { if (lamda !=0){ 
    y = ((x+1)^(lamda)-1)/lamda    
  } 
    else {     
      y = log(x+1)  
    }  
  } 
  else {    
    if (lamda!=2){    
      y=-((-x+1)^(2-lamda)-1)/(2-lamda)  
    }   
    else 
    {     
      y=-log(-x+1)    
    }  
  }  
  y 
} 

power_trans_vec=function(x,lamda){ 
  y=array(0) 
  for (i in 1:length(x)){ 
    y[i]=power_trans(x[i],lamda)   
  }   
  y 
}

Johnson = function(x,lamda){  
  n=length(x)   
  lam=array(0) 
  for (i in 1:length(lamda)){    
    y=array(0)   
    y=power_trans_vec(x,lamda[i])    
    var=(n-1)*var(y)/n    
    lam[i]=-(log(2*pi)+log(var))*(n/2)-((sum((y-mean(y))^2))/(2*var))+(lamda[i]-1)*sum(sign(x)*log(abs(x)+1))  
  }  
  
  a=as.data.frame(cbind(lam,lamda)) 
  m=lamda[which(lam==max(lam))]  
  ggplot(data=a,aes(x=lamda,y=lam))+
    geom_line(col=2)+geom_vline(xintercept = m)+theme_light() 
}
Johnson(D$V2,c(-1.5,-1.3,-1.2,-0.9,-0.8)) 
X_2 = power_trans_vec(D$V2,-1.2)

## We apply this transformation and check for normality.

shapiro.test(X_2)


##  Tests of (Multivariate) Normality

## Tests
 
## Royston's test 

mvn(D.Y, mvnTest = "royston")

## Group Wise Testing

## Group 0

mvn(d_0, mvnTest = "royston")

## Group 1

mvn(d_1, mvnTest = "royston")

## Group 2

mvn(d_2, mvnTest = "royston")

## Group 3

mvn(d_3, mvnTest = "royston")


## Dropping V2 variable.

cor(D$V2,D$V3)  ## Corr b/w V2 & V3

##  Testing Of Independence B/w V2 & V3

cor.test(D$V2,D$V3,method = "kendall",alternative = "less")

## Using Factor Analysis

fa.none <- fa(r=D[,-6],nfactors = 2,fm="pa",max.iter=100,rotate="varimax",scores="regression")
fa.diagram(fa.none)


##  Testing Equality of Population Cov Matrices and means

## Box's M test

boxM(cbind(D$V1,D$V3,D$V4,D$V5),D$V6)

## MANOVA

m = manova(cbind(V1,V3,V4,V5)~V6,D)  #Working Model 
summary(m)


## LDA ( Linear Discriminant Analysis )

## We apply the method of ( One - Out ) Cross-Validation to assess how well LDA performs. 

D1 = D[,-2]
s = 0    

for(i in 1:98) 
{  
  d = D1[-i,]  
  l = lda(V6~.,d)  
  c = as.numeric((l%>%predict(D1[i,]))$class)-1  
  if(c != D1[i,5]) s = s+1  
}

## Calculating APER (By method of ( One - Out ) Cross-Validation)

s/98

## Confusion Matrix

Y5 = D$V6
d.y = as.data.frame(cbind(Y1,Y2,Y3,Y4,Y5))

Cross_valid_error=function(d) 
{   
  pred_val=unlist(lapply(seq_along(rownames(d)),function(x){predict(lda(Y5~.,d[-x,],tol=10^(-8)),d[x,])$class})) 
  return(aer(d.y$Y5,pred_val))
} 

Cross_valid_confusion=function(d)
{   
  pred_val=unlist(lapply(seq_along(rownames(d)), function(x){predict(lda(Y5~.,d[-x,],tol=10^(-8)),d[x,])$class}))  
  return(confusion(Y5,pred_val)) 
}

lda=lda(d.y$Y5~.,d.y,tol=10^(-8)) 

Cross_valid_confusion(d.y)  

plot(as.matrix(d.y[-1])%*%as.matrix(coef(lda)),col=d.y$Y5,pch=16) 


## PCA [ Principal Component Analysis ]

my_pca <- prcomp(D.Y, scale = TRUE,center = TRUE, retx = T)
names(my_pca)

## Summary Of the PCA

summary(my_pca)

## Bi Plot

biplot(my_pca, main = "Biplot", scale = 0)

## ??? The plots showing proportion of variance explained by 
## each principal component and cumulative proportion of variance explained by each component are as follows :

my_pca.var <- my_pca$sdev^2 
propve <- my_pca.var / sum(my_pca.var)
par(mfrow = c(1,2))
plot(propve, xlab = "principal component", ylab = "Proportion of Variance Explained", ylim = c(0, 1), type = "b",main = "Scree Plot")
plot(cumsum(propve),xlab = "Principal Component", ylab = "Cumulative Proportion of Variance Explained",ylim = c(0, 1), type = "b") 

##  Factor Analysis

## For Full Model

## Bartlett's test

cortest.bartlett(D) 

## ??? We first check with one factor. 

fa.none <- fa(r=D[,-6],nfactors = 1,fm="pa",max.iter=100,rotate="varimax",scores="regression")
fa.none
fa.diagram(fa.none)

## ??? We now check with two factors. 

fa.none <- fa(r=D[,-6],nfactors = 2,fm="pa",max.iter=100,rotate="varimax",scores="regression")
fa.none
fa.diagram(fa.none)

##  Model after dropping V2

## PCA METHOD

## ??? We first check for one factor :

factanal.none.f <- factanal(D.Y, factors=1, scores = c("regression"), rotation = "varimax")
print(factanal.none.f)

## ??? We now check for two factor :

factanal.none.f <- factanal(D.Y, factors=2, scores = c("regression"), rotation = "varimax")
print(factanal.none.f)

## Maximum Likelihood Method

## ??? We first check for one factor :

fa.none <- fa(r=cbind(Y1,Y2,Y3,Y4),nfactors = 1,fm="pa",max.iter=100,rotate="varimax",scores="regression")
fa.none
fa.diagram(fa.none)

## ??? We now check for two factor :

fa.none <- fa(r=cbind(Y1,Y2,Y3,Y4),nfactors = 2,fm="pa",max.iter=100,rotate="varimax",scores="regression")
fa.none
fa.diagram(fa.none)
