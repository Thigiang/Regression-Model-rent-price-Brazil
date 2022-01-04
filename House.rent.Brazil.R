setwd("/Users/gabati/Documents/SJSU_FALL21/261A/Assignment/Project data/House to rent/archive (9)/")
library(ggplot2)
library(readxl)
library(MASS)
library(leaps)
library(olsrr)
house.rent<-read.csv("houses_to_rent_v2.csv")
#View(house.rent)
#Defining function to plot residuals and qqplot
residualPlot<-function(data,reg,name){
  plot(data, rstudent(reg), xlab=name)
}
qqplot<-function(reg){
  qqnorm(rstudent(reg))
  qqline(rstudent(reg))
}

      #use average to deal with missing values in floor feature
      house.rent$floor[which(house.rent$floor=="-")]<-0
      house.rent$floor<-as.numeric(house.rent$floor)
      house.rent$floor[which(house.rent$floor==0)]<-round(mean(house.rent$floor),0)
      
  ###SUMMARYDATA
      ###CATEGORICAL 
      library(ggplot2)
      city<-ggplot(data = house.rent,aes(x=city, y=rent.amount..R..))+
        geom_bar(stat='identity',col='skyblue')
      
      
      animal<-ggplot(data = house.rent,aes(x=animal, y=rent.amount..R..))+
        geom_bar(stat='identity',col='blue')
      
      fur<-ggplot(data = house.rent,aes(x=furniture, y=rent.amount..R..))+
        geom_bar(stat='identity',col='skyblue')
      
      library("ggpubr")
      library("cowplot")
      (cate<-ggarrange(city, animal,fur, ncol=3,nrow=1))
      
      #####NUMERICAL
      #area: might need to drop area that is greater than 1000  :  655 2398 2424 3560 4814 5130 5916 8791 9242
      summary.data<-function(ordata,new){
        summ<-ggtexttable((round(summary(ordata),2)))
        boxpl<-ggplot(data=house.rent, aes(x=ordata))+
          geom_boxplot(col='blue',fill='skyblue1')+
          scale_y_discrete()
        
        boxpl1<-ggplot(data=NULL,aes(x=new))+
          geom_boxplot(col='blue',fill='skyblue1')+
          scale_y_discrete()
        ggarrange(boxpl+coord_flip(),summ,boxpl1+coord_flip(), ncol=3,nrow=1,widths = c(2,1))
      }
      
      summary.data(house.rent$area,x$AR)
      
#Cleaning data before we work on the regression model based on the above plot
      dropped.value<-c(6244,6646,2860,256, 6980,6231,2929, 1445,655,2398,2424,3560,4814,5130,5916,8791,9242,2563)
      x<-house.rent[,c(1:9,11:12)]
      x<-x[-dropped.value,]
      y<-house.rent$rent.amount..R..
      y<-y[-dropped.value]
      names(x)<-c("C","AR","R","B","PK","FL","AN","FR","HOA","Tax","I")
      x2<-cbind(x,x$AR^2,x$R^2,x$B^2,x$PK^2,x$FL^2,x$HOA^2,x$Tax^2,x$I^2)
      names(x2)<-c("C","AR","R","B","PK","FL","AN","FR","HOA","Tax","I","AR^2","R^2","B^2","PK^2","FL^2","HOA^2","Tax^2","I^2")

#Check multicolinearity with cor()
      cor(x2[,c(2:6,9:19)]) #seem like they are correlated (more than 60%), the term squares are strongly correlated
#USING CENTER TO REDUCE THE MULTICOLINEARITY 
      cx<-sweep(x2[,c(2:6,9:11)],2,FUN="-",apply(x2[,c(2:6,9:11)],2,mean))
      cx2<-cbind(cx,cx$AR^2,cx$R^2,cx$B^2,cx$PK^2,cx$FL^2,cx$HOA^2,cx$Tax^2,cx$I^2)
      names(cx2)<-c("cxAR","cxR","cxB","cxPK","cxFL","cxHOA","cxTax","cxI","cxAR^2","cxR^2","cxB^2","cxPK^2","cxFL^2","cxHOA^2","cxTax^2","cxI^2")
#Check correlation after centering, check VIF and eigenvalues   
      cor(cx2)
      (VIF=diag(solve(cor(cx2)))) #VIF AR =10.12, VIF B = 6.24
      ed<-eigen(cor(cx2))  #eigenvalues
      max(ed$values)/min(ed$values) #k=93.017
#start regression model with secondegree
      res2<-lm(y~x$C+cx$AR+cx$R+cx$B+cx$PK+cx$FL+cx$HOA+cx$Tax+cx$I+I(cx$AR^2)+I(cx$R^2)+I(cx$B^2)+I(cx$PK^2)+I(cx$FL^2)+I(cx$HOA^2)+I(cx$Tax^2)+I(cx$I^2))
      summary(res2)
      qqplot(res2) #serve violation on normality assumption
      bc<-boxcox(res2) #check if the model needs transformation
      lambda<-bc$x[which.max(bc$y)]
      y.prime<-y^lambda
      res2p<-lm(y.prime~x$C+cx$AR+cx$R+cx$B+cx$PK+cx$FL+x$AN+x$FR+ cx$HOA+cx$Tax+
                  cx$I+I(cx$AR^2)+I(cx$R^2)+I(cx$B^2)+I(cx$PK^2)+I(cx$FL^2)+I(cx$HOA^2)+
                  I(cx$Tax^2)+I(cx$I^2)+
                  cx$AR:cx$I+cx$AR:cx$Tax+cx$AR:cx$HOA+cx$I:cx$HOA+cx$Tax:cx$I+cx$Tax:cx$HOA)
      summary(res2p) #MSRES reduce 
      qqplot(res2p) #normality assumption has not been solved
     
      boxcox(res2p)
      #Trying transformation on the regressor Area and Insurance by using natural log
      res2p<-lm(y~x$C+cx$AR+cx$R+cx$B+cx$PK+cx$FL+x$AN+x$FR+ cx$HOA+cx$Tax+
                  I(cx$AR^2)+I(cx$R^2)+I(cx$B^2)+I(cx$PK^2)+I(cx$FL^2)+I(cx$HOA^2)+
                  I(cx$Tax^2)+
                  cx$AR:cx$Tax+cx$AR:cx$HOA+cx$Tax:cx$HOA)
      summary(res2p)
      qqplot(res2p)
      bc<-boxcox(res2p) #check if the model needs transformation
      lambda<-bc$x[which.max(bc$y)]
      y.prime<-y^lambda
      res2p<-lm(y.prime~x$C+cx$AR+cx$R+cx$B+cx$PK+cx$FL+x$AN+x$FR+ cx$HOA+cx$Tax+
                  cx$I+I(cx$AR^2)+I(cx$R^2)+I(cx$B^2)+I(cx$PK^2)+I(cx$FL^2)+I(cx$HOA^2)+
                  I(cx$Tax^2)+I(cx$I^2)+cx$AR:cx$I)
      summary(res2p) #MSRES reduce 
      qqplot(res2p)
      residualPlot(res2p$fitted.values,res2p)
      which(cx$I>600) #might need to remove 2180
      
      
      residualPlot(cx$I,res2p,name="cxI")
      residualPlot(cx$AR,res2p,name="cxAR")
      residualPlot(cx$R,res2p,name="cxR")
      residualPlot(cx$B,res2p,name="cxB")
      residualPlot(cx$PK,res2p,name="cxPK")
      residualPlot(cx$FL,res2p,name="cxFL")
      residualPlot(cx$HOA,res2p,name="cxHOA")
      residualPlot(cx$Tax,res2p,name="cxTax")
      
      #Check for ourliers:
      D<-cooks.distance(res2p)
      which(D>1) #there is outlier at index 1697 and 2810 but when we drop the outlier, the data
      #does not look better, so we will kip these two numbers
      
      
#Variable selection
      
      res=ols_step_all_possible(res2p) 
      summary(res2p)
      res$n[which.min(res$cp)]
      res$n[which.min(res$aic)]
      res$n[which.max(res$rsquare)]
      res=ols_step_all_possible_betas(RES)       
      
res0<-lm(y~1,data=house.rent)
add1(res0,y~x$C+cx$AR+cx$R+cx$B+cx$PK+cx$FL+x$AN+x$FR+ cx$HOA+cx$Tax+cx$I+
       I(cx$AR^2)+I(cx$R^2)+I(cx$B^2)+I(cx$PK^2)+I(cx$FL^2)+I(cx$HOA^2)+
       I(cx$Tax^2)+I(cx$I^2)+
       cx$AR:cx$Tax+cx$AR:cx$HOA+cx$Tax:cx$HOA,test="F") #add cx$I
res1<-lm(y~cx$I)
drop1(res1,y~cx$I,test="F")

add1(res1,y~x$C+cx$AR+cx$R+cx$B+cx$PK+cx$FL+x$AN+x$FR+ cx$HOA+cx$Tax+cx$I+
       I(cx$AR^2)+I(cx$R^2)+I(cx$B^2)+I(cx$PK^2)+I(cx$FL^2)+I(cx$HOA^2)+
       I(cx$Tax^2)+I(cx$I^2)+
       cx$AR:cx$Tax+cx$AR:cx$HOA+cx$Tax:cx$HOA,test="F") #cx$HOA
res2<-lm(y~cx$HOA+cx$I)
drop1(res2,y~cx$HOA+cx$I,test="F")

add1(res2,y~x$C+cx$AR+cx$R+cx$B+cx$PK+cx$FL+x$AN+x$FR+ cx$HOA+cx$Tax+cx$I+
       I(cx$AR^2)+I(cx$R^2)+I(cx$B^2)+I(cx$PK^2)+I(cx$FL^2)+I(cx$HOA^2)+
       I(cx$Tax^2)+I(cx$I^2)+
       cx$AR:cx$Tax+cx$AR:cx$HOA+cx$Tax:cx$HOA,test="F") #cx$AR
res3<-lm(y~cx$AR+cx$HOA+cx$I)
drop1(res3,y~cx$AR+cx$HOA+cx$I,test="F")

add1(res3,y~x$C+cx$AR+cx$R+cx$B+cx$PK+cx$FL+x$AN+x$FR+ cx$HOA+cx$Tax+cx$I+
       I(cx$AR^2)+I(cx$R^2)+I(cx$B^2)+I(cx$PK^2)+I(cx$FL^2)+I(cx$HOA^2)+
       I(cx$Tax^2)+I(cx$I^2)+
       cx$AR:cx$Tax+cx$AR:cx$HOA+cx$Tax:cx$HOA,test="F") #I(cx$HOA^2)
res4<-lm(y~cx$AR+cx$HOA+cx$I+I(cx$HOA^2))
drop1(res4,y~cx$AR+cx$HOA+cx$I+I(cx$HOA^2),test="F")

add1(res4,y~x$C+cx$AR+cx$R+cx$B+cx$PK+cx$FL+x$AN+x$FR+ cx$HOA+cx$Tax+cx$I+
       I(cx$AR^2)+I(cx$R^2)+I(cx$B^2)+I(cx$PK^2)+I(cx$FL^2)+I(cx$HOA^2)+
       I(cx$Tax^2)+I(cx$I^2)+
       cx$AR:cx$Tax+cx$AR:cx$HOA+cx$Tax:cx$HOA,test="F") #x$C
res5<-lm(y~x$C+cx$AR+cx$HOA+cx$I+I(cx$HOA^2))
drop1(res5,y~x$C+cx$AR+cx$HOA+cx$I+I(cx$HOA^2),test="F")

add1(res5,y~x$C+cx$AR+cx$R+cx$B+cx$PK+cx$FL+x$AN+x$FR+ cx$HOA+cx$Tax+cx$I+
       I(cx$AR^2)+I(cx$R^2)+I(cx$B^2)+I(cx$PK^2)+I(cx$FL^2)+I(cx$HOA^2)+
       I(cx$Tax^2)+I(cx$I^2)+
       cx$AR:cx$Tax+cx$AR:cx$HOA+cx$Tax:cx$HOA,test="F") #cx$AR:cx$HOA
res6<-lm(y~x$C+cx$AR+cx$HOA+cx$I+I(cx$HOA^2)+cx$AR:cx$HOA)
drop1(res6,y~x$C+cx$AR+cx$HOA+cx$I+I(cx$HOA^2)+cx$AR:cx$HOA,test="F")

add1(res6,y~x$C+cx$AR+cx$R+cx$B+cx$PK+cx$FL+x$AN+x$FR+ cx$HOA+cx$Tax+cx$I+
       I(cx$AR^2)+I(cx$R^2)+I(cx$B^2)+I(cx$PK^2)+I(cx$FL^2)+I(cx$HOA^2)+
       I(cx$Tax^2)+I(cx$I^2)+
       cx$AR:cx$Tax+cx$AR:cx$HOA+cx$Tax:cx$HOA,test="F") #I(cx$I^2)
res7<-lm(y~x$C+cx$AR+cx$HOA+cx$I+I(cx$HOA^2)+I(cx$I^2)+cx$AR:cx$HOA)
drop1(res7,y~x$C+cx$AR+cx$HOA+cx$I+I(cx$HOA^2)+I(cx$I^2)+cx$AR:cx$HOA,test="F")

add1(res7,y~x$C+cx$AR+cx$R+cx$B+cx$PK+cx$FL+x$AN+x$FR+ cx$HOA+cx$Tax+cx$I+
       I(cx$AR^2)+I(cx$R^2)+I(cx$B^2)+I(cx$PK^2)+I(cx$FL^2)+I(cx$HOA^2)+
       I(cx$Tax^2)+I(cx$I^2)+
       cx$AR:cx$Tax+cx$AR:cx$HOA+cx$Tax:cx$HOA,test="F") #I(cx$FL^2)
res8<-lm(y~x$C+cx$AR+cx$HOA+cx$I+I(cx$FL^2)+I(cx$HOA^2)+I(cx$I^2)+cx$AR:cx$HOA)
drop1(res8,y~x$C+cx$AR+cx$HOA+cx$I+I(cx$FL^2)+I(cx$HOA^2)+I(cx$I^2)+cx$AR:cx$HOA,test="F")

add1(res8,y~x$C+cx$AR+cx$R+cx$B+cx$PK+cx$FL+x$AN+x$FR+ cx$HOA+cx$Tax+cx$I+
       I(cx$AR^2)+I(cx$R^2)+I(cx$B^2)+I(cx$PK^2)+I(cx$FL^2)+I(cx$HOA^2)+
       I(cx$Tax^2)+I(cx$I^2)+
       cx$AR:cx$Tax+cx$AR:cx$HOA+cx$Tax:cx$HOA,test="F") #cx$R
res9<-lm(y~x$C+cx$AR+cx$R+cx$HOA+cx$I+I(cx$FL^2)+I(cx$HOA^2)+I(cx$I^2)+cx$AR:cx$HOA)
drop1(res9,y~x$C+cx$AR+cx$R+cx$HOA+cx$I+I(cx$FL^2)+I(cx$HOA^2)+I(cx$I^2)+cx$AR:cx$HOA,test="F")

add1(res9,y~x$C+cx$AR+cx$R+cx$B+cx$PK+cx$FL+x$AN+x$FR+ cx$HOA+cx$Tax+cx$I+
       I(cx$AR^2)+I(cx$R^2)+I(cx$B^2)+I(cx$PK^2)+I(cx$FL^2)+I(cx$HOA^2)+
       I(cx$Tax^2)+I(cx$I^2)+
       cx$AR:cx$Tax+cx$AR:cx$HOA+cx$Tax:cx$HOA,test="F") #I(cx$R^2)
res10<-lm(y~x$C+cx$AR+cx$R+cx$HOA+cx$I+I(cx$R^2)+I(cx$FL^2)+I(cx$HOA^2)+I(cx$I^2)+cx$AR:cx$HOA)
drop1(res10,y~x$C+cx$AR+cx$R+cx$HOA+cx$I+I(cx$R^2)+I(cx$FL^2)+I(cx$HOA^2)+I(cx$I^2)+cx$AR:cx$HOA,test="F")

add1(res10,y~x$C+cx$AR+cx$R+cx$B+cx$PK+cx$FL+x$AN+x$FR+ cx$HOA+cx$Tax+cx$I+
       I(cx$AR^2)+I(cx$R^2)+I(cx$B^2)+I(cx$PK^2)+I(cx$FL^2)+I(cx$HOA^2)+
       I(cx$Tax^2)+I(cx$I^2)+
       cx$AR:cx$Tax+cx$AR:cx$HOA+cx$Tax:cx$HOA,test="F") #x$FR
res11<-lm(y~x$C+cx$AR+cx$R+x$FR+cx$HOA+cx$I+I(cx$R^2)+I(cx$FL^2)+I(cx$HOA^2)+I(cx$I^2)+cx$AR:cx$HOA)
drop1(res11,y~x$C+cx$AR+cx$R+x$FR+cx$HOA+cx$I+I(cx$R^2)+I(cx$FL^2)+I(cx$HOA^2)+I(cx$I^2)+cx$AR:cx$HOA,test="F")

add1(res11,y~x$C+cx$AR+cx$R+cx$B+cx$PK+cx$FL+x$AN+x$FR+ cx$HOA+cx$Tax+cx$I+
       I(cx$AR^2)+I(cx$R^2)+I(cx$B^2)+I(cx$PK^2)+I(cx$FL^2)+I(cx$HOA^2)+
       I(cx$Tax^2)+I(cx$I^2)+
       cx$AR:cx$Tax+cx$AR:cx$HOA+cx$Tax:cx$HOA,test="F") #I(cx$AR^2)
res12<-lm(y~x$C+cx$AR+cx$R+x$FR+cx$HOA+cx$I+I(cx$AR^2)+I(cx$R^2)+I(cx$FL^2)+I(cx$HOA^2)+I(cx$I^2)+cx$AR:cx$HOA)
drop1(res12,y~x$C+cx$AR+cx$R+x$FR+cx$HOA+cx$I+I(cx$AR^2)+I(cx$R^2)+I(cx$FL^2)+I(cx$HOA^2)+I(cx$I^2)+cx$AR:cx$HOA,test="F")

add1(res12,y~x$C+cx$AR+cx$R+cx$B+cx$PK+cx$FL+x$AN+x$FR+ cx$HOA+cx$Tax+cx$I+
       I(cx$AR^2)+I(cx$R^2)+I(cx$B^2)+I(cx$PK^2)+I(cx$FL^2)+I(cx$HOA^2)+
       I(cx$Tax^2)+I(cx$I^2)+
       cx$AR:cx$Tax+cx$AR:cx$HOA+cx$Tax:cx$HOA,test="F") #cx$PK
res13<-lm(y~x$C+cx$AR+cx$R+cx$PK+x$FR+cx$HOA+cx$I+I(cx$AR^2)+I(cx$R^2)+I(cx$FL^2)+I(cx$HOA^2)+I(cx$I^2)+cx$AR:cx$HOA)
drop1(res13,y~x$C+cx$AR+cx$R+cx$PK+x$FR+cx$HOA+cx$I+I(cx$AR^2)+I(cx$R^2)+I(cx$FL^2)+I(cx$HOA^2)+I(cx$I^2)+cx$AR:cx$HOA,test="F")

add1(res13,y~x$C+cx$AR+cx$R+cx$B+cx$PK+cx$FL+x$AN+x$FR+ cx$HOA+cx$Tax+cx$I+
       I(cx$AR^2)+I(cx$R^2)+I(cx$B^2)+I(cx$PK^2)+I(cx$FL^2)+I(cx$HOA^2)+
       I(cx$Tax^2)+I(cx$I^2)+
       cx$AR:cx$Tax+cx$AR:cx$HOA+cx$Tax:cx$HOA,test="F") #cx$Tax
res14<-lm(y~x$C+cx$AR+cx$R+cx$PK+x$FR+cx$HOA+cx$Tax+cx$I+I(cx$AR^2)+I(cx$R^2)+I(cx$FL^2)+I(cx$HOA^2)+I(cx$I^2)+cx$AR:cx$HOA)
drop1(res14,y~x$C+cx$AR+cx$R+cx$PK+x$FR+cx$HOA+cx$Tax+cx$I+I(cx$AR^2)+I(cx$R^2)+I(cx$FL^2)+I(cx$HOA^2)+I(cx$I^2)+cx$AR:cx$HOA,test="F")

add1(res14,y~x$C+cx$AR+cx$R+cx$B+cx$PK+cx$FL+x$AN+x$FR+ cx$HOA+cx$Tax+cx$I+
       I(cx$AR^2)+I(cx$R^2)+I(cx$B^2)+I(cx$PK^2)+I(cx$FL^2)+I(cx$HOA^2)+
       I(cx$Tax^2)+I(cx$I^2)+
       cx$AR:cx$Tax+cx$AR:cx$HOA+cx$Tax:cx$HOA,test="F") #cx$AR:cx$Tax
res15<-lm(y~x$C+cx$AR+cx$R+cx$PK+x$FR+cx$HOA+cx$Tax+cx$I+I(cx$AR^2)+I(cx$R^2)+I(cx$FL^2)+I(cx$HOA^2)+I(cx$I^2)+cx$AR:cx$HOA)
drop1(res15,y~x$C+cx$AR+cx$R+cx$PK+x$FR+cx$HOA+cx$Tax+cx$I+I(cx$AR^2)+I(cx$R^2)+I(cx$FL^2)+I(cx$HOA^2)+I(cx$I^2)+cx$AR:cx$HOA,test="F")



add1(res15,y~x$C+cx$AR+cx$R+cx$B+cx$PK+cx$FL+x$AN+x$FR+ cx$HOA+cx$Tax+cx$I+
       I(cx$AR^2)+I(cx$R^2)+I(cx$B^2)+I(cx$PK^2)+I(cx$FL^2)+I(cx$HOA^2)+
       I(cx$Tax^2)+I(cx$I^2)+
       cx$AR:cx$HOA+cx$Tax:cx$HOA,test="F") #x$AN
res16<-lm(y~x$C+cx$AR+cx$R+cx$PK+x$AN+x$FR+cx$HOA+cx$Tax+cx$I+I(cx$AR^2)+I(cx$R^2)+I(cx$FL^2)+I(cx$HOA^2)+I(cx$I^2)+cx$AR:cx$HOA)
drop1(res16,y~x$C+cx$AR+cx$R+cx$PK+x$AN+x$FR+cx$HOA+cx$Tax+cx$I+I(cx$AR^2)+I(cx$R^2)+I(cx$FL^2)+I(cx$HOA^2)+I(cx$I^2)+cx$AR:cx$HOA,test="F")

add1(res16,y~x$C+cx$AR+cx$R+cx$B+cx$PK+cx$FL+x$AN+x$FR+ cx$HOA+cx$Tax+cx$I+
       I(cx$AR^2)+I(cx$R^2)+I(cx$B^2)+I(cx$PK^2)+I(cx$FL^2)+I(cx$HOA^2)+
       I(cx$Tax^2)+I(cx$I^2)+
       cx$AR:cx$HOA+cx$Tax:cx$HOA,test="F") #Cannot add more variable

#Run model again
resva<-lm(y~x$C+cx$AR+cx$R+cx$PK+x$AN+x$FR+cx$HOA+cx$Tax+I(cx$AR^2)+I(cx$R^2)+I(cx$FL^2)+I(cx$HOA^2)+cx$AR:cx$HOA)
bc<-boxcox(resva)
y.trans<-y^(bc$x[which.max(bc$y)])
resva<-lm(y.trans~x$C+cx$AR+cx$R+cx$PK+x$AN+x$FR+cx$HOA+cx$Tax+cx$I+
            I(cx$AR^2)+I(cx$R^2)+I(cx$FL^2)+I(cx$HOA^2)+I(cx$I^2)+cx$AR:cx$HOA)
summary(resva) 
par(mfrow=c(3,3))
qqplot(resva) #the non-normality violation has been solved
residualPlot(cx$I,resva,name="cxI")
residualPlot(cx$AR,resva,name="cxAR")
residualPlot(cx$R,resva,name="cxR")
residualPlot(cx$B,resva,name="cxB")
residualPlot(cx$PK,resva,name="cxPK")
residualPlot(cx$FL,resva,name="cxFL")
residualPlot(cx$HOA,resva,name="cxHOA")
residualPlot(cx$Tax,resva,name="cxTax")




