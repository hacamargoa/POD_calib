library(plyr)
library(scatterplot3d)
HI<-list.files("C:/Users/hac809/Desktop/POD6", pattern="^Hi*")
Yield<-list.files("C:/Users/hac809/Desktop/POD6", pattern="^Yield*")
POD<-list.files("C:/Users/hac809/Desktop/POD6", pattern="^POD*")
vars<-list(HI,Yield,POD)

var<-list()
k=1
for (j in vars){
  tem_<-list()
  a=1
for (i in j){
  temp<-read.table(paste0("C:/Users/hac809/Desktop/POD6/",i),header=T)
  temp$O3<-a*15
  tem_[[i]]=temp
  a=a+1
  }
  var[[k]]<-do.call(rbind,tem_,)
  rownames(var[[k]]) <- NULL
  k=k+1
  }

#calling the 0 effect in yield and subsetting based on
HI_0<-subset(read.table("C:/Users/hac809/Desktop/POD6/0_HI.out",h=T),Year>=1987&Year<=1999);HI_0$O3<-0
YI_0<-subset(read.table("C:/Users/hac809/Desktop/POD6/0_yield.out",h=T),Year>=1987&Year<=1999);YI_0$O3<-0
POD_0<-subset(read.table("C:/Users/hac809/Desktop/POD6/0_POD6.out",h=T),Year>=1987&Year<=1999);POD_0$O3<-0
vars_0<-list(HI_0,YI_0,POD_0)
Wh_data<-read.table("C:/Users/hac809/Desktop/POD6/ref_wheat.csv",h=T,sep =",")
for (i in 1:length(vars_0)){ 
  temp<-vars_0[[i]]
  temp2<-var[[i]]
  temp<-temp[paste0(temp$Lon,temp$Lat,temp$Year)%in%paste0(Wh_data$Lon,Wh_data$Lat,Wh_data$Year),]
  temp2<-temp2[paste0(temp2$Lon,temp2$Lat,temp2$Year)%in%paste0(Wh_data$Lon,Wh_data$Lat,Wh_data$Year),]
  vars_0[[i]]<-temp
  var[[i]]<-temp2
}
#reference regression
ref_reg<-data.frame(POD6=seq(0,10))
ref_reg$rel_yield<-1-0.0384*ref_reg$POD6
ref_reg$rel_HI<-1-0.0167*ref_reg$POD6

#linear regresion
var_nam<-c("HI","Yield","POD")
Reg_diff<-data.frame("Num"=1)
Reg_diff<-as.data.frame(seq(1:110));names(Reg_diff)[1]<-"Num"
for (i in Reg_diff$Num){
  temp<-list()
  temp_<-data.frame(Num=rep(i,10),PODk=rep(0,10),PODm=rep(0,10))
  for (j in 1:3){
  temp1<-subset(var[[j]],Num==i)
  temp2<-cbind(temp_,vars_0[[j]])
  temp[[j]]<-rbind(temp2,temp1)[c(1:6,12,15)]
  colnames(temp[[j]])[7]<-var_nam[j]
  }
  test<-Reduce(join,temp)
  a<-numeric();b<-numeric()
  for (l in unique(test$O3)){
    test1<-test[which(test$O3==l),9]/test[which(test$O3==0),9]
    test2<-test[which(test$O3==l),7]/test[which(test$O3==0),7]
    a<-c(a,test1)
    b<-c(b,test2)
    }
 test$rel_yi<-a
 test$rel_Hi<-b
 regY<-lm(test$rel_yi~test$POD)
 regH<-lm(test$rel_Hi~test$POD)
 ref_reg$rel_yieldL<-regY[[1]][1]+regY[[1]][2]*ref_reg$POD6
 ref_reg$rel_HIL<-regH[[1]][1]+regH[[1]][2]*ref_reg$POD6
 Reg_diff[i,"PODk"]<-test[11,2]
 Reg_diff[i,"PODm"]<-test[11,3]
 Reg_diff[i,"InterY"]<-regY[[1]][1]; Reg_diff[i,"SlopeY"]<-regY[[1]][2]
 Reg_diff[i,"InterH"]<-regH[[1]][1]; Reg_diff[i,"SlopeH"]<-regH[[1]][2]
 Reg_diff[i,"MSy"]<-sum((ref_reg$rel_yield-ref_reg$rel_yieldL)^2)/11
 Reg_diff[i,"MSh"]<-sum((ref_reg$rel_HI-ref_reg$rel_HIL)^2)/11
 Reg_diff[i,"R2y"]<-summary(regY)$adj.r.squared
 Reg_diff[i,"R2h"]<-summary(regH)$adj.r.squared
}
Reg_diff$funct<-sqrt(Reg_diff$MSy^2+Reg_diff$MSh^2)

