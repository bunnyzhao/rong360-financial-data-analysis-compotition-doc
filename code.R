library(randomForest)
setwd("C:/Users/Administrator/Desktop/dataseries/开放数据")
user=read.table('user.final.txt',header=T)
order_train=read.table('order_train.txt',header=T)
order_test=read.table('order_test_no_label.txt',header=T)
quality=read.table('quality.final.txt',sep="\t",header=T)
product=read.table('product.final.txt',sep="\t",header=T)
length(unique(product[,1]))
intersect(quality[,1],order_train[,1])[1]
setwd("C:/Users/Administrator/Desktop")
data=read.csv("1.csv",header=T)
em=vector(mode='numeric',length=ncol(data))
per=em
for (i in 1:ncol(data))
{
  em[i]=length(which(complete.cases(data[,i])==FALSE))
  per[i]=em[i]/nrow(data)
}
data1=data[,-which(per>0.95)]
data2=data1[,-c(1,2,7,8)]
data3=data2[,1:32]
data3$result=as.factor(data3$result) 
par0=data3[data3$result==0,]
par1=data3[data3$result==1,]
par01=rbind(par0[createFolds(par0$limit,k=3,list=T)[[1]],],par1)
par02=rbind(par0[createFolds(par0$limit,k=3,list=T)[[2]],],par1)
par03=rbind(par0[createFolds(par0$limit,k=3,list=T)[[3]],],par1)
fold1=createFolds(par01$result,k=7,list=T)
fold2=createFolds(par02$result,k=7,list=T)
fold3=createFolds(par03$result,k=7,list=T)
pre1=vector(mode='list',length=7)
pre2=vector(mode='list',length=7)
pre3=vector(mode='list',length=7) 
for (i in 1:7){
model=randomForest(result~.,data=par01[fold1[[i]],])
pre1[[i]]=predict(model,test3)
}
for (i in 1:7){
  model=randomForest(result~.,data=par02[fold2[[i]],])
  pre2[[i]]=predict(model,test3)
}
for (i in 1:7){
  model=randomForest(result~.,data=par03[fold3[[i]],])
  pre3[[i]]=predict(model,test3)
}
a=cbind(pre1[[1]],pre1[[2]],pre1[[3]],pre1[[4]],pre1[[5]],
        pre1[[6]],pre1[[7]])
b=cbind(pre2[[1]],pre2[[2]],pre2[[3]],pre2[[4]],pre2[[5]],
          pre2[[6]],pre2[[7]])
c=cbind(pre3[[1]],pre3[[2]],pre3[[3]],pre3[[4]],pre3[[5]],
          pre3[[6]],pre3[[7]])
setwd("C:/Users/Administrator/Desktop")
test=read.csv("2.csv",header=T)
test1=test[,-which(per>0.95)]
test2=test1[,-c(1,2,7,8)]
test3=test2[,1:32][,-4]
test4=test2[,-c(33,34)]
test4=test4[which(complete.cases(test4$pv)==T),]
rank4=test4$rank[(complete.cases(test2$pv)==T)]
test4=test4[,-4]
names(test3)[1]='date_pass'
names(test4)[1]='date_pass'
data4=data2[which(complete.cases(data2$pv)==T),]
data4=data4[,-c(33,34)]
data4$result=as.factor(data4$result)
par0=data4[which(data4$result==0),]
par1=data4[which(data4$result==1),]
fold=createFolds(par0$result,k=3,list=T)
par01=rbind(par0[fold[[1]],],par1)
par02=rbind(par0[fold[[2]],],par1)
par03=rbind(par0[fold[[3]],],par1)
fold1=createFolds(par01$result,k=3,list=T)
fold2=createFolds(par02$result,k=3,list=T)
fold3=createFolds(par03$result,k=3,list=T)
## fold=createFolds(data4$result,k=3,list=T)
pre1=vector(mode="list",length=3)
pre2=vector(mode="list",length=3)
pre3=vector(mode="list",length=3)
 
for (i in 1:3){
model=randomForest(result~.,data=par01[fold1[[i]],])
pre1[[i]]=predict(model,test4)
}
 
for (i in 1:3){
  model=randomForest(result~.,data=par02[fold1[[i]],])
  pre2[[i]]=predict(model,test4)
}
 
for (i in 1:3){
  model=randomForest(result~.,data=par03[fold1[[i]],])
  pre3[[i]]=predict(model,test4)
}
 
pre4=cbind(rank4,pre1[[1]],pre1[[2]],pre1[[3]],
                 pre2[[1]],pre2[[2]],pre2[[3]],
                 pre3[[1]],pre3[[2]],pre3[[3]])
pre4=pre4-1
write.csv(pre4,'mo.csv',row.names=F)
#运行环境：	
#软件:建模过程：R;并表过程：SAS  
#系统要求：Win 7
#需要加载的包randomForest  ggplot2 
#Code中的1.csv,2.csv为第二部分中通过SAS代码并表之后的训练集和测试集。

