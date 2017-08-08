
list_survd=c()

for (i in c(1:length(levels(as.factor(data_ori$trial))))){
  temp =subset(data_ori, data_ori$trial == i)
  
  survdata= c()
  ND=0
  for (j in c(1: nrow(temp))) {
    D=temp[j,]
    survdata= c(survdata, rep(D$time,D$dead-ND))
    
    ND= D$dead
  }
  Survd=as.data.frame(as.numeric(survdata))
  names(Survd)= c("time")
  Survd$event= 1
  Survd$genotype = temp$genotype[1]
  Survd$trial = temp$trial[1]
  T2=Surv(time=Survd$time, event=Survd$event)
  #if (i==1){plot(survfit(T2 ~1), col= "blue", xlim= c(0,90))}
  #lines(survfit(T2 ~1), col= "blue")
  list_survd=rbind(list_survd,Survd )  
  
}