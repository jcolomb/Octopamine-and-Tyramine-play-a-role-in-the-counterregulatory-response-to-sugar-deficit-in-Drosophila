##fig.3
# LD 50 ####

#calculate LD50 for each group

# Group1 (w+ females)

data<- read.csv("data/fig3_survival.csv")
data= changegenotype(data)

data_ori=data[data$genotype == "w+",]
data_ori=na.omit(data_ori)
list_p05= c()
list_p08= c()
list_p10=c()

for (i in c(1:length(levels(as.factor(data_ori$trial))))){
  temp =subset(data_ori, data_ori$trial == i)
  y=cbind(temp$alive, temp$dead)
  model.results = glm(y~temp$time, binomial)
  summary(model.results)
  k=dose.p(model.results, p=c(0.5,0.8,0.0001))
  list_p05= c(list_p05,as.numeric(k[1]))
  list_p08= c(list_p08,as.numeric(k[2]))
  list_p10=c(list_p10,as.numeric(k[3]))
}

Group1_p05= list_p05
dead20_w = list_p08
Group1_p10=list_p10


# Group2 (t?h females)

data_ori=data[data$genotype == "tßhnM18",]
data_ori=na.omit(data_ori)
list_p05= c()
list_p08= c()
list_p10=c()

for (i in c(1:length(levels(as.factor(data_ori$trial))))){
  temp =subset(data_ori, data_ori$trial == i)
  y=cbind(temp$alive, temp$dead)
  model.results = glm(y~temp$time, binomial)
  summary(model.results)
  k=dose.p(model.results, p=c(0.5,0.8,0.0001))
  list_p05= c(list_p05,as.numeric(k[1]))
  list_p08= c(list_p08,as.numeric(k[2]))
  list_p10=c(list_p10,as.numeric(k[3]))
}

Group2_p05= list_p05
dead20_tbh = list_p08
Group2_p10=list_p10

# make table of the LD50 data for statistics

tbhw <- data.frame(genotype=c("w","w","w","w","w","w","w","w","w","w",
                              "w","w","w","w","w","w",
                              "tbh","tbh","tbh","tbh","tbh","tbh","tbh","tbh","tbh","tbh",
                              "tbh","tbh","tbh","tbh","tbh","tbh"),
                   LD50=c(Group1_p05,Group2_p05))
tbhw= changegenotype(tbhw)
attach(tbhw)
t.test(LD50~genotype)
wilcox.test(LD50~genotype)
pairwise.wilcox.test(LD50, genotype,p.adjust="bonferroni")

a <- aggregate(LD50 ~ genotype + genotype , tbhw, function(i) c(val=length(i), ypos=quantile(i)[2]))

Fig3B=ggplot(tbhw, aes(x=genotype, y=LD50, fill=genotype)) +
  geom_boxplot(position = position_dodge(width=0.9)) +
  ylab("50% death rate [h]") +
  xlab("genotype") +
  scale_fill_manual(values=c(color_tbh, color_contol1), guide= F) + # column fill and legend title/ -lables
  scale_y_continuous(limits = c(0,80))+
  Chrisitheme+
  geom_text(x = 1.5, y = 75, label = "*", size = starsize)+
  geom_text(data = a, aes(y= 5,label= paste0("n = ",LD50[,1])), 
            position = position_dodge(width=0.9))
##
data=data %>% mutate (percent_alive = alive/(alive+dead))
#data$time= as.numeric(as.character(data$time))
#levels (data$time)= c(0:88)
# lineplot.CI(time,percent_alive,group=genotype,data=data,cex=2,
#             x.cont=T,
#             xlab="Starvation time [h]",ylab="Proportion surviving",
#             cex.lab=1.1,x.leg=1,
#             err.width=0.1,err.col=c(color_tbh, color_contol2),
#             pch=c(20,20),col=c(color_tbh, color_contol2),
#             leg.bty="n",
#             legend=T,ylim=c(0,1) ,
#             xaxt="n", yaxt= "n")
# grid(NULL, 2, lwd = 1)
# axis(side=1,las=1)
# axis(side=2,las=1)
# abline(h=0.5, lty = 3)

View(data)
data2= data %>% group_by(time, genotype) %>% summarise(N=n(), quotient1= mean(percent_alive, na.rm=T), se= sd(percent_alive, na.rm=T)/ sqrt(N-1))


Fig3A=
  ggplot(data2, aes(x=time, y=quotient1, colour=genotype))+
  geom_point(size=point_size)+ 
  geom_errorbar(aes(ymin=quotient1-se, ymax=quotient1+se), width=.05, lwd = 0.8)+
  geom_line(lwd=0.8)+
  ylab ("Proportion surviving")+
  xlab("Starvation time [h]")+
  geom_hline (yintercept =0.5,linetype =3)+
  scale_colour_manual(values=c(color_tbh, color_contol2))+
  Chrisitheme+
  theme(legend.justification=c(1,1), legend.position=c(1,1))

