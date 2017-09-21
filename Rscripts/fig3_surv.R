##fig.3
#survival version

library(ggfortify)
library(survival)

data<- read.csv("data/fig3_survival.csv")
data= changegenotype(data)

data_ori=data[data$genotype == "w+",]
data_ori=na.omit(data_ori)

source("Rscripts/survival_changedata.r")


controldata=list_survd

data_ori=data[data$genotype == "tßhnM18",]
data_ori=na.omit(data_ori)

source("Rscripts/survival_changedata.r")

#T2=Surv(list_survd$time, list_survd$event)
#lines(survfit( T2~ list_survd$genotype), col= "red")

total= rbind ( controldata, list_survd)



A=survfit( Surv(time, event)~ genotype, data= total )
Fig3C=ggplot2::autoplot(A,  surv.geom ="line") +
  ylab ("Proportion surviving")+
  xlab("Starvation time [h]")+
  geom_hline (yintercept =0.5,linetype =3)+
  scale_colour_manual(values=c(color_tbh, color_contol2))+
  Chrisitheme+
  theme(legend.justification=c(1,1), legend.position=c(1,1))


#   
#   ggsurvplot (A, surv.median.line = "hv",
#            , conf.int=TRUE, pval=TRUE,  
#            legend.labs=c( "tßhnM18", "W+"), legend.title="Genotype",  
#            main="Kaplan-Meier Curve for fly survival" 
#            )
# ggsurv(A)
coxph(Surv(time, event)~ trial+ genotype, data= total)
coxph(Surv(time, event)~ genotype, data= total)

#looking at difference between LD50 and median survival time
x=quantile (survfit( Surv(time, event)~ genotype+ trial, data= total ),probs = c( 0.5), conf.int = F)
x2=c(rep (levels(total$genotype)[1],length(unique(total$trial))),
rep (levels(total$genotype)[2],length(unique(total$trial))))

Data = as.data.frame(cbind(genotype=x2,MST=as.numeric(x)))
Data$MST=as.numeric(x)
Data$LD =c(Group2_p05,Group1_p05)
boxplot(Data$MST ~Data$genotype)
plot(Data$MST ,Data$LD, xlim = c(0,80),ylim = c(0,80))
abline(a = 0, b = 1, col = 2)

# ggplot(Data, aes(x=genotype, y=MST, fill=genotype)) +
#   geom_boxplot(position = position_dodge(width=0.9)) +
#   ylab("median survival time [h]") +
#   xlab("genotype") +
#   scale_fill_manual(values=c(color_tbh, color_contol1), guide= F) + # column fill and legend title/ -lables
#   scale_y_continuous(limits = c(0,80))+
#   Chrisitheme+
#   geom_text(x = 1.5, y = 75, label = "*", size = starsize)+
#   geom_text(data = a, aes(y= 5,label= paste0("n = ",LD50[,1])), 
#             position = position_dodge(width=0.9))
# 
# wilcox.test(Data$LD~Data$genotype)
# wilcox.test(Data$MST~Data$genotype)
