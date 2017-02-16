##Fig.2
## start with absorbance:


## Measured glucose+trehalose in mg/ml ####

mg <- read.csv2("data/fig2_mg.csv")
mg = changegenotype(mg)
#get values depending on mean with outliers taken out
mg= mg%>%
  mutate (dilution_1a = dilution_1a/mean*mean_noextr) %>%
  mutate (dilution_1b = dilution_1b/mean*mean_noextr) %>%
  mutate (dilution_2a = dilution_2a/mean*mean_noextr) %>%
  mutate (dilution_2b = dilution_2b/mean*mean_noextr) 
names(mg)
# 4 different value for the calibration gives 4 values for the total amount of sugar
# here we take the mean of these four as a unique value for one experiment.
mg=mg %>% 
  rowwise() %>% 
  mutate (rangedata= diff(range(c (dilution_1a,dilution_1b,dilution_2a,dilution_2b))))%>%
  mutate(Sugarcontent = median(c (dilution_1a,dilution_1b,dilution_2a,dilution_2b)))

mg= ungroup(mg)

#we group the values by genotype and starvation and summarise to get values for the barplot
mg2= mg %>% group_by(genotype,starvation)

Summary_mg=mg2 %>%
  summarise(N= n(),amount = mean(Sugarcontent), se = sd(Sugarcontent)/sqrt(N-1))

# plotting barplots
Fig2A=ggplot(Summary_mg, aes(x=starvation, y=amount, fill=genotype)) + 
  geom_bar(position=position_dodge(),col="black",width=0.7,stat = "identity") +
  geom_errorbar(aes(ymin=amount-se, ymax=amount+se),
                width=0.1,position=position_dodge(0.7)) +
  ylab("Trehalose + glucose [mg/ml]") +
  xlab("") +
  scale_fill_manual(values=c(color_tbh, color_contol1)) +
  
  #scale_fill_manual(values=c("grey","white")) + # column fill and legend title/ -lables
  scale_y_continuous(breaks=0:20*5, limits = c(0,25)) +
  Chrisitheme
Fig2A
#plotting boxplots

ggplot(mg2, aes(x=starvation,y=Sugarcontent, fill=genotype))+
  geom_boxplot()+
  #geom_violin()+
  # geom_jitter()+
  ylab("Trehalose + glucose [mg/ml]") +
  xlab("After 20h starvation") +
  scale_fill_manual(values=c(color_tbh, color_contol1)) +
  
  #scale_fill_manual(values=c("grey","white")) + # column fill and legend title/ -lables
  scale_y_continuous(breaks=0:20*5, limits = c(0,35)) 

statmg= mg %>% filter(starvation == "yes")

plot(statmg$Sugarcontent~statmg$genotype)
wilcox.test(statmg$Sugarcontent~statmg$genotype)

##
data <- read.csv2("data/fig2_normalized.csv")
cdata = data %>% group_by(fill)
cdata2= cdata %>% summarise(N=n(), quotient1= mean(quotient), se= sd(quotient)/ sqrt(N-1))

cdata2$genotype= cdata2$fill
cdata2 = changegenotype(cdata2)
cdata2$fill <- factor(cdata2$genotype)

#boxplot
ggplot(cdata, aes(x=fill, y=quotient, fill=fill))+
  geom_boxplot()+
  #geom_violin()+
  # geom_jitter()+
  ylab("Change in glucose content") +
  xlab("") +
  scale_fill_manual(values=c(color_tbh, color_contol1), guide = F)  # column fill and legend title/ -lables
#scale_y_continuous(breaks=0:10*0.2, limits = c(-1,1))

#cdata %>% group_by(date)
temp= spread(cdata, fill,quotient)
temp = temp [,c(1,ncol(temp)-1,ncol(temp))]
temp2=gather (temp, genotype, quotient ,-date, na.rm = T) %>% arrange(date)

levels(temp2$date)= c(levels(temp2$date),c("art1","art2"))
temp2$date[c(8,10,14,16)]= c("art1","art1","art2","art2")


cdata3=spread(temp2, genotype, quotient)  
cdata3$tbh-cdata3$`w+`

cdata4=cdata3[-1,]
wilcox.test(cdata4$tbh-cdata4$`w+`, paired= F)
t.test(cdata3$tbh,cdata3$`w+`, paired= F)

# barplot
Fig2B= ggplot(cdata2, aes(x=fill, y=quotient1, fill=fill)) + 
  geom_bar(position=position_dodge(),col="black",width=0.7,stat = "identity") +
  geom_errorbar(aes(ymin=quotient1-se, ymax=quotient1+se),
                width=0.1,position=position_dodge(0)) +
  ylab("Change in glucose content") +
  xlab("") +
  scale_fill_manual(values=c(color_tbh, color_contol1), guide = F) + # column fill and legend title/ -lables
  scale_y_continuous(breaks=0:10*0.2, limits = c(0,1)) +
  Chrisitheme+
  geom_text(x = 1.5, y = 0.6, label = "*", size = starsize)

Fig2B
t.test(cdata$quotient~cdata$fill)