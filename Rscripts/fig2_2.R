rawdata <- read_csv("data/fig2_trehalose.csv")

# correct group names
rawdata$group [rawdata$group == "w_starved"] = "7w_starved"
rawdata$group [rawdata$group == "tbh_starved"] = "8tbh_starved"

#exclude data with no starved point
rawdata = rawdata[rawdata$date != "02/07/2013",]

rawdata$group=factor(rawdata$group)
#summary(rawdata$group)
#nrow(rawdata)

t=rawdata[rawdata$group =="3trehalose_standard",] %>%
  rowwise %>%
  mutate (Range = max (absorbance1,absorbance2,absorbance3,absorbance4,absorbance5)- min(absorbance1,absorbance2,absorbance3,absorbance4,absorbance5))%>%
  mutate (Med = median(c(absorbance1,absorbance2,absorbance3,absorbance4,absorbance5))) %>%
  mutate (Mean = mean (c(absorbance1,absorbance2,absorbance3,absorbance4,absorbance5))) %>%
  mutate (Coeff_treh = 4*6.24/Med) # 6,24 microgram of trehalose in the calibration solution, x4 because of concentration differences

#View(t)
#t %>% transmute(diff= mean -Mean)
t=ungroup(t)

t1 = t %>% dplyr::select( date, Coeff_treh)

t=rawdata[rawdata$group =="5w_fed",] %>%
  rowwise %>%
  mutate (wfed_Range = max (absorbance1,absorbance2,absorbance3,absorbance4,absorbance5)- min(absorbance1,absorbance2,absorbance3,absorbance4,absorbance5))%>%
  mutate (wfed_med = median(c(absorbance1,absorbance2,absorbance3,absorbance4,absorbance5))) 

ti = t %>% dplyr::select( date, wfed_Range,wfed_med)
  
t2 = inner_join(t1, ti)  

t=rawdata[rawdata$group =="6tbh_fed",] %>%
  rowwise %>%
  mutate (tbhfed_Range = max (absorbance1,absorbance2,absorbance3,absorbance4,absorbance5)- min(absorbance1,absorbance2,absorbance3,absorbance4,absorbance5))%>%
  mutate (tbhfed_med = median(c(absorbance1,absorbance2,absorbance3,absorbance4,absorbance5))) 

ti = t %>% dplyr::select( date, tbhfed_Range,tbhfed_med)

t3 = cbind(t2, ti)  


t=rawdata[rawdata$group =="7w_starved",] %>%
  rowwise %>%
  mutate (wst_Range = max (absorbance1,absorbance2,absorbance3,absorbance4,absorbance5)- min(absorbance1,absorbance2,absorbance3,absorbance4,absorbance5))%>%
  mutate (wst_med = median(c(absorbance1,absorbance2,absorbance3,absorbance4,absorbance5))) 

ti = t %>% dplyr::select( date, wst_Range,wst_med)

t4 = cbind(t3, ti)  


t=rawdata[rawdata$group =="8tbh_starved",] %>%
  rowwise %>%
  mutate (tbhst_Range = max (absorbance1,absorbance2,absorbance3,absorbance4,absorbance5)- min(absorbance1,absorbance2,absorbance3,absorbance4,absorbance5))%>%
  mutate (tbhst_med = median(c(absorbance1,absorbance2,absorbance3,absorbance4,absorbance5))) 

ti = t %>% dplyr::select( date, tbhst_Range,tbhst_med)

t5 = cbind(t4, ti)   
t5 <- t5[, !duplicated(colnames(t5))]
t6 = t5 %>% rowwise %>%
  mutate (WFED= wfed_med*Coeff_treh)%>%
  mutate (tbhFED= tbhfed_med*Coeff_treh)%>%
  mutate (Wst= wst_med*Coeff_treh)%>%
  mutate (tbhst= tbhst_med*Coeff_treh) %>%
  mutate (changew = Wst-WFED) %>%
  mutate (changetbh = tbhst-tbhFED)%>%
  mutate (quotientw = (Wst-WFED)/(Wst+WFED))%>%
  mutate (quotienttbh = (tbhst-tbhFED)/(tbhst+tbhFED))
  
wilcox.test(t6$changetbh,t6$changew, paired= T)
boxplot (t6$changetbh,t6$changew)

wilcox.test(t6$quotientw-t6$quotienttbh, paired= F)
boxplot (-t6$quotientw,-t6$quotienttbh)

boxplot (t6$tbhFED,t6$WFED, t6$tbhst,t6$Wst )
#stat_summary(t6$tbhFED,t6$WFED, t6$tbhst,t6$Wst )

## plot:


t7=t6 %>%
gather( group,Sugarcontent, WFED:tbhst) 

t7$genotype = NA
t7$starved = NA


t7$genotype [t7$group == "WFED"] = "w+"
t7$genotype [t7$group == "tbhFED"] = "tßhnM18"
t7$genotype [t7$group == "Wst"]= "w+"
t7$genotype [t7$group == "tbhst"] = "tßhnM18"

t7$starved [t7$group == "WFED"] = "no"
t7$starved [t7$group == "tbhFED"] = "no"
t7$starved [t7$group == "Wst"]= "yes"
t7$starved [t7$group == "tbhst"] = "yes"

a <- aggregate(Sugarcontent ~ starved + genotype , t7, function(i) c(val=length(i), ypos=quantile(i)[2]))

Fig2A= ggplot(t7, aes(x=starved,y=Sugarcontent, fill=genotype))+
  geom_boxplot(position = position_dodge(width=0.9))+
  #geom_violin()+
  # geom_jitter()+
  ylab("Trehalose + glucose [mg/ml]") +
  xlab("After 20h starvation") +
  scale_fill_manual(values=c(color_tbh, color_contol1)) +
  
  #scale_fill_manual(values=c("grey","white")) + # column fill and legend title/ -lables
  scale_y_continuous(breaks=0:20*5, limits = c(0,35)) +
  Chrisitheme+
  geom_text(data = a, aes(y= 1,label= paste0("n = ",Sugarcontent[,1])), 
            position = position_dodge(width=0.9))

boxplot (-t6$quotientw,-t6$quotienttbh)
t8= t6%>%
  dplyr::select (date,"w+"=quotientw, "tßhnM18"=quotienttbh)

dat.m <- melt(t8,id.vars='date', measure.vars=c('tßhnM18', 'w+'))

Fig2B=
  ggplot(dat.m, aes(x=variable, y=-value, fill=variable))+
  geom_boxplot(position = position_dodge(width=0.9))+
  #geom_violin()+
  # geom_jitter()+
  ylab("Index of change") +
  xlab("Genotype") +
  scale_fill_manual(values=c(color_tbh, color_contol1), guide = F)+
  Chrisitheme+
  geom_text(x = 1.5, y = 0.8, label = "*", size = starsize) 
