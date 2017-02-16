#fig4


# tbh rescue by HS

####     heat 3h before test  ####

## all data pooled for 140h, 3h recovery (138h, 140h cross1 and 2, 
# and different recoveries (only 3h used) with new flies)

all140 <-  read.csv2("data/fig4_HS140.csv")
#change genotype names
all140= changegenotype(all140)
#recalculate score from raw data (modifiable)
all140=score_PER (all140)


factor(all140$genotype)

a <- aggregate(SCORE ~ genotype + genotype , all140, function(i) c(val=length(i), ypos=quantile(i)[2]))

F4A=
ggplot(all140, aes(x=genotype, y=SCORE, fill=genotype)) +
  geom_boxplot(position = position_dodge(width=0.9)) +
  ylab(TITLE_PER) +
  xlab ("genotype")+
  ggtitle("heat shocked 3h before test (flies starved 138-140h)") +
  scale_fill_manual(values=c(color_tbh,color_rescue,color_contol1), guide=F)+
  
                     # column fill and legend title/ -lables
  scale_y_continuous(breaks=0:7, limits = c(0,8))+
  Chrisitheme+
  theme(legend.position = "right")+
  geom_text(data = a, aes(y= SCORE[,2]+0.2,label= paste0("n = ",SCORE[,1])), 
            position = position_dodge(width=0.9))+
  geom_text(x = 1.5, y = 7.5, label = "*", size = starsize)+
  geom_text(x = 2.5, y = 7.5, label = "*", size = starsize)+


  geom_segment(x=1+int_segm, xend= 2-int_segm, y =7.45,yend=7.45, size= 0.5)+
  geom_segment(x=2+int_segm, xend= 3-int_segm, y =7.45,yend=7.45, size= 0.5)

#table(genotype)
pairwise.wilcox.test(all140$SCORE,all140$genotype,p.adjust="bonferroni")


####     heat during starvation ####
# heat once a day 
# starvation at 18degrees for 118h

data <- read.csv2("data/fig4_HS118_96.csv")
HS118 <- subset(data,starvation!="ninetysix")
#HS118 <- data

#change genotype names
HS118= changegenotype(HS118)
#recalculate score from raw data (modifiable)
HS118=score_PER (HS118)


a <- aggregate(SCORE ~ genotype + genotype , HS118, function(i) c(val=length(i), ypos=quantile(i)[2]))

F4B=ggplot(HS118, aes(x=genotype, y=SCORE, fill=genotype)) +
  geom_boxplot(position = position_dodge(width=0.9)) +
  ylab(TITLE_PER) +
  xlab("genotype")+
  ggtitle("heat shocked once a day (flies starved 118h)") +
  scale_fill_manual(values=c(color_tbh,color_rescue,color_contol1), guide=F) + # column fill and legend title/ -lables
  scale_y_continuous(breaks=0:7, limits = c(0,8))+
  Chrisitheme+
  theme(legend.position = "right")  +
  geom_text(data = a, aes(y= SCORE[,2]+0.2,label= paste0("n = ",SCORE[,1])), 
            position = position_dodge(width=0.9))+
  geom_text(x = 2, y = 7.5, label = "*", size = starsize)+
  geom_text(x = 2.5, y = 7.8, label = "*", size = starsize)+
  geom_segment(x=2+int_segm, xend= 3-int_segm, y =7.75,yend=7.75, size= 0.5)+
  geom_segment(x=1+int_segm, xend= 3-int_segm, y =7.45,yend=7.45, size= 0.5)



pairwise.wilcox.test(HS118$SCORE[HS118$starvation_time=="118"],HS118$genotype[HS118$starvation_time=="118"],p.adjust="bonferroni")

