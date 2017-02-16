### electrophysiology
# fed and starved flies 
# starvation vials supplied with water (Evian) for 20 h.
# recorded from 1 to 3 different labellar sensilla of an individual fly (13-15 flies in total)
# stimulated with 100 mM sucrose
# some of the sensilla did not always respond (non-responders, 0 in data)
# Mean spike numbers within 1 second (0.2s-1.2s) 
# Without No-Responders


# only tbh and w+ (no positive controls ) ####
# without "not responders" 
# two experiments
# all sensilla pooled
n_fun <- function(x){
  return(data.frame(y = 10, label = paste0("n = ",length(x))))
}
  
  record_tbhw <- read.csv2("data/fig5_tbh-w.csv")
  #change genotype names
  record_tbhw= changegenotype( record_tbhw)

 
  # boxplots
  a <- aggregate(spikes ~ starvation + genotype , record_tbhw, function(i) length(i))
  F5A =
    ggplot(record_tbhw, aes(x=starvation, y=spikes, fill=genotype)) +
  geom_boxplot(position = position_dodge(width=0.9)) +
  #ggtitle("All flies without Non-Responders")+
  ylab("Number of Spikes") +
  xlab("") +
    scale_fill_manual(values=c(color_tbh, color_contol1)) +
  #scale_fill_manual(values=c("grey","white","white")) + # column fill and legend title/ -lables
  scale_y_continuous(breaks=0:90*10, limits=c(0,100)) +
    geom_text(data = a, aes(y=100,label= c("a","b","ab","a")), 
              position = position_dodge(width=0.9), fontface="bold")+
    geom_text(data = a, aes(y=2,label= paste0("n = ",spikes)), 
                position = position_dodge(width=0.9))+
    #stat_summary(fun.data = n_fun, geom = "text")
    Chrisitheme+
    theme(legend.position = "right")
    
 
  pairwise.wilcox.test(record_tbhw$spikes,record_tbhw$group,p.adjust="bonferroni" )
  
# only CS and w1118 ####
  
  CSw1118 <- read.csv2("data/fig5_CS-w1118.csv")
                       


  #boxplot
  a <- aggregate(spikes ~ starvation + genotype , CSw1118, function(i) length(i))
  
 F5B= ggplot(CSw1118, aes(x=starvation, y=spikes, fill=genotype)) +
    geom_boxplot(position =position_dodge(width=0.9)) +
    #ggtitle("All flies without Non-Responders")+
    ylab("Number of Spikes") +
    xlab("") +
    #scale_fill_manual(values=c(color_tbh, color_contol1)) +
    #scale_fill_manual(values=c("grey","white","white")) + # column fill and legend title/ -lables
    scale_y_continuous(breaks=0:90*10, limits=c(0,100)) +
    geom_text(data = a, aes(y=100,label= c("a","c","b","d")), 
              position = position_dodge(width=0.9), fontface="bold")+
   geom_text(data = a, aes(y=2,label= paste0("n = ",spikes)), 
             position = position_dodge(width=0.9))+
    Chrisitheme+
    theme(legend.position = "right")
  
  


  pairwise.wilcox.test(CSw1118$spikes,CSw1118$group)
 

# # barplots (not in the manuscript) ####
#   
#   #  CS and w1118
#     library("plyr") 
#     cCSw1118  <- ddply(CSw1118 , .(starvation,genotype1), summarise,                          # .()includes all the variables
#                    N      = length(spikes),
#                  spike = mean(spikes),
#                  sd     = sd(spikes),
#                  se     = sd(spikes) / sqrt(length(spikes)) )
#     library("ggplot2")
#     ggplot(cCSw1118,aes(x=genotype1,y=spike, fill=starvation)) + 
#     geom_bar(position=position_dodge(),col="black") +
#     geom_errorbar(aes(ymin=spike-se, ymax=spike+se),
#                 width=.2,position=position_dodge(.9)) +
#     ylab("Number of spikes") +
#     xlab("") +
#     scale_fill_manual(values=c("darkgrey","white","white","white"),name="",
#                     labels=c("Fed","After starvation")) + # column fill and legend title/ -lables
#     scale_y_continuous(breaks=0:15*10, limits = c(0,100)) +
#     theme_bw() +                                                           # to make background white, has to be before "opts"
#     opts(axis.title.x = theme_text(size=20),                               #(face="bold", colour="black")
#        axis.title.y = theme_text(angle=90,size=20),
#        axis.text.x  = theme_text(hjust=0.5, size=16),
#        axis.line = theme_segment(colour = "black"),
#        panel.grid.major = theme_blank(),
#        panel.grid.minor = theme_blank(),
#        panel.border = theme_blank())
#   
#     table(starvation,genotype)
#     x=aov(spikes~starvation*genotype)
#     x=aov(spikes[starvation!="fed"]~genotype[starvation!="fed"])
#     summary(x)
#     TukeyHSD(x)
#   
#   # tbh and w+
#   
#     library("plyr") 
#     crecord <- ddply(record_tbhw, .(genotype,starvation), summarise,                          # .()includes all the variables
#                  N      = length(spikes),
#                  spike = mean(spikes),
#                  sd     = sd(spikes),
#                  se     = sd(spikes) / sqrt(length(spikes)) )
#     library("ggplot2")
#     ggplot(crecord , aes(x=starvation, y=spike, fill=genotype)) + 
#     geom_bar(position=position_dodge(),col="black") +
#     geom_errorbar(aes(ymin=spike-se, ymax=spike+se),
#                 width=.2,position=position_dodge(.9)) +
#     ylab("Number of spikes") +
#     xlab("") +
#     scale_fill_manual(values=c("white","darkgrey"),name="") + # column fill and legend title/ -lables
#     scale_y_continuous(breaks=0:15*10, limits = c(0,80)) +
#     #scale_x_discrete(limits=c("tbh","w+")) +
#     theme_bw() +                                                           # to make background white, has to be before "opts"
#     opts(axis.title.x = theme_text(size=20),                               #(face="bold", colour="black")
#        axis.title.y = theme_text(angle=90,size=20),
#        axis.text.x  = theme_text(hjust=0.5, size=16),
#        axis.line = theme_segment(colour = "black"),
#        panel.grid.major = theme_blank(),
#        panel.grid.minor = theme_blank(),
#        panel.border = theme_blank())
# 
#     table(genotype,starvation)
#     s=aov(spikes~genotype*starvation)
#     summary(s)
#     TukeyHSD(s)
