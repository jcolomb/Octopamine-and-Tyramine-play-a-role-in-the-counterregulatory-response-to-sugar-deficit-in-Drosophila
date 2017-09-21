
# All UAStbh rescues
# script for boxplots of all groups and statistics (mutant control vs. rescue for each group)

 
  
# boxplot of all groups ####

    all <- read.csv2("data/fig6_all-rescues.csv")
    #recalculate score from raw data (modifiable)
    all=score_PER (all)
    all$genotype ->temp
    all$genotype  = all$females
    all = changegenotype(all)
    all$Xchromosome = all$genotype
    all$genotype = temp
  #attach(all)

    all_subset <- subset(all,females!="w")  
    
    #library("ggplot2")
    a <- aggregate(SCORE ~ males + Xchromosome , all_subset, function(i) c(val=length(i), ypos=quantile(i)[2]))
    
    F7=ggplot(all_subset, aes(x=males, y=SCORE, fill=Xchromosome)) +
      geom_boxplot(position = position_dodge(width=0.9)) +
      #geom_violin()+
      xlab("Gal4 drivers") +
      #ggtitle("Rescue using UAS-Tbh with different drivers")+
      ylab(TITLE_PER) +
      scale_fill_manual(values=c(color_tbh,color_rescue)) + # column fill and legend title/ -lables
      scale_x_discrete(limits=c("Actin","Tdc1","nSyb","Tdc2","NP7088"))+
      scale_y_continuous(breaks=0:7, limits = c(0,8)) +  
      Chrisitheme+
      geom_text(data = a, aes(y= SCORE[,2]+0.2,label= paste0("n = ",SCORE[,1])), 
                position = position_dodge(width=0.9))+
      geom_text(x = 1, y = 7.5, label = "*", size = starsize)+
      geom_text(x = 2, y = 7.5, label = "*", size = starsize)+
      geom_text(x = 3, y = 7.5, label = "*", size = starsize)
  
    # ggplot(all, aes(x=males, y=SCORE, fill=females)) +
    #   geom_boxplot() +
    #   xlab("Gal4 drivers") +
    #   ggtitle("rescue using UAS-Tbh with different drivers")+
    #   ylab(TITLE_PER) +
    #   scale_fill_manual(values=c(color_tbh,color_rescue, color_contol1)) + # column fill and legend title/ -lables
    #   scale_x_discrete(limits=c("Actin","Tdc1","nSyb","Tdc2","NP7088"))+
    #   scale_y_continuous(breaks=0:8) +  
    #   Chrisitheme+
    #   legend(legend.position = "right")
    # 
#     table(females,males)

# statistics for each group ####

       
    # nSyb

    
    data =  subset(all_subset, genotype %in% c( "Actintbh", "ActinUAS") )
    table(data$genotype)
    wilcox.test(data$SCORE~data$genotype)
    
    data =  subset(all_subset, genotype %in% c( "Tdc1tbh", "Tdc1UAS") )
    table(data$genotype)
    wilcox.test(data$SCORE~data$genotype)
    
    data =  subset(all_subset, genotype %in% c( "nSybtbh", "nSybUAS") ) 
    table(data$genotype)
    wilcox.test(data$SCORE~data$genotype)

    data =  subset(all_subset, genotype %in% c( "Tdc2tbh", "Tdc2UAS") )
    table(data$genotype)
    wilcox.test(data$SCORE~data$genotype)
    
    data =  subset(all_subset, genotype %in% c( "NP7088tbh", "NP7088UAS") )
    table(data$genotype)
    wilcox.test(data$SCORE~data$genotype)
    
   