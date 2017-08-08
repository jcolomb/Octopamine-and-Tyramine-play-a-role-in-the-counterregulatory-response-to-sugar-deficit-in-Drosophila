
#####  Receptormutants PER 

#### TA-rec ####
    
  # only homozygous mutants 
 
    TA_homo <- read.csv2("data/TA-rec-homo.csv")
    #recalculate score from raw data (modifiable)
    TA_homo=score_PER (TA_homo)
    

    #TA_homo = na.omit(TA_homo)
    TA_homo$genotype = as.factor(TA_homo$receptor) 
    levels (TA_homo$genotype)= c("TyrR", "TyrR,TyrII", "TyrII", "Control")

    
    a <- aggregate(SCORE ~ genotype + genotype , TA_homo, function(i) c(val=length(i), ypos=quantile(i)[2]))
    
    F7A2=ggplot(TA_homo, aes(x=genotype, y=SCORE, fill=genotype)) + # says which column in the table should be the x/y axis, fill says which groups you have within the columns
      geom_boxplot(position = position_dodge(width=0.9)) +
      ylab(TITLE_PER) +
      xlab("") +
      scale_fill_manual(values=c(color_tbh,color_tbh,color_tbh, color_contol1), guide=FALSE) +
      #,name="Females",
      # labels=c("tßhnM18","w+")) + # column fill and legend title/ -lables
      scale_y_continuous(breaks=0:7, limits = c(0,8))+
      Chrisitheme+
      geom_text(x = 1, y = 7.5, label = "*", size = starsize)+
      geom_text(x = 3, y = 7.5, label = "*", size = starsize)+
      geom_text(data = a, aes(y= SCORE[,2]+0.2,label= paste0("n = ",SCORE[,1])), 
                position = position_dodge(width=0.9))
    
    #attach(TA_homo)
    #library("ggplot2")
   
  
 
  wilcox.test(TA_homo$SCORE[TA_homo$receptor!="CG7431f05682"&TA_homo$receptor!="TyRIId29"]~TA_homo$receptor[TA_homo$receptor!="CG7431f05682"&TA_homo$receptor!="TyRIId29"])
  wilcox.test(TA_homo$SCORE[TA_homo$receptor!="TyRII-CG7431d124"&TA_homo$receptor!="TyRIId29"]~TA_homo$receptor[TA_homo$receptor!="TyRII-CG7431d124"&TA_homo$receptor!="TyRIId29"])
  wilcox.test(TA_homo$SCORE[TA_homo$receptor!="TyRII-CG7431d124"&TA_homo$receptor!="CG7431f05682"]~TA_homo$receptor[TA_homo$receptor!="TyRII-CG7431d124"&TA_homo$receptor!="CG7431f05682"])  
 



#### OAMB ####  

  ## females

    OAMBfemales <- read.csv2("data/OAMBfemales.csv")
  #recalculate score from raw data (modifiable)
  OAMBfemales=score_PER (OAMBfemales)
  
  levels (OAMBfemales$genotype)= c("OAMB286", "OAMB584", "Control")
  a <- aggregate(SCORE ~ genotype + genotype , OAMBfemales, function(i) c(val=length(i), ypos=quantile(i)[3]))
  
  F7A3=ggplot(OAMBfemales, aes(x=genotype, y=SCORE, fill=genotype)) + # says which column in the table should be the x/y axis, fill says which groups you have within the columns
    geom_boxplot(position = position_dodge(width=0.9)) +
    ylab(TITLE_PER) +
    xlab("") +
    scale_fill_manual(values=c(color_tbh,color_tbh, color_contol1), guide=FALSE) +
    #,name="Females",
    # labels=c("tßhnM18","w+")) + # column fill and legend title/ -lables
    scale_y_continuous(breaks=0:7, limits = c(0,8))+
    Chrisitheme+
    geom_text(data = a, aes(y= SCORE[,2]+0.2,label= paste0("n = ",SCORE[,1])), 
              position = position_dodge(width=0.9))
  
  
  
    attach(OAMBfemales)
   # library("ggplot2")
    ggplot(OAMBfemales, aes(x=group, y=SCORE,fill=group)) +
      geom_boxplot() +
      xlab("Females") +
      ylab("Median PER") +
      scale_y_continuous(breaks=0:8) +
      scale_fill_manual(values=c("grey","grey","white"), guide=F) + # column fill and legend title/ -lables
      Chrisitheme
    
    #table(group,sex)
    quantile(SCORE[group=="OAMB286"])   
    quantile(SCORE[group=="OAMB584"]) 
    quantile(SCORE[group=="w"]) 

   # statistics
    wilcox.test(SCORE[group!="OAMB286"]~group[group!="OAMB286"])
    wilcox.test(SCORE[group!="OAMB584"]~group[group!="OAMB584"])



# #### Oct?2R-OA2R ####
# 
# ## only homozygous females
#   
#   OA2R_homo <- read.csv2("data/OA2Rfemales-homozygous.csv")
#     #recalculate score from raw data (modifiable)
#     OA2R_homo=score_PER (OA2R_homo)
#     #OA2R_homo$receptor
#   ggplot(OA2R_homo, aes(x=receptor, y=SCORE,fill=receptor)) +
#   geom_boxplot() +
#   xlab("") +
#   ylab(TITLE_PER) +
#   scale_fill_manual(values=c("white","grey","grey"),
#                     labels=c("Control","Mutant","Mutant"), guide=F ) + 
#   scale_y_continuous(breaks=0:8) +
#   scale_x_discrete(limits=c("delta3.22oct\xdf2R","delta4.3oct\xdf2R","CS"))+
#   Chrisitheme
#   
# 
# 
# attach(OA2R_homo)
#   wilcox.test(SCORE[receptor!="delta4.3oct\xdf2R"]~receptor[receptor!="delta4.3oct\xdf2R"],paired=F)
#   wilcox.test(SCORE[receptor!="delta3.22oct\xdf2R"]~receptor[receptor!="delta3.22oct\xdf2R"],paired=F)
# 
# 

#### honoka ####

  ##  females after 7generation of outcrossing with w+;;TM3,Ubx/MKRS,Sb
    
    honokaf <- read.csv2("data/honokafemales.csv")
  #recalculate score from raw data (modifiable)
  honokaf=score_PER (honokaf)
  honokaf = na.omit(honokaf)
  honokaf$genotype = as.factor(honokaf$group) 
  levels (honokaf$genotype)= c("honoka", "Control")
  honokaf$SCORE
  
  a <- aggregate(SCORE ~ genotype + genotype , honokaf, function(i) c(val=length(i), ypos=quantile(i)[2]))
  
  F7A1=ggplot(honokaf, aes(x=genotype, y=SCORE, fill=genotype)) + # says which column in the table should be the x/y axis, fill says which groups you have within the columns
    geom_boxplot(position = position_dodge(width=0.9)) +
    ylab(TITLE_PER) +
    xlab("") +
    scale_fill_manual(values=c(color_tbh, color_contol1), guide=FALSE) +
    #,name="Females",
    # labels=c("tßhnM18","w+")) + # column fill and legend title/ -lables
    scale_y_continuous(breaks=0:7, limits = c(0,8))+
    Chrisitheme+
    geom_text(x = 1.5, y = 7.5, label = "*", size = starsize)+
    geom_text(data = a, aes(y= SCORE[,2]+0.2,label= paste0("n = ",SCORE[,1])), 
              position = position_dodge(width=0.9))
  

    attach((honokaf))
    wilcox.test(SCORE~group)




 