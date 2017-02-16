
#####  Receptormutants PER 

#### TA-rec ####
    
  # only homozygous mutants 
 
    TA_homo <- read.csv2("data/TA-rec-homo.csv")
    #recalculate score from raw data (modifiable)
    TA_homo=score_PER (TA_homo)
    
    #attach(TA_homo)
    #library("ggplot2")
    ggplot(TA_homo, aes(x=receptor, y=SCORE,fill=code)) +
    geom_boxplot() +
    xlab("Females") +
    ylab(TITLE_PER) +
    scale_fill_manual(values=c("grey","grey","grey","white"), guide=F) + # column fill and legend title/ -lables
    scale_x_discrete(limits=c("CG7431f05682","TyRIId29","TyRII-CG7431d124","w"))+
    scale_y_continuous(breaks=0:8) +
    Chrisitheme
  
 
  wilcox.test(SCORE[receptor!="CG7431f05682"&receptor!="TyRIId29"]~receptor[receptor!="CG7431f05682"&receptor!="TyRIId29"])
  wilcox.test(SCORE[receptor!="TyRII-CG7431d124"&receptor!="TyRIId29"]~receptor[receptor!="TyRII-CG7431d124"&receptor!="TyRIId29"])
  wilcox.test(SCORE[receptor!="TyRII-CG7431d124"&receptor!="CG7431f05682"]~receptor[receptor!="TyRII-CG7431d124"&receptor!="CG7431f05682"])  
 



#### OAMB ####  

  ## females

    OAMBfemales <- read.csv2("data/OAMBfemales.csv")
  #recalculate score from raw data (modifiable)
  OAMBfemales=score_PER (OAMBfemales)
  
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



#### Oct?2R-OA2R ####

## only homozygous females
  
  OA2R_homo <- read.csv2("data/OA2Rfemales-homozygous.csv")
    #recalculate score from raw data (modifiable)
    OA2R_homo=score_PER (OA2R_homo)
    #OA2R_homo$receptor
  ggplot(OA2R_homo, aes(x=receptor, y=SCORE,fill=receptor)) +
  geom_boxplot() +
  xlab("") +
  ylab(TITLE_PER) +
  scale_fill_manual(values=c("white","grey","grey"),
                    labels=c("Control","Mutant","Mutant"), guide=F ) + 
  scale_y_continuous(breaks=0:8) +
  scale_x_discrete(limits=c("delta3.22oct\xdf2R","delta4.3oct\xdf2R","CS"))+
  Chrisitheme
  


attach(OA2R_homo)
  wilcox.test(SCORE[receptor!="delta4.3oct\xdf2R"]~receptor[receptor!="delta4.3oct\xdf2R"],paired=F)
  wilcox.test(SCORE[receptor!="delta3.22oct\xdf2R"]~receptor[receptor!="delta3.22oct\xdf2R"],paired=F)



#### honoka ####

  ##  females after 7generation of outcrossing with w+;;TM3,Ubx/MKRS,Sb
    
    honokaf <- read.csv2("data/honokafemales.csv")
  #recalculate score from raw data (modifiable)
  honokaf=score_PER (honokaf)
    attach(honokaf)
    #library("ggplot2")
    ggplot(honokaf, aes(x=group, y=SCORE,fill=group)) +
      geom_boxplot() +
      xlab("Females") +
      ylab("Median PER") +
      scale_fill_manual(values=c("grey","white"), guide=F) + # column fill and legend title/ -lables
      scale_y_continuous(breaks=0:8) +
      Chrisitheme
    
    wilcox.test(SCORE~group)




 