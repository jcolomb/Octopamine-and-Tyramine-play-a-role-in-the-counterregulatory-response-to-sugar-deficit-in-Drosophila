## Figure 1.
#read data
allfem <- read.csv2("data/fig1_all-females-boxplot.csv")

#change genotype names
allfem = changegenotype(allfem)
#recalculate score from raw data (modifiable)
allfem=score_PER (allfem)




#plot figure 1b
a <- aggregate(SCORE ~ genotype + genotype , allfem, function(i) c(val=length(i), ypos=quantile(i)[2]))

F1B=ggplot(allfem, aes(x=genotype, y=SCORE, fill=genotype)) + # says which column in the table should be the x/y axis, fill says which groups you have within the columns
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

#statistics
pairwise.wilcox.test(allfem$SCORE,allfem$genotype)


#curves
#test if column 13 is correct
all(rowSums(allfem[,5:12])-allfem[,13]== 0)

# get proportion of flies answering at each concentration
R=summarise_all(group_by( allfem, genotype), mean)[,c(1,5:12)]
#add proper column names
concentrations=c(0,0.1,0.3,0.6,1,3,10,30)
names (R) = c("genotype", concentrations)


#tidy data in a form for the plot
d <- melt(R)
#plot curve
F1A= ggplot(d, aes(x=(as.numeric(as.character(variable))), y= value, color = genotype,shape = genotype))+
  geom_point(size=point_size)+  geom_line() +
  scale_x_log10(breaks=concentrations)+ #x axis on log scale
  scale_y_continuous(limits = c(0,1))+
  
  scale_colour_manual(values = c(color_tbh, color_contol2))+
  ylab("Proportion responding") +
  xlab("sucrose concentration [%]")+
  scale_shape_discrete(solid=T) +
  Chrisitheme+
  theme(legend.position = c(0.12,0.9))

