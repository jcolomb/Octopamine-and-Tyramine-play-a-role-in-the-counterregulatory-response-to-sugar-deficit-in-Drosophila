library (tidyverse)
library("cowplot")
library(MASS)
library(reshape2)


Chrisitheme <- theme_bw() +     # to make background white, has to be before "opts"
  theme(axis.title.x = element_text(size=20),   #(face="bold", colour="black")
       axis.title.y = element_text(angle=90,size=20),
       axis.text.x  = element_text(hjust=0.5, size=16),
       axis.text.y  = element_text( size=16),
       legend.text = element_text(size = 16),
       legend.position = c(0.9,0.9),
       axis.line = element_line(colour = "black"),
       panel.grid.major = element_blank(),
       panel.grid.minor = element_blank(),
       panel.border = element_blank(),
       panel.background = element_blank()
       ) #
point_size=3
starsize= 10 # size of stars signaling significant differences
int_segm =0.05 # define size of the segments to show group differences, low number -> longer line

color_tbh = "sky blue"
color_contol1= rgb(0.80, .6, 0.7)
color_contol2=  rgb(0.80, .6, 0.7)
color_rescue="yellow"

color_tbh = rgb(0,.45,.70)
color_contol1= rgb(0.8, .7, 0.6)
color_contol2=  color_contol1
color_rescue=rgb(.95,.90,.25)

TITLE_PER= "Total number of PER"

#color_tbh = "grey"
#color_contol1= "white"
#color_contol2= "black"
#color_rescue="lightgrey"

changegenotype <- function(allfem) {
  allfem$genotype = as.character(allfem$genotype)
  allfem$genotype [allfem$genotype == "tbh"] <- "tßhnM18"
  allfem$genotype [allfem$genotype == "TBH"] <- "tßhnM18"
  allfem$genotype [allfem$genotype == "w"] <- "w+"
  allfem$genotype [allfem$genotype == "hs"] <- "tßhnM18; hs-tßh"
  allfem$genotype [allfem$genotype == "HStbh"] <- "tßhnM18; hs-tßh"
  allfem$genotype [allfem$genotype == "UAS"] <- "tßhnM18, UAS-tßh"
  allfem$genotype = as.factor(allfem$genotype)
  allfem
}

score_PER <- function(data) {
  data= data %>% mutate (SCORE = X0+X0.1+X0.3+X0.6+X1+X3+X10+X30)
}

## Figure 1.
source ("Rscripts/fig1.r")
source ("Rscripts/fig2_2.r")
source ("Rscripts/fig3.r")
source ("Rscripts/fig4.r")
source ("Rscripts/fig5.r")
source ("Rscripts/fig7.r")





  


pdf(width=11.17, height= 4.57)

ggdraw()+
  draw_plot(F1A, x = 0, y = 0, width = .66, height = 1) +
  draw_plot(F1B+geom_jitter(), x = .66, y = 0, width = .33, height = 1) +
  draw_plot_label(label = c("A", "B"), size = 18,
                  x = c(0, 0.66), y = c(1, 1))
ggdraw()+
  draw_plot(Fig2A+geom_jitter(), x = 0, y = 0, width = .66, height = 1) +
  draw_plot(Fig2B+geom_jitter(), x = .66, y = 0, width = .33, height = 1) +
  draw_plot_label(label = c("A", "B"), size = 18,
                  x = c(0,0.66), y = c(1, 1))
ggdraw()+
  draw_plot(Fig3A, x = 0, y = 0, width = .66, height = 1) +
  draw_plot(Fig3B+geom_jitter(), x = .66, y = 0, width = .33, height = 1) +
  draw_plot_label(label = c("A", "B"), size = 18,
                  x = c(0, 0.66), y = c(1, 1))

ggdraw()+
  draw_plot(F4A+geom_jitter(), x = 0, y = 0, width = .5, height = 1) +
  draw_plot(F4B+geom_jitter(), x = .5, y = 0, width = .5, height = 1) +
  draw_plot_label(label = c("A", "B"), size = 18,
                  x = c(0, 0.5), y = c(1, 1))
ggdraw()+
  draw_plot(F5A+geom_jitter(), x = 0, y = 0, width = .5, height = 1) +
  draw_plot(F5B+geom_jitter(), x = .5, y = 0, width = .5, height = 1) +
  draw_plot_label(label = c("A", "B"), size = 18,
                  x = c(0, 0.5), y = c(1, 1))
F7+geom_jitter()

dev.off()
