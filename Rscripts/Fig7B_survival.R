
# Survival - only females #
# Receptor mutants
# LD 50


# honoka ####

RecMut <- read.csv2("data/Fig7_survival.csv")
RecMut <- na.omit(RecMut)
#attach(RecMut)

data_ori <- subset(RecMut, genotype == "honoka")
source("Rscripts/survival_changedata.r")
honokadata = list_survd

data_ori = subset(RecMut, code == "12")
source("Rscripts/survival_changedata.r")
wt_hono = list_survd

total = rbind (wt_hono, honokadata)
total = droplevels(total)
levels(total$genotype) = c("honoka", "Control")
total_2 =total

A = survfit(Surv(time, event) ~ genotype, data = total)
Fig7B1=ggplot2::autoplot(A, surv.geom ="line") +
  ylab ("Proportion surviving") +
  xlab("Starvation time [h]") +
  geom_hline (yintercept = 0.5, linetype = 3) +
  scale_colour_manual(values = c(color_tbh, color_contol2)) +
  Chrisitheme +
  theme(legend.justification = c(1, 1),
        legend.position = c(1, 1))


coxph(Surv(time, event) ~ genotype + trial, data = total)
#coxph(Surv(time, event) ~ genotype, data = total)




## TA rec ####


data_ori = subset(RecMut, code == "1")
source("Rscripts/survival_changedata.r")
cg7431 = list_survd

data_ori = subset(RecMut, code == "3")
source("Rscripts/survival_changedata.r")
CG7431_TyrII = list_survd


data_ori = subset(RecMut, code == "4")
source("Rscripts/survival_changedata.r")
TyrII = list_survd

data_ori = subset(RecMut, code == "9")
source("Rscripts/survival_changedata.r")
wt_tyr = list_survd
wt_tyr$genotype = "A_Control"

total = rbind (wt_tyr, cg7431, CG7431_TyrII, TyrII)
total = droplevels(total)
total$genotype = as.factor(total$genotype)
levels(total$genotype) = c("Control", "TyrR", "TyrR,TyrII", "TyrII")
  total_2 =rbind(total_2,total)
  A = survfit(Surv(time, event) ~ genotype, data = total)
  Fig7B2=ggplot2::autoplot(A, surv.geom ="line") +
    ylab ("Proportion surviving") +
    xlab("Starvation time [h]") +
    geom_hline (yintercept = 0.5, linetype = 3) +
    scale_colour_manual(values=c( color_contol2,color_tbh,color_rescue, color_other))+
    Chrisitheme +
    theme(legend.justification = c(1, 1),
          legend.position = c(1, 1))
  
coxph(Surv(time, event) ~ genotype + trial, data = total)
total = rbind (wt_tyr, CG7431_TyrII)
total = droplevels(total)
coxph(Surv(time, event) ~ genotype + trial, data = total)
total = rbind (wt_tyr, TyrII)
total = droplevels(total)
coxph(Surv(time, event) ~ genotype + trial, data = total)
total = rbind (wt_tyr, cg7431)
total = droplevels(total)
coxph(Surv(time, event) ~ genotype + trial, data = total)






## OAMB ####



data_ori = subset(RecMut, code == "5")
source("Rscripts/survival_changedata.r")
OAMB286 = list_survd

data_ori = subset(RecMut, code == "6")
source("Rscripts/survival_changedata.r")
OAMB584 = list_survd

data_ori = subset(RecMut, code == "10")
source("Rscripts/survival_changedata.r")
control_oamb = list_survd

total = rbind (control_oamb, OAMB286, OAMB584)
total = droplevels(total)
levels(total$genotype) = c ("oamb286", "oamb584" ,"Control")
  total_2 =rbind(total_2,total)
A = survfit(Surv(time, event) ~ genotype, data = total)
Fig7B3=ggplot2::autoplot(A, surv.geom ="line") +
  ylab ("Proportion surviving") +
  xlab("Starvation time [h]") +
  geom_hline (yintercept = 0.5, linetype = 3) +
  scale_colour_manual(values=c(color_tbh, color_rescue,color_contol2))+
  Chrisitheme +
  theme(legend.justification = c(1, 1),
        legend.position = c(1, 1))


total = rbind (control_oamb, OAMB584)
total = droplevels(total)
coxph(Surv(time, event) ~ genotype + trial, data = total)

total = rbind (control_oamb, OAMB286)
total = droplevels(total)
coxph(Surv(time, event) ~ genotype + trial, data = total)

A = survfit(Surv(time, event) ~ genotype, data = total_2)
ggplot2::autoplot(A, 
                  , conf.int = TRUE, surv.geom ="line") +
ylab ("Proportion surviving") +
xlab("Starvation time [h]") +
geom_hline (yintercept = 0.5, linetype = 3) +
#scale_colour_manual(values=c(color_tbh, color_contol2))+
Chrisitheme +
theme(legend.justification = c(1, 1),
legend.position = c(1, 1))

