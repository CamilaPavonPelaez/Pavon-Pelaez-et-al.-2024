require(fitdistrplus)
library(multcomp)
require(GGally)
library(doBy)
library(ggpubr)
require(ggplot2)
library(dplyr)
library(forcats)
require(sjPlot)


#Laboratory data
dlab <- read.csv("Lab.data.PavonPelaez.etal.csv", header=T, sep=";", dec=".")

dlab$Location <- as.factor(dlab$Location)
dlab$Group <- as.factor(dlab$Group)
dlab$MaleID <- as.factor(dlab$MaleID)
dlab$FemaleID <- as.factor(dlab$FemaleID)
dlab$Mate <- as.factor(dlab$Mate)


#===================== Male size differences between groups =====================
dlabms <- subset(dlab, Male.size != "NA")

norm <- fitdist(dlabms$Male.size,"norm")
lnorm <- fitdist(dlabms$Male.size,"lnorm")
gamma <- fitdist(dlabms$Male.size, "gamma")
par(mfrow = c(1,2), cex.lab = 1.5, cex.axis = 1.3, mai = c(1,1,0.2,0.2))
cdfcomp(list(norm, lnorm, gamma), legendtext = c("normal","lognormal", "gamma"), 
        xlogscale = T, ylogscale = T, main = "")
qqcomp(list(norm, lnorm, gamma), legendtext = c("normal","lognormal", "gamma"), main = "")
par(mfrow = c(1,1), cex.lab = 1, cex.axis = 1) 
gofstat(list(norm, lnorm, gamma))$aic # similar


a1.1 <- lm(Male.size ~ Group, dlabms)
summary(a1.1)
#Coefficients: N 
#                  Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)     3.56630    0.05516  64.656   <2e-16 ***
#  GroupWorthless -0.01764    0.07875  -0.224    0.823     


se <- function(x){sd(x)/sqrt(length(x))}

tablaMS <- summaryBy(Male.size ~ Location*Group ,
                     data = dlabms, FUN = c(length, mean, sd, se, min, max))
# Location     Group       N   Male.size.mean Male.size.sd Male.size.se Male.size.min Male.size.max
#1 Center-North Nutritive  11      3.609091   0.4805300  0.14488525         3.20         4.80
#2 Center-North Worthless  11      3.536364   0.2110579  0.06363636         3.20         4.00
#3 Center-South Nutritive  19      3.189474   0.2605864  0.05978262         2.58         3.58
#4 Center-South Worthless  19      3.165263   0.2200346  0.05047939         2.82         3.58
#5        North Nutritive  9       3.855556   0.2455153  0.08183844         3.50         4.20
#6        North Worthless  6       3.850000   0.2428992  0.09916317         3.60         4.20
#7        South Nutritive  15      3.838667   0.1939759  0.05008437         3.35         4.11
#8        South Worthless  16      3.899375   0.2636277  0.06590693         3.47         4.58



#=========================== EXPLORATORY =======================================
#===============================================================================
ggpairs(dlab[,c(1,3,4,5,7,10,11,12,13,14)], mapping = aes(color = Location)) + theme_bw()
# No strong correlations (Except WrapNum & WrapDur)


se <- function(x){sd(x)/sqrt(length(x))}

# Wrapping duration
dlabwd <- subset(dlab, WrapDur != "NA")
tableWD <- summaryBy(WrapDur ~ Location*Group,
                     data = dlabwd, FUN = c(length, mean, se, sd))
tableWD
tableWN <- summaryBy(WrapNum ~ Location*Group,
                     data = dlabwd, FUN = c(length, mean, se, sd))
tableWN

# Mating duration
dlabmd <- subset(dlab, InsDur != "NA")
tableMD <- summaryBy(InsDur ~ Location*Group,
                     data = dlabmd, FUN = c(length, mean, se, sd))
tableMD
tableMN <- summaryBy(InsNum ~ Location*Group,
                     data = dlabmd, FUN = c(length, mean, se, sd))
tableMN


#============================== STATS ==========================================
#===============================================================================

#==============  Silk Wrapping Duration  ==============
dlabwd <- subset(dlab, WrapDur != "NA")

norm <- fitdist(dlabwd$WrapDur,"norm")
lnorm <- fitdist(dlabwd$WrapDur,"lnorm")
gamma <- fitdist(dlabwd$WrapDur, "gamma")
par(mfrow = c(1,2), cex.lab = 1.5, cex.axis = 1.3, mai = c(1,1,0.2,0.2))
cdfcomp(list(norm, lnorm, gamma),legendtext = c("normal","lognormal", "gamma"), 
        xlogscale = T, ylogscale = T, main = "")
qqcomp(list(norm, lnorm, gamma), legendtext = c("normal", "lognormal", "gamma"), main = "")
par(mfrow = c(1,1), cex.lab = 1, cex.axis = 1) 
gofstat(list(norm,lnorm, gamma))$aic # gamma

m1 <- glm(WrapDur ~ W.gifts.prop*Group, data = dlabwd, family = Gamma)
summary(m1)
#Coefficients: 
#                            Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                  0.26440    0.06694   3.950 0.000143 ***
#W.gifts.prop                 0.02157    0.11198   0.193 0.847650    
#GroupWorthless               0.07777    0.09289   0.837 0.404407    
#W.gifts.prop:GroupWorthless -0.19541    0.14794  -1.321 0.189450


#==============  Number of Silk Wrapping Bouts  ==============

m2 <- glm(WrapNum ~ W.gifts.prop*Group, data = dlabwd, family = poisson)
summary(m2)
#Coefficients: 
#                            Estimate Std. Error z value Pr(>|z|)    
#(Intercept)                  0.98998    0.24665   4.014 5.98e-05 ***
#W.gifts.prop                -0.29920    0.41710  -0.717    0.473    
#GroupWorthless              -0.01247    0.33823  -0.037    0.971    
#W.gifts.prop:GroupWorthless  0.25488    0.56758   0.449    0.653  


#==============  Mating Duration  ==============
dlabmd <- subset(dlab, InsDur != "NA")

norm <- fitdist(dlabmd$InsDur,"norm")
lnorm <- fitdist(dlabmd$InsDur,"lnorm")
gamma <- fitdist(dlabmd$InsDur, "gamma")

par(mfrow = c(1,2), cex.lab = 1.5, cex.axis = 1.3, mai = c(1,1,0.2,0.2))
cdfcomp(list(norm, lnorm, gamma), legendtext = c("normal","lognormal", "gamma"), 
        xlogscale = T, ylogscale = T, main = "")
qqcomp(list(norm,lnorm, gamma), legendtext = c("normal", "lognormal", "gamma"), main = "")
par(mfrow = c(1,1), cex.lab = 1, cex.axis = 1) 
gofstat(list(norm, lnorm, gamma))$aic # gamma

m3 <- glm(InsDur ~ W.gifts.prop*Group, data = dlabmd, family = Gamma)
summary(m3)
#Coefficients:
#                              Estimate Std. Error t value Pr(>|t|)  
#  (Intercept)                   0.4081     0.1743   2.342   0.0222 *
#  W.gifts.prop                  0.3291     0.3297   0.998   0.3219  
#  GroupWorthless                0.6716     0.2945   2.281   0.0258 *
#  W.gifts.prop:GroupWorthless  -1.0814     0.4809  -2.248   0.0279 *


#==============  Number of Insertions  ==============
dlabni <- subset(dlab, InsNum != "NA")

m4 <- glm(InsNum ~ W.gifts.prop*Group, data = dlabni, family = poisson)
summary(m4)
#Coefficients:
#                             Estimate Std. Error z value Pr(>|z|)   
# (Intercept)                   0.8925     0.2806   3.181  0.00147 **
# W.gifts.prop                  0.2085     0.4758   0.438  0.66120   
# GroupWorthless               -0.3185     0.4194  -0.759  0.44763   
# W.gifts.prop:GroupWorthless   0.3297     0.6986   0.472  0.63696


#============================= FIGURES =========================================
#===============================================================================

my.theme <- theme(axis.title=element_text(size=23,face= "italic"),
               axis.text= element_text(size=19),
               panel.border=element_rect(fill=NA,linewidth=0.5,color="black"),
               panel.background = element_rect(fill=NA, color="white"))


dlabwd <- subset(dlab, WrapDur != "NA")
wd <- dlabwd %>% # Wrapping duration vs Group*W.gifts.prop
  mutate(Location = fct_relevel(Location, "South", "Center-South", "Center-North", "North")) %>%
  ggplot(aes(x = W.gifts.prop, y = WrapDur)) + 
  geom_jitter(width = 0.01, shape = 21, size = 2, aes(col = Location, fill = Group))+
  scale_fill_manual(values = c('azure4', 'white')) +
  geom_smooth(method = "glm", se = T, fullrange = T, size = 1.0, col = "darkslategray",
              aes(lty = Group)) +
  labs(x = "Worthless gift frequencies", y = "Silk wrapping duration (min)") +
  theme(legend.text = element_text(size = 15), legend.position = "right",
        legend.title = element_text(size = 16),
        plot.margin = margin(1, 0.5, 0.5, 0.5, "cm"))+
  scale_x_continuous("Worthless gift frequencies", labels = as.numeric(dlabwd$W.gifts.prop), breaks = dlabwd$W.gifts.prop) +
  my.theme 

wn <- dlabwd %>% # Number of rapping bouts vs Group*W.gifts.prop
  mutate(Location = fct_relevel(Location, "South", "Center-South", "Center-North", "North")) %>%
  ggplot(aes(x = W.gifts.prop, y = WrapNum)) + 
  geom_jitter(width = 0.01, shape = 21, size = 2, aes(col = Location, fill = Group)) +
  scale_fill_manual(values = c('azure4', 'white')) +
  geom_smooth(method = "glm", se = T, fullrange = T, linewidth = 1.0, col = "darkslategray",
              aes(lty=Group)) +
  labs(x = "Worthless gift frequencies", y = "Number of silk wrapping bouts") +
  theme(legend.text = element_text(size = 15), legend.position = "right",
        legend.title = element_text(size = 16),
        plot.margin = margin(1, 0.5, 0.5, 0.5, "cm"))+
  scale_x_continuous("Worthless gift frequencies", labels = as.numeric(dlabwd$W.gifts.prop), breaks = dlabwd$W.gifts.prop) +
  my.theme 

dlabmd <- subset(dlab, InsDur != "NA")
md <- dlabmd %>% # Mating duration vs Group*W.gifts.prop
  mutate(Location = fct_relevel(Location, "South", "Center-South", "Center-North", "North")) %>%
  ggplot(aes(x = W.gifts.prop, y = InsDur)) + 
  geom_jitter(width = 0.01, shape = 21, size = 2, aes(col = Location, fill = Group)) +
  scale_fill_manual(values = c('azure4', 'white')) +
  geom_smooth(method = "glm", se = T, fullrange = T, linewidth = 1.0, col = "darkslategray",
              aes(lty = Group)) +
  labs(x = "Worthless gift frequencies", y = "Mating duration (min)") +
  theme(legend.text = element_text(size = 15), legend.position = "right",
        legend.title = element_text(size = 16),
        plot.margin = margin(0, 0.5, 0.5, 0.5, "cm"))+
  scale_x_continuous("Worthless gift frequencies", labels = as.numeric(dlabwd$W.gifts.prop), breaks = dlabwd$W.gifts.prop) +
  my.theme 

dlabni <- subset(dlab, InsNum != "NA")
ni <- dlabni %>% # Number of insertions vs Group*W.gifts.prop
  mutate(Location = fct_relevel(Location, "South", "Center-South", "Center-North", "North")) %>%
  ggplot(aes(x = W.gifts.prop, y = InsNum)) + 
  geom_jitter(width = 0.01, shape = 21, size = 2, aes(col = Location, fill = Group))+
  scale_fill_manual(values = c('azure4', 'white')) +
  geom_smooth(method = "glm", se = T, fullrange = T, linewidth = 1.0, col = "darkslategray",
              aes(lty = Group)) +
  labs(x = "Worthless gift frequencies", y = "Number of insertions") +
  theme(legend.text = element_text(size = 15), legend.position = "right",
        legend.title = element_text(size = 16),
        plot.margin = margin(0, 0.5, 0.5, 0.5, "cm"))+
  scale_x_continuous("Worthless gift frequencies", labels = as.numeric(dlabwd$W.gifts.prop), breaks = dlabwd$W.gifts.prop) +
  my.theme 

ggarrange(wd + rremove("x.title"), wn + rremove("x.title"), md, ni,  
          labels = c("A", "B", "C", "D"),
          font.label = list(size=23),
          align = "v",
          ncol = 2, nrow = 2,
          common.legend = TRUE,
          legend = c("right"))

