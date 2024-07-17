require(GGally)
require(fitdistrplus)
library(lme4)
library(lmerTest)
require(sjPlot)
require(DHARMa)
library(multcomp)
require(ggplot2)
library(dplyr)
library(forcats)
library(AICcmodavg)
library(ggpubr)

# Field data per sampling date
data <- read.csv("Field.data.PavonPelaez.etal.csv", header=T, sep=";", dec=".")
# Field data per Individuals
datai <- read.csv("Field.indiv.data.PavonPelaez.etal.csv", sep=";", dec=".")

data$Population <- as.factor(data$Population)
data$Location <- as.factor(data$Location)
data$Date <- as.factor(data$Date)
data$Month <- as.factor(data$Month)

data$YearMonth <- interaction(data$Date, data$Month)


# Exploratory Analyses
# found no relationship between worthless gift frequencies 
# and the annual mean or variation of temperature, or annual mean precipitation

# We explored field data from the four locations (South, Center-South, Center-North, North) 
# by performing Generalized Linear Mixed Models (GLMMs) with Binomial distribution for worthless gift frequencies. 
# We additionally performed Linear Mixed Models (LMMs) with Gaussian distribution for male size and prey availability 
# (both variables logarithm transformed). We included the combination of month (September, October, November, December) 
# and year (2015, 2016, 2021, 2022) as random effects in all models, as well as the population.

# We used GLMMs with Binomial distributions to assess the effect of the independent variables on the likelihood 
# of worthless gifts (all variables scaled and not strongly correlated). 
# We used the bias-corrected version of the Akaike Information Criterion (AICc) in a model selection approach. 
# Our set of candidate models included 11 models with the frequency of worthless gifts as the response variable, 
# and every possible combination between the fixed effects of precipitation variation, prey number, male size, 
# as well as single two-way interactions between each pair of these variables, as fixed effects. 
# We included the combination of month (September, October, November, December) and year (2015, 2016, 2021, 2022) 
# as random effects in all models, as well as the population.  

#=========================== EXPLORATORY =======================================
#===============================================================================
# Location and ecological variables
ggpairs(data[,c(7,8,9,10,20,24,26,27)], mapping = aes(color = Location))+theme_bw()
# Location, latitude and climatic variables
ggpairs(data[,c(2,3,4,5,6,7,8,9,26)], mapping = aes(color = Location))+theme_bw()


# ============================== PREY ==============================
# ==================================================================

norm <- fitdist(data$Prey/10,"norm")
lnorm <- fitdist(data$Prey/10,"lnorm")
gamma <- fitdist(data$Prey/10, "gamma")

par(mfrow=c(1,2), cex.lab=1.5, cex.axis=1.3, mai=c(1,1,0.2,0.2))
cdfcomp(list(norm, lnorm, gamma),legendtext=c("normal","lognormal", "gamma"), 
        xlogscale=T, ylogscale=T, main="")
qqcomp(list(norm,lnorm, gamma),legendtext=c("normal","lognormal", "gamma"), main="")

par(mfrow=c(1,1), cex.lab=1, cex.axis=1) 
gofstat(list(norm,lnorm, gamma))$aic # LogNorm

log.prey.model <- lmer(log(Prey) ~ Location +
                           (1|YearMonth) + (1|Population), data = data)
summary(log.prey.model)

#Random effects:
#Groups     Name        Variance  Std.Dev. 
#YearMonth  (Intercept) 1.083e+00 1.041e+00
#Population (Intercept) 4.190e-10 2.047e-05
#Residual               2.980e+00 1.726e+00
#Number of obs: 46, groups:  YearMonth, 14; Population, 6

#Fixed effects:
#                     Estimate Std. Error      df t value Pr(>|t|)    
#(Intercept)            4.8995     0.5952 22.3971   8.231 3.21e-08 ***
#LocationCenter-South   0.6245     0.7208 37.3400   0.866    0.392    
#LocationNorth          0.4692     0.9104 31.4113   0.515    0.610    
#LocationSouth          0.1622     0.9000 38.5027   0.180    0.858


# ============================== SIZE ==============================
# ==================================================================

datai$Population <- as.factor(datai$Population)
datai$Location <- as.factor(datai$Location)
datai$Date <- as.factor(datai$Date)
datai$Month <- as.factor(datai$Month)

datai$YearMonth <- interaction(datai$Date, datai$Month)

dataz <- subset(datai, Male.size != "NA")

norm <- fitdist(dataz$Male.size,"norm")
lnorm <- fitdist(dataz$Male.size,"lnorm")
gamma <- fitdist(dataz$Male.size, "gamma")
par(mfrow=c(1,2), cex.lab=1.5, cex.axis=1.3, mai=c(1,1,0.2,0.2))
cdfcomp(list(norm, lnorm, gamma),legendtext=c("normal","lognormal", "gamma"), 
        xlogscale=T, ylogscale=T, main="")
qqcomp(list(norm,lnorm, gamma),legendtext=c("normal","lognormal", "gamma"), main="")
par(mfrow=c(1,1), cex.lab=1, cex.axis=1) 
gofstat(list(norm,lnorm, gamma))$aic # LogNorm

log.size.model <- lmer(log(Male.size) ~ Location +
                           (1|YearMonth) + (1|Population), data = datai)
summary(log.size.model)
#Random effects:
#Groups     Name        Variance  Std.Dev.
#YearMonth  (Intercept) 0.0078310 0.08849 
#Population (Intercept) 0.0009978 0.03159 
#Residual               0.0080812 0.08990 
#Number of obs: 1319, groups:  YearMonth, 14; Population, 6

#Fixed effects:
#                       Estimate Std. Error       df t value Pr(>|t|)    
#  (Intercept)           1.44836    0.03322  7.62380  43.595 2.02e-10 ***
#  LocationCenter-South -0.14758    0.03279  2.16858  -4.501   0.0395 *  
#  LocationNorth         0.09376    0.04008  2.15231   2.339   0.1353    
#  LocationSouth        -0.05686    0.04049  2.24067  -1.404   0.2828 


# ========================= WORTHLESS GIFT =========================
# ==================================================================

# Excluding dates with less than 3 gifts collected
datawg <- subset(data, Gifts > 2)

norm <- fitdist(datawg$W.gifts.prop,"norm")
lnorm <- fitdist(datawg$W.gifts.prop,"lnorm")
gamma <- fitdist(datawg$W.gifts.prop, "gamma")
par(mfrow = c(1,2), cex.lab=1.5, cex.axis=1.3, mai=c(1,1,0.2,0.2))
cdfcomp(list(norm, lnorm, gamma),legendtext=c("normal","lognormal", "gamma"), 
        xlogscale=T, ylogscale=T, main="")
qqcomp(list(norm,lnorm, gamma),legendtext=c("normal","lognormal", "gamma"), main="")
par(mfrow=c(1,1), cex.lab=1, cex.axis=1) 
gofstat(list(norm,lnorm, gamma))$aic # Norm & Gamma

w.gift.model.1 <- glmer(cbind(Worthless.gifts, (Gifts - Worthless.gifts)) ~ Location + 
                          (1|YearMonth) + (1|Population), 
                        family=binomial, data = datawg)
summary(w.gift.model.1)
#Random effects:
#Groups     Name        Variance Std.Dev.
#YearMonth  (Intercept) 0.1311   0.3621  
#Population (Intercept) 0.1236   0.3515  
#Number of obs: 38, groups:  YearMonth, 13; Population, 6

#Fixed effects:
#                       Estimate Std. Error z value Pr(>|z|)    
#  (Intercept)           -0.6157     0.3347  -1.839 0.065849 .  
#  LocationCenter-South   1.5127     0.4253   3.556 0.000376 ***
#  LocationNorth          1.4966     0.6673   2.243 0.024920 *  
#  LocationSouth          0.1045     0.5726   0.182 0.855254 


#===== WG vs. climatic variables ================

## re-scaling variables.
data2 <- datawg
data2$Precip.mean <- scale(datawg$Precip.mean)
data2$Precip.var <- scale(datawg$Precip.var)
data2$Temp.mean <- scale(datawg$Temp.mean)
data2$Temp.var <- scale(datawg$Temp.var)

w.gift.model.2 <- glmer(cbind(Worthless.gifts, (Gifts - Worthless.gifts)) ~ 
                          Precip.mean + Precip.var + Temp.mean + Temp.var + 
                          (1|YearMonth), 
                        family=binomial, data = data2)
summary(w.gift.model.2)
#Random effects:
#  Groups    Name        Variance Std.Dev.
#YearMonth (Intercept) 0.1144   0.3382  
#Number of obs: 38, groups:  YearMonth, 13

#Fixed effects:
#                Estimate Std. Error z value Pr(>|z|)  
#(Intercept)       0.1364     0.1546   0.882   0.3776  
#Precip.mean      -0.7052     0.5784  -1.219   0.2227  
#Precip.var        1.2592     0.5677   2.218   0.0266 *
#Temp.mean        -0.3080     0.8756  -0.352   0.7250  
#Temp.var         -0.5254     0.3203  -1.641   0.1009 


#====================================================================
#### ====== WORTHLESS GIFT PROBABILITY =============================

# fixed effects
ggpairs(datawg[,c(3, 7, 10, 27)], mapping = aes(color = Location))+theme_bw()

## re-scaling all variables.
data2 <- datawg
data2$Male.size <- scale(datawg$Male.size)
data2$Prey <- scale(datawg$Prey)
data2$Precip.var <- scale(datawg$Precip.var)

model.gift <- glmer(cbind(Worthless.gifts, (Gifts - Worthless.gifts)) ~
                      Prey + Male.size + Precip.var + (1|YearMonth) + (1|Population),
                    data = data2, family = binomial)
summary(model.gift)

giftmodel.N <- glmer(cbind(Worthless.gifts, (Gifts - Worthless.gifts)) ~ 1 +
                       (1|YearMonth) + (1|Population), data = data2, family = binomial) # Null model
giftmodel.1prey <- update(giftmodel.N, . ~ . + Prey) # Only prey  
giftmodel.1M.size <- update(giftmodel.N, . ~ . + Male.size) # Only male size
giftmodel.1var.precip <- update(giftmodel.N, . ~ . + Precip.var) # Only var(precip)

giftmodel.2.size.prey <- update(giftmodel.1M.size, . ~ . + Prey) # prey and male size 
giftmodel.2.size.var.precip <- update(giftmodel.1M.size, . ~ . + Precip.var) #  male size and var(precip)
giftmodel.2.prey.var.precip <- update(giftmodel.1prey, . ~ . + Precip.var) # prey and var(precip) 

giftmodel.3.var.precip.prey.Msize <- update(giftmodel.2.size.prey, . ~ . + Precip.var) # 3 single effects 

giftmodel.2.size.prey.int <- update(giftmodel.2.size.prey, . ~ . + Prey:Male.size) # 
giftmodel.2.size.var.precip.int <- update(giftmodel.2.size.var.precip, . ~ . + Male.size:Precip.var) # 
giftmodel.2.prey.var.precip.int <- update(giftmodel.2.prey.var.precip, . ~ . + Precip.var:Prey) # 

candidate.models <- list(giftmodel.N, giftmodel.1prey, giftmodel.1M.size, giftmodel.1var.precip, 
                         giftmodel.2.size.prey, giftmodel.2.size.var.precip, giftmodel.2.prey.var.precip,
                         giftmodel.3.var.precip.prey.Msize, 
                         giftmodel.2.size.prey.int, giftmodel.2.size.var.precip.int, giftmodel.2.prey.var.precip.int)

candidate.names <- c("Null", "Prey", "M.size", "var(Precip)", 
        "Prey + M.size", "var(Precip) + M.size", "var(Precip) + Prey",
        "var(Precip) + Prey + M.size", 
        "Prey + M.size + (Prey*M.size)", "var(Precip) + M.size + (var(Precip)*M.size)", "var(Precip) + Prey + (var(Precip)*Prey)")
mod.table.results <- aictab(cand.set = candidate.models, modnames = candidate.names)
write.csv(mod.table.results, file = "Model selection RESULTS.csv")

# ==== First model
summary(giftmodel.1var.precip)
#Random effects:
#Groups     Name        Variance Std.Dev.
#YearMonth  (Intercept) 0.32118  0.5667  
#Population (Intercept) 0.09238  0.3039  
#Number of obs: 38, groups:  YearMonth, 13; Population, 6

#Fixed effects:
#               Estimate Std. Error z value Pr(>|z|)    
#(Intercept)      0.1926     0.2382   0.808    0.419    
#Precip.var       0.7784     0.1719   4.528 5.95e-06 ***


# ==== Second model
summary(giftmodel.2.prey.var.precip)
#Random effects:
#Groups     Name        Variance Std.Dev.
#YearMonth  (Intercept) 0.07153  0.2674  
#Population (Intercept) 0.28760  0.5363  
#Number of obs: 38, groups:  YearMonth, 13; Population, 6

#Fixed effects:
#               Estimate Std. Error z value Pr(>|z|)   
#(Intercept)      0.1424     0.2626   0.542  0.58751   
#Prey            -0.2042     0.1211  -1.686  0.09171 . 
#Precip.var       0.6925     0.2515   2.754  0.00589 **


# ==== Third model
summary(giftmodel.2.size.var.precip)
#Random effects:
#Groups     Name        Variance Std.Dev.
#YearMonth  (Intercept) 0.2463   0.4963  
#Population (Intercept) 0.1414   0.3760  
#Number of obs: 38, groups:  YearMonth, 13; Population, 6

#Fixed effects:
#               Estimate Std. Error z value Pr(>|z|)    
#(Intercept)      0.2057     0.2442   0.842    0.400    
#Male.size        0.1422     0.1544   0.921    0.357    
#Precip.var       0.8203     0.2013   4.075  4.6e-05 ***


my.theme=theme(axis.title=element_text(size=15),
               axis.text= element_text(size=10),
               panel.border=element_rect(fill=NA,linewidth=0.5,color="black"),
               panel.background = element_rect(fill=NA, color="white"))

  ggplot(datawg, aes(x=Precip.var, y=W.gifts.prop)) + 
    geom_jitter(width = 0.01, size = 2, aes(col=Location))+
    geom_smooth(method = "glm", se = T, fullrange = T, linewidth = 1.0, col = "darkslategray") +
    labs(x="var(Precipitations)",y="Worthless gifts proportion") +
    theme(legend.text = element_text(size = 13), legend.position = "right",
          legend.title = element_text(size = 14)) +
    my.theme 
  

# ========================== Figures ========================= 
# ==================== Climatic variables ====================

# Subset only one obs/population
dfig<- subset(data, Sample == 1)

my.theme=theme(axis.title=element_text(size=17),
               axis.title.y = element_text(face= "italic"),
               axis.text= element_text(size=15),
               panel.border=element_rect(fill=NA,linewidth=0.5,color="black"),
               panel.background = element_rect(fill=NA, color="white"))


mp <- dfig %>%
  mutate(Location = fct_relevel(Location, "South", "Center-South", "Center-North", "North")) %>%
  ggplot(aes(x=Latitude, y=Precip.mean)) + 
  geom_jitter(width=0.1, size=3.0,aes(col=Location))+
  geom_smooth(method="glm",se=T, fullrange=T, size=1.0, col="darkgrey")+
  labs(x="Latitude",y="Mean precipitation (mm/day)")+
  theme(legend.text=element_text(size = 13),legend.position="right",
        legend.title = element_text(size=14))+
  my.theme 

vp <- dfig %>%
  mutate(Location = fct_relevel(Location, "South", "Center-South", "Center-North", "North")) %>%
  ggplot(aes(x=Latitude, y=Precip.var)) + 
  geom_jitter(width=0.1, size=3.0,aes(col=Location))+
  geom_smooth(method="glm",se=T, fullrange=T, linetype=0)+
  labs(x="Latitude",y="Precip. variability (mm/day)")+
  theme(legend.text=element_text(size = 13),legend.position="right",
        legend.title = element_text(size=14))+
  my.theme

mt <- dfig %>%
  mutate(Location = fct_relevel(Location, "South", "Center-South", "Center-North", "North")) %>%
  ggplot(aes(x=Latitude, y=Temp.mean)) + 
  geom_jitter(width=0.1, size=3.0,aes(col=Location))+
  geom_smooth(method="glm",se=T, fullrange=T, size=1.0, col="darkgrey")+
  labs(x="Latitude",y="Mean temperature (C)")+
  theme(legend.text=element_text(size = 13),legend.position="right",
        legend.title = element_text(size=14))+
  my.theme 

vt <- dfig %>%
  mutate(Location = fct_relevel(Location, "South", "Center-South", "Center-North", "North")) %>%
  ggplot(aes(x=Latitude, y=Temp.var)) + 
  geom_jitter(width=0.1, size=3.0,aes(col=Location))+
  geom_smooth(method="glm",se=T, fullrange=T, size=1.0, col="darkgrey")+
  labs(x="Latitude",y="Temp. variability (C)")+
  theme(legend.text=element_text(size = 13),legend.position="right",
        legend.title = element_text(size=14))+
  my.theme 


ggarrange(mt+ rremove("x.title"), mp+ rremove("x.title"), vt, vp,   
          labels = c("B", "C", "D", "E"),
          font.label = list(size=18),
          align = "v",
          ncol = 2, nrow = 2,
          legend = "none")


# ======================== Figures ======================== 
# ==================== Field variables ====================

my.theme=theme(axis.title=element_text(size=23),
               axis.title.y = element_text(face= "italic"),
               axis.text= element_text(size=16),
               panel.border=element_rect(fill=NA,linewidth=0.5,color="black"),
               panel.background = element_rect(fill=NA, color="white"))


# Subset only one obs/population
dfig<- subset(data, Sample == 1)

pv <- dfig %>%
  mutate(Location = fct_relevel(Location, "South", "Center-South", "Center-North", "North")) %>%
  ggplot( aes(x=Location, y=Precip.var)) + 
  geom_boxplot(size=1,aes(col=Location), outlier.shape = NA)+ 
  geom_jitter(width=0.01, shape=21, col="black", size=1.5,aes(fill=Location))+
  labs(x="Location",y="Precipitation variation (mm/day)")+
  theme(legend.text=element_text(size = 13),legend.position="right",
        legend.title = element_text(size=14),
        plot.margin = margin(1, 0.5, 1, 0.5, "cm"))+
  my.theme

pa <- data %>%
  mutate(Location = fct_relevel(Location, "South", "Center-South", "Center-North", "North")) %>%
  ggplot( aes(x=Location, y=log(Prey))) + 
  geom_boxplot(size=1,aes(col=Location), outlier.shape = NA)+ 
  geom_jitter(width=0.01, shape=21, col="black", size=1.5,aes(fill=Location))+ 
  labs(x="Location",y="log(Prey availability)")+
  theme(legend.text=element_text(size = 13),legend.position="right",
        legend.title = element_text(size=14),
        plot.margin = margin(0, 0.5, 0.5, 0.5, "cm"))+
  my.theme

datawg <- subset(data, Gifts > 2)
wf <- datawg %>%
  mutate(Location = fct_relevel(Location, "South", "Center-South", "Center-North", "North")) %>%
  ggplot( aes(x=Location, y=W.gifts.prop)) + 
  geom_boxplot(size=1,aes(col=Location), outlier.shape = NA)+ 
  geom_jitter(width=0.01, shape=21, col="black", size=1.5,aes(fill=Location))+ 
  labs(x="Location",y="Worthless gifts proportions")+
  theme(legend.text=element_text(size = 13),legend.position="right",
        legend.title = element_text(size=14),
        plot.margin = margin(1, 0.5, 1, 0.5, "cm"))+
  my.theme

# Individuals' data
datai$Population <- as.factor(datai$Population)
datai$Location <- as.factor(datai$Location)
datai$Date <- as.factor(datai$Date)
datai$Month <- as.factor(datai$Month)

dataz <- subset(datai, Male.size != "NA")

ms <- dataz %>%
  mutate(Location = fct_relevel(Location, "South", "Center-South", "Center-North", "North")) %>%
  ggplot( aes(x=Location, y=Male.size)) + 
  geom_boxplot(size=1,aes(col=Location), outlier.shape = NA)+ 
  geom_jitter(width=0.1, shape=21, col="black", size=1.5,aes(fill=Location))+
  labs(x="Location",y="Male size (mm)")+
  theme(legend.text=element_text(size = 13),legend.position="right",
        legend.title = element_text(size=14),
        plot.margin = margin(0, 0.5, 0.5, 0.5, "cm"))+
  my.theme


# without legend:
ggarrange(wf+ rremove("x.title")+ rremove("x.text"), pv+ rremove("x.title")+ rremove("x.text"), pa, ms,  
          labels = c("A", "B", "C", "D"),
          font.label = list(size=23),
          align = "v",
          ncol = 2, nrow = 2,
          legend = "none")

