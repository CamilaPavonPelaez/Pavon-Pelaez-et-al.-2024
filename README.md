# Variability in precipitation weakens sexual selection for nuptial gifts in spiders  

Authors: Camila Pavón-Peláez, Vinicius Diniz, Williams Paredes-Munguía, Renato A. Texeira, Luiz Ernesto Costa-Schmidt, Adalberto J. Santos, Bruno A. Buzatto and Maria J. Albo  

Corresponding author: Maria J. Albo mjalbograna@gmail.com  

In this study we used a gift-giving spider species as model to evaluate the environmental effects over the males' sexual trait of producing nutritive nuptial gifts (fresh prey wrapped in silk) or the alternative mating tactic of worthless gifts (prey leftovers or plant parts wrapped in silk). We examined changes in males’ sexual trait and female choice among six populations living under different climatic conditions. We found that large variation in precipitation limits female choice, potentially favoring the spread of deceptive worthless gifts. In populations under highly variable conditions and with the highest frequencies of worthless gifts (70%), males offering such gifts acquire higher mating durations than those offering nutritive gifts. In contrast, in populations with less variable conditions and the lowest worthless gift frequencies (36%), females shortened mating duration to males offering worthless gifts.  

MJA conceived the idea and designed the field and laboratory work. CPP, VD, LECS, WPM and MJA performed field collections. CPP performed the behavioral experiments. CPP and BAB carried out the statistical analyses. CPP and MJA wrote the paper with the input from BAB, AS and RT. All authors critically revised all versions.  

## List of files:  

### Data file: Field.data.PavonPelaez.etal.csv  
This is a matrix of field and climatic data obtained from our first experimental design sampling six populations along the species' geographical distribution during the mating season. This matrix contains the summarized data collected from our field work, resulting in one observation per each sampling date (46 obs.). Therefore, the individual measurements of size and weights are averaged.  

Variables:  
- Population: Name of the population from which the data were collected.  
- Precip.mean: Annual mean precipitation (mm/day) calculated using the data obtained from NASA public information website for the area of each population ("location").  
- Precip.var: Annual variation in precipitation (mm/day) calculated using the data obtained from NASA public information website for the area of each population ("location").  
- Temp.mean: Annual mean temperature (°C) calculated using the data obtained from NASA public information website for the area of each population ("location").  
- Temp.var: Annual variation in temperature (°C) calculated using the data obtained from NASA public information website for the area of each population ("location").  
- Latitude: Decimal degrees of latitude.  
- Location: A factor with levels corresponding to the locations assigned to the populations in regards to their position along the latitudinal gradient of the species distribution. This includes the "North" location as the northern extreme of the distribution, "South" as the southern extreme, "Center-North" as the area located between the center and the northern extreme, and "Center-South" as the area located between the center and the southern extreme.  
- Date: Year in which the sampling was undertaken.  
- Month: Month when the sampling was undertaken.  
- Male.size: Mean of male body size measured as cephalothorax width (mm) for the males measured in each sampling date.  
- Male.weight: Mean male body mass (g) for the males measured in each sampling date.  
- GiftWeight: Mass of the entire gift collected (g).  
- Female.size: Mean female body size measured as cephalothorax width (mm) for the females measured in each sampling date.  
- Female.weight: Mean female body mass (g) for the females measured in each sampling date.  
- MalesWithGift: Number of adult males collected that were carrying gifts.  
- MalesWithoutGift: Number of adult males collected that were not carrying gifts.  
- MalesSub: Number of sub-adult males collected.  
- Females: Number of adult females collected.  
- FemalesSub: Number of sub-adult females collected.  
- FemalesEggsac: Number of adult females collected that were carrying an eggsac.  
- FemalesSpiderlings: Number of adult females collected that were carrying spiderlings.  
- Juveniles: Number of juveniles collected.  
- TotalSpiders: Total Number of spiders collected.  
- Gifts: Number of gifts collected of both gift types (Nutritive and Worthless).  
- Worthless.gifts: Number of worthless gifts collected.  
- W.gifts.prop: Proportion of worthless gifts calculated by dividing the number of worthless gifts by the total number of gifts.  
- Prey: Number of prey collected as a proxy for availability.  
- Sample: Only one data sample (observation) per population. This variable was created to produce figure 3 with climatic variables.  

### Data file: Field.indiv.data.PavonPelaez.etal.csv  
In order to perform the exploratory analysis of male size and its graphic representation we used this second matrix of field data including individuals' data as observations (1360 obs.).  

Variables:  
- Population: Name of the population from which the data were collected.  
- Latitude: Decimal degrees of latitude.  
- Location: A factor with levels corresponding to the locations assigned to the populations in regards to their position along the latitudinal gradient of the species distribution. This includes the "North" location as the northern extreme of the distribution, "South" as the southern extreme, "Center-North" as the area located between the center and the northern extreme, and "Center-South" as the area located between the center and the southern extreme.  
- Date: Year in which the sampling was undertaken.  
- Month: Month when the sampling was undertaken.  
- ID: Number identifying the individuals measured.  
- Male.size: Individual male body size measured as cephalothorax width (mm) for each male measured in each sampling date.  
- Male.weight: Individual male body mass (g) for each male measured in each sampling date.  
- GiftWeight: Weight of the gift (g) measured after collected.  
- GiftType: Type of gift collected (0=Nutritive gift/1=Worthless gift).  
- Female.size: Individual female body size measured as cephalothorax width (mm) for each female measured in each sampling date.  
- Female.weight: Individual female body mass (g) for each female measured in each sampling date.  

### Analysis script: Field.script.PavonPelaez.etal.R  
R script analyzing field and climatic data with the models described in the methods section titled "Deceptive worthless gifts across the species distribution range".  

### Data file: Lab.data.PavonPelaez.etal.csv  
This matrix of laboratory data was obtained from our second experimental design including the results of the behavioral responses of individual males (114 obs.).  

Variables:  
- Latitude: Decimal degrees of latitude.  
- Location: A factor with levels corresponding to the locations assigned to the populations in regards to their position along the latitudinal gradient of the species distribution. This includes the "North" location as the northern extreme of the distribution, "South" as the southern extreme, "Center-North" as the area located between the center and the northern extreme, and "Center-South" as the area located between the center and the southern extreme.  
- W.gifts.prop: Proportion of worthless gifts calculated by dividing the number of worthless gifts by the total number of gifts.  
- Precip.var: Annual variation in precipitation (mm/day) calculated using the data obtained from NASA public information website for the area of each population ("location").  
- Group: Experimental group (Nutritive/Worthless).  
- MaleID: Number identifying individual males.  
- Male.Size: Male body size measured as cephalothorax width (mm).  
- FemaleID: Number identifying individual females.  
- Female.Size: Female body size measured as cephalothorax width (mm).  
- WrapDur: Duration of the silk wrapping of the gift (min).  
- WrapNum: Number of silk wrapping bouts.  
- Mate: Mating access of the male (0=male did not mate/1=male mated).  
- InsDur: Sum of the duration of all pedipalp insertions (min).  
- InsNum: Number of pedipalp insertions.  

### Analysis script: Lab.script.PavonPelaez.etal.R  
R script analyzing laboratory data with the models described in the methods section titled "Silk wrapping and mating success of deceptive worthless gift across populations".  

## Versions of packages and software used.  
R version 3.6.3 (2020-02-29).  

Platform: x86_64-w64-mingw32/x64 (64-bit).  

Running under: Windows 10 x64 (build 17134).  

#### Attached base packages:  
- stats.  
- graphics.  
- grDevices.  
- utils.  
- datasets.  
- methods.  
- base.  

#### Other attached packages:  
 - DHARMa_0.2.7  
 - lmerTest_3.1-1  
 - lme4_1.1-21  
 - Matrix_1.2-18  
 - ggpubr_0.4.0  
 - forcats_0.5.0  
 - dplyr_1.0.6  
 - GGally_2.1.2  
 - ggplot2_3.3.5  
 - doBy_4.6.5  
 - multcomp_1.4-20  
 - TH.data_1.1-1  
 - mvtnorm_1.1-1  
 - fitdistrplus_1.0-14  
 - npsurv_0.4-0  
 - lsei_1.2-0  
 - survival_3.1-8  
 - MASS_7.3-51.5  

