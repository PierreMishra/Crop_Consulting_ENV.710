library(dplyr)
library(ggplot2)

#### Importing cotton data
cottondata<-read.csv("cotton2019.csv", header=TRUE)
table(cottondata$treatmentA)
table(cottondata$treatmentB)
colnames(cottondata)

#### Boxplot
ggplot(data = cottondata, aes(y = cottondata$cotton.kg)) +
  geom_boxplot(col="black", fill="cyan", alpha = 1) +
  labs(title="Distribution of cotton yields (n=270)", y = "Cotton yield (kg/acre)") +
  theme_bw() +
  ylim(c(0, 2000))

#### Boxplot for logged data

ggplot(data = cottondata, aes(y = cottondata$cotton.kg)) +
  geom_boxplot(col="black", fill="cyan", alpha = 1) +
  labs(title = "Distribution of logged cotton yields (n=270)", y = "Cotton yield (kg/acre)") +
  theme_bw() +
  coord_trans(y = "log10", limy = c(1, 10000))

#### Logged cotton yield looks better in terms of normality. Lets check using histogram

#### Histogram
ggplot(data=cottondata, aes(x=cottondata$cotton.kg)) +
  geom_histogram(col="black", fill="cyan", alpha = 1) +
  labs(title= "Distribution of cotton yield (n=270)", y = "Count", x = "Cotton yield (kg/acre)") + 
  theme_bw()

#### Histogram for logged data
cottondata$cotton.kg.log<-log(cottondata$cotton.kg)
ggplot(data = cottondata, aes(x = cottondata$cotton.kg.log)) +
  geom_histogram(col="black", fill="cyan", alpha = 1) +
  labs(title= "Distribution of logged cotton yield (n=270)", y = "Count", x = "Log cotton yield (kg/acre)") +
  theme_bw() 

#### Unlogged cotton yield is positively skewed while logged yield is closer to normal distribution
#### Conduting shapiro wilk test of normailty. Null hypothesis is that data are normally distributed. P-value of less than 0.05 means that we can reject null hypothesis

  shapiro.test(cottondata$cotton.kg.log)

### Our data is not normal because p-value is less than 0.05

####################################################################
####################################################################
####                                                            ####
####            One-way ANOVA on management practices           ####
####                                                            ####
####################################################################
####################################################################


#### Ho: No difference in cotton yield across management practices
#### Ha: Atleast 1 mean cotton yeild for a managment practice is different then that of the other

#### Making factor variables for management practices
cottondata$treatmentA<-as.factor(cottondata$treatmentA)

levels(cottondata$treatmentA)[levels(cottondata$treatmentA)==1] <-"Organic"
levels(cottondata$treatmentA)[levels(cottondata$treatmentA)==2] <-"Conventional"
levels(cottondata$treatmentA)[levels(cottondata$treatmentA)==3] <-"Integrated Pest Management"

#### Boxplot
ggplot(data = cottondata, aes(x = cottondata$treatmentA,
                                   y = cottondata$cotton.kg.log, fill=cottondata$treatmentA)) +
  geom_boxplot() + 
  theme_bw() +
  labs(title="Logged cotton yield (kg/acre) by management practices", x = "Management practices", y = "Log cotton yield (kg/acre)") + scale_fill_discrete(name="Management practices")
  coord_trans(y = "log10")

#### Conducting one way Anova test

management.one.way <- aov(data = cottondata, formula = cotton.kg.log ~ treatmentA)
summary(management.one.way)

#### F-value = 48.59
#### p-value = <2e-16
#### Since put p-value is less than 0.05, we reject null hypothesis. There is a difference of cotton yield among management practices

#### Checking for normality within each group

table(cottondata$treatmentA)

organic<-filter(cottondata, treatmentA=="Organic")
conventional<-filter(cottondata, treatmentA=="Conventional")
ipm<-filter(cottondata, treatmentA=="Integrated Pest Management")

shapiro.test(organic$cotton.kg.log)
shapiro.test(conventional$cotton.kg.log)
shapiro.test(ipm$cotton.kg.log)

#### Except the organic group, both groups are not normal

#### Test for equal variance
bartlett.test(cottondata$cotton.kg.log, cottondata$treatmentA)

#### Due to a very tiny p-value, we reject null hypothesis. The data does not have equal variance\

#### Because we do not meet the assumptions of equal variances among groups and normal distribution, we will conduct a non-parametric test
kruskal.test(cotton.kg.log ~ treatmentA, data=cottondata)
#### p-value is less than 0.05 which means that there is a significant difference in cotton yield of each mangement practice among each other

#### Conducting post-hoc test
TukeyHSD(management.one.way)
##### there is a significant difference of log cotton yield between organic and both
##### other orders, but no significant different between conventional and integrated pest management practices (p>0.05)

#### Backtransforming logged data
exp(0.8777198) #Conventional-Organic 
exp(1.0857604) #Integrated Pest Management-Organic 
exp(0.2080406) #Integrated Pest Management-Conventional


####################################################################
####################################################################
####                                                            ####
####            One-way ANOVA on fertilizer practices           ####
####                                                            ####
####################################################################
####################################################################

#### Ho: No difference in cotton yield across fertilizer quantity
#### Ha: Atleast 1 mean cotton yeild for a fertilizer quantity is different than that of the other

#### Making factor variables for management practices
cottondata$treatmentB<-as.factor(cottondata$treatmentB)


levels(cottondata$treatmentB)[levels(cottondata$treatmentB)==1] <-"50 lbs/acre"
levels(cottondata$treatmentB)[levels(cottondata$treatmentB)==2] <-"75 lbs/acre"
levels(cottondata$treatmentB)[levels(cottondata$treatmentB)==3] <-"100 lbs/acre"



#### Boxplot
ggplot(data = cottondata, aes(x = cottondata$treatmentB,
                              y = cottondata$cotton.kg.log, fill=cottondata$treatmentB)) +
  geom_boxplot() + 
  theme_bw() +
  labs(title="Logged cotton yield (kg/acre) by fertilizer application", x = "Fertilizer quantity", y = "Log cotton yield (kg/acre)") + scale_fill_discrete(name="Fertilizer quantity") +
  coord_trans(y = "log10")

#### Conducting one way Anova test

fertilizer.one.way <- aov(data = cottondata, formula = cotton.kg.log ~ treatmentB)
summary(fertilizer.one.way)

#### F-value = 5.982
#### p-value = 0.00287
#### Since put p-value is less than 0.05, we reject null hypothesis. There is a difference of cotton yield among fertilizer practices

#### Checking for normality within each group

table(cottondata$treatmentB)

f50<-filter(cottondata, treatmentB=="50 lbs/acre")
f75<-filter(cottondata, treatmentB=="75 lbs/acre")
f100<-filter(cottondata, treatmentB=="100 lbs/acre")

shapiro.test(f50$cotton.kg.log)
shapiro.test(f75$cotton.kg.log)
shapiro.test(f100$cotton.kg.log)


#### None of the groups are normal

#### Test for equal variance
bartlett.test(cottondata$cotton.kg.log, cottondata$treatmentB)

#### Due to a very tiny p-value, we reject null hypothesis. The data does not have equal variance

#### Because we do not meet the assumptions of equal variances among groups and normal distribution, we will conduct a non-parametric test
kruskal.test(cotton.kg.log ~ treatmentB, data=cottondata)
#### p-value is less than 0.05 which means that there is a significant difference in cotton yield of each mangement practice among each other


#### Conducting post-hoc test
TukeyHSD(fertilizer.one.way)
##### there is a significant difference of log cotton yield between organic and both
##### other orders, but no significant different between conventional and integrated pest management practices (p>0.05)

#### Backtransforming logged data
exp(-0.01487382) #75-50
exp(0.39249556) #100-50
exp(0.40736938) #100-75

####################################################################
####################################################################
####                                                            ####
####            Two-way ANOVA on fertilizer practices           ####
####                                                            ####
####################################################################
####################################################################

#### Displaying observations for each combination of variables
table(cottondata$treatmentA, cottondata$treatmentB)

#### plot to visualize:
ggplot(data = cottondata, 
       aes(x = cottondata$treatmentA, 
           y = cottondata$cotton.kg.log,
           fill = cottondata$treatmentB)) +
  geom_boxplot() +
  coord_trans(y = "log10") +
  theme_bw() +
  labs(title = "Logged cotton yield (kg/acre) for different management and fertilizer practices", x = "Management practices", y = "Log cotton yield (kg/acre)")+
  scale_fill_discrete(name = "Fertilizer quantity")

#### Performing two-way ANOVA
two.way <- aov(cottondata$cotton.kg.log ~ cottondata$treatmentA + cottondata$treatmentB)
summary(two.way)

#### Post-hoc
TukeyHSD(two.way)

#### Backtransforming to get the multiplicative factor
exp(0.8777198) # conven-org
exp(1.0857604) # ipm-org
exp(0.2080406) # ipm-conventional
exp(-0.01487382) # 75-50
exp(0.39249556) #100-50
exp(0.40736938) #100-75

#### Performing two-way to see the effect of interaction
two.way.interaction<-aov(data = cottondata, cotton.kg.log ~ treatmentA + treatmentB + treatmentA*treatmentB)
summary(two.way.interaction) 

#### Post hoc
TukeyHSD(two.way.interaction)

#### Backtransforming comparisons with significant differences
exp(1.0897) #conv50-org50.............2.97
exp(1.457)  #ipm50-org50..............4.29
exp(1.3855)  #conv75-org50............3.99
exp(1.4066)  #ipm75-org50.............4.08
exp(1.2511)  #org100-org50............3.49
exp(1.1192)  #conv100-org50...........3.06
exp(1.3543)  #ipm100-org50............3.87
exp(1.3795) #conv50-org75.............3.97
exp(1.747)  #ipm50-org75..............5.74
exp(1.675) #conv75-org75..............5.34
exp(1.696) #ipm75-org75...............5.45
exp(1.540) #org100-org75..............4.66
exp(1.408) #conv100-org75.............4.09
exp(1.644) #ipm100-org75..............5.18

#### Summary statistics overall
summary(cottondata$cotton.kg)
sd(cottondata$cotton.kg)
max(cottondata$cotton.kg)-min(cottondata$cotton.kg)
IQR(cottondata$cotton.kg)

#### Summary statistics for cotton yield of organic farming
summary(organic$cotton.kg)
sd(organic$cotton.kg)
max(organic$cotton.kg)-min(organic$cotton.kg)
IQR(organic$cotton.kg)

#### Summary statistics for cotton yield of conventional farming
summary(conventional$cotton.kg)
sd(conventional$cotton.kg)
max(conventional$cotton.kg)-min(conventional$cotton.kg)
IQR(conventional$cotton.kg)

#### Summary statistics for cotton yield of integrated pest management
summary(ipm$cotton.kg)
sd(ipm$cotton.kg)
max(ipm$cotton.kg)-min(ipm$cotton.kg)
IQR(ipm$cotton.kg)

#### Summary statistics for cotton yield of fertilizer application of 50lbs N/acre
summary(f50$cotton.kg)
sd(f50$cotton.kg)
max(f50$cotton.kg)-min(f50$cotton.kg)
IQR(f50$cotton.kg)

#### Summary statistics for cotton yield of fertilizer application of 75lbs N/acre
summary(f75$cotton.kg)
sd(f75$cotton.kg)
max(f75$cotton.kg)-min(f75$cotton.kg)
IQR(f75$cotton.kg)

#### Summary statistics for cotton yield of fertilizer application of 100lbs N/acre
summary(f100$cotton.kg)
sd(f100$cotton.kg)
max(f100$cotton.kg)-min(f100$cotton.kg)
IQR(f100$cotton.kg)

