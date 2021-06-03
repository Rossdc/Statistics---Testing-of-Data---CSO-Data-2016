Df <- read.csv("Merge_Data.csv", header = TRUE, sep = ",")
summary(Df)
colnames(Df)

library(dplyr)
library(tidyverse)
library(ggfortify)
library(tidyr)
library(ggplot2)
library(GGally)
library(hrbrthemes)
library(broom)
library(reshape2)
library(ggpubr)
library(dunn.test)


Df[!complete.cases(Df),]

##CHECK INSIGHTS SUBSETTING AGAINST EXCEL SPREADSHEET

##### INSIGHT 1 - Unemployment by Region & Gender #####

Insight2 <- Df[,c(461, 462, 464, 470,471,473,809)]

Insight2 <- Insight2 %>% rowwise %>% mutate(LAHF = T8_1_LAHFF+ T8_1_LAHFM,
                                            ULGUP =T8_1_ULGUPJF + T8_1_ULGUPJM,
                                            LFFJ = T8_1_LFFJM + T8_1_LFFJF)
                                            

Insight2Males <- Insight2[,c(1:3,7)]

Insight2Males <- Insight2Males %>% rowwise %>% rename(LAHF = T8_1_LAHFM,
                                                      ULGUP = T8_1_ULGUPJM,
                                                      LFFJ = T8_1_LFFJM,
                                                      Region = NUTS3NAME)

Insight2Females <- Insight2[,c(4:7)]

Insight2Females <- Insight2Females %>% rowwise %>% rename(LAHF = T8_1_LAHFF,
                                                          ULGUP = T8_1_ULGUPJF,
                                                          LFFJ = T8_1_LFFJF,
                                                          Region = NUTS3NAME)
Insight2Females$Gender <- "Female"
Insight2Males$Gender <- "Male"

Insight2spruced <- rbind(Insight2Females, Insight2Males)

Insight2spruced %>% group_by(Gender, Region) %>%
  summarise(across(
    .cols = is.numeric, 
    .fns = list(Mean = mean), na.rm = TRUE, 
    .names = "{col}_{fn}"
  ))

Insight2spruced %>% group_by(Gender, Region) %>%
  summarise(across(
    .cols = is.numeric, 
    .fns = list(Total = sum), na.rm = TRUE, 
    .names = "{col}_{fn}"
  ))


Insight2spruced %>% group_by(Gender, Region) %>%
  summarise(across(
    .cols = is.numeric, 
    .fns = list(Median = median), na.rm = TRUE, 
    .names = "{col}_{fn}"
  )) 

Insight2spruced %>% group_by(Gender, Region) %>%
  summarise(across(
    .cols = is.numeric, 
    .fns = list(SD = sd), na.rm = TRUE, 
    .names = "{col}_{fn}"
  )) 

Insight2_5 <- Insight2spruced %>% group_by(Gender, Region) %>%
  mutate(Total = rowSums(across(where(is.numeric))))

Insight2_5 <-Insight2_5 %>% group_by(Gender, Region) %>%
  summarise(across(everything(), sum))  

Insight2_5  %>% group_by(Region) %>%
  summarise(LFFJTot = sum(LFFJ),ULGUPTot = sum(ULGUP), LAHFTot = sum(LAHF))

Insight2_5  %>% group_by(Region) %>%
  summarise(LFFJTot = sum(LFFJ),ULGUPTot = sum(ULGUP), LAHFTot = sum(LAHF)) %>%
  mutate(Total = rowSums(across(where(is.numeric))))

Insight2_5  %>% group_by(Gender) %>%
  summarise(LFFJTot = sum(LFFJ),ULGUPTot = sum(ULGUP), LAHFTot = sum(LAHF)) %>%
  mutate(Total = rowSums(across(where(is.numeric))))

#Total Unemployment

melt181 <- melt(Insight2_5, id.vars = c("Region", "Gender"))

ggplot(melt181, aes(Region, value, fill = Gender, colour = Gender)) + geom_col(alpha = 0.3) + facet_wrap(~variable,scales='free') +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90)) +
  ggtitle("Unemployment by Gender across Regions") +
  theme(plot.title = element_text(hjust = 0.5))

#As there are clearly more stay at home mothers - we will exclude stay at home column from analysis

Insight2_5 <- select(Insight2_5, -c(LAHF))

#Stat Analysis

#Removing LAHF column
Insight2spruced <- select(Insight2spruced, -c(LAHF))

#Far More Men unemployed than Women
Insight2spruced %>% group_by(Gender) %>%
  summarise(LFFJTot = sum(LFFJ),ULGUPTot = sum(ULGUP)) %>%
  mutate(Total = rowSums(across(where(is.numeric))))

#Unemployment by region by gender

Insight2spruced$Region <- as.factor(Insight2spruced$Region)
Insight2spruced$Gender <- as.factor(Insight2spruced$Gender)

#Visualing Distribution of data

#DENSITY PLOTS LFFJ
ggplot(Insight2spruced, aes(LFFJ)) + geom_density(color = "black", fill = "lightblue", alpha = 0.4) + geom_vline(aes(xintercept = mean(LFFJ)), color = "#FC4E08", linetype = "dashed", size = 1) + 
  geom_vline(aes(xintercept = median(LFFJ)), color = "blue", linetype = 4, size = 1) +
  facet_wrap(~Region) +
  theme_bw() +
  ggtitle("Density Plot") +
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Looking for First Time Job")

#DENSITY PLOTS ULGUP
ggplot(Insight2spruced, aes(ULGUP)) + geom_density(color = "black", fill = "lightblue", alpha = 0.4) + geom_vline(aes(xintercept = mean(ULGUP)), color = "#FC4E08", linetype = "dashed", size = 1) + 
  geom_vline(aes(xintercept = median(ULGUP)), color = "blue", linetype = 4, size = 1) +
  facet_wrap(~Region) +
  theme_bw() +
  ggtitle("Density Plot") +
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Unemployed Having Lost or Given Up Previous Job")

ggplot(Insight2spruced, aes(LAHF)) + geom_density(color = "black", fill = "lightblue", alpha = 0.4) + geom_vline(aes(xintercept = mean(LAHF)), color = "#FC4E08", linetype = "dashed", size = 1) + 
  geom_vline(aes(xintercept = median(LAHF)), color = "blue", linetype = 4, size = 1) +
  facet_wrap(~Region) +
  theme_bw() +
  ggtitle("Density Plot") +
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Looking After Home and Family")

MTI1 <- select(Insight2spruced,-c("Gender"))
mtInsight2 = melt(MTI1, id.vars='Region')

#BOXPLOT
ggplot(mtInsight2, aes(variable, value)) + geom_boxplot(aes(group = variable, fill = variable)) + theme_bw()+
  stat_boxplot(geom ='errorbar') +
  facet_wrap(~Region) +
  coord_flip() +
  ggtitle("Unemployment Categories Across Regions") +
  theme(plot.title = element_text(hjust = 0.5))

#TEST FOR NORMALITY ON BOTH SETS OF DATA NEEDED HERE

#Pearson Chi Test - Non normal distribution

NT1 <- Insight2spruced %>% select(where(is.numeric))

sapply(NT1, pearson.test)

#Kruskal First, then Dunn Test

#Gender ~ LFFJ
kruskal.test(LFFJ ~ Gender, data = Insight2spruced)

#Region ~ LFFJ
kruskal.test(LFFJ ~ Region, data = Insight2spruced)

#Region ~ LAHF
kruskal.test(LAHF ~ Region, data = Insight2spruced)

#Gender ~ ULGUP
kruskal.test(ULGUP ~ Gender, data = Insight2spruced)

#Region ~ ULGUP

kruskal.test(ULGUP ~ Region, data = Insight2spruced)

##Dwass-Steel-Critchlow-Fligner Test by Region and by Gender (seperate tests)

library(PMCMRplus)

#Region ~ LFFJ
list1 <- dscfAllPairsTest(LFFJ ~ Region, data = Insight2spruced)

library(rcompanion)

list1 <- PMCMRTable(list1, reverse = TRUE, digits = 3)

list1[which(list1[,2]<0.05),]

#Region ~ ULGUP
list2 <- dscfAllPairsTest(ULGUP ~ Region, data = Insight2spruced)

list2 <- PMCMRTable(list2, reverse = TRUE, digits = 3)

list2[which(list2[,2]<0.05),]

#Region ~ LAHF
list3 <- dscfAllPairsTest(LAHF ~ Region, data = Insight2spruced)

list3 <- PMCMRTable(list3, reverse = TRUE, digits = 3)

list3[which(list3[,2]<0.05),]

###COMPARING UNEMPLOYMENT BY GENDER PER REGION GIVEN DIFFERING SAMPLE SIZES AND KRUSKAL TEST ON GENDER

#LFFJ

wilcox.test(Insight2Females$LFFJ[which(Insight2Females$Region == "Dublin")], Insight2Males$LFFJ[which(Insight2Males$Region == "Dublin")], method = "bonferroni", paired = FALSE)

wilcox.test(Insight2Females$LFFJ[which(Insight2Females$Region == "Border")], Insight2Males$LFFJ[which(Insight2Males$Region == "Border")], method = "bonferroni", paired = FALSE)

wilcox.test(Insight2Females$LFFJ[which(Insight2Females$Region == "Mid-East")], Insight2Males$LFFJ[which(Insight2Males$Region == "Mid-East")], method = "bonferroni", paired = FALSE)

wilcox.test(Insight2Females$LFFJ[which(Insight2Females$Region == "Mid-West")], Insight2Males$LFFJ[which(Insight2Males$Region == "Mid-West")], method = "bonferroni", paired = FALSE)

#No difference in those looking for first time job in the Midlands
wilcox.test(Insight2Females$LFFJ[which(Insight2Females$Region == "Midlands")], Insight2Males$LFFJ[which(Insight2Males$Region == "Midlands")], method = "bonferroni", paired = FALSE)

wilcox.test(Insight2Females$LFFJ[which(Insight2Females$Region == "South-East")], Insight2Males$LFFJ[which(Insight2Males$Region == "South-East")], method = "bonferroni", paired = FALSE)

wilcox.test(Insight2Females$LFFJ[which(Insight2Females$Region == "South-West")], Insight2Males$LFFJ[which(Insight2Males$Region == "South-West")], method = "bonferroni", paired = FALSE)

wilcox.test(Insight2Females$LFFJ[which(Insight2Females$Region == "West")], Insight2Males$LFFJ[which(Insight2Males$Region == "West")], method = "bonferroni", paired = FALSE)

#ULGUP

wilcox.test(Insight2Females$ULGUP[which(Insight2Females$Region == "Dublin")], Insight2Males$ULGUP[which(Insight2Males$Region == "Dublin")], method = "bonferroni", paired = FALSE)

wilcox.test(Insight2Females$ULGUP[which(Insight2Females$Region == "Border")], Insight2Males$ULGUP[which(Insight2Males$Region == "Border")], method = "bonferroni", paired = FALSE)

wilcox.test(Insight2Females$ULGUP[which(Insight2Females$Region == "Mid-East")], Insight2Males$ULGUP[which(Insight2Males$Region == "Mid-East")], method = "bonferroni", paired = FALSE)

wilcox.test(Insight2Females$ULGUP[which(Insight2Females$Region == "Mid-West")], Insight2Males$ULGUP[which(Insight2Males$Region == "Mid-West")], method = "bonferroni", paired = FALSE)

wilcox.test(Insight2Females$ULGUP[which(Insight2Females$Region == "Midlands")], Insight2Males$ULGUP[which(Insight2Males$Region == "Midlands")], method = "bonferroni", paired = FALSE)

wilcox.test(Insight2Females$ULGUP[which(Insight2Females$Region == "South-East")], Insight2Males$ULGUP[which(Insight2Males$Region == "South-East")], method = "bonferroni", paired = FALSE)

wilcox.test(Insight2Females$ULGUP[which(Insight2Females$Region == "South-West")], Insight2Males$ULGUP[which(Insight2Males$Region == "South-West")], method = "bonferroni", paired = FALSE)

wilcox.test(Insight2Females$ULGUP[which(Insight2Females$Region == "West")], Insight2Males$ULGUP[which(Insight2Males$Region == "West")], method = "bonferroni", paired = FALSE)

##### INSIGHT 2 & 3 - Transport - Leaving Early & commute time, and difference between county's and regions #####

Insight3 <- Df[,c(646:699, 809,811)]
colnames(Insight3)

#Commute Time
Insight3_1 <- Df[,c(692:697,699,809,811)]

Insight3_1 <- Insight3_1 %>%
  mutate(Tunder15mins = T11_3_D1,
         T15_30mins = T11_3_D2,
         T30_45_mins = T11_3_D3,
         T45_60mins = T11_3_D4,
         T60_90mins = T11_3_D5,
         T_90mins = T11_3_D6,
         Region = NUTS3NAME)

Insight3_1 <- Insight3_1 %>% select (-c(T11_3_D1, T11_3_D2, T11_3_D3, T11_3_D4, T11_3_D5, T11_3_D6, T11_3_T ))

group_by(Insight3_1, Region) %>%
  summarise(across(
    .cols = is.numeric, 
    .fns = list(Mean = mean), na.rm = TRUE, 
    .names = "{col}_{fn}"
  ))

group_by(Insight3_1, Region) %>%
  summarise(across(
    .cols = is.numeric, 
    .fns = list(Median = median), na.rm = TRUE, 
    .names = "{col}_{fn}"
  ))

group_by(Insight3_1, Region) %>%
  summarise(across(
    .cols = is.numeric, 
    .fns = list(SD = sd), na.rm = TRUE, 
    .names = "{col}_{fn}"
  ))


#Leaving Early

Insight3_2 <- Df[,c(682:689, 809,811)]

Insight3_2 <- Insight3_2 %>%
  mutate(b4_630 = T11_2_T1,
         T630_700 = T11_2_T2,
         T701_730 = T11_2_T3,
         T731_800 = T11_2_T4,
         T801_830 = T11_2_T5,
         T831_900 = T11_2_T6,
         T901_930 = T11_2_T7,
         T930aft = T11_2_T8,
         Region = NUTS3NAME)

Insight3_2 <- Insight3_2 %>% select (-c(COUNTYNAME, T11_2_T1, T11_2_T2, T11_2_T3, T11_2_T4, T11_2_T5, T11_2_T6, T11_2_T7, T11_2_T8 ))

print.data.frame(group_by(Insight3_2, Region) %>%
  summarise(across(
    .cols = is.numeric, 
    .fns = list(Mean = mean), na.rm = TRUE, 
    .names = "{col}_{fn}"
  )))

print.data.frame(group_by(Insight3_2, Region) %>%
  summarise(across(
    .cols = is.numeric, 
    .fns = list(Median = median), na.rm = TRUE, 
    .names = "{col}_{fn}"
  )))

print.data.frame(group_by(Insight3_2, Region) %>%
  summarise(across(
    .cols = is.numeric, 
    .fns = list(SD = sd), na.rm = TRUE, 
    .names = "{col}_{fn}"
  )))

Insight3_2 <- Insight3_2 %>% select(-c(Region))

#Combine Columns

Insight3 <- cbind(Insight3_1, Insight3_2)

Insight3 <- Insight3 %>% select(-c(NUTS3NAME))

InsightTravelTime <- Insight3 %>%
  select(c("Region", "Tunder15mins", "T15_30mins" ,"T30_45_mins", "T45_60mins", "T60_90mins", "T_90mins"))

fancy_scientific <- function(l) {
  # turn in to character string in scientific notation
  l <- format(l, scientific = TRUE)
  # quote the part before the exponent to keep all the digits
  l <- gsub("^(.*)e", "'\\1'e", l)
  # turn the 'e+' into plotmath format
  l <- gsub("e", "%*%10^", l)
  # return this as an expression
  parse(text=l)
}

mt3 = melt(InsightTravelTime, id.vars='Region')
ggplot(mt3) +
  geom_col(aes(variable, value, group=Region, fill = Region)) +
  facet_grid(~Region, scales="free_x") +
  theme(axis.text.x = element_text(angle=90)) +
  scale_y_continuous(labels = fancy_scientific) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90)) +
  ggtitle("Travel Time by Region") +
  theme(plot.title = element_text(hjust = 0.5)) + xlab("Travel Time") + ylab("count")

InsightLeavingTime <-  Insight3 %>%
  select(c("Region","b4_630", "T630_700", "T701_730", "T731_800", "T801_830", "T831_900", "T901_930", "T930aft"))

mymode <- function(x) {
  t <- table(x)
  names(t)[ which.max(t) ]
}

#Descriptive Stastics for travel time
InsightTravelTime %>% group_by(Region) %>%
  summarise(across(
    .cols = is.numeric, 
    .fns = list(Mean = mean), na.rm = TRUE, 
    .names = "{col}_{fn}"
  ))

InsightTravelTime %>% group_by(Region) %>%
  summarise(across(
    .cols = is.numeric, 
    .fns = list(Median = median), na.rm = TRUE, 
    .names = "{col}_{fn}"
  ))

InsightTravelTime %>% group_by(Region) %>%
  summarise(across(
    .cols = is.numeric, 
    .fns = list(SD = sd), na.rm = TRUE, 
    .names = "{col}_{fn}"
  ))


mt4 = melt(InsightLeavingTime, id.vars='Region')
ggplot(mt4) +
  geom_col(aes(variable, value, group=Region, fill = Region)) +
  facet_grid(~Region, scales="free_x") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90)) +
  ggtitle("Leaving Time by Region") +
  theme(plot.title = element_text(hjust = 0.5)) + xlab("Leaving Time") + ylab("count")

InsightTravelTime2 <- Insight3 %>%
  select(c("Tunder15mins", "T15_30mins" ,"T30_45_mins", "T45_60mins", "T60_90mins", "T_90mins"))

#Histogram - Need to provide order
ggplot(gather(InsightTravelTime2, cols, value), aes(x = value)) + 
  geom_histogram(binwidth = 2) + facet_grid(.~cols) + xlim(0,150) +
  theme_bw()

ggplot(gather(InsightTravelTime2, cols, value), aes(x = value)) + 
  geom_histogram(binwidth = 2) + facet_grid(.~cols) + xlim(0,150) +
  theme_bw()  +
  ggtitle("Histogram - Commute Time") +
  theme(plot.title = element_text(hjust = 0.5)) + ylab("count")

ggplot(mt4) +
  geom_histogram(aes(value), binwidth = 2) +
  facet_grid(~variable, scales="free_x") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90)) +
  ggtitle("Histogram - Leaving Time") +
  theme(plot.title = element_text(hjust = 0.5)) + xlab("Leaving Time") + ylab("count")

InsightLeavingTime2 <-  Insight3 %>%
  select(c("b4_630", "T630_700", "T701_730", "T731_800", "T801_830", "T831_900", "T901_930", "T930aft"))

ggplot(gather(InsightLeavingTime2, cols, value), aes(x = value)) + 
  geom_histogram(binwidth = 2) + facet_grid(.~cols) + xlim(0,150) +
  theme_bw()  +
  ggtitle("Histogram - Leaving Time") +
  theme(plot.title = element_text(hjust = 0.5))

InsightLeavingTime %>% group_by(Region) %>%
  summarise(across(
    .cols = is.numeric, 
    .fns = list(Mean = mean), na.rm = TRUE, 
    .names = "{col}_{fn}"
  ))

InsightLeavingTime %>% group_by(Region) %>%
  summarise(across(
    .cols = is.numeric, 
    .fns = list(SD = sd), na.rm = TRUE, 
    .names = "{col}_{fn}"
  ))

InsightLeavingTime %>% group_by(Region) %>%
  summarise(across(
    .cols = is.numeric, 
    .fns = list(Median = median), na.rm = TRUE, 
    .names = "{col}_{fn}"
  ))

InsightLeavingTime %>% group_by(Region) %>%
  summarise(across(
    .cols = is.numeric, 
    .fns = list(IQR = IQR), na.rm = TRUE, 
    .names = "{col}_{fn}"
  ))

InsightLeavingTime %>% group_by(Region) %>%
  summarise(across(Mode = mymode))

#Totals by Region
tibble2 <- group_by(InsightLeavingTime, Region) %>%
  summarise(across(
    .cols = is.numeric, 
    .fns = list(sum = sum), na.rm = TRUE, 
    .names = "{col}_{fn}"
  ))

tibble2

tibble2 %>%
  bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Total"))

#Totals by Region
tibble3 <- group_by(InsightTravelTime, Region) %>%
  summarise(across(
    .cols = is.numeric, 
    .fns = list(sum = sum), na.rm = TRUE, 
    .names = "{col}_{fn}"
  ))

tibble3 %>%
  bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Total"))

#Anderson Darling test to confirm observation data comes from a non normal distribution

library(nortest)

#Leaving Time columns
sapply(InsightLeavingTime2, ad.test)

#Travel time columns
sapply(InsightTravelTime2, ad.test)

#Boxplots to check for outliers - as lots of outliers, moods test is more appropriate

library(ggplot2)
ggplot(mt3, aes(Region, value)) + geom_boxplot(aes(group = Region, fill = Region)) + theme_bw()+
  xlab("Region") + 
  stat_boxplot(geom ='errorbar') +
  facet_wrap(~variable) +
  theme(axis.text.x = element_text(angle=90)) +
  coord_flip() +
  ggtitle("Boxplot - Commute Travel Times by Region") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(mt4, aes(Region, value)) + geom_boxplot(aes(group = Region, fill = Region)) + theme_bw()+
  xlab("Region") + 
  stat_boxplot(geom ='errorbar') +
  facet_wrap(~variable) +
  theme(axis.text.x = element_text(angle=90)) +
  coord_flip() +
  ggtitle("Boxplot - Commute Leaving Times by Region") +
  theme(plot.title = element_text(hjust = 0.5))

library(RVAideMemoire)

#Moods Median Test - Travel Time
mood.medtest(Tunder15mins ~ Region, data  = InsightTravelTime, exact = FALSE)
mood.medtest(T15_30mins ~ Region, data  = InsightTravelTime, exact = FALSE)
mood.medtest(T30_45_mins ~ Region, data  = InsightTravelTime, exact = FALSE)
mood.medtest(T45_60mins ~ Region, data  = InsightTravelTime, exact = FALSE)
mood.medtest(T60_90mins ~ Region, data  = InsightTravelTime, exact = FALSE)
mood.medtest(T_90mins ~ Region, data  = InsightTravelTime, exact = FALSE)

#Dunn Test - Travel Time

dunn.test::dunn.test(InsightTravelTime$Tunder15mins,InsightTravelTime$Region, method = "bonferroni")
dunn.test::dunn.test(InsightTravelTime$T15_30mins,InsightTravelTime$Region, method = "bonferroni")
dunn.test::dunn.test(InsightTravelTime$T30_45_mins,InsightTravelTime$Region, method = "bonferroni")
dunn.test::dunn.test(InsightTravelTime$T45_60mins,InsightTravelTime$Region, method = "bonferroni")
dunn.test::dunn.test(InsightTravelTime$T60_90mins,InsightTravelTime$Region, method = "bonferroni")
dunn.test::dunn.test(InsightTravelTime$T_90mins,InsightTravelTime$Region, method = "bonferroni")

#Moods Median Test - Leaving Time
mood.medtest(b4_630 ~ Region, data  = InsightLeavingTime, exact = FALSE)
mood.medtest(T630_700 ~ Region, data  = InsightLeavingTime, exact = FALSE)
mood.medtest(T701_730 ~ Region, data  = InsightLeavingTime, exact = FALSE)
mood.medtest(T731_800 ~ Region, data  = InsightLeavingTime, exact = FALSE)
mood.medtest(T801_830 ~ Region, data  = InsightLeavingTime, exact = FALSE)
mood.medtest(T831_900 ~ Region, data  = InsightLeavingTime, exact = FALSE)
mood.medtest(T901_930 ~ Region, data  = InsightLeavingTime, exact = FALSE)
mood.medtest(T930aft ~ Region, data  = InsightLeavingTime, exact = FALSE)

#Dunn Test - Leaving Time

library(magrittr)
library(tidyverse)
library(asbio)

dunn.test::dunn.test(InsightLeavingTime$b4_630,InsightLeavingTime$Region, method = "bonferroni")
dunn.test::dunn.test(InsightLeavingTime$T630_700,InsightLeavingTime$Region, method = "bonferroni")
dunn.test::dunn.test(InsightLeavingTime$T701_730,InsightLeavingTime$Region, method = "bonferroni")
dunn.test::dunn.test(InsightLeavingTime$T731_800,InsightLeavingTime$Region, method = "bonferroni")
dunn.test::dunn.test(InsightLeavingTime$T801_830,InsightLeavingTime$Region, method = "bonferroni")
dunn.test::dunn.test(InsightLeavingTime$T831_900,InsightLeavingTime$Region, method = "bonferroni")
dunn.test::dunn.test(InsightLeavingTime$T901_930,InsightLeavingTime$Region, method = "bonferroni")
dunn.test::dunn.test(InsightLeavingTime$T930aft,InsightLeavingTime$Region, method = "bonferroni")

#####Maybe remove#####

ymatrix <- as.matrix(InsightTravelTime)
xmatrix <- as.matrix(InsightLeavingTime)

install.packages("rrr")
library(rrr)

#Multivariate regression
multivar_reg <- t(cov(ymatrix, xmatrix) %*% solve(cov(xmatrix)))
  
## separate multiple regression

lm1 <- lm(ymatrix[,1] ~ xmatrix)$coeff
lm2 <- lm(ymatrix[,2] ~ xmatrix)$coeff
lm3 <- lm(ymatrix[,3] ~ xmatrix)$coeff
lm4 <- lm(ymatrix[,4] ~ xmatrix)$coeff
lm5 <- lm(ymatrix[,5] ~ xmatrix)$coeff
lm6 <- lm(ymatrix[,6] ~ xmatrix)$coeff

lmcombined <- cbind(lm1, lm2, lm3,lm4,lm5,lm6)

rank_trace(xmatrix, ymatrix) 

rank_trace(xmatrix, ymatrix, plot = FALSE)

rank_trace(xmatrix, ymatrix, type = "cva")

rr <- rrr(xmatrix, ymatrix, rank = "full") 

residuals(xmatrix, ymatrix, rank = 1, plot = FALSE)

residuals(xmatrix, ymatrix, rank = 1)

#####Creating Model#####

library(reshape2)

dm1 <- melt(Insight3[,c("Region", "Tunder15mins","T15_30mins","T30_45_mins","T45_60mins","T60_90mins", "T_90mins")], measure.vars=c("Tunder15mins","T15_30mins","T30_45_mins","T45_60mins","T60_90mins", "T_90mins"))
dm2 <- melt(Insight3[,c("Region","b4_630","T630_700","T701_730","T731_800", "T801_830", "T831_900", "T901_930", "aftT930")], measure.vars=c("b4_630","T630_700","T701_730","T731_800", "T801_830", "T831_900", "T901_930", "aftT930"))

library(dplyr)
dm1_spliced <- sample_n(dm1, 500)
dm2_spliced <- sample_n(dm2, 500)

dm_spliced <- merge(dm2_spliced, dm1_spliced, by ="Region")

Insight3


.ls.objects <- function (pos = 1, pattern, order.by,
                         decreasing=FALSE, head=FALSE, n=5) {
  napply <- function(names, fn) sapply(names, function(x)
    fn(get(x, pos = pos)))
  names <- ls(pos = pos, pattern = pattern)
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.prettysize <- napply(names, function(x) {
    capture.output(print(object.size(x), units = "auto")) })
  obj.size <- napply(names, object.size)
  obj.dim <- t(napply(names, function(x)
    as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
  names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")
  if (!missing(order.by))
    out <- out[order(out[[order.by]], decreasing=decreasing), ]
  if (head)
    out <- head(out, n)
  out
}

lsos <- function(..., n=10) {
  .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}

lsos()

##### INSIGHT 4 - Correlation between stay at home parent(s) & having kids #####

Insight4 <- Df[,c(304:308,464,473)]

library(PerformanceAnalytics)
chart.Correlation(Insight4, histogram=TRUE, pch=19)

corrplot::corrplot(cor(Insight4, method = "kendall"), method = "number", type = "upper", tl.col="black", tl.cex=0.8, tl.srt=70, bg = "lightgrey")

Insight4_1 <- select(Insight4, -c(T8_1_LAHFM))

chart.Correlation(Insight4_1, histogram=TRUE, pch=19, method = "kendall")

mt2 = melt(Insight4_1, id.vars='T8_1_LAHFF')

ggscatter(mt2, x = "value", y = "T8_1_LAHFF", add = "reg.line", facet.by = "variable", palette = "jco", conf.int = TRUE, color = "variable") +
  stat_cor(method = "kendall", aes(color = variable), label.y = 150)

ggplot(mt2, aes(x = value, color = variable, fill = variable)) + 
  geom_density() + facet_wrap(.~variable) + xlim(0,100) +
  theme_bw()

#####MAYBE REMOVE####
ggplot(mt2) +
  geom_jitter(aes(value,T8_1_LAHFF, colour=variable),) + geom_smooth(aes(value,T8_1_LAHFF, colour=variable), method=lm, se=TRUE) +
  facet_wrap(~variable, scales="free_x") +
  theme_bw()

ggplot(mt2) +
  geom_jitter(aes(value,T8_1_LAHFF, colour = variable)) + geom_smooth(aes(value,T8_1_LAHFF, colour = variable), method=lm, se=TRUE)

ggplot(mt2) +
  geom_jitter(aes(value,T8_1_LAHFF)) + geom_smooth(aes(value,T8_1_LAHFF), method=lm, se=FALSE)

#####DO DESCRIPTION STATISTICS FOR INSIGHT 4, AND GET VALUES OF STAY AT HOME MOTHERS VS NUMBER OF CHILDREN####

Insight4_1 %>% summarise(across(
  .cols = is.numeric, 
  .fns = list(sum = sum), na.rm = TRUE, 
  .names = "{col}_{fn}"
))

Insight4_1 %>% summarise(across(
  .cols = is.numeric, 
  .fns = list(mean = mean), na.rm = TRUE, 
  .names = "{col}_{fn}"
))


#####REMOVE####
LinearModel <- lm(T8_1_LAHFF ~ T4_6_0C + T4_6_1C + T4_6_2C + T4_6_3C + T4_6_GE4C, data = Insight4_1)
summary(LinearModel)
autoplot(LinearModel)

fit2 <- lm(T8_1_LAHFF ~ T4_6_0C + T4_6_1C + T4_6_2C + T4_6_3C, data = Insight4_1)
summary(fit2)

fit3 <- lm(T8_1_LAHFF ~ T4_6_0C + T4_6_1C + T4_6_2C, data = Insight4_1)
summary(fit3)

fit4 <- lm(T8_1_LAHFF ~ T4_6_0C + T4_6_1C, data = Insight4_1)
summary(fit4)

fit5 <- lm(T8_1_LAHFF ~ T4_6_0C, data = Insight4_1)
summary(fit5)

fit6 <- lm(T8_1_LAHFF ~ T4_6_3C, data = Insight4_1)
summary(fit6)

fit7 <- lm(T8_1_LAHFF ~ T4_6_GE4C, data = Insight4_1)
summary(fit7)

anova(fit5, fit4, fit3, fit2, LinearModel, test = "Chisq")
#SAME OUTCOME - LIKELIHOOD RATIO TEST
anova(fit5, fit4, fit3, fit2, LinearModel, test = "LRT")

step<- step(LinearModel, direction="backward", trace = FALSE)
summary(step)$coeff

b<- ggplot2::fortify(step)
resid<- ggplot(b, aes(x= .fitted, y= .resid)) + geom_point(colour="blue")+ stat_smooth(method="lm")
resid<- resid + ggtitle("Residuals Vs Fitted")
normal <- ggplot(b, aes(qqnorm(.stdresid)[[1]], .stdresid)) + geom_point(colour="blue")+ stat_smooth(method="lm")
normal <- normal + ggtitle("Q-Q Normal")

modelmetrics <- augment(fit6)
head(modelmetrics)

ggplot(modelmetrics, aes(modelmetrics$T4_6_3C, modelmetrics$T8_1_LAHFF)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE) +
  geom_segment(aes(xend = modelmetrics$T4_6_3C, yend = .fitted), color = "red", size = 0.3) +
  theme_bw()

skewness(modelmetrics$.resid)

ggplot(modelmetrics, aes(x=.resid))+
  geom_histogram(binwidth = 0.8)+
  xlab("Residuals")

kurtosis(modelmetrics$.resid)

mean(modelmetrics$.resid)

library(gridExtra)
grid.arrange(resid, normal)

ggplot(b, aes(x=.resid))+
  geom_histogram(binwidth = 2)+
  xlab("Residuals")

#Very small mean
mean(b$.resid)

#####MAYBE REMOVE####

modelmetrics <- augment(LinearModel2)
head(modelmetrics)

ggplot(modelmetrics, aes(modelmetrics$value, modelmetrics$T8_1_LAHFF)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE) +
  geom_segment(aes(xend = modelmetrics$value, yend = .fitted), color = "red", size = 0.3) +
  theme_bw()

##### INSIGHT 5 - Difference in home ownership by region #####

Insight5 <- Df[,c(392,393,399, 809,811)]

Insight5$Homeowners <- rowSums(subset(Insight5, select = c("T6_3_OMLH", "T6_3_OOH")))

Insight5$NotHomeowners <- (Insight5$T6_3_TH - Insight5$Homeowners)

#Ratio of Homeowners to Non Homeowners

Insight5$ratio <- Insight5$Homeowners/(Insight5$NotHomeowners + Insight5$Homeowners)

colnames(Insight5)[4] <- "Region"

ggplot(Insight5, mapping = aes(Region, T6_3_TH)) +
  geom_col(alpha=.9, fill='lightblue') +
  geom_col(mapping = aes(Region, Homeowners), alpha=.9, fill='pink') +
  theme_bw() + scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  xlab("Region") + ylab("Total") + ggtitle("Households - Homeowners vs Non Homeowners")

###TO WORK ON
melt182 <- melt(Insight5, id.vars = c("NotHomeowners", "Region", "Homeowners"))

ggplot(melt182, aes(Region, value, fill = Gender, colour = Gender)) + geom_col(alpha = 0.3) + facet_wrap(~variable,scales='free') +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90)) +
  ggtitle("Unemployment by Gender across Regions") +
  theme(plot.title = element_text(hjust = 0.5))

#Ratio Analysis
group_by(Insight5, Region = NUTS3NAME) %>%
  summarise(
    count = n(),
    mean = mean(ratio),
    median = median(ratio),
    sd = sd(ratio),
    median = median(ratio),
    IQR = IQR(ratio)
  )

#Visualization of data via boxplot
ggplot(Insight5, aes(Region, ratio, color = Region)) + geom_boxplot() + theme_bw()+
  xlab("Region") +
  stat_boxplot(geom ='errorbar') + stat_summary(fun=mean, geom="line", aes(group=1))  + 
  stat_summary(fun=mean, geom="point")

#Determining if normal distribution

#By region
ggplot(Insight5, aes(ratio)) + geom_density(color = "black", fill = "lightblue", alpha = 0.4) + geom_vline(aes(xintercept = mean(ratio)), color = "#FC4E08", linetype = "dashed", size = 1) + 
  geom_vline(aes(xintercept = median(ratio)), color = "blue", linetype = 4, size = 1) +
  facet_wrap(~Region) +
  theme_bw()

#all together
ggplot(Insight5, aes(ratio)) + geom_density(color = "black", fill = "lightblue", alpha = 0.4) + geom_vline(aes(xintercept = mean(ratio)), color = "#FC4E08", linetype = "dashed", size = 1) + 
  geom_vline(aes(xintercept = median(ratio)), color = "blue", linetype = 4, size = 1) +
  theme_bw()

##DO HISTOGRAM

##Cramer- Von Mises test for normality
cvm.test(Insight5$ratio)

#Kruskal Test

kruskal.test(ratio ~ Region, data = Insight5)

#As the p-value is less than the significance level 0.05, 
#we can conclude that there are significant differences between the levels of home ownership in these regions.

#Conover-Iman Test of Multiple Comparisons Using Rank Sums To identify where significant differences arise between regions

library(conover.test)

ct1 <- conover.test(Insight5$ratio, Insight5$Region, method = "Bonferroni", kw = FALSE, table = TRUE)

filter(ct1, ct1$P.adjusted <0.05)