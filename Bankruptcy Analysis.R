# Install all needed packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(haven)) install.packages("haven", repos = "http://cran.us.r-project.org")
if(!require(ggrepel)) install.packages("ggrepel", repos = "http://cran.us.r-project.org")
if(!require(broom)) install.packages("broom", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")

# Load all needed packages
library(tidyverse)
library(caret)
library(haven)
library(ggrepel)
library(broom)
library(readr)
library(lubridate)

#Timeout
options(timeout = 120)

# This code will automatically download and load the two necessary datasets
# This first one is all the bankruptcies filed in 2022 in Shelby County, which originally from the federal judicial center
ShelbyBrupurl <- "https://github.com/jacobsteimer/USBankruptcyZIP/raw/main/Shelby_Brup_2022.csv"
Shelby_2022_Brup_data <- readr::read_csv(ShelbyBrupurl)

# this second one is some census data I pulled together
Census_file_url <- "https://github.com/jacobsteimer/USBankruptcyZIP/raw/main/CensusData.csv"
census_data <- readr::read_csv(Census_file_url)

#Then, let's count up the number of bankruptcies in each ZIP and the number of Chapter 13 Bankruptcies in each ZIP
ShelbyZIPcounts <- Shelby_2022_Brup_data %>% group_by(ZIP) %>% summarize(Bankruptcies = n())
ShelbyThirteenZIPcounts <- Shelby_2022_Brup_data %>% filter(ORGFLCHP == 13) %>% group_by(ZIP) %>% summarize(Thirteens = n())
ShelbyBrupAndThirteen <- inner_join(ShelbyZIPcounts,ShelbyThirteenZIPcounts, by = "ZIP")

#Let's also calculate the percentage of Bankruptcies that are Chapter 13
ShelbyBrupAndThirteen <- ShelbyBrupAndThirteen %>% mutate(ThirteenPerc = (Thirteens/Bankruptcies))

#Now, let's join that with our census data
census_data <- census_data %>% mutate(ZIP = GEOID)
ShelbyBrupAndThirteen <- ShelbyBrupAndThirteen %>% mutate(ZIP = as.character(ZIP))
SC_Brup_w_Census <- inner_join(ShelbyBrupAndThirteen,census_data,by = "ZIP")
SC_Brup_w_Census <- SC_Brup_w_Census %>% filter(Population > 0)

# And let's go ahead and turn Bankruptcies and Thirteens to "per thousand residents" statistics
SC_Brup_w_Census <- SC_Brup_w_Census %>% mutate(BrupPerThou = Bankruptcies/Population*1000) %>% mutate(ThirteenPerThou = Thirteens/Population*1000)

# Also, let's divide MedIncome by 10,000. A difference in $10,000 between ZIP codes is a more understandable change than a difference of $1.
SC_Brup_w_Census$MedIncome <- SC_Brup_w_Census$MedIncome/10000 

#Now, let's analyze!
#let's start with a simple correlation between race and bankruptcies
Race_cor <- cor(SC_Brup_w_Census$BlackPercent,SC_Brup_w_Census$BrupPerThou)
print(Race_cor)
Race_lm <- lm(BrupPerThou ~ BlackPercent, data = SC_Brup_w_Census)
tidy(Race_lm)

# Woah! What a massive correlation. --0.948

#Let's try an lm with all six variables
SC_fitlm <- lm(BrupPerThou ~ BlackPercent + LabForceParticipation + HomeOwnRate + UnRate + MedIncome + BachPercent, data = SC_Brup_w_Census)
tidy(SC_fitlm)
# Once again, race looks crazy powerful. 
# But since some of the economic statistics have a lot of co linearity, let's re-run it with fewer variables.
SC_fitlm2 <- lm(BrupPerThou ~ BlackPercent + UnRate + BachPercent, data = SC_Brup_w_Census)
tidy(SC_fitlm2)
# Race continues to have the most statistically significant impact, although unemployment rate and bachelor degrees look much more important.

# Let's now just look at Chapter 13 bankruptcies
SC_fitlm3 <- lm(ThirteenPerThou ~ BlackPercent + LabForceParticipation + HomeOwnRate + UnRate + MedIncome + BachPercent, data = SC_Brup_w_Census)
tidy(SC_fitlm3)
# Race actually looks slightly less important but still appears the most important. Let's again run with the three most important
SC_fitlm4 <- lm(ThirteenPerThou ~ BlackPercent + UnRate + BachPercent, data = SC_Brup_w_Census)
tidy(SC_fitlm4)
# Unemployment rate looks much more powerful than it once did. But, to be fair, a one percent increase in the unemployment rate is a much bigger jump than a one percent increase in race.

#Let's also try random forrest
ctrl <- trainControl(method = "cv", number = 5)
SC_fitrf <- train(BrupPerThou ~ BlackPercent + LabForceParticipation + HomeOwnRate + UnRate + MedIncome + BachPercent, data = SC_Brup_w_Census, method = "rf", trControl = ctrl, na.action = na.omit)
var_importance__SC_rf <- varImp(SC_fitrf)
print(var_importance__SC_rf)
#Not surprisingly, rf sees race as the most important variable
SC_fitrf13 <- train(ThirteenPerThou ~ BlackPercent + LabForceParticipation + HomeOwnRate + UnRate + MedIncome + BachPercent, data = SC_Brup_w_Census, method = "rf", trControl = ctrl, na.action = na.omit)
var_importance_13_SC_rf <- varImp(SC_fitrf)
print(var_importance_13_SC_rf)
# The same with 13s

#How many Shelby County filings are repeats?
Shelby_2022_Brup_data %>% group_by(PRFILE) %>% summarize(n())
2488/(2588+2508)
RepeatPerc <- 2488/(2588+2508)

#Granular info about Shelby ZIPs
ShelbyGranular <- Shelby_2022_Brup_data %>% select(GEOID = ZIP, ORGFLCHP, TOTASSTS,TOTLBLTS,TOTDBT,CNTMNTHI)
SummaryofSCGranular <- ShelbyGranular %>% group_by(ORGFLCHP) %>% summarise(asstmean = mean(TOTASSTS, na.rm = TRUE), liabmean = mean(TOTLBLTS, na.rm = TRUE), debtmean = mean(TOTDBT, na.rm = TRUE), medincome = median(CNTMNTHI, na.rm = TRUE))
BlackZIPs <- Shelby_2022_Brup_data %>% mutate(GEOID = as.character(GEOID),ZIP = as.character(ZIP)) %>% inner_join(census_data) %>% filter(BlackPercent > .5)
MajorityBlackGranular <- BlackZIPs %>% select(GEOID = ZIP, ORGFLCHP, TOTASSTS,TOTLBLTS,TOTDBT,CNTMNTHI)
SummaryofBlackGranular <- MajorityBlackGranular %>% group_by(ORGFLCHP) %>% summarise(asstmean = mean(TOTASSTS, na.rm = TRUE), liabmean = mean(TOTLBLTS, na.rm = TRUE), debtmean = mean(TOTDBT, na.rm = TRUE), medincome = median(CNTMNTHI, na.rm = TRUE))

SCgranular <- ShelbyGranular %>% summarise(asstmed = median(TOTASSTS, na.rm = TRUE), liabmed = median(TOTLBLTS, na.rm = TRUE), debtmed = median(TOTDBT, na.rm = TRUE), medincome = median(CNTMNTHI, na.rm = TRUE))

#_____LR_______

install.packages("car")

library(car)

SC_fitlm <- lm(BrupPerThou ~ BlackPercent + LabForceParticipation + HomeOwnRate + UnRate + MedIncome + BachPercent, data = SC_Brup_w_Census)
tidy(SC_fitlm)

vif_results <- vif(SC_fitlm)

print(vif_results)



#LR Corr between race and median income -0.817
Medinc_cor <- cor(SC_Brup_w_Census$MedIncome,SC_Brup_w_Census$BrupPerThou)
print(Medinc_cor)

#R-squared of % Black regression 0.897
SC_Racelm <- lm(BrupPerThou ~ BlackPercent, data = SC_Brup_w_Census)
summary(SC_Racelm)$r.squared

#R-squared of Median income regression 0.668
SC_Medinclm <- lm(BrupPerThou ~ MedIncome, data = SC_Brup_w_Census)
summary(SC_Medinclm)$r.squared

dev.off()
plot(SC_Medinclm, which = 1)