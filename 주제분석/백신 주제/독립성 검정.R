library(tidyverse)
library(ggplot2)
library(vcd)
library(DescTools)
library(vcdExtra)

setwd('C:/Users/iyuo1/OneDrive/Documents/P-SAT/주제분석/백신 주제')

train <- read.csv('final_train.csv')

train_label <- read.csv('training_set_labels.csv')

train2 <- train2 %>% mutate(h1n1_vaccine = train_label$h1n1_vaccine,
                            seasonal_vaccine = train_label$seasonal_vaccine)

train2 <- read.csv('training_set_features.csv')


-------------------------------------------------------------
# Y변수(h1n1_vaccine)과 X변수 간의 독립성 검정

# h1n1_concern
chisq.test(train$h1n1_concern, train$h1n1_vaccine)
# h1n1_knowlege
chisq.test(train$h1n1_knowledge, train$h1n1_vaccine)

# behavioral_antiviral_meds
chisq.test(train$behavioral_antiviral_meds, train$h1n1_vaccine)
# behavioral_avoidance
chisq.test(train$behavioral_avoidance, train$h1n1_vaccine)
# behavioral_face_mask
chisq.test(train$behavioral_face_mask, train$h1n1_vaccine)
# behavioral_wash_hands
chisq.test(train$behavioral_wash_hands, train$h1n1_vaccine)
# behavioral_large_gatherings
chisq.test(train$behavioral_large_gatherings, train$h1n1_vaccine)
# behavioral_outside_home
chisq.test(train$behavioral_outside_home, train$h1n1_vaccine)
# behavioral_touch_face
chisq.test(train$behavioral_touch_face, train$h1n1_vaccine)

# doctor_recc_h1n1
chisq.test(train$doctor_recc_h1n1, train$h1n1_vaccine)
# doctor_recc_seasonal
chisq.test(train$doctor_recc_seasonal, train$h1n1_vaccine)
# chronic_med_condition
chisq.test(train$chronic_med_condition, train$h1n1_vaccine)
# child_under_6_months
chisq.test(train$child_under_6_months, train$h1n1_vaccine)
# health_worker
chisq.test(train$health_worker, train$h1n1_vaccine)
# health_insurance
chisq.test(train$health_insurance, train$h1n1_vaccine)

# opinion_h1n1_vacc_effective
MHChisqTest(table(train$opinion_h1n1_vacc_effective, train$h1n1_vaccine))
# opinion_h1n1_risk
MHChisqTest(table(train$opinion_h1n1_risk, train$h1n1_vaccine))
# opinion_h1n1_sick_from_vacc
MHChisqTest(table(train$opinion_h1n1_sick_from_vacc, train$h1n1_vaccine))
# opinion_seas_vacc_effective
MHChisqTest(table(train$opinion_seas_vacc_effective, train$h1n1_vaccine))
# opinion_seas_risk
MHChisqTest(table(train$opinion_seas_risk, train$h1n1_vaccine))
# opinion_seas_sick_from_vacc
MHChisqTest(table(train$opinion_seas_sick_from_vacc, train$h1n1_vaccine))

# age_group
MHChisqTest(table(train$age_group, train$h1n1_vaccine))
# education
MHChisqTest(table(train$age_group, train$h1n1_vaccine))
# race
chisq.test(train$race, train$h1n1_vaccine)
# sex
chisq.test(train$sex, train$h1n1_vaccine)
# income_poverty
MHChisqTest(table(train$income_poverty, train$h1n1_vaccine))
# marital_status
chisq.test(train$marital_status, train$h1n1_vaccine)
# rent_or_own
chisq.test(train$rent_or_own, train$h1n1_vaccine)
# employment_status
chisq.test(train$employment_status, train$h1n1_vaccine)
# hhs_geo_region
chisq.test(train$hhs_geo_region, train$h1n1_vaccine)
# census_msa
chisq.test(train$census_msa, train$h1n1_vaccine)
# household_adults
MHChisqTest(table(train$household_adults, train$h1n1_vaccine))
# household_children
MHChisqTest(table(train$household_children, train$h1n1_vaccine))
# employment_industry
chisq.test(train$employment_industry, train$h1n1_vaccine)
# employment_occupation
chisq.test(train$age_group, train$h1n1_vaccine)




------------------------------------------------------------
# 재범주화
library(plyr)

# age_group
train$age_group <- train$age_group %>% 
  revalue(c('18 - 34 Years' = 0, '35 - 44 Years' = 1, '45 - 54 Years' = 2, '55 - 64 Years' = 3, '65+ Years' = 4))

# education
train$education <- train$education %>% 
  revalue(c('< 12 Years' = 0, '12 Years' = 1, 'Some College' = 2, 'College Graduate' =3))

# income_poverty
train$income_poverty <- train$income_poverty %>% 
  revalue(c('Below Poverty' = 0, '<= $75,000, Above Poverty' = 1, '> $75,000' = 2))

# employment_industry
distinct(train, employment_industry)

train$employment_industry <- train$employment_industry %>% 
  revalue(c('pxcmvdjn' = 'A', 'rucpziij' = 'B', 'wxleyezf' = 'C', 'saaquncn' = 'D', 'xicduogh' = 'E',
            'ldnlellj' = 'F', 'wlfvacwt' = 'G', 'nduyfdeo' = 'H', 'fcxhlnwr' = 'I', 'vjjrobsf' = 'J',
            'arjwrbjb' = 'K', 'atmlpfrs' = 'L', 'msuufmds' = 'N', 'xqicxuve' = 'M', 'phxvnwax' = 'O',
            'dotnnunm' = 'P', 'mfikgejo' = 'Q', 'cfqqtusy' = 'R', ' mcubkhph' = 'S', 'haxffmxo' = 'T',
            'qnlwzans' = 'U'))

# employment_occupation
distinct(train, employment_occupation)

train$employment_occupation <- train$employment_occupation %>% 
  revalue(c('xgwztkwe' = 'a', 'xtkaffoo' = 'b', 'emcorrxb' = 'c', 'vlluhbov' ='d', 'xqwwgdyp' = 'e',
            'ccgxvspp' = 'f', 'qxajmpny' = 'g', 'kldqjyjy' ='h', 'mxkfnird' = 'i', 'hfxkjkmi' = 'j',
            'bxpfxfdn' = 'k', 'ukymxvdu' = 'l', 'cmhcxjea' = 'n', 'haliazsg' = 'm', 'dlvbwzss' = 'o',
            'xzmlyyjv' = 'p', 'oijqvulv' = 'q', 'rcertsgn' = 'r', 'tfqavkke' = 's', 'hodpvpew' = 't',
            'uqqtjvyb' = 'u', 'pvmttkik' = 'v', 'dcjcmpih' = 'w'))

# hhs_geo_region
train$hhs_geo_region <- train$hhs_geo_region %>% 
  revalue(c('oxchjgsf' = 'Region1', 'bhuqouqj' = 'Region2', 'qufhixun' = 'Region3', 'lrircsnp' = 'Region4',
            
            'atmpeygn' = 'Region5', 'lzgpxyit' = 'Region6', 'fpwskwrf' = 'Region7', 'mlyzmhmf' = 'Region8',
            'dqpwygqj' = 'Region9', 'kbazzjca' = 'Region10'))

--------------------------------------------------------------
# 시각화
par(mfrow=c(2,1))

mos1 <- mosaicplot(~factor(opinion_h1n1_vacc_effective) + h1n1_vaccine, data = train2, color = c('#9795DB', '#F7DFE3'), 
                   main = 'Vaccination by opinion on H1N1_vaccine')
mos2 <- mosaicplot(~factor(opinion_h1n1_risk) + h1n1_vaccine, data = train2, color = c('#9795DB', '#F7DFE3'),
                   main = 'Vaccination by opinion on H1N1_vaccine')
mos3 <- mosaicplot(~factor(opinion_h1n1_sick_from_vacc) + h1n1_vaccine, data = train2, color = c('#9795DB', '#F7DFE3'),
                   main = 'Vaccination by opinion on H1N1_vaccine')

mos4 <- mosaicplot(~factor(opinion_seas_vacc_effective) + seasonal_vaccine, data = train2, color = c('#91B6E2', '#DAEFF5'),
                   main = 'Vaccination by opinion on SEASONAL_vaccine')
mos5 <- mosaicplot(~factor(opinion_seas_risk) + seasonal_vaccine, data = train2, color = c('#91B6E2', '#DAEFF5'),
                   main = 'Vaccination by opinion on SEASONAL_vaccine')
mos6 <- mosaicplot(~factor(opinion_seas_sick_from_vacc) + seasonal_vaccine, data = train2, color = c('#91B6E2', '#DAEFF5'),
                   main = 'Vaccination by opinion on SEASONAL_vaccine')

