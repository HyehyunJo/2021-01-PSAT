library(tidyverse)
library(gvlma)

setwd('C:/Users/iyuo1/OneDrive/Documents/P-SAT/주제분석/백신 주제')
train <- read.csv('final_train.csv')

library(car)
library(lmtest)



# h1n1_vacc
h1n1_glm <- glm(h1n1_vaccine ~ opinion_h1n1_risk + opinion_h1n1_vacc_effective +
                 opinion_h1n1_sick_from_vacc + doctor_recc_h1n1 + h1n1_knowledge + health_worker, 
                data = train, family = 'binomial')

summary(h1n1_glm)

anova(h1n1_glm, test = 'Chisq')

vif(h1n1_glm) #다중공선성 없음
dwtest(h1n1_glm) #자기상관성 없음

# h1n1 예방접종 의도 
predict.glm(h1n1_glm, type="response") 
h1n1_glm$fitted.values


# seasonal_vacc
seasonal_glm <- glm(seasonal_vaccine ~ opinion_seas_risk + opinion_seas_vacc_effective +
                    opinion_seas_sick_from_vacc + doctor_recc_seasonal + health_worker, 
                    data = train, family = 'binomial')

summary(seasonal_glm)

anova(seasonal_glm, test = 'Chisq')

vif(seasonal_glm) #다중공선성 있긴 함..
dwtest(seasonal_glm) #자기상관성 없음

predict.glm(seasonal_glm, type="response") 
seasonal_glm$fitted.values
