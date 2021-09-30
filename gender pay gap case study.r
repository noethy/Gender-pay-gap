# gender pay gap case study (https://www.glassdoor.com/research/app/uploads/sites/2/2019/03/GD_Report_AnalyzingGenderPayGap_v2-2.pdf)
install.packages("tidyverse")
install.packages("tidymodels")
install.packages("devtools")
library(tidyverse)
library(tidymodels)
library(devtools)
install_github("Hendrik147/HR_Analytics_in_R_book_v2/HRAnalytics")
library(HRAnalytics) # helper package

# set to show more digits in tibble format
options(pillar.sigfig=7)
# disable scientific notation
options(scipen = 999)

# 1 load data and prep
genderpay <- read_csv("https://glassdoor.box.com/shared/static/beukjzgrsu35fqe59f7502hruribd5tt.csv")
genderpay
skim_without_charts(genderpay)
ggplot(genderpay, aes(x = basePay)) + 
    geom_histogram(aes(y = ..density..), bins = 20, color = "black", fill = "white") + # have to have aes(y = ..density..) to create density line
    stat_function(fun = dnorm, args = list(mean = mean(genderpay$basePay), sd = sd(genderpay$basePay))) # add bell curve
# prep data
# why log transformation: The coefficient on x will give the approximate percentage pay gap
# between men and women — what we call the “adjusted” gender pay gap.
# log(y) = beta0 + beta1 * x ==> exp(log(y)) = exp(beta0 + beta1 * x) ==> y = exp(beta0) * exp (beta1 * x)
# so if there is one unit change in x, there will be a certain percentage change in y.
genderpay %>%
    # make age brackets, <25, 25 - 34. 35 - 44, 45 - 54, >54
    mutate(age_bin = cut(age,
                        breaks = c(0, 25, 35, 45, 55, Inf),
                        right = FALSE)
            ) %>%
    # add a column for total compensation
    mutate(total_comp = basePay + bonus) %>%
    # log10 of compensation
    mutate(log_base = log(basePay),
            log_bonus = log(bonus),
            log_total = log(total_comp)) %>%
    # apply to all the character variable,fct_infreq: reorder levels by # of obs from largest to smallest
    mutate_if(is_character, fct_infreq) %>% # same as mutate(across(where(is_character), fct_infreq))
    mutate(age_bin = fct_infreq(age_bin)) ->
genderpay_clean
# # visualize total_comp
# ggplot(genderpay_clean, aes(x = total_comp)) + 
#     geom_histogram(aes(y = ..density..), bins = 20, color = "black", fill = "white") + # have to have aes(y = ..density..) to create density line
#     stat_function(fun = dnorm, args = list(mean = mean(genderpay_clean$total_comp), sd = sd(genderpay_clean$total_comp))) # add bell curve
# # right-skewed, visualize log_total
# ggplot(genderpay_clean, aes(x = log_total)) + 
#     geom_histogram(aes(y = ..density..), bins = 20, color = "black", fill = "white") + # have to have aes(y = ..density..) to create density line
#     stat_function(fun = dnorm, args = list(mean = mean(genderpay_clean$log_total), sd = sd(genderpay_clean$log_total))) # add bell curve

# 2. EDA, care about gender, look at summary stats group by gender
# base pay
genderpay_gender_base <- genderpay_clean %>%
    filter(!is.na(basePay)) %>% # filter out missing data in basePay
    group_by(gender) %>%
    summarize(mean_base = mean(basePay),
            median_base = median(basePay),
            count = n())
# bonus
genderpay_gender_bonus <- genderpay_clean %>%
    filter(!is.na(bonus)) %>% # filter out missing data in basePay
    group_by(gender) %>%
    summarize(mean_bonus = mean(bonus),
            median_bonus = median(bonus),
            count = n())
# total compensation
genderpay_gender_total <- genderpay_clean %>%
    filter(!is.na(total_comp)) %>% # filter out missing data in basePay
    group_by(gender) %>%
    summarize(mean_total = mean(total_comp),
            median_total = median(total_comp),
            count = n())
# performance review
genderpay_gender_eval <- genderpay_clean %>%
    filter(!is.na(perfEval)) %>% # filter out missing data in basePay
    group_by(gender) %>%
    summarize(mean_eval = mean(perfEval),
            median_eval = median(perfEval),
            count = n())

# 3. gender is a high level variable, lower level variables like department or job type can have diff directions of effect
# when combining these lower levels, these effects dissapear. simpson's paradox
# total compensation by department and gender
genderpay_dept_gender_total <- genderpay_clean %>%
    filter(!is.na(total_comp)) %>% # filter out missing data in basePay
    group_by(dept, gender) %>%
    summarize(mean_total = mean(total_comp),
            median_total = median(total_comp),
            count = n())
# all differnces between male and female are in the same direction within each department, mean_male > mean_female

# total compensation by job type and gender
genderpay_job_gender_total <- genderpay_clean %>%
    filter(!is.na(total_comp)) %>% # filter out missing data in basePay
    group_by(jobTitle, gender) %>%
    summarize(mean_total = mean(total_comp),
            median_total = median(total_comp),
            count = n())
# mean_male < mean_female for jobs: Data Scientist, Financial Analyst, Graphic Designer, Manager, Warehouse Associate

# 4. run different regression models
# 4.1 on log base pay
# unadjusted pay gap, with no controls
model1 <- lm(log_base ~ gender, data = genderpay_clean)

# add control for age, education and performance evaluations
model2 <- lm(log_base ~ gender + age_bin + edu + perfEval, data = genderpay_clean)

# add all controls 
model3 <- lm(log_base ~ gender + age_bin + edu + perfEval + dept + seniority + jobTitle, data = genderpay_clean)

summary(model1)
summary(model2)
summary(model3)

# lm(formula = log_base ~ gender, data = genderpay_clean)
# Coefficients:
#              Estimate Std. Error t value             Pr(>|t|)    
# (Intercept)  11.46182    0.01228 933.208 < 0.0000000000000002 ***
# genderFemale -0.09532    0.01795  -5.309          0.000000136 ***
# ---
# Residual standard error: 0.2833 on 998 degrees of freedom
# Multiple R-squared:  0.02747,	Adjusted R-squared:  0.02649 
# F-statistic: 28.19 on 1 and 998 DF,  p-value: 0.0000001359

# lm(formula = log_base ~ gender + age_bin + edu + perfEval, data = genderpay_clean)
# Coefficients:
#                 Estimate Std. Error t value             Pr(>|t|)    
# (Intercept)    11.628541   0.025005 465.057 < 0.0000000000000002 ***
# genderFemale   -0.100189   0.014717  -6.808      0.0000000000172 ***
#...
# ---
# Residual standard error: 0.2305 on 990 degrees of freedom
# Multiple R-squared:  0.3612,	Adjusted R-squared:  0.3554 
# F-statistic:  62.2 on 9 and 990 DF,  p-value: < 0.00000000000000022

# lm(formula = log_base ~ gender + age_bin + edu + perfEval + dept + 
#     seniority + jobTitle, data = genderpay_clean)
# Coefficients:
#                               Estimate Std. Error t value             Pr(>|t|)    
# (Intercept)                 11.0069458  0.0228870 480.925 < 0.0000000000000002 ***
# genderFemale                -0.0109446  0.0092455  -1.184              0.23679    
#...
# ---
# Residual standard error: 0.1306 on 976 degrees of freedom
# Multiple R-squared:  0.7977,	Adjusted R-squared:  0.793 
# F-statistic: 167.4 on 23 and 976 DF,  p-value: < 0.00000000000000022

# model1: females' base pay is about 9.5% lower than males'
# model2: females' base pay is about 10% lower than males'
# model3: when accounting for all for controls, there appears no gender difference in base pay

# 4.2 gender pay gap may hide only within certain department or job type
# 4.2.1 by department
model3_dept <- lm(log_base ~ gender * dept + age_bin + edu + perfEval + seniority + jobTitle, data = genderpay_clean)
summary(model3_dept)
# lm(formula = log_base ~ gender * dept + age_bin + edu + perfEval + 
#     seniority + jobTitle, data = genderpay_clean)
# Coefficients:
#                                    Estimate  Std. Error t value             Pr(>|t|)    
# (Intercept)                     11.00163847  0.02379843 462.284 < 0.0000000000000002 ***
# genderFemale                     0.00152592  0.01849800   0.082               0.9343    
# genderFemale:deptSales          -0.02625359  0.02581574  -1.017               0.3094    
# genderFemale:deptManagement     -0.01845582  0.02635455  -0.700               0.4839    
# genderFemale:deptAdministration  0.00042550  0.02634167   0.016               0.9871    
# genderFemale:deptEngineering    -0.01954256  0.02643102  -0.739               0.4599    

# intercept is the baseline: male_operations
# genderFemale: female_operations - male_operations = 0.0015
# genderFemale:deptSales: (female_sales - male_sales) - (female_operations - male_operations) = -0.026 
#        ==> female_sales - male_sales = 0.0015 +(-0.026) = -0.0245
# genderFemale:deptManagement: (female_management - male_management)- (female_operations - male_operations) = -0.018
#        ==> female_management - male_management = 0.0015 +(-0.018) = -0.003
# genderFemale:deptAdministration: (female_admin - male_admin) - (female_operations - male_operations) = 0.0004
#        ==> female_admin - male_admin = 0.0015 + 0.0004 = 0.0019
# genderFemale:deptEngineering: (female_engineer - male_engineer) - (female_operations - male_operations) = -0.0195
#        ==> female_engineer - male_engineer = 0.00115 + (-0.0195) = -0.01835
# there appears no gender pay gap within any department

# 4.2.2 by job type
model3_job <- lm(log_base ~ gender * jobTitle + age_bin + edu + perfEval + seniority + dept, data = genderpay_clean)
summary(model3_job)
# lm(formula = log_base ~ gender * jobTitle + age_bin + edu + 
# Coefficients:
#                                             Estimate  Std. Error t value             Pr(>|t|)    
# (Intercept)                              11.06381589  0.04428569 249.828 < 0.0000000000000002 ***
# genderFemale                             -0.07249123  0.04166122  -1.740             0.082174 .  
# genderFemale:jobTitleSoftware Engineer    0.06153120  0.06380774   0.964             0.335126    
# genderFemale:jobTitleData Scientist       0.07797259  0.04885494   1.596             0.110815    
# genderFemale:jobTitleFinancial Analyst    0.04947928  0.04891321   1.012             0.311995    
# genderFemale:jobTitleGraphic Designer     0.08654749  0.04956530   1.746             0.081106 .  
# genderFemale:jobTitleIT                   0.04858348  0.04959843   0.980             0.327560    
# genderFemale:jobTitleSales Associate      0.03962232  0.04993143   0.794             0.427661    
# genderFemale:jobTitleDriver               0.07365239  0.04993271   1.475             0.140529    
# genderFemale:jobTitleManager              0.06243692  0.05438176   1.148             0.251202    
# genderFemale:jobTitleWarehouse Associate  0.08028064  0.05003561   1.604             0.108937    

# female_mark - male_mark = -0.072
# female_se - male_se = 0.062 + (-0.072) = -0.01
# female_ds - male_ds = 0.078 + (-0.072) = 0.006
# female_fa - male_fa = 0.049 + (-0.072) = -0.023
# female_gd - male_gd = 0.087 + (-0.072) = 0.015
# female_it - male_it = 0.049 + (-0.072) = -0.023
# female_sa - male_sa = 0.040 + (-0.072) = -0.032
# female_dr - male_dr = 0.074 + (-0.072) = 0.002
# female_ma - male_ma = 0.062 + (-0.072) = -0.01
# female_wa - male_wa = 0.080 + (-0.072) = 0.008

# there appears to be no gender pay gap within each job type

