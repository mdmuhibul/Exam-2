
Md Muhibul Islam
November 17, 2022
Exam 2



Question 1(a) 
pnorm(-0.45, 4, 8.9, log.p = FALSE)

1(b) 
pnorm(2.56, 8, 3.2, lower.tail = TRUE, log.p = FALSE)

1(c)
pnorm(0.126, 3, 7.2, lower.tail = TRUE, log.p = FALSE)

1(d) 
19.68/8.2 =2.4 
pt(2.4, 32, lower.tail = FALSE)*2

1(e)
-2.64/6.6 = -0.4
pt(-0.4, 11, lower.tail = FALSE)*2



Question 4
use_varb =(age=42) & (age>42 and <85) & (age>=85) dat_use<-subset(ATUS 2018-2021_us, use_varb)

Question 5

attach(dat_ATUS)
summary(dat_ATUS)

  summary(data_newf$EDUC)
EDUC = sample ("masters degree","no masters degree","NA"), 2805, replace = TRUE)
ACT_EDUC = sample("Max", "min"), 1288, replace = TRUE)
x = data.frame(EDUC,FAMINCOME)
CrossTable(EDUC,FAMINCOME)
HADMASTERS = sample("Masters degree","9th grade","NA"), 2805, replace =
                    TRUE)

Model_temp1 <- lm(dat_ATUS$UHRSWORKT ~ FAMINCOME)
Restrictf <- (dat_ATUS$GENID_DESCRIBE == “female”)
Restrictm <- (dat_ATUS$GENID_DESCRIBE == “male)
data_newf <- subset(dat_ATUS, restrictf)
data_newm <- subset(dat_ATUS, restrictm)

ACT_PCARE = sample(c("max", "min"), 1140, replace = TRUE)
x = data.frame(HADEDUC, 9th grade)
CrossTable(HADEDUC, 9th grade)
sd(summary(HADEDUC, 9th grade))
t.test(summary(data_newm$EDUC),var.equal = TRUE)
sd(summary(data_newf$EDUC))
t.test(summary(data_newf$EDUC),var.equal = TRUE)



Question 6: 

attach(dat_ATUS)
summary(dat_ATUS)
table(dat_ATUS)
any_time_childcare <- (dat_ATUS$ACT_childcare >0
any_time_childcare ~ AGE + SEX + RACE + HISPAN + MARST + EDUC                    
d_childcare <- data.frame(model.matrix(~ dat_ATUS$CARENHH))

install.packages("standardize")

require("standardize")
set.seed(654321)
NN <- length(dat_for_analysis_sub$any_time_childcare)
restrict_1 <- (runif(NN) < 0.1)
summary(restrict_1)
dat_train <- subset(dat_for_analysis_sub, restrict_1)
dat_test <- subset(dat_for_analysis_sub, !restrict_1)


install.packages("tidyverse")

summary(dat_ATUS$EDUC_r)
d_educ_r <- data.frame(model.matrix(~ dat_ATUS$EDUC_r))


summary(ACT_CARENHH)
levels(dat_ATUS$CARENHH)
d_marstat <- data.frame(model.matrix(~ dat_ATUS$MARST))
d_race <- data.frame(model.matrix(~ dat_ATUS$RACE)) 
d_sex <- data.frame(model.matrix(~ dat_ATUS$SEX)
                    set.seed(654321))
d_region <- data.frame(model.matrix(~ dat_ATUS$REGION))


 THEY DO NOT SEEM EXOGENOUS.

it does not seem plausible. The statistics are statistically sugnificant.
I found binomial and logistic model error.




Qiuestion 7

attach(dat_ATUS)
summary(dat_ATUS)
summary(model_logit1)
model_logit1 <- glm(number of hours~ CAREHH,
family = binomial, data = dat_ATUS)
table(dat_ATUS$CAREHH,dat_ATUS$CAREHH)


pred_vals <- predict(model_logit1, s_dat_test, type = "response")
pred_model_logit1 <- (pred_vals > 0.5)
table(pred = pred_model_logit1, true = dat_test$CAREHH)
summary(as.numeric(dat_ATUS$CAREHH))
model_logit1 <- glm(sobj$formula, family = binomial, data = sobj$data)

table(pred = pred_model_lpm1, true = dat_test$number_of_care_hours)
pred_vals_lpm <- predict(model_lpm1, s_dat_test)

They do not seem exogenous. Polynomials in age are important with dummy variables.
they do not seem plausible.
two types of error are made by thge model. these are binomial and logistic model error.




Question 8

Now I am going to use some fancy model instead of OLS or logit.

install.packages("standardize")
require("standardize")
NN <- length(dat_for_analysis_sub$any_time_sports)
restrict_1 <- (runif(NN) < 0.1)
summary(restrict_1)
dat_train <- subset(dat_for_analysis_sub, restrict_1)
dat_test <- subset(dat_for_analysis_sub, !restrict_1)

names(dat_for_analysis_sub)
names(dat_for_analysis_sub) <- sub("dat_ATUS.","",names(dat_for_analysis_sub))
names(dat_for_analysis_sub) <- sub("_r",".",names(dat_for_analysis_sub))
names(dat_for_analysis_sub)[1] <- "any_time_sports"
names(dat_for_analysis_sub)[2] <- "AGE"
names(dat_for_analysis_sub)[18] <- "Hispanic"
names(dat_for_analysis_sub)[19] <- "SEX"

install.packages("tidyverse")
require(tidyverse)
any_time_sports <- (dat_ATUS$ACT_SPORTS > 0)
names(dat_for_analysis_sub)

install.packages("randomForest")
set.seed
model_randFor <- randomForest(as.factor(any_time_sports) ~ ., data = sobj$data, importance=TRUE, proximity=TRUE)
print(model_randFor)
round(importance(model_randFor),)
varImpPlot(model_randFor)

pred_model1 <- predict(model_randFor,  s_dat_test)
table(pred = pred_model1, true = dat_test$any_time_sports)


install.packages("e1071")
 
tuned_parameters <- tune.svm(as.factor(any_time_sports) ~ ., data = sobj$data, gamma = 10^(-3:0), cost = 10^(-2:2)) 
# summary(tuned_parameters)
require(e1071)


install.packages("elasticnet")
require(glmnet)
model1_elasticnet <-  glmnet(as.matrix(sobj$data[,-1]),sobj$data$any_time_sports) 
print(model1_elasticnet)

cvmodel1_elasticnet = cv.glmnet(data.matrix(sobj$data[,-1]),data.matrix(sobj$data$any_time_sports)) 
cvmodel1_elasticnet$lambda.min
log(cvmodel1_elasticnet$lambda.min)
coef(cvmodel1_elasticnet, s = "lambda.min")

pred1_elasnet <- predict(model1_elasticnet, newx = data.matrix(s_dat_test), s = cvmodel1_elasticnet$lambda.min)
pred_model1_elasnet <- (pred1_elasnet < mean(pred1_elasnet)) 
table(pred = pred_model1_elasnet, true = dat_test$any_time_sports)

model2_elasticnet <-  glmnet(as.matrix(sobj$data[,-1]),sobj$data$any_time_sports, alpha = 0) 
or try different alpha values to see if you can improve
print(model2_elasticnet
      
install.packages("spikeslab")
require(spikeslab)
set.seed(15839)
model1_spikeslab <- spikeslab(sobj$formula, data = sobj$data)
summary(model1_spikeslab)
print(model1_spikeslab)
plot(model1_spikeslab)



Qu 2



