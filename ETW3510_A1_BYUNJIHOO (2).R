# ETW3510_Applied Econometrics Methods
# Assessment_01:Logit/Probit Model

rm(list=ls())
library(tidyverse)
library(wooldridge)
library(gtsummary)
library(stargazer)
library(dplyr)
library(ggplot2)
library(car)
library(pscl)
library(stargazer)
library(margins)

# Loading the csv file data.
df <- read.csv("/Users/jihoo/Desktop/MONASH/Assignments/ETW3510/A1/computer-use.csv")

# Set seed number using last 4 digits of YOUR Student ID number
set.seed(8945)

# Draw sub-sample with 380 observations, and save it as df_sample
df_sample <- df[sample(nrow(df), size = 380, replace = FALSE),]

summary(df_sample)

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# = = = = = = E x p l o r a t o r y   D a t a   A n a l y s i s = = = = = = 
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# Summary Statistics
df_sample %>%
  group_by(Use) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))  

# Factorising 'Use' variable in to 0=Not_frequent and 1=Frequent
df_sample$Use <- factor(df_sample$Use, levels = c(0,1),
                        labels = c("Not_frequent" , "Frequent"))

# Frequency table of each variables
tbl_summary(df_sample, statistic = list(all_continuous() ~ c("{mean}, {sd}")))

# Dependent variable counting and frequency table
table(df_sample$Use)
prop.table(table(df_sample$Use))


# Visualization of 'Allowance' demographic variable relationship and 'Use'

df_sample %>%
  mutate(
    Use = trimws(Use),
    Allowance = factor(Allowance, levels = c("None", "RM1-RM5", "RM6-RM10", "More than RM10"))
  ) %>%
  filter(Use == "Frequent") %>%
  count(Allowance) %>%
  ggplot(aes(x = Allowance, y = n)) +
  geom_col(fill = "blue", width = 0.5) +
  labs(
    title = "Number of Frequent Users for Allowance",
    x = "Allowance",
    y = "Frequent count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))



# Visualisation of UTAUT(FC.Int) variable and 'Use' relationship
df_sample %>%
  mutate(
    FC.Int_bin = cut(FC.Int,
                     breaks = c(0, 1, 2, 3, 4, 5),
                     include.lowest = TRUE,
                     right = FALSE,
                     labels = c("0~0.99", "1~1.99", "2~2.99", "3~3.99", "4~5")),
    Use_binary = ifelse(Use == "Frequent", 1, 0)
  ) %>%
  group_by(FC.Int_bin) %>%
  summarise(freq_rate = mean(Use_binary)) %>%
  ggplot(aes(x = FC.Int_bin, y = freq_rate)) +
  geom_col(fill = "darkgreen", width = 0.5) +
  labs(
    title = "Frequent Use Rate by FC.Int Score Range",
    x = "FC.Int Range",
    y = "Frequent Use Rate"
  ) +
  theme_minimal()

# Visualisation of SI.Family and Use
df_sample %>%
  mutate(
    SI.Family_bin = cut(SI.Family,
                        breaks = c(0, 1, 2, 3, 4, 5),
                        include.lowest = TRUE,
                        right = FALSE,
                        labels = c("0~0.99", "1~1.99", "2~2.99", "3~3.99", "4~5")),
    Use_binary = ifelse(Use == "Frequent", 1, 0)
  ) %>%
  group_by(SI.Family_bin) %>%
  summarise(freq_rate = mean(Use_binary)) %>%
  ggplot(aes(x = SI.Family_bin, y = freq_rate)) +
  geom_col(fill = "red", width = 0.5) +
  labs(
    title = "Exploratory Analysis: Frequent Use Rate by SI.Family Score Range",
    x = "SI.Family Range (by Integer)",
    y = "Frequent Use Rate"
  ) +
  theme_minimal()


# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# = = = = = = = = = = E m p i r i c a l   A n a l y s i s = = = = = = = = = =
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# Factorizing the 'Location', Gender' and 'Allowance'.
df_sample$Location <- as.factor(df_sample$Location)
df_sample$Gender <- as.factor(df_sample$Gender)
df_sample$Allowance <- as.factor(df_sample$Allowance)


# --- logit model building---
logit_model_01 <- glm(Use ~ Age + Gender + Allowance + Location + PerfExp + EffExp + 
                        SI.Family + FC.Ext,
                      data = df_sample,
                      family = binomial(link = "logit"))

logit_model_02 <- glm(Use ~ Age + Gender + Allowance + Location + PerfExp + EffExp + 
                        SI.Family + FC.Int,
                      data = df_sample,
                      family = binomial(link = "logit"))

logit_model_03 <- glm(Use ~ Age + Gender + Allowance + Location + PerfExp + EffExp + 
                        SI.Teachers + FC.Ext,
                      data = df_sample,
                      family = binomial(link = "logit"))

logit_model_04 <- glm(Use ~ Age + Gender + Allowance + Location + PerfExp + EffExp + 
                        SI.Teachers + FC.Int,
                      data = df_sample,
                      family = binomial(link = "logit"))

logit_model_05 <- glm(Use ~ Age + Gender + Allowance + Location + PerfExp + EffExp + 
                        SI.Peers + FC.Ext,
                      data = df_sample,
                      family = binomial(link = "logit"))

logit_model_06 <- glm(Use ~ Age + Gender + Allowance + Location + PerfExp + EffExp + 
                        SI.Peers + FC.Int,
                      data = df_sample,
                      family = binomial(link = "logit"))



# ACCURACY
df_sample$Use <- ifelse(df_sample$Use == "Frequent", 1, 0)

pred_Lgt1 <- ifelse(predict(logit_model_01, type = "response") > 0.5, 1, 0)
pred_Lgt2 <- ifelse(predict(logit_model_02, type = "response") > 0.5, 1, 0)
pred_Lgt3 <- ifelse(predict(logit_model_03, type = "response") > 0.5, 1, 0)
pred_Lgt4 <- ifelse(predict(logit_model_04, type = "response") > 0.5, 1, 0)
pred_Lgt5 <- ifelse(predict(logit_model_05, type = "response") > 0.5, 1, 0)
pred_Lgt6 <- ifelse(predict(logit_model_06, type = "response") > 0.5, 1, 0)


actual <- df_sample$Use

acc_L1 <- mean(pred_Lgt1 == actual)
acc_L2 <- mean(pred_Lgt2 == actual)
acc_L3 <- mean(pred_Lgt3 == actual)
acc_L4 <- mean(pred_Lgt4 == actual)
acc_L5 <- mean(pred_Lgt5 == actual)
acc_L6 <- mean(pred_Lgt6 == actual)



# Total comparision of logit model
cat("                ", "logit_model_01 |","logit_model_02 |","logit_model_03 |","logit_model_04 |","logit_model_05 |","logit_model_06 |", "\n",
    "  AIC", "           ", AIC(logit_model_01), "    |  ",AIC(logit_model_02), "    |  ",AIC(logit_model_03),"    |  ",AIC(logit_model_04),"    |  ",AIC(logit_model_05),"    |  ",AIC(logit_model_06),"\n",
    "  BIC", "           ", BIC(logit_model_01), "    |  ",BIC(logit_model_02), "    |  ",BIC(logit_model_03),"    |  ",BIC(logit_model_04),"    |  ",BIC(logit_model_05),"    |  ",BIC(logit_model_06),"\n",
    "log-lik", "        ", logLik(logit_model_01), "    |  ",logLik(logit_model_02), "   |  ",logLik(logit_model_03),"   |  ",logLik(logit_model_04),"   |  ",logLik(logit_model_05),"   |  ",logLik(logit_model_06),"\n",
    "McFadden's R²   ", pR2(logit_model_01)["McFadden"], "   |  ",pR2(logit_model_02)["McFadden"],"  |  ",pR2(logit_model_03)["McFadden"],"  |  ",pR2(logit_model_04)["McFadden"],"  |  ",pR2(logit_model_05)["McFadden"],"  |  ",pR2(logit_model_06)["McFadden"], "\n",
    "Accuracy           ", round(acc_L1 * 100, 2), "     |    ", round(acc_L2 * 100, 2), "     |    ",round(acc_L3 * 100, 2), "     |     ",round(acc_L4 * 100, 2), "    |     ",round(acc_L5 * 100, 2), "    |    ",round(acc_L6 * 100, 2))






# --- Probit model---
probit_model_01 <- glm(Use ~ Age + Gender + Allowance + Location + PerfExp + EffExp + 
                         SI.Family + FC.Ext,
                       data = df_sample,
                       family = binomial(link = "probit"))

probit_model_02 <- glm(Use ~ Age + Gender + Allowance + Location + PerfExp + EffExp + 
                         SI.Family + FC.Int,
                       data = df_sample,
                       family = binomial(link = "probit"))

probit_model_03 <- glm(Use ~ Age + Gender + Allowance + Location + PerfExp + EffExp + 
                         SI.Teachers + FC.Ext,
                       data = df_sample,
                       family = binomial(link = "probit"))

probit_model_04 <- glm(Use ~ Age + Gender + Allowance + Location + PerfExp + EffExp + 
                         SI.Teachers + FC.Int,
                       data = df_sample,
                       family = binomial(link = "probit"))

probit_model_05 <- glm(Use ~ Age + Gender + Allowance + Location + PerfExp + EffExp + 
                         SI.Peers + FC.Ext,
                       data = df_sample,
                       family = binomial(link = "probit"))

probit_model_06 <- glm(Use ~ Age + Gender + Allowance + Location + PerfExp + EffExp + 
                         SI.Peers + FC.Int,
                       data = df_sample,
                       family = binomial(link = "probit"))



# ACCURACY

pred_Prb1 <- ifelse(predict(probit_model_01, type = "response") > 0.5, 1, 0)
pred_Prb2 <- ifelse(predict(probit_model_02, type = "response") > 0.5, 1, 0)
pred_Prb3 <- ifelse(predict(probit_model_03, type = "response") > 0.5, 1, 0)
pred_Prb4 <- ifelse(predict(probit_model_04, type = "response") > 0.5, 1, 0)
pred_Prb5 <- ifelse(predict(probit_model_05, type = "response") > 0.5, 1, 0)
pred_Prb6 <- ifelse(predict(probit_model_06, type = "response") > 0.5, 1, 0)

acc_P1 <- mean(pred_Prb1 == actual)
acc_P2 <- mean(pred_Prb2 == actual)
acc_P3 <- mean(pred_Prb3 == actual)
acc_P4 <- mean(pred_Prb4 == actual)
acc_P5 <- mean(pred_Prb5 == actual)
acc_P6 <- mean(pred_Prb6 == actual)


# Total comparision of probit model
cat("                ", "probit_model_01 |","probit_model_02 |","probit_model_03 |","probit_model_04 |","probit_model_05 |","probit_model_06 |", "\n",
    "  AIC", "           ", AIC(probit_model_01), "    |  ",AIC(probit_model_02), "    |  ",AIC(probit_model_03),"    |  ",AIC(probit_model_04),"    |  ",AIC(probit_model_05),"    |  ",AIC(probit_model_06),"\n",
    "  BIC", "            ", BIC(probit_model_01), "     |  ",BIC(probit_model_02), "    |  ",BIC(probit_model_03),"    |  ",BIC(probit_model_04),"    |  ",BIC(probit_model_05),"    |  ",BIC(probit_model_06),"\n",
    "log-lik", "        ", logLik(probit_model_01), "    |  ",logLik(probit_model_02), "   |  ",logLik(probit_model_03),"   |  ",logLik(probit_model_04),"   |  ",logLik(probit_model_05),"   |  ",logLik(probit_model_06),"\n",
    "McFadden's R²   ", pR2(probit_model_01)["McFadden"], "   |  ",pR2(probit_model_02)["McFadden"],"   |  ",pR2(probit_model_03)["McFadden"],"  |  ",pR2(probit_model_04)["McFadden"],"  |  ",pR2(probit_model_05)["McFadden"],"  |  ",pR2(probit_model_06)["McFadden"], "\n",
    "Accuracy           ", round(acc_P1 * 100, 2), "     |    ", round(acc_P2 * 100, 2), "        |    ",round(acc_P3 * 100, 2), "        |     ",round(acc_P4 * 100, 2), "    |     ",round(acc_P5 * 100, 2), "    |    ",round(acc_P6 * 100, 2))



# Ultimate Model:

best_logit <- logit_model_02
summary(best_logit)

best_probit <- probit_model_02
summary(best_probit)



# Hypothesis Testing (Joint Significant)
fit.estimator <- estimator
nullmod.estimator <- glm(Use~ 1, data = df_sample, family = binomial(link = "logit"))

LU = logLik(fit.estimator)
LR = logLik(nullmod.estimator)

LR_calc = 2*(LU-LR)
LR_crit = qchisq(0.95, 10)


cat("          LU", "          LR", "\n",
    "value", logLik(fit.estimator),"  ",  logLik(nullmod.estimator), "\n",
    "calc", "               ", LR_calc, "\n",
    "crit", "               ", LR_crit)

if (LR_calc > LR_crit) {
  print("The Null-Hypothesis is REJECTED")
}



#  = =  C O M P A R I S I O N   = = 
pred_best_logit <- ifelse(predict(best_logit, type = "response") > 0.5, 1, 0)
acc_best_logit <- mean(pred_best_logit == actual)

pred_best_probit <- ifelse(predict(best_probit, type = "response") > 0.5, 1, 0)
acc_best_probit <- mean(pred_best_probit == actual)

cat("                ", "logit_model ","Probit model", "\n",
    "  AIC", "          ", AIC(best_logit), "  ",AIC(best_probit),"\n",
    "  BIC", "          ", BIC(best_logit), "  ", BIC(best_probit), "\n",
    "log-lik        ", logLik(best_logit), " ", logLik(best_probit),"\n",
    "McFadden's R²  ", pR2(best_logit)["McFadden"]," ",  pR2(best_probit)["McFadden"], "\n",
    "Accuracy          ", round(acc_best_logit * 100, 2),"      ",round(acc_best_probit * 100, 2))


# Selecting the ultimate model
estimator <- best_logit
pred_estimator <- ifelse(predict(estimator, type = "response") > 0.5, 1, 0)
acc_estimator <- mean(pred_estimator == actual)
acc_estimator

#  (i) Which variables significantly influence a student’s use of computers for learning?
#      Which variables increase or decrease the probability of a student’s use of computers for learning?

summary(estimator)

estimator

stargazer(estimator, type = "text")


# McFadden R^2
pR2(estimator)["McFadden"]
# 0.3665382 

# Marginal effect of Logit Model
summary(margins(estimator))

# Visualisation of computer use by categories of Location

# Probability of Male_none
pred.Urban.M0 <- predict(estimator, newdata = data.frame(Use = "Frequent",
                                                         Gender = "Male",
                                                         Allowance = "None",
                                                         Location = "Urban",
                                                         Age = mean(df_sample$Age),
                                                         PerfExp = mean(df_sample$PerfExp),
                                                         EffExp = mean(df_sample$EffExp),
                                                         SI.Family = mean(df_sample$SI.Family),
                                                         FC.Int = mean(df_sample$FC.Int))
                         , type = "response")

pred.Rural.M0 <- predict(estimator, newdata = data.frame(Use = "Frequent",
                                                         Gender = "Male",
                                                         Allowance = "None",
                                                         Location = "Rural",
                                                         Age = mean(df_sample$Age),
                                                         PerfExp = mean(df_sample$PerfExp),
                                                         EffExp = mean(df_sample$EffExp),
                                                         SI.Family = mean(df_sample$SI.Family),
                                                         FC.Int = mean(df_sample$FC.Int))
                         , type = "response")


# Probability of Male_1~5
pred.Urban.M15 <- predict(estimator, newdata = data.frame(Use = "Frequent",
                                                          Gender = "Male",
                                                          Allowance = "RM1-RM5",
                                                          Location = "Urban",
                                                          Age = mean(df_sample$Age),
                                                          PerfExp = mean(df_sample$PerfExp),
                                                          EffExp = mean(df_sample$EffExp),
                                                          SI.Family = mean(df_sample$SI.Family),
                                                          FC.Int = mean(df_sample$FC.Int))
                          , type = "response")

pred.Rural.M15 <- predict(estimator, newdata = data.frame(Use = "Frequent",
                                                          Gender = "Male",
                                                          Allowance = "RM1-RM5",
                                                          Location = "Rural",
                                                          Age = mean(df_sample$Age),
                                                          PerfExp = mean(df_sample$PerfExp),
                                                          EffExp = mean(df_sample$EffExp),
                                                          SI.Family = mean(df_sample$SI.Family),
                                                          FC.Int = mean(df_sample$FC.Int))
                          , type = "response")


# Probability of Male_6~10
pred.Urban.M610 <- predict(estimator, newdata = data.frame(Use = "Frequent",
                                                           Gender = "Male",
                                                           Allowance = "RM6-RM10",
                                                           Location = "Urban",
                                                           Age = mean(df_sample$Age),
                                                           PerfExp = mean(df_sample$PerfExp),
                                                           EffExp = mean(df_sample$EffExp),
                                                           SI.Family = mean(df_sample$SI.Family),
                                                           FC.Int = mean(df_sample$FC.Int))
                           , type = "response")

pred.Rural.M610 <- predict(estimator, newdata = data.frame(Use = "Frequent",
                                                           Gender = "Male",
                                                           Allowance = "RM6-RM10",
                                                           Location = "Rural",
                                                           Age = mean(df_sample$Age),
                                                           PerfExp = mean(df_sample$PerfExp),
                                                           EffExp = mean(df_sample$EffExp),
                                                           SI.Family = mean(df_sample$SI.Family),
                                                           FC.Int = mean(df_sample$FC.Int))
                           , type = "response")



# Probability of Male_10
pred.Urban.M10 <- predict(estimator, newdata = data.frame(Use = "Frequent",
                                                          Gender = "Male",
                                                          Allowance = "More than RM10",
                                                          Location = "Urban",
                                                          Age = mean(df_sample$Age),
                                                          PerfExp = mean(df_sample$PerfExp),
                                                          EffExp = mean(df_sample$EffExp),
                                                          SI.Family = mean(df_sample$SI.Family),
                                                          FC.Int = mean(df_sample$FC.Int))
                          , type = "response")

pred.Rural.M10 <- predict(estimator, newdata = data.frame(Use = "Frequent",
                                                          Gender = "Male",
                                                          Allowance = "More than RM10",
                                                          Location = "Rural",
                                                          Age = mean(df_sample$Age),
                                                          PerfExp = mean(df_sample$PerfExp),
                                                          EffExp = mean(df_sample$EffExp),
                                                          SI.Family = mean(df_sample$SI.Family),
                                                          FC.Int = mean(df_sample$FC.Int))
                          , type = "response")




# ========Female=======
# Probability of Female_none
pred.Urban.F0 <- predict(estimator, newdata = data.frame(Use = "Frequent",
                                                         Gender = "Female",
                                                         Allowance = "None",
                                                         Location = "Urban",
                                                         Age = mean(df_sample$Age),
                                                         PerfExp = mean(df_sample$PerfExp),
                                                         EffExp = mean(df_sample$EffExp),
                                                         SI.Family = mean(df_sample$SI.Family),
                                                         FC.Int = mean(df_sample$FC.Int))
                         , type = "response")

pred.Rural.F0 <- predict(estimator, newdata = data.frame(Use = "Frequent",
                                                         Gender = "Female",
                                                         Allowance = "None",
                                                         Location = "Rural",
                                                         Age = mean(df_sample$Age),
                                                         PerfExp = mean(df_sample$PerfExp),
                                                         EffExp = mean(df_sample$EffExp),
                                                         SI.Family = mean(df_sample$SI.Family),
                                                         FC.Int = mean(df_sample$FC.Int))
                         , type = "response")


# Probability of Female_1~5
pred.Urban.F15 <- predict(estimator, newdata = data.frame(Use = "Frequent",
                                                          Gender = "Female",
                                                          Allowance = "RM1-RM5",
                                                          Location = "Urban",
                                                          Age = mean(df_sample$Age),
                                                          PerfExp = mean(df_sample$PerfExp),
                                                          EffExp = mean(df_sample$EffExp),
                                                          SI.Family = mean(df_sample$SI.Family),
                                                          FC.Int = mean(df_sample$FC.Int))
                          , type = "response")

pred.Rural.F15 <- predict(estimator, newdata = data.frame(Use = "Frequent",
                                                          Gender = "Female",
                                                          Allowance = "RM1-RM5",
                                                          Location = "Rural",
                                                          Age = mean(df_sample$Age),
                                                          PerfExp = mean(df_sample$PerfExp),
                                                          EffExp = mean(df_sample$EffExp),
                                                          SI.Family = mean(df_sample$SI.Family),
                                                          FC.Int = mean(df_sample$FC.Int))
                          , type = "response")


# Probability of Female_6~10
pred.Urban.F610 <- predict(estimator, newdata = data.frame(Use = "Frequent",
                                                           Gender = "Female",
                                                           Allowance = "RM6-RM10",
                                                           Location = "Urban",
                                                           Age = mean(df_sample$Age),
                                                           PerfExp = mean(df_sample$PerfExp),
                                                           EffExp = mean(df_sample$EffExp),
                                                           SI.Family = mean(df_sample$SI.Family),
                                                           FC.Int = mean(df_sample$FC.Int))
                           , type = "response")

pred.Rural.F610 <- predict(estimator, newdata = data.frame(Use = "Frequent",
                                                           Gender = "Female",
                                                           Allowance = "RM6-RM10",
                                                           Location = "Rural",
                                                           Age = mean(df_sample$Age),
                                                           PerfExp = mean(df_sample$PerfExp),
                                                           EffExp = mean(df_sample$EffExp),
                                                           SI.Family = mean(df_sample$SI.Family),
                                                           FC.Int = mean(df_sample$FC.Int))
                           , type = "response")



# Probability of Female_10
pred.Urban.F10 <- predict(estimator, newdata = data.frame(Use = "Frequent",
                                                          Gender = "Female",
                                                          Allowance = "More than RM10",
                                                          Location = "Urban",
                                                          Age = mean(df_sample$Age),
                                                          PerfExp = mean(df_sample$PerfExp),
                                                          EffExp = mean(df_sample$EffExp),
                                                          SI.Family = mean(df_sample$SI.Family),
                                                          FC.Int = mean(df_sample$FC.Int))
                          , type = "response")

pred.Rural.F10 <- predict(estimator, newdata = data.frame(Use = "Frequent",
                                                          Gender = "Female",
                                                          Allowance = "More than RM10",
                                                          Location = "Rural",
                                                          Age = mean(df_sample$Age),
                                                          PerfExp = mean(df_sample$PerfExp),
                                                          EffExp = mean(df_sample$EffExp),
                                                          SI.Family = mean(df_sample$SI.Family),
                                                          FC.Int = mean(df_sample$FC.Int))
                          , type = "response")




pred.Urban.avr <- mean(pred.Urban.M0,pred.Urban.M15,
                       pred.Urban.M610,pred.Urban.M10,
                       pred.Urban.F0,pred.Urban.F15,
                       pred.Urban.F610,pred.Urban.F10)

pred.Rural.avr <- mean(pred.Rural.M0,pred.Rural.M15,
                       pred.Rural.M610,pred.Rural.M10,
                       pred.Rural.F0,pred.Rural.F15,
                       pred.Rural.F610,pred.Rural.F10)


cat("                          ", "Urban", "      ", "Rural", "\n", 
    "  Male_Allowance: 0     ",pred.Urban.M0, "  ", pred.Rural.M0, "\n",
    "  Male_Allowance: 1~5   ", pred.Urban.M15, "  ",pred.Rural.M15, "\n",
    "  Male_Allowance: 6~10  ", pred.Urban.M610,"  ", pred.Rural.M610, "\n",
    "  Male_Allowance: >10   ", pred.Urban.M10, "  ", pred.Rural.M610, "\n",
    "Female_Allowance: 0     ",pred.Urban.F0, "  ",pred.Rural.F0, "\n",
    "Female_Allowance: 1~5   ", pred.Urban.F15, "  ",pred.Rural.F15, "\n",
    "Femael_Allowance: 6~10  ", pred.Urban.F610,"    ", pred.Rural.F610, "\n",
    "Female_Allowance: >10   ", pred.Urban.F10, "  ", pred.Rural.F10, "\n",
    "    Average             ", pred.Urban.avr,"  ",pred.Rural.avr)



vis_data <- data.frame(
  Group = rep(c(
    "Male_0", "Male_1~5", "Male_6~10", "Male_>10",
    "Female_0", "Female_1~5", "Female_6~10", "Female_>10",
    "Average"
  ), each = 2),
  Location = rep(c("Urban", "Rural"), times = 9),
  Probability = c(
    pred.Urban.M0, pred.Rural.M0,
    pred.Urban.M15, pred.Rural.M15,
    pred.Urban.M610, pred.Rural.M610,
    pred.Urban.M10, pred.Rural.M10,
    pred.Urban.F0, pred.Rural.F0,
    pred.Urban.F15, pred.Rural.F15,
    pred.Urban.F610, pred.Rural.F610,
    pred.Urban.F10, pred.Rural.F10,
    pred.Urban.avr, pred.Rural.avr
  )
)


ggplot(vis_data, aes(x = Group, y = Probability, fill = Location)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  scale_fill_manual(values = c("Urban" = "tomato", "Rural" = "skyblue")) +
  labs(
    title = "Predicted Probability of Computer Use by Group (Urban vs Rural)",
    x = "Gender + Allowance Group",
    y = "Predicted Probability",
    fill = "Location"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 14)
  )



# Visualization of probability of computer use across the range of UTAUR variable values.
utaut_vars <- c("PerfExp", "EffExp", "SI.Family", "FC.Int")


age_mean <- mean(df_sample$Age, na.rm = TRUE)



cat_combinations <- expand.grid(
  Gender = unique(df_sample$Gender),
  Allowance = unique(df_sample$Allowance),
  Location = unique(df_sample$Location),
  stringsAsFactors = FALSE
)

result_df <- data.frame()

for (var in utaut_vars) {
  for (val in 1:5) {
    probs <- c()
    for (i in 1:nrow(cat_combinations)) {
      row <- cat_combinations[i, ]
      
      new_data <- data.frame(
        Age = age_mean,
        Gender = row$Gender,
        Allowance = row$Allowance,
        Location = row$Location,
        PerfExp = mean(df_sample$PerfExp, na.rm = TRUE),
        EffExp = mean(df_sample$EffExp, na.rm = TRUE),
        SI.Family = mean(df_sample$SI.Family, na.rm = TRUE),
        FC.Int = mean(df_sample$FC.Int, na.rm = TRUE)
      )
      new_data[[var]] <- val
      prob <- predict(estimator, newdata = new_data, type = "response")
      probs <- c(probs, prob)
    }
    result_df <- bind_rows(result_df, data.frame(
      UTAUT = var,
      Score = as.factor(val),
      MeanProb = mean(probs)
    ))
  }
}



ggplot(result_df, aes(x = UTAUT, y = MeanProb, fill = Score)) +
  geom_col(position = "dodge") +
  scale_fill_brewer(palette = "Set1") +
  labs(
    title = "Predicted Probability of Computer Use by UTAUT Variable and Score",
    x = "UTAUT Variable",
    y = "Predicted Probability",
    fill = "UTAUT Score"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# Average partial effects of two statistically significant non-discrete regressors.

summary(estimator)
# Two most significant regressor:
# PerfExp
# FC.Int 

# APE calculation
xb <- predict(estimator, type = "link")
APE_PerfExp <- mean(dlogis(xb)) * coef(estimator)["PerfExp"]
APE_PerfExp

APE_FC.Int <- mean(dlogis(xb)) * coef(estimator)["FC.Int"]
APE_FC.Int

cat("            Average_Partial_Effect", "\n",
    "PerfExp    ", APE_PerfExp, "\n",
    "FC.Int    ", APE_FC.Int)

# Case application

pred.value <- predict(estimator, newdata = data.frame(Use = "Frequent",
                                                      Gender = "Female",
                                                      Allowance = "RM1-RM5",
                                                      Location = "Rural",
                                                      Age = 12,
                                                      PerfExp = mean(df_sample$PerfExp),
                                                      EffExp = mean(df_sample$EffExp),
                                                      SI.Family = mean(df_sample$SI.Family),
                                                      FC.Int = mean(df_sample$FC.Int))
                      , type = "response")
pred.value
# 0.3665382 
