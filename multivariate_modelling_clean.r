library(languageR)
library(dplyr)
library(ggplot2)
library(ggstatsplot)
library(lme4)
library(lmerTest)
library(dplyr)
library(psych)
library(trend)
library(reshape)
library(reshape2)
library(readxl)
library(car)
library(PerformanceAnalytics)
library(Hmisc)
# Load the data
data = read.csv("data_SOTU.csv", header = TRUE, sep = ",")

# Modify genre values
data$genre[data$genre == "written"] <- "Written"

# Summary of the data
summary(data)

subj = data[data$role.in.sub != "obj",]
obj = data[data$role.in.sub == "obj",]

data$role.in.sub[data$role.in.sub != 'obj'] <- "subj"
# Subject relative clauses

# minimal adequate model
# LoE
LoE = glm(as.factor(relativizer)~LoE, family = 'binomial', subj)
summary(LoE)
Anova(LoE, test = 'Wald')

# distance
distance = glm(as.factor(relativizer)~distance, family = 'binomial', subj)
summary(distance)
Anova(distance, test = 'Wald')

# antecedent_length
antecedent_length = glm(as.factor(relativizer)~antecedent_length, family = 'binomial', subj)
summary(antecedent_length)
Anova(antecedent_length, test = 'Wald')

# nested
nested = glm(as.factor(relativizer)~nested, family = 'binomial', subj)
summary(nested)
Anova(nested, test = 'Wald')

# preceding_relativizer
preceding_relativizer = glm(as.factor(relativizer)~preceding_relativizer, family = 'binomial', subj)
summary(preceding_relativizer)
Anova(preceding_relativizer, test = 'Wald')

# ttr
ttr = glm(as.factor(relativizer)~ttr, family = 'binomial', subj)
summary(ttr)
Anova(ttr, test = 'Wald')

# mean_word_length
mean_word_length = glm(as.factor(relativizer)~mean_word_length, family = 'binomial', subj)
summary(mean_word_length)
Anova(mean_word_length, test = 'Wald')

# mean_sentence_length
mean_sentence_length = glm(as.factor(relativizer)~mean_sentence_length, family = 'binomial', subj)
summary(mean_sentence_length)
Anova(mean_sentence_length, test = 'Wald')

# nouns_per_10000_words
nouns_per_10000_words = glm(as.factor(relativizer)~nouns_per_10000_words, family = 'binomial', subj)
summary(nouns_per_10000_words)
Anova(nouns_per_10000_words, test = 'Wald')

# personal_pronouns_per_10000_words
personal_pronouns_per_10000_words = glm(as.factor(relativizer)~personal_pronouns_per_10000_words, family = 'binomial', subj)
summary(personal_pronouns_per_10000_words)
Anova(personal_pronouns_per_10000_words, test = 'Wald')

# genre
genre = glm(as.factor(relativizer)~genre, family = 'binomial', subj)
summary(genre)
Anova(genre, test = 'Wald')

# year
year = glm(as.factor(relativizer)~year, family = 'binomial', subj)
summary(year)
Anova(year, test = 'Wald')


subj_2 = subj[,c('relativizer', 'LoE', 'distance',"antecedent_length","nested","preceding_relativizer", 
"mean_word_length","mean_sentence_length","personal_pronouns_per_10000_words","genre",'year','President')]


cor(subj_2$personal_pronouns_per_10000_words, subj_2$mean_word_length)

# log transformation of LoE, mean_sentence_length, personal_pronouns_per_10000_words
subj_2$LoE = log(as.numeric(subj_2$LoE))
subj_2$distance = as.numeric(subj_2$distance)
subj_2$year = as.numeric(subj_2$year)
subj_2$antecedent_length = as.numeric(subj_2$antecedent_length)
subj_2$mean_word_length = as.numeric(subj_2$mean_word_length)
subj_2$mean_sentence_length = log(as.numeric(subj_2$mean_sentence_length))
subj_2$personal_pronouns_per_10000_words = log(as.numeric(subj_2$personal_pronouns_per_10000_words))



numbers = c("year", "distance", "LoE",  "antecedent_length", "mean_word_length", "mean_sentence_length", "personal_pronouns_per_10000_words")

subj_3 = subj_2[,numbers]
subj_3 = as.matrix(subj_3)
plot(varclus(subj_3))



# plot
theme_set(theme_bw(base_size = 10, base_line_size = 1))

avg_RC_L <- aggregate(distance ~ year, data, mean)
avg_RC_L

ggplot(avg_RC_L, aes(x = year, y = distance)) +
# , group = role.in.sub,  color = role.in.sub
  geom_point(size=1) +
  geom_smooth() +
  labs(x = "Year", y = "Distance")
  # scale_color_manual(values = c("subj" = "black", "obj" = "grey"))
  ggsave("09_Distance_by_year.jpg", width = 3.60, height = 1.80, dpi = 1500)


data$LoE = as.numeric(data$LoE)
avg_LoE <- aggregate(LoE ~ year, data, mean)
avg_LoE

ggplot(avg_LoE, aes(x = year, y = LoE)) +
  geom_point(size=1) +
  geom_smooth() +
  labs(x = "Year", y = "Relative clause length")
  ggsave("09_RC_length_by_year.jpg", width = 3.60, height = 1.80, dpi = 1500)

avg_ant_l <- aggregate(antecedent_length ~ year, data, mean)
ggplot(avg_ant_l, aes(x = year, y = antecedent_length)) +
  geom_point(size=1) +
  geom_smooth() +
  labs(x = "Year", y = "Head noun length")
  ggsave("09_Antecedent_length_by_year.jpg", width = 3.60, height = 1.80, dpi = 1500)


ggplot(data, aes(x = year, y = mean_word_length)) +
  geom_point(size=1) +
  geom_smooth() +
  labs(x = "Year", y = "Mean word length")
  ggsave("09_Mean_word_length_by_year.jpg", width = 3.60, height = 1.80, dpi = 1500)



ggplot(data, aes(x = year, y = mean_sentence_length)) +
  geom_point(size=1) +
  geom_smooth() +
  labs(x = "Year", y = "Mean sentence length")
  ggsave("09_Mean_sentence_length_by_year.jpg", width = 3.60, height = 1.80, dpi = 1500)

ggplot(data, aes(x = year, y = personal_pronouns_per_10000_words)) +
  geom_point(size=1) +
  geom_smooth() +
  labs(x = "Year", y = "Personal pronouns")
  ggsave("09_Personal_pronouns_by_year.jpg", width = 3.60, height = 1.80, dpi = 1500)


ggplot(data, aes(x = year, y = nouns_per_10000_words)) +
  geom_point(size=1) +
  geom_smooth() +
  labs(x = "Year", y = "Nouns")
  ggsave("09_Nouns_by_year.png", width = 3.60, height = 1.80, dpi = 330)


library(lme4)
# distance + LoE + year  
mm_model <- glmer(as.factor(relativizer) ~ 
                    distance +
                    LoE +
                    # year+ 
                    antecedent_length + 
                    ## nested +
                    preceding_relativizer +
                    mean_word_length +
                    ## mean_sentence_length +
                    ## personal_pronouns_per_10000_words +
                    ## genre +
                    (1  | President) , 
                  family = binomial, 
                  control=glmerControl(optimizer="bobyqa"),
                  data = subj_2)


summary(mm_model)

# Random effects:
#  Groups    Name        Variance Std.Dev.
#  President (Intercept) 3.13     1.769
# Number of obs: 8255, groups:  President, 43

# Fixed effects:
#                             Estimate Std. Error z value Pr(>|z|)
# (Intercept)                -10.64051    1.40435  -7.577 3.54e-14 ***
# distance                     0.06876    0.01240   5.544 2.96e-08 ***
# LoE                          0.39386    0.04826   8.160 3.34e-16 ***
# antecedent_length            0.06594    0.01154   5.715 1.10e-08 ***
# preceding_relativizerthat   -0.98616    0.25302  -3.898 9.72e-05 ***
# preceding_relativizerwhich  -0.16734    0.25024  -0.669   0.5037
# preceding_relativizerzero   -0.47249    0.25875  -1.826   0.0678 .
# mean_word_length             2.38654    0.31505   7.575 3.59e-14 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Correlation of Fixed Effects:
#                (Intr) distnc LoE    antcd_ prcdng_rltvzrt prcdng_rltvzrw
# distance       -0.025
# LoE            -0.078 -0.133
# antcdnt_lng    -0.024 -0.026 -0.026
# prcdng_rltvzrt -0.145 -0.020 -0.008 -0.013
# prcdng_rltvzrw -0.134 -0.013 -0.004 -0.004  0.958
# prcdng_rltvzrz -0.150 -0.016 -0.002 -0.007  0.929          0.935
# mn_wrd_lngt    -0.960  0.003  0.009 -0.033 -0.023         -0.039
#                prcdng_rltvzrz
# distance
# LoE
# antcdnt_lng
# prcdng_rltvzrt
# prcdng_rltvzrw
# prcdng_rltvzrz
# mn_wrd_lngt    -0.016

vif(mm_model)
plot(mm_model)

plot(log(subj_2$distance))

# Exponentiate coefficients to calculate odds ratios
coef_estimates <- fixef(mm_model)
odds_ratios <- exp(coef_estimates)
odds_ratios

#                (Intercept)                   distance
#               2.392675e-05               1.071177e+00
#                        LoE          antecedent_length
#               1.482695e+00               1.068167e+00
#  preceding_relativizerthat preceding_relativizerwhich
#               3.730068e-01               8.459111e-01
#  preceding_relativizerzero           mean_word_length
#               6.234466e-01               1.087577e+01


predicted <- predict(mm_model, type = "response")
predicted_class <- ifelse(predicted>0.5, 1, 0)
actual_class <- ifelse(subj_2$relativizer == "which", 1, 0)
correctly_predicted <- sum(predicted_class == actual_class)/length(actual_class)
correctly_predicted #  0.8055724

#baseline
class_proportions <- table(actual_class) / length(actual_class)
majority_class_proportion <- max(class_proportions)
majority_class_proportion # 0.6512417

# Somers' Dxy
# install.packages('irr')
library('Hmisc')
somers_dxy <- somers2(actual_class, predicted_class)
somers_dxy

  #          C          Dxy            n      Missing
  #  0.8027371    0.6054743 8255.0000000    0.0000000




# object relative clauses
obj_2 = obj[,c('relativizer', 'LoE', 'distance',"antecedent_length","nested","preceding_relativizer",
"ttr","mean_word_length","mean_sentence_length","nouns_per_10000_words","personal_pronouns_per_10000_words",
"genre",'year','President')]


obj_2$relativizer[obj_2$relativizer %in% c("which", "that")] <- "which/that"
obj_2$relativizer <- as.factor(obj_2$relativizer)


obj_2$distance = as.numeric(obj_2$distance)
obj_2$LoE = log(as.numeric(obj_2$LoE))
obj_2$year = as.numeric(obj_2$year)
obj_2$antecedent_length = as.numeric(obj_2$antecedent_length)
obj_2$mean_word_length = obj_2$mean_word_length
obj_2$mean_sentence_length = log(as.numeric(obj_2$mean_sentence_length))
obj_2$nouns_per_10000_words = log(obj_2$nouns_per_10000_words)
obj_2$personal_pronouns_per_10000_words = log(obj_2$personal_pronouns_per_10000_words)


# minimal adequate model
# LoE
LoE = glm(as.factor(relativizer)~LoE, family = 'binomial', obj_2)
summary(LoE)
Anova(LoE, test = 'Wald')

# distance
distance = glm(as.factor(relativizer)~distance, family = 'binomial', obj_2)
summary(distance)
Anova(distance, test = 'Wald')

# antecedent_length
antecedent_length = glm(as.factor(relativizer)~antecedent_length, family = 'binomial', obj_2)
summary(antecedent_length)
Anova(antecedent_length, test = 'Wald')

# nested
nested = glm(as.factor(relativizer)~nested, family = 'binomial', obj_2)
summary(nested)
Anova(nested, test = 'Wald')

# preceding_relativizer
preceding_relativizer = glm(as.factor(relativizer)~preceding_relativizer, family = 'binomial', obj_2)
summary(preceding_relativizer)
Anova(preceding_relativizer, test = 'Wald')

# ttr
ttr = glm(as.factor(relativizer)~ttr, family = 'binomial', obj_2)
summary(ttr)
Anova(ttr, test = 'Wald')

# mean_word_length
mean_word_length = glm(as.factor(relativizer)~mean_word_length, family = 'binomial', obj_2)
summary(mean_word_length)
Anova(mean_word_length, test = 'Wald')

# mean_sentence_length
mean_sentence_length = glm(as.factor(relativizer)~mean_sentence_length, family = 'binomial', obj_2)
summary(mean_sentence_length)
Anova(mean_sentence_length, test = 'Wald')

# nouns_per_10000_words
nouns_per_10000_words = glm(as.factor(relativizer)~nouns_per_10000_words, family = 'binomial', obj_2)
summary(nouns_per_10000_words)
Anova(nouns_per_10000_words, test = 'Wald')

# personal_pronouns_per_10000_words
personal_pronouns_per_10000_words = glm(as.factor(relativizer)~personal_pronouns_per_10000_words, family = 'binomial', obj_2)
summary(personal_pronouns_per_10000_words)
Anova(personal_pronouns_per_10000_words, test = 'Wald')

# genre
genre = glm(as.factor(relativizer)~genre, family = 'binomial', obj_2)
summary(genre)
Anova(genre, test = 'Wald')

# year
year = glm(as.factor(relativizer)~year, family = 'binomial', obj_2)
summary(year)
Anova(year, test = 'Wald')

# LoE:year
LoE_year = glm(as.factor(relativizer)~LoE*year, family = 'binomial', obj_2)
summary(LoE_year)
Anova(LoE_year, test = 'Wald')



variables = c('relativizer','LoE', 'distance',"antecedent_length","preceding_relativizer",
"mean_word_length","mean_sentence_length","nouns_per_10000_words","personal_pronouns_per_10000_words",'genre','year','President')

obj_3 = obj_2[,variables]


mm_model_obj <- glmer(as.factor(relativizer) ~ 
                    distance +
                    LoE +
                    # year +
                    # antecedent_length + 
                    # nested +
                    # preceding_relativizer +
                    # mean_word_length +
                    mean_sentence_length +
                    # nouns_per_10000_words +
                    # personal_pronouns_per_10000_words +
                    # (1 | genre) + 
                    (1  | President) , 
                  family = binomial, 
                  control=glmerControl(optimizer="bobyqa"), 
                  data = obj_3)

# Random effects:
#  Groups    Name        Variance Std.Dev.
#  President (Intercept) 0.8925   0.9447
# Number of obs: 4174, groups:  President, 43

# Fixed effects:
#                      Estimate Std. Error z value Pr(>|z|)
# (Intercept)           8.29112    1.30558   6.351 2.15e-10 ***
# distance             -0.71304    0.03226 -22.100  < 2e-16 ***
# LoE                  -0.56986    0.06611  -8.620  < 2e-16 ***
# mean_sentence_length -1.37984    0.38692  -3.566 0.000362 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Correlation of Fixed Effects:
#             (Intr) distnc LoE
# distance    -0.096
# LoE         -0.018 -0.176
# mn_sntnc_ln -0.986  0.021 -0.061

summary(mm_model_obj)
summary(mm_model_obj)$coefficients
plot(mm_model)

vif(mm_model_obj)
# Exponentiate coefficients to calculate odds ratios
coef_estimates <- fixef(mm_model_obj)
odds_ratios <- exp(coef_estimates)
odds_ratios

#          (Intercept)             distance                  LoE
#         3988.2898168            0.4901506            0.5656049
# mean_sentence_length
#            0.2516200

predicted <- predict(mm_model_obj, type = "response")
predicted_class <- ifelse(predicted>0.5, 1, 0)
actual_class <- ifelse(obj_3$relativizer == "zero", 1, 0)
correctly_predicted <- sum(predicted_class == actual_class)/length(actual_class)
correctly_predicted # 0.8231912

#baseline
class_proportions <- table(actual_class) / length(actual_class)
majority_class_proportion <- max(class_proportions)
majority_class_proportion # 0.5059895

# Somers' Dxy
# install.packages('irr')
# install.packages('Hmisc')
# install.packages("caret", dependencies = c("Depends", "Suggests"))
library('Hmisc')
somers_dxy <- somers2(actual_class, predicted_class)
somers_dxy

  #          C          Dxy            n      Missing
  #  0.8233667    0.6467335 4174.0000000    0.0000000


# install.packages('PerformanceAnalytics')
library('PerformanceAnalytics')
chart.Correlation(subj_3)
