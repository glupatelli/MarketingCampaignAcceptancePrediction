### EDA ###

library(dplyr)
library(gmodels)
library(corrgram)
library(corrplot)
library(ggplot2)
library(PerformanceAnalytics)
library(readr)

df <- read_csv("C:/Users/Guilherme Lupatelli/Desktop/Git Repository/Project 2 - Marketing Campaign Acceptance Prediction/df_eda.csv")
df <- subset(df, select = -...1)


###  Creating an outlier function ###

# I will consider a sample with more than 3 std of distribution as an outlier

remove_outlier2 <- function(variable, na.rm = TRUE) {
  
  H1 <- mean(variable, na.rm = na.rm) + 3*sd(variable, na.rm = na.rm)
  H2 <- mean(variable, na.rm = na.rm) - 3*sd(variable, na.rm = na.rm)
  
  y <- variable
  
  y[variable < H2] <- NA
  
  y[variable > H1] <- NA
  
  return(y)
}

outlier2 <- function(variable, na.rm = TRUE) {
  
  H1 <- mean(variable, na.rm = na.rm) + 3*sd(variable, na.rm = na.rm)
  H2 <- mean(variable, na.rm = na.rm) - 3*sd(variable, na.rm = na.rm)
  
  y <- variable
  
  y[variable >= H2 & variable <= H1] <- NA
  
  return(y)
}


attach(df)
dim(df)
str(df)
summary(df)


############ Target Variable - Response ############

table(Response)
prop.table(table(Response))


###Features

##Income

#truncate income over de 100k

hist(Income)
boxplot(Income)
boxplot(Income ~ Response) # Comparing with target variable

#Analyzing
par(mfrow = c(2,1))

noout_Income <- remove_outlier2(Income)
noout_Income2 <- noout_Income[!is.na(noout_Income)]
hist(noout_Income)
out_Income <- outlier2(Income)
out_Income2 <- out_Income[!is.na(out_Income)]
hist(out_Income2)
max(noout_Income2)
min(noout_Income2)

boxplot(noout_Income2)
boxplot(out_Income2)

out_Income2_box <- df[df$Income > max(noout_Income2),c("Income", "Response")]
boxplot(out_Income2_box$Income ~ out_Income2_box$Response)
noout_Income2_box <- df[df$Income <= max(noout_Income2), c("Income", "Response")]
boxplot(noout_Income2_box$Income ~ noout_Income2_box$Response)

length(out_Income2) / nrow(df)
#0,4% outliers

prop.table(table(noout_Income2_box$Response))
#15% of positive Response
prop.table(table(out_Income2_box$Response))
#0% of positive Response

## Transforming into bins to run chi-square test

q_Income <- quantile(Income, probs = c(0.1, 0.2, 0.3, 0.4, 0.5,
                                       0.6, 0.7, 0.8, 0.9, 1))
q_Income

df_Income <- df[, c('Income', 'Response')]

df_Income$fx_Income <- cut(df_Income$Income,
                           breaks= c(-10000000,
                                     q_Income[1],
                                     q_Income[2],
                                     q_Income[3],
                                     q_Income[4],
                                     q_Income[5],
                                     q_Income[6],
                                     q_Income[7],
                                     q_Income[8],
                                     q_Income[9],
                                     q_Income[10]),
                           labels=c('<= 10%', '<= 20%', '<= 30%', '<= 40%', '<= 50%',
                                    '<= 60%', '<= 70%', '<= 80%', '<= 90%', '<= 100%'),
                           right=T)

CrossTable(df_Income$fx_Income, df_Income$Response, chisq = T)

## KS test

par(mfrow = c(1,1))

ks.test(df_Income[df_Income$Response == 0,]$Income,
        df_Income[df_Income$Response == 1,]$Income)
plot(ecdf(df_Income[df_Income$Response == 0,]$Income),
     xlim = range(c(df_Income[df_Income$Response == 0,]$Income,
                    df_Income[df_Income$Response == 1,]$Income)), col = 'red')
plot(ecdf(df_Income[df_Income$Response == 1,]$Income),
     add = TRUE, lty = "dashed", col = 'blue')

#Conclusion: significant low p-value for chi-square test and ks test


### Analyzing the feature distribution, regarding the target variable ###

Income_quant <- quantile(df$Income, probs = c(0, 0.1, 0.2, 0.3, 0.4, 0.5,
                                              0.6, 0.7, 0.8, 0.9, 1))

df$faixa_Income <- as.numeric(cut(df$Income,
                                  breaks= c(unique(Income_quant)),
                                  labels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                                  right=F))

df$faixa_Income[is.na(df$faixa_Income)] <- 10

hist(df$faixa_Income)

Income_faixa <- df %>%
  group_by(faixa_Income) %>%
  summarise(obs = n(), Income = mean(Income), Response = sum(Response),
            pct_Response = sum(Response)/n())

hist(Income_faixa$pct_Response)
par(mfrow = c(1,1))
plot(Income_faixa$Income, Income_faixa$pct_Response)
abline(h=sum(Response) / nrow(df), col="red")


feature <- c()
p_chisq_test <- c()
p_ks_test <- c()
min_value <- c()
max_value <- c()
min_no_outlier <- c()
max_no_outlier <- c()

p_chisq_test <- CrossTable(df_Income$fx_Income, df_Income$Response, chisq = T)$chisq[3]$p.value

df_stats <- data.frame('Income', p_chisq_test)

colnames(df_stats) <- c('feature', 'p-value')


##########################################################################################


##enrolled_months

hist(enrolled_months)
boxplot(enrolled_months)
boxplot(enrolled_months ~ Response) # Comparing with target variable

# Analyzing outliers
par(mfrow = c(2,1))

noout_enrolled_months <- remove_outlier2(enrolled_months)
noout_enrolled_months2 <- noout_enrolled_months[!is.na(noout_enrolled_months)]
hist(noout_enrolled_months)
out_enrolled_months <- outlier2(enrolled_months)
out_enrolled_months2 <- out_enrolled_months[!is.na(out_enrolled_months)]
hist(out_enrolled_months2)
max(noout_enrolled_months2)
min(noout_enrolled_months2)

boxplot(noout_enrolled_months2)
boxplot(out_enrolled_months2)

out_enrolled_months2_box <- df[df$enrolled_months > max(noout_enrolled_months2),c("enrolled_months", "Response")]
boxplot(out_enrolled_months2_box$enrolled_months ~ out_enrolled_months2_box$Response)
noout_enrolled_months2_box <- df[df$enrolled_months <= max(noout_enrolled_months2), c("enrolled_months", "Response")]
boxplot(noout_enrolled_months2_box$enrolled_months ~ noout_enrolled_months2_box$Response)

length(out_enrolled_months2) / nrow(df)
#0% outliers

prop.table(table(noout_enrolled_months2_box$Response))
#15% of positive Response
prop.table(table(out_enrolled_months2_box$Response))
#0% of positive Response

## Transforming into bins to run chis-square test

q_enrolled_months <- quantile(enrolled_months, probs = c(0.1, 0.2, 0.3, 0.4, 0.5,
                                       0.6, 0.7, 0.8, 0.9, 1))
q_enrolled_months

df_enrolled_months <- df[, c('enrolled_months', 'Response')]

df_enrolled_months$fx_enrolled_months <- cut(df_enrolled_months$enrolled_months,
                           breaks= c(-10000000,
                                     q_enrolled_months[1],
                                     q_enrolled_months[2],
                                     q_enrolled_months[3],
                                     q_enrolled_months[4],
                                     q_enrolled_months[5],
                                     q_enrolled_months[6],
                                     q_enrolled_months[7],
                                     q_enrolled_months[8],
                                     q_enrolled_months[9],
                                     q_enrolled_months[10]),
                           labels=c('<= 10%', '<= 20%', '<= 30%', '<= 40%', '<= 50%',
                                    '<= 60%', '<= 70%', '<= 80%', '<= 90%', '<= 100%'),
                           right=T)

CrossTable(df_enrolled_months$fx_enrolled_months, df_enrolled_months$Response, chisq = T)

## KS test

par(mfrow = c(1,1))

ks.test(df_enrolled_months[df_enrolled_months$Response == 0,]$enrolled_months,
        df_enrolled_months[df_enrolled_months$Response == 1,]$enrolled_months)
plot(ecdf(df_enrolled_months[df_enrolled_months$Response == 0,]$enrolled_months),
     xlim = range(c(df_enrolled_months[df_enrolled_months$Response == 0,]$enrolled_months,
                    df_enrolled_months[df_enrolled_months$Response == 1,]$enrolled_months)), col = 'red')
plot(ecdf(df_enrolled_months[df_enrolled_months$Response == 1,]$enrolled_months),
     add = TRUE, lty = "dashed", col = 'blue')

#Conclusion: significant low p-value for chi-square test and ks test


### Analyzing the feature distribution, regarding the target variable ###

enrolled_months_quant <- quantile(df$enrolled_months, probs = c(0, 0.1, 0.2, 0.3, 0.4, 0.5,
                                                0.6, 0.7, 0.8, 0.9, 1))

df$faixa_enrolled_months <- as.numeric(cut(df$enrolled_months,
                                   breaks= c(unique(enrolled_months_quant)),
                                   labels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                                   right=F))

df$faixa_enrolled_months[is.na(df$faixa_enrolled_months)] <- 10

hist(df$faixa_enrolled_months)

enrolled_months_faixa <- df %>%
  group_by(faixa_enrolled_months) %>%
  summarise(obs = n(), enrolled_months = mean(enrolled_months), Response = sum(Response),
            pct_Response = sum(Response)/n())

hist(enrolled_months_faixa$pct_Response)
par(mfrow = c(1,1))
plot(enrolled_months_faixa$enrolled_months, enrolled_months_faixa$pct_Response)
abline(h=sum(Response) / nrow(df), col="red")

feature <- c()
p_chisq_test <- c()
p_ks_test <- c()
min_value <- c()
max_value <- c()
min_no_outlier <- c()
max_no_outlier <- c()

p_chisq_test <- CrossTable(df_enrolled_months$fx_enrolled_months, df_enrolled_months$Response, chisq = T)$chisq[3]$p.value
df_stats2 <- data.frame('enrolled_months', p_chisq_test)

df_stats2

colnames(df_stats2) <- c('feature', 'p-value')

df_stats <- rbind(df_stats, df_stats2)


##########################################################################################


##education_ordinary_domain

hist(education_ordinary_domain)
boxplot(education_ordinary_domain)
boxplot(education_ordinary_domain ~ Response) # Comparing with target variable

#Analyzing
par(mfrow = c(2,1))

noout_education_ordinary_domain <- remove_outlier2(education_ordinary_domain)
noout_education_ordinary_domain2 <- noout_education_ordinary_domain[!is.na(noout_education_ordinary_domain)]
hist(noout_education_ordinary_domain)
out_education_ordinary_domain <- outlier2(education_ordinary_domain)
out_education_ordinary_domain2 <- out_education_ordinary_domain[!is.na(out_education_ordinary_domain)]
hist(out_education_ordinary_domain2)
max(noout_education_ordinary_domain2)
min(noout_education_ordinary_domain2)

boxplot(noout_education_ordinary_domain2)
boxplot(out_education_ordinary_domain2)

out_education_ordinary_domain2_box <- df[df$education_ordinary_domain > max(noout_education_ordinary_domain2),c("education_ordinary_domain", "Response")]
boxplot(out_education_ordinary_domain2_box$education_ordinary_domain ~ out_education_ordinary_domain2_box$Response)
noout_education_ordinary_domain2_box <- df[df$education_ordinary_domain <= max(noout_education_ordinary_domain2), c("education_ordinary_domain", "Response")]
boxplot(noout_education_ordinary_domain2_box$education_ordinary_domain ~ noout_education_ordinary_domain2_box$Response)

length(out_education_ordinary_domain2) / nrow(df)
#0% outliers

prop.table(table(noout_education_ordinary_domain2_box$Response))
#15% of positive Response
prop.table(table(out_education_ordinary_domain2_box$Response))
#0% of positive Response

## Transforming into bins to run chis-square test

#q_education_ordinary_domain <- quantile(education_ordinary_domain, probs = c(0.1, 0.2, 0.3, 0.4, 0.5,
#                                       0.6, 0.7, 0.8, 0.9, 1))
#q_education_ordinary_domain

df_education_ordinary_domain <- df[, c('education_ordinary_domain', 'Response')]

#df_education_ordinary_domain$fx_education_ordinary_domain <- cut(df_education_ordinary_domain$education_ordinary_domain,
#                           breaks= c(-10000000,
#                                     q_education_ordinary_domain[1],
#                                     q_education_ordinary_domain[2],
#                                     q_education_ordinary_domain[3],
#                                     q_education_ordinary_domain[4],
#                                     q_education_ordinary_domain[5],
#                                     q_education_ordinary_domain[6],
#                                     q_education_ordinary_domain[7],
#                                     q_education_ordinary_domain[8],
#                                     q_education_ordinary_domain[9],
#                                     q_education_ordinary_domain[10]),
#                           labels=c('<= 10%', '<= 20%', '<= 30%', '<= 40%', '<= 50%',
#                                    '<= 60%', '<= 70%', '<= 80%', '<= 90%', '<= 100%'),
#                           right=T)

CrossTable(df_education_ordinary_domain$education_ordinary_domain, df_education_ordinary_domain$Response, chisq = T)

## KS test

par(mfrow = c(1,1))

ks.test(df_education_ordinary_domain[df_education_ordinary_domain$Response == 0,]$education_ordinary_domain,
        df_education_ordinary_domain[df_education_ordinary_domain$Response == 1,]$education_ordinary_domain)
plot(ecdf(df_education_ordinary_domain[df_education_ordinary_domain$Response == 0,]$education_ordinary_domain),
     xlim = range(c(df_education_ordinary_domain[df_education_ordinary_domain$Response == 0,]$education_ordinary_domain,
                    df_education_ordinary_domain[df_education_ordinary_domain$Response == 1,]$education_ordinary_domain)), col = 'red')
plot(ecdf(df_education_ordinary_domain[df_education_ordinary_domain$Response == 1,]$education_ordinary_domain),
     add = TRUE, lty = "dashed", col = 'blue')

#Conclusion: significant low p-value for chi-square test


### Analyzing the feature distribution, regarding the target variable ###

education_ordinary_domain_quant <- quantile(df$education_ordinary_domain, probs = c(0, 0.1, 0.2, 0.3, 0.4, 0.5,
                                                0.6, 0.7, 0.8, 0.9, 1))

df$faixa_education_ordinary_domain <- as.numeric(cut(df$education_ordinary_domain,
                                   breaks= c(unique(education_ordinary_domain_quant)),
                                   labels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                                   right=F))

df$faixa_education_ordinary_domain[is.na(df$faixa_education_ordinary_domain)] <- 10

hist(df$faixa_education_ordinary_domain)

education_ordinary_domain_faixa <- df %>%
  group_by(faixa_education_ordinary_domain) %>%
  summarise(obs = n(), education_ordinary_domain = mean(education_ordinary_domain), Response = sum(Response),
            pct_Response = sum(Response)/n())

hist(education_ordinary_domain_faixa$pct_Response)
par(mfrow = c(1,1))
plot(education_ordinary_domain_faixa$education_ordinary_domain, education_ordinary_domain_faixa$pct_Response)
abline(h=sum(Response) / nrow(df), col="red")

feature <- c()
p_chisq_test <- c()
p_ks_test <- c()
min_value <- c()
max_value <- c()
min_no_outlier <- c()
max_no_outlier <- c()

p_chisq_test <- CrossTable(df_education_ordinary_domain$education_ordinary_domain, df_education_ordinary_domain$Response, chisq = T)$chisq[3]$p.value
df_stats3 <- data.frame('education_ordinary_domain', p_chisq_test)

df_stats3

colnames(df_stats3) <- c('feature', 'p-value')

df_stats <- rbind(df_stats, df_stats3)


##########################################################################################


##marital_status_ordinary_domain

hist(marital_status_ordinary_domain)
boxplot(marital_status_ordinary_domain)
boxplot(marital_status_ordinary_domain ~ Response) # Comparing with target variable

#Analyzing
par(mfrow = c(2,1))

noout_marital_status_ordinary_domain <- remove_outlier2(marital_status_ordinary_domain)
noout_marital_status_ordinary_domain2 <- noout_marital_status_ordinary_domain[!is.na(noout_marital_status_ordinary_domain)]
hist(noout_marital_status_ordinary_domain)
out_marital_status_ordinary_domain <- outlier2(marital_status_ordinary_domain)
out_marital_status_ordinary_domain2 <- out_marital_status_ordinary_domain[!is.na(out_marital_status_ordinary_domain)]
hist(out_marital_status_ordinary_domain2)
max(noout_marital_status_ordinary_domain2)
min(noout_marital_status_ordinary_domain2)

boxplot(noout_marital_status_ordinary_domain2)
boxplot(out_marital_status_ordinary_domain2)

out_marital_status_ordinary_domain2_box <- df[df$marital_status_ordinary_domain > max(noout_marital_status_ordinary_domain2),c("marital_status_ordinary_domain", "Response")]
boxplot(out_marital_status_ordinary_domain2_box$marital_status_ordinary_domain ~ out_marital_status_ordinary_domain2_box$Response)
noout_marital_status_ordinary_domain2_box <- df[df$marital_status_ordinary_domain <= max(noout_marital_status_ordinary_domain2), c("marital_status_ordinary_domain", "Response")]
boxplot(noout_marital_status_ordinary_domain2_box$marital_status_ordinary_domain ~ noout_marital_status_ordinary_domain2_box$Response)

length(out_marital_status_ordinary_domain2) / nrow(df)
#0% outliers

prop.table(table(noout_marital_status_ordinary_domain2_box$Response))
#15% of positive Response
prop.table(table(out_marital_status_ordinary_domain2_box$Response))
#0% of positive Response

## Transforming into bins to run chis-square test

#q_marital_status_ordinary_domain <- quantile(marital_status_ordinary_domain, probs = c(0.1, 0.2, 0.3, 0.4, 0.5,
#                                       0.6, 0.7, 0.8, 0.9, 1))
#q_marital_status_ordinary_domain

df_marital_status_ordinary_domain <- df[, c('marital_status_ordinary_domain', 'Response')]

#df_marital_status_ordinary_domain$fx_marital_status_ordinary_domain <- cut(df_marital_status_ordinary_domain$marital_status_ordinary_domain,
#                           breaks= c(-10000000,
#                                     q_marital_status_ordinary_domain[1],
#                                     q_marital_status_ordinary_domain[2],
#                                     q_marital_status_ordinary_domain[3],
#                                     q_marital_status_ordinary_domain[4],
#                                     q_marital_status_ordinary_domain[5],
#                                     q_marital_status_ordinary_domain[6],
#                                     q_marital_status_ordinary_domain[7],
#                                     q_marital_status_ordinary_domain[8],
#                                     q_marital_status_ordinary_domain[9],
#                                     q_marital_status_ordinary_domain[10]),
#                           labels=c('<= 10%', '<= 20%', '<= 30%', '<= 40%', '<= 50%',
#                                    '<= 60%', '<= 70%', '<= 80%', '<= 90%', '<= 100%'),
#                           right=T)

CrossTable(df_marital_status_ordinary_domain$marital_status_ordinary_domain, df_marital_status_ordinary_domain$Response, chisq = T)

## KS test

par(mfrow = c(1,1))

ks.test(df_marital_status_ordinary_domain[df_marital_status_ordinary_domain$Response == 0,]$marital_status_ordinary_domain,
        df_marital_status_ordinary_domain[df_marital_status_ordinary_domain$Response == 1,]$marital_status_ordinary_domain)
plot(ecdf(df_marital_status_ordinary_domain[df_marital_status_ordinary_domain$Response == 0,]$marital_status_ordinary_domain),
     xlim = range(c(df_marital_status_ordinary_domain[df_marital_status_ordinary_domain$Response == 0,]$marital_status_ordinary_domain,
                    df_marital_status_ordinary_domain[df_marital_status_ordinary_domain$Response == 1,]$marital_status_ordinary_domain)), col = 'red')
plot(ecdf(df_marital_status_ordinary_domain[df_marital_status_ordinary_domain$Response == 1,]$marital_status_ordinary_domain),
     add = TRUE, lty = "dashed", col = 'blue')

#Conclusion: significant low p-value for chi-square test


### Analyzing the feature distribution, regarding the target variable ###

marital_status_ordinary_domain_quant <- quantile(df$marital_status_ordinary_domain, probs = c(0, 0.1, 0.2, 0.3, 0.4, 0.5,
                                                                                    0.6, 0.7, 0.8, 0.9, 1))

df$faixa_marital_status_ordinary_domain <- as.numeric(cut(df$marital_status_ordinary_domain,
                                                     breaks= c(unique(marital_status_ordinary_domain_quant)),
                                                     labels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                                                     right=F))

df$faixa_marital_status_ordinary_domain[is.na(df$faixa_marital_status_ordinary_domain)] <- 10

hist(df$faixa_marital_status_ordinary_domain)

marital_status_ordinary_domain_faixa <- df %>%
  group_by(faixa_marital_status_ordinary_domain) %>%
  summarise(obs = n(), marital_status_ordinary_domain = mean(marital_status_ordinary_domain), Response = sum(Response),
            pct_Response = sum(Response)/n())

hist(marital_status_ordinary_domain_faixa$pct_Response)
par(mfrow = c(1,1))
plot(marital_status_ordinary_domain_faixa$marital_status_ordinary_domain, marital_status_ordinary_domain_faixa$pct_Response)
abline(h=sum(Response) / nrow(df), col="red")

feature <- c()
p_chisq_test <- c()
p_ks_test <- c()
min_value <- c()
max_value <- c()
min_no_outlier <- c()
max_no_outlier <- c()

p_chisq_test <- CrossTable(df_marital_status_ordinary_domain$marital_status_ordinary_domain, df_marital_status_ordinary_domain$Response, chisq = T)$chisq[3]$p.value
df_stats4 <- data.frame('marital_status_ordinary_domain', p_chisq_test)

df_stats4

colnames(df_stats4) <- c('feature', 'p-value')

df_stats <- rbind(df_stats, df_stats4)

##########################################################################################


##age

hist(age)
boxplot(age)
boxplot(age ~ Response) # Comparing with target variable

# Analyzing outliers
par(mfrow = c(2,1))

noout_age <- remove_outlier2(age)
noout_age2 <- noout_age[!is.na(noout_age)]
hist(noout_age)
out_age <- outlier2(age)
out_age2 <- out_age[!is.na(out_age)]
hist(out_age2)
max(noout_age2)
min(noout_age2)

boxplot(noout_age2)
boxplot(out_age2)

out_age2_box <- df[df$age > max(noout_age2),c("age", "Response")]
boxplot(out_age2_box$age ~ out_age2_box$Response)
noout_age2_box <- df[df$age <= max(noout_age2), c("age", "Response")]
boxplot(noout_age2_box$age ~ noout_age2_box$Response)

length(out_age2) / nrow(df)
#0% outliers

prop.table(table(noout_age2_box$Response))
#15% of positive Response
prop.table(table(out_age2_box$Response))
#0% of positive Response

## Transforming into bins to run chis-square test

q_age <- quantile(age, probs = c(0.1, 0.2, 0.3, 0.4, 0.5,
                                                         0.6, 0.7, 0.8, 0.9, 1))
q_age

df_age <- df[, c('age', 'Response')]

df_age$fx_age <- cut(df_age$age,
                                             breaks= c(-10000000,
                                                       q_age[1],
                                                       q_age[2],
                                                       q_age[3],
                                                       q_age[4],
                                                       q_age[5],
                                                       q_age[6],
                                                       q_age[7],
                                                       q_age[8],
                                                       q_age[9],
                                                       q_age[10]),
                                             labels=c('<= 10%', '<= 20%', '<= 30%', '<= 40%', '<= 50%',
                                                      '<= 60%', '<= 70%', '<= 80%', '<= 90%', '<= 100%'),
                                             right=T)

CrossTable(df_age$fx_age, df_age$Response, chisq = T)

## KS test

par(mfrow = c(1,1))

ks.test(df_age[df_age$Response == 0,]$age,
        df_age[df_age$Response == 1,]$age)
plot(ecdf(df_age[df_age$Response == 0,]$age),
     xlim = range(c(df_age[df_age$Response == 0,]$age,
                    df_age[df_age$Response == 1,]$age)), col = 'red')
plot(ecdf(df_age[df_age$Response == 1,]$age),
     add = TRUE, lty = "dashed", col = 'blue')

#Conclusion: not significant high p-value for chi-square test and ks test


### Analyzing the feature distribution, regarding the target variable ###

age_quant <- quantile(df$age, probs = c(0, 0.1, 0.2, 0.3, 0.4, 0.5,
                                                                0.6, 0.7, 0.8, 0.9, 1))

df$faixa_age <- as.numeric(cut(df$age,
                                           breaks= c(unique(age_quant)),
                                           labels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                                           right=F))

df$faixa_age[is.na(df$faixa_age)] <- 10

hist(df$faixa_age)

age_faixa <- df %>%
  group_by(faixa_age) %>%
  summarise(obs = n(), age = mean(age), Response = sum(Response),
            pct_Response = sum(Response)/n())

hist(age_faixa$pct_Response)
par(mfrow = c(1,1))
plot(age_faixa$age, age_faixa$pct_Response)
abline(h=sum(Response) / nrow(df), col="red")

feature <- c()
p_chisq_test <- c()
p_ks_test <- c()
min_value <- c()
max_value <- c()
min_no_outlier <- c()
max_no_outlier <- c()

p_chisq_test <- CrossTable(df_age$fx_age, df_age$Response, chisq = T)$chisq[3]$p.value
df_stats5 <- data.frame('age', p_chisq_test)

df_stats5

colnames(df_stats5) <- c('feature', 'p-value')

df_stats <- rbind(df_stats, df_stats5)

##########################################################################################


##Kidhome

hist(Kidhome)
boxplot(Kidhome)
boxplot(Kidhome ~ Response) # Comparing with target variable

#Analyzing
par(mfrow = c(2,1))

noout_Kidhome <- remove_outlier2(Kidhome)
noout_Kidhome2 <- noout_Kidhome[!is.na(noout_Kidhome)]
hist(noout_Kidhome)
out_Kidhome <- outlier2(Kidhome)
out_Kidhome2 <- out_Kidhome[!is.na(out_Kidhome)]
hist(out_Kidhome2)
max(noout_Kidhome2)
min(noout_Kidhome2)

boxplot(noout_Kidhome2)
boxplot(out_Kidhome2)

out_Kidhome2_box <- df[df$Kidhome > max(noout_Kidhome2),c("Kidhome", "Response")]
boxplot(out_Kidhome2_box$Kidhome ~ out_Kidhome2_box$Response)
noout_Kidhome2_box <- df[df$Kidhome <= max(noout_Kidhome2), c("Kidhome", "Response")]
boxplot(noout_Kidhome2_box$Kidhome ~ noout_Kidhome2_box$Response)

length(out_Kidhome2) / nrow(df)
#0% outliers

prop.table(table(noout_Kidhome2_box$Response))
#15% of positive Response
prop.table(table(out_Kidhome2_box$Response))
#0% of positive Response

## Transforming into bins to run chis-square test

#q_Kidhome <- quantile(Kidhome, probs = c(0.1, 0.2, 0.3, 0.4, 0.5,
#                                       0.6, 0.7, 0.8, 0.9, 1))
#q_Kidhome

df_Kidhome <- df[, c('Kidhome', 'Response')]

#df_Kidhome$fx_Kidhome <- cut(df_Kidhome$Kidhome,
#                           breaks= c(-10000000,
#                                     q_Kidhome[1],
#                                     q_Kidhome[2],
#                                     q_Kidhome[3],
#                                     q_Kidhome[4],
#                                     q_Kidhome[5],
#                                     q_Kidhome[6],
#                                     q_Kidhome[7],
#                                     q_Kidhome[8],
#                                     q_Kidhome[9],
#                                     q_Kidhome[10]),
#                           labels=c('<= 10%', '<= 20%', '<= 30%', '<= 40%', '<= 50%',
#                                    '<= 60%', '<= 70%', '<= 80%', '<= 90%', '<= 100%'),
#                           right=T)

CrossTable(df_Kidhome$Kidhome, df_Kidhome$Response, chisq = T)

## KS test

par(mfrow = c(1,1))

ks.test(df_Kidhome[df_Kidhome$Response == 0,]$Kidhome,
        df_Kidhome[df_Kidhome$Response == 1,]$Kidhome)
plot(ecdf(df_Kidhome[df_Kidhome$Response == 0,]$Kidhome),
     xlim = range(c(df_Kidhome[df_Kidhome$Response == 0,]$Kidhome,
                    df_Kidhome[df_Kidhome$Response == 1,]$Kidhome)), col = 'red')
plot(ecdf(df_Kidhome[df_Kidhome$Response == 1,]$Kidhome),
     add = TRUE, lty = "dashed", col = 'blue')

#Conclusion: significant low p-value for chi-square test


### Analyzing the feature distribution, regarding the target variable ###

Kidhome_quant <- quantile(df$Kidhome, probs = c(0, 0.1, 0.2, 0.3, 0.4, 0.5,
                                              0.6, 0.7, 0.8, 0.9, 1))

df$faixa_Kidhome <- as.numeric(cut(df$Kidhome,
                                  breaks= c(unique(Kidhome_quant)),
                                  labels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                                  right=F))

df$faixa_Kidhome[is.na(df$faixa_Kidhome)] <- 10

hist(df$faixa_Kidhome)

Kidhome_faixa <- df %>%
  group_by(faixa_Kidhome) %>%
  summarise(obs = n(), Kidhome = mean(Kidhome), Response = sum(Response),
            pct_Response = sum(Response)/n())

hist(Kidhome_faixa$pct_Response)
par(mfrow = c(1,1))
plot(Kidhome_faixa$Kidhome, Kidhome_faixa$pct_Response)
abline(h=sum(Response) / nrow(df), col="red")

feature <- c()
p_chisq_test <- c()
p_ks_test <- c()
min_value <- c()
max_value <- c()
min_no_outlier <- c()
max_no_outlier <- c()

p_chisq_test <- CrossTable(df_Kidhome$Kidhome, df_Kidhome$Response, chisq = T)$chisq[3]$p.value
df_stats6 <- data.frame('Kidhome', p_chisq_test)

df_stats6

colnames(df_stats6) <- c('feature', 'p-value')

df_stats <- rbind(df_stats, df_stats6)


##########################################################################################


##Teenhome - Tratar como variável de classe

hist(Teenhome)
boxplot(Teenhome)
boxplot(Teenhome ~ Response) # Comparing with target variable

# Analyzing outliers
par(mfrow = c(2,1))

noout_Teenhome <- remove_outlier2(Teenhome)
noout_Teenhome2 <- noout_Teenhome[!is.na(noout_Teenhome)]
hist(noout_Teenhome)
out_Teenhome <- outlier2(Teenhome)
out_Teenhome2 <- out_Teenhome[!is.na(out_Teenhome)]
hist(out_Teenhome2)
max(noout_Teenhome2)
min(noout_Teenhome2)

boxplot(noout_Teenhome2)
boxplot(out_Teenhome2)

out_Teenhome2_box <- df[df$Teenhome > max(noout_Teenhome2),c("Teenhome", "Response")]
boxplot(out_Teenhome2_box$Teenhome ~ out_Teenhome2_box$Response)
noout_Teenhome2_box <- df[df$Teenhome <= max(noout_Teenhome2), c("Teenhome", "Response")]
boxplot(noout_Teenhome2_box$Teenhome ~ noout_Teenhome2_box$Response)

length(out_Teenhome2) / nrow(df)
#0% outliers

prop.table(table(noout_Teenhome2_box$Response))
#15% of positive Response
prop.table(table(out_Teenhome2_box$Response))
#0% of positive Response

## Transforming into bins to run chis-square test

#q_Teenhome <- quantile(Teenhome, probs = c(0.1, 0.2, 0.3, 0.4, 0.5,
#                                       0.6, 0.7, 0.8, 0.9, 1))
#q_Teenhome

df_Teenhome <- df[, c('Teenhome', 'Response')]

#df_Teenhome$fx_Teenhome <- cut(df_Teenhome$Teenhome,
#                           breaks= c(-10000000,
#                                     q_Teenhome[1],
#                                     q_Teenhome[2],
#                                     q_Teenhome[3],
#                                     q_Teenhome[4],
#                                     q_Teenhome[5],
#                                     q_Teenhome[6],
#                                     q_Teenhome[7],
#                                     q_Teenhome[8],
#                                     q_Teenhome[9],
#                                     q_Teenhome[10]),
#                           labels=c('<= 10%', '<= 20%', '<= 30%', '<= 40%', '<= 50%',
#                                    '<= 60%', '<= 70%', '<= 80%', '<= 90%', '<= 100%'),
#                           right=T)

CrossTable(df_Teenhome$Teenhome, df_Teenhome$Response, chisq = T)

## KS test

par(mfrow = c(1,1))

ks.test(df_Teenhome[df_Teenhome$Response == 0,]$Teenhome,
        df_Teenhome[df_Teenhome$Response == 1,]$Teenhome)
plot(ecdf(df_Teenhome[df_Teenhome$Response == 0,]$Teenhome),
     xlim = range(c(df_Teenhome[df_Teenhome$Response == 0,]$Teenhome,
                    df_Teenhome[df_Teenhome$Response == 1,]$Teenhome)), col = 'red')
plot(ecdf(df_Teenhome[df_Teenhome$Response == 1,]$Teenhome),
     add = TRUE, lty = "dashed", col = 'blue')

#Conclusion: significant low p-value for chi-square test


### Analyzing the feature distribution, regarding the target variable ###

Teenhome_quant <- quantile(df$Teenhome, probs = c(0, 0.1, 0.2, 0.3, 0.4, 0.5,
                                                0.6, 0.7, 0.8, 0.9, 1))

df$faixa_Teenhome <- as.numeric(cut(df$Teenhome,
                                   breaks= c(unique(Teenhome_quant)),
                                   labels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                                   right=F))

df$faixa_Teenhome[is.na(df$faixa_Teenhome)] <- 10

hist(df$faixa_Teenhome)

Teenhome_faixa <- df %>%
  group_by(faixa_Teenhome) %>%
  summarise(obs = n(), Teenhome = mean(Teenhome), Response = sum(Response),
            pct_Response = sum(Response)/n())

hist(Teenhome_faixa$pct_Response)
par(mfrow = c(1,1))
plot(Teenhome_faixa$Teenhome, Teenhome_faixa$pct_Response)
abline(h=sum(Response) / nrow(df), col="red")

feature <- c()
p_chisq_test <- c()
p_ks_test <- c()
min_value <- c()
max_value <- c()
min_no_outlier <- c()
max_no_outlier <- c()

p_chisq_test <- CrossTable(df_Teenhome$Teenhome, df_Teenhome$Response, chisq = T)$chisq[3]$p.value
df_stats7 <- data.frame('Teenhome', p_chisq_test)

df_stats7

colnames(df_stats7) <- c('feature', 'p-value')

df_stats <- rbind(df_stats, df_stats7)


##########################################################################################


##children_number

hist(children_number)
boxplot(children_number)
boxplot(children_number ~ Response) # Comparing with target variable

# Analyzing outliers
par(mfrow = c(2,1))

noout_children_number <- remove_outlier2(children_number)
noout_children_number2 <- noout_children_number[!is.na(noout_children_number)]
hist(noout_children_number)
out_children_number <- outlier2(children_number)
out_children_number2 <- out_children_number[!is.na(out_children_number)]
hist(out_children_number2)
max(noout_children_number2)
min(noout_children_number2)

boxplot(noout_children_number2)
boxplot(out_children_number2)

out_children_number2_box <- df[df$children_number > max(noout_children_number2),c("children_number", "Response")]
boxplot(out_children_number2_box$children_number ~ out_children_number2_box$Response)
noout_children_number2_box <- df[df$children_number <= max(noout_children_number2), c("children_number", "Response")]
boxplot(noout_children_number2_box$children_number ~ noout_children_number2_box$Response)

length(out_children_number2) / nrow(df)
#0% outliers

prop.table(table(noout_children_number2_box$Response))
#15% of positive Response
prop.table(table(out_children_number2_box$Response))
#0% of positive Response

## Transforming into bins to run chis-square test

#q_children_number <- quantile(children_number, probs = c(0.1, 0.2, 0.3, 0.4, 0.5,
#                                       0.6, 0.7, 0.8, 0.9, 1))
#q_children_number

df_children_number <- df[, c('children_number', 'Response')]

#df_children_number$fx_children_number <- cut(df_children_number$children_number,
#                           breaks= c(-10000000,
#                                     q_children_number[1],
#                                     q_children_number[2],
#                                     q_children_number[3],
#                                     q_children_number[4],
#                                     q_children_number[5],
#                                     q_children_number[6],
#                                     q_children_number[7],
#                                     q_children_number[8],
#                                     q_children_number[9],
#                                     q_children_number[10]),
#                           labels=c('<= 10%', '<= 20%', '<= 30%', '<= 40%', '<= 50%',
#                                    '<= 60%', '<= 70%', '<= 80%', '<= 90%', '<= 100%'),
#                           right=T)

CrossTable(df_children_number$children_number, df_children_number$Response, chisq = T)

## KS test

par(mfrow = c(1,1))

ks.test(df_children_number[df_children_number$Response == 0,]$children_number,
        df_children_number[df_children_number$Response == 1,]$children_number)
plot(ecdf(df_children_number[df_children_number$Response == 0,]$children_number),
     xlim = range(c(df_children_number[df_children_number$Response == 0,]$children_number,
                    df_children_number[df_children_number$Response == 1,]$children_number)), col = 'red')
plot(ecdf(df_children_number[df_children_number$Response == 1,]$children_number),
     add = TRUE, lty = "dashed", col = 'blue')

#Conclusion: significant low p-value for chi-square test


### Analyzing the feature distribution, regarding the target variable ###

children_number_quant <- quantile(df$children_number, probs = c(0, 0.1, 0.2, 0.3, 0.4, 0.5,
                                                  0.6, 0.7, 0.8, 0.9, 1))

df$faixa_children_number <- as.numeric(cut(df$children_number,
                                    breaks= c(unique(children_number_quant)),
                                    labels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                                    right=F))

df$faixa_children_number[is.na(df$faixa_children_number)] <- 10

hist(df$faixa_children_number)

children_number_faixa <- df %>%
  group_by(faixa_children_number) %>%
  summarise(obs = n(), children_number = mean(children_number), Response = sum(Response),
            pct_Response = sum(Response)/n())

hist(children_number_faixa$pct_Response)
par(mfrow = c(1,1))
plot(children_number_faixa$children_number, children_number_faixa$pct_Response)
abline(h=sum(Response) / nrow(df), col="red")

feature <- c()
p_chisq_test <- c()
p_ks_test <- c()
min_value <- c()
max_value <- c()
min_no_outlier <- c()
max_no_outlier <- c()

p_chisq_test <- CrossTable(df_children_number$children_number, df_children_number$Response, chisq = T)$chisq[3]$p.value
df_stats8 <- data.frame('children_number', p_chisq_test)

df_stats8

colnames(df_stats8) <- c('feature', 'p-value')

df_stats <- rbind(df_stats, df_stats8)


##########################################################################################


##Recency

hist(Recency)
boxplot(Recency)
boxplot(Recency ~ Response) # Comparing with target variable

#Analyzing
par(mfrow = c(2,1))

noout_Recency <- remove_outlier2(Recency)
noout_Recency2 <- noout_Recency[!is.na(noout_Recency)]
hist(noout_Recency)
out_Recency <- outlier2(Recency)
out_Recency2 <- out_Recency[!is.na(out_Recency)]
hist(out_Recency2)
max(noout_Recency2)
min(noout_Recency2)

boxplot(noout_Recency2)
boxplot(out_Recency2)

out_Recency2_box <- df[df$Recency > max(noout_Recency2),c("Recency", "Response")]
boxplot(out_Recency2_box$Recency ~ out_Recency2_box$Response)
noout_Recency2_box <- df[df$Recency <= max(noout_Recency2), c("Recency", "Response")]
boxplot(noout_Recency2_box$Recency ~ noout_Recency2_box$Response)

length(out_Recency2) / nrow(df)
#0% outliers

prop.table(table(noout_Recency2_box$Response))
#15% of positive Response
prop.table(table(out_Recency2_box$Response))
#0% of positive Response

## Transforming into bins to run chis-square test

q_Recency <- quantile(Recency, probs = c(0.1, 0.2, 0.3, 0.4, 0.5,
                                       0.6, 0.7, 0.8, 0.9, 1))
q_Recency

df_Recency <- df[, c('Recency', 'Response')]

df_Recency$fx_Recency <- cut(df_Recency$Recency,
                           breaks= c(-10000000,
                                     q_Recency[1],
                                     q_Recency[2],
                                     q_Recency[3],
                                     q_Recency[4],
                                     q_Recency[5],
                                     q_Recency[6],
                                     q_Recency[7],
                                     q_Recency[8],
                                     q_Recency[9],
                                     q_Recency[10]),
                           labels=c('<= 10%', '<= 20%', '<= 30%', '<= 40%', '<= 50%',
                                    '<= 60%', '<= 70%', '<= 80%', '<= 90%', '<= 100%'),
                           right=T)

CrossTable(df_Recency$fx_Recency, df_Recency$Response, chisq = T)

## KS test

par(mfrow = c(1,1))

ks.test(df_Recency[df_Recency$Response == 0,]$Recency,
        df_Recency[df_Recency$Response == 1,]$Recency)
plot(ecdf(df_Recency[df_Recency$Response == 0,]$Recency),
     xlim = range(c(df_Recency[df_Recency$Response == 0,]$Recency,
                    df_Recency[df_Recency$Response == 1,]$Recency)), col = 'red')
plot(ecdf(df_Recency[df_Recency$Response == 1,]$Recency),
     add = TRUE, lty = "dashed", col = 'blue')

#Conclusion: significant low p-value for chi-square test and ks test


### Analyzing the feature distribution, regarding the target variable ###

Recency_quant <- quantile(df$Recency, probs = c(0, 0.1, 0.2, 0.3, 0.4, 0.5,
                                                0.6, 0.7, 0.8, 0.9, 1))

df$faixa_Recency <- as.numeric(cut(df$Recency,
                                   breaks= c(unique(Recency_quant)),
                                   labels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                                   right=F))

df$faixa_Recency[is.na(df$faixa_Recency)] <- 10

hist(df$faixa_Recency)

Recency_faixa <- df %>%
  group_by(faixa_Recency) %>%
  summarise(obs = n(), Recency = mean(Recency), Response = sum(Response),
            pct_Response = sum(Response)/n())

hist(Recency_faixa$pct_Response)
par(mfrow = c(1,1))
plot(Recency_faixa$Recency, Recency_faixa$pct_Response)
abline(h=sum(Response) / nrow(df), col="red")

feature <- c()
p_chisq_test <- c()
p_ks_test <- c()
min_value <- c()
max_value <- c()
min_no_outlier <- c()
max_no_outlier <- c()

p_chisq_test <- CrossTable(df_Recency$fx_Recency, df_Recency$Response, chisq = T)$chisq[3]$p.value
df_stats9 <- data.frame('Recency', p_chisq_test)

df_stats9

colnames(df_stats9) <- c('feature', 'p-value')

df_stats <- rbind(df_stats, df_stats9)


##########################################################################################


##MntWines

hist(MntWines)
boxplot(MntWines)
boxplot(MntWines ~ Response) # Comparing with target variable

#Analyzing
par(mfrow = c(2,1))

noout_MntWines <- remove_outlier2(MntWines)
noout_MntWines2 <- noout_MntWines[!is.na(noout_MntWines)]
hist(noout_MntWines)
out_MntWines <- outlier2(MntWines)
out_MntWines2 <- out_MntWines[!is.na(out_MntWines)]
hist(out_MntWines2)
max(noout_MntWines2)
min(noout_MntWines2)

boxplot(noout_MntWines2)
boxplot(out_MntWines2)

out_MntWines2_box <- df[df$MntWines > max(noout_MntWines2),c("MntWines", "Response")]
boxplot(out_MntWines2_box$MntWines ~ out_MntWines2_box$Response)
noout_MntWines2_box <- df[df$MntWines <= max(noout_MntWines2), c("MntWines", "Response")]
boxplot(noout_MntWines2_box$MntWines ~ noout_MntWines2_box$Response)

length(out_MntWines2) / nrow(df)
#0,7% outliers

prop.table(table(noout_MntWines2_box$Response))
#15% of positive Response
prop.table(table(out_MntWines2_box$Response))
#67% of positive Response

## Transforming into bins to run chis-square test

q_MntWines <- quantile(MntWines, probs = c(0.1, 0.2, 0.3, 0.4, 0.5,
                                       0.6, 0.7, 0.8, 0.9, 1))
q_MntWines

df_MntWines <- df[, c('MntWines', 'Response')]

df_MntWines$fx_MntWines <- cut(df_MntWines$MntWines,
                           breaks= c(-10000000,
                                     q_MntWines[1],
                                     q_MntWines[2],
                                     q_MntWines[3],
                                     q_MntWines[4],
                                     q_MntWines[5],
                                     q_MntWines[6],
                                     q_MntWines[7],
                                     q_MntWines[8],
                                     q_MntWines[9],
                                     q_MntWines[10]),
                           labels=c('<= 10%', '<= 20%', '<= 30%', '<= 40%', '<= 50%',
                                    '<= 60%', '<= 70%', '<= 80%', '<= 90%', '<= 100%'),
                           right=T)

CrossTable(df_MntWines$fx_MntWines, df_MntWines$Response, chisq = T)

## KS test

par(mfrow = c(1,1))

ks.test(df_MntWines[df_MntWines$Response == 0,]$MntWines,
        df_MntWines[df_MntWines$Response == 1,]$MntWines)
plot(ecdf(df_MntWines[df_MntWines$Response == 0,]$MntWines),
     xlim = range(c(df_MntWines[df_MntWines$Response == 0,]$MntWines,
                    df_MntWines[df_MntWines$Response == 1,]$MntWines)), col = 'red')
plot(ecdf(df_MntWines[df_MntWines$Response == 1,]$MntWines),
     add = TRUE, lty = "dashed", col = 'blue')

#Conclusion: significant low p-value for chi-square test and ks test


### Analyzing the feature distribution, regarding the target variable ###

MntWines_quant <- quantile(df$MntWines, probs = c(0, 0.1, 0.2, 0.3, 0.4, 0.5,
                                                0.6, 0.7, 0.8, 0.9, 1))

df$faixa_MntWines <- as.numeric(cut(df$MntWines,
                                   breaks= c(unique(MntWines_quant)),
                                   labels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                                   right=F))

df$faixa_MntWines[is.na(df$faixa_MntWines)] <- 10

hist(df$faixa_MntWines)

MntWines_faixa <- df %>%
  group_by(faixa_MntWines) %>%
  summarise(obs = n(), MntWines = mean(MntWines), Response = sum(Response),
            pct_Response = sum(Response)/n())

hist(MntWines_faixa$pct_Response)
par(mfrow = c(1,1))
plot(MntWines_faixa$MntWines, MntWines_faixa$pct_Response)
abline(h=sum(Response) / nrow(df), col="red")

feature <- c()
p_chisq_test <- c()
p_ks_test <- c()
min_value <- c()
max_value <- c()
min_no_outlier <- c()
max_no_outlier <- c()

p_chisq_test <- CrossTable(df_MntWines$fx_MntWines, df_MntWines$Response, chisq = T)$chisq[3]$p.value
df_stats10 <- data.frame('MntWines', p_chisq_test)

df_stats10

colnames(df_stats10) <- c('feature', 'p-value')

df_stats <- rbind(df_stats, df_stats10)


##########################################################################################


##wines_perc

hist(wines_perc)
boxplot(wines_perc)
boxplot(wines_perc ~ Response) # Comparing with target variable

# Analyzing outliers
par(mfrow = c(2,1))

noout_wines_perc <- remove_outlier2(wines_perc)
noout_wines_perc2 <- noout_wines_perc[!is.na(noout_wines_perc)]
hist(noout_wines_perc)
out_wines_perc <- outlier2(wines_perc)
out_wines_perc2 <- out_wines_perc[!is.na(out_wines_perc)]
hist(out_wines_perc2)
max(noout_wines_perc2)
min(noout_wines_perc2)

boxplot(noout_wines_perc2)
boxplot(out_wines_perc2)

out_wines_perc2_box <- df[df$wines_perc > max(noout_wines_perc2),c("wines_perc", "Response")]
boxplot(out_wines_perc2_box$wines_perc ~ out_wines_perc2_box$Response)
noout_wines_perc2_box <- df[df$wines_perc <= max(noout_wines_perc2), c("wines_perc", "Response")]
boxplot(noout_wines_perc2_box$wines_perc ~ noout_wines_perc2_box$Response)

length(out_wines_perc2) / nrow(df)
#0% outliers

prop.table(table(noout_wines_perc2_box$Response))
#15% of positive Response
prop.table(table(out_wines_perc2_box$Response))
#0% of positive Response

## Transforming into bins to run chis-square test

q_wines_perc <- quantile(wines_perc, probs = c(0.1, 0.2, 0.3, 0.4, 0.5,
                                           0.6, 0.7, 0.8, 0.9, 1))
q_wines_perc

df_wines_perc <- df[, c('wines_perc', 'Response')]

df_wines_perc$fx_wines_perc <- cut(df_wines_perc$wines_perc,
                               breaks= c(-10000000,
                                         q_wines_perc[1],
                                         q_wines_perc[2],
                                         q_wines_perc[3],
                                         q_wines_perc[4],
                                         q_wines_perc[5],
                                         q_wines_perc[6],
                                         q_wines_perc[7],
                                         q_wines_perc[8],
                                         q_wines_perc[9],
                                         q_wines_perc[10]),
                               labels=c('<= 10%', '<= 20%', '<= 30%', '<= 40%', '<= 50%',
                                        '<= 60%', '<= 70%', '<= 80%', '<= 90%', '<= 100%'),
                               right=T)

CrossTable(df_wines_perc$fx_wines_perc, df_wines_perc$Response, chisq = T)

## KS test

par(mfrow = c(1,1))

ks.test(df_wines_perc[df_wines_perc$Response == 0,]$wines_perc,
        df_wines_perc[df_wines_perc$Response == 1,]$wines_perc)
plot(ecdf(df_wines_perc[df_wines_perc$Response == 0,]$wines_perc),
     xlim = range(c(df_wines_perc[df_wines_perc$Response == 0,]$wines_perc,
                    df_wines_perc[df_wines_perc$Response == 1,]$wines_perc)), col = 'red')
plot(ecdf(df_wines_perc[df_wines_perc$Response == 1,]$wines_perc),
     add = TRUE, lty = "dashed", col = 'blue')

#Conclusion: not significant high p-value for chi-square test and ks test


### Analyzing the feature distribution, regarding the target variable ###

wines_perc_quant <- quantile(df$wines_perc, probs = c(0, 0.1, 0.2, 0.3, 0.4, 0.5,
                                                  0.6, 0.7, 0.8, 0.9, 1))

df$faixa_wines_perc <- as.numeric(cut(df$wines_perc,
                                    breaks= c(unique(wines_perc_quant)),
                                    labels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                                    right=F))

df$faixa_wines_perc[is.na(df$faixa_wines_perc)] <- 10

hist(df$faixa_wines_perc)

wines_perc_faixa <- df %>%
  group_by(faixa_wines_perc) %>%
  summarise(obs = n(), wines_perc = mean(wines_perc), Response = sum(Response),
            pct_Response = sum(Response)/n())

hist(wines_perc_faixa$pct_Response)
par(mfrow = c(1,1))
plot(wines_perc_faixa$wines_perc, wines_perc_faixa$pct_Response)
abline(h=sum(Response) / nrow(df), col="red")

feature <- c()
p_chisq_test <- c()
p_ks_test <- c()
min_value <- c()
max_value <- c()
min_no_outlier <- c()
max_no_outlier <- c()

p_chisq_test <- CrossTable(df_wines_perc$fx_wines_perc, df_wines_perc$Response, chisq = T)$chisq[3]$p.value
df_stats11 <- data.frame('wines_perc', p_chisq_test)

df_stats11

colnames(df_stats11) <- c('feature', 'p-value')

df_stats <- rbind(df_stats, df_stats11)


##########################################################################################


##MntFruits

hist(MntFruits)
boxplot(MntFruits)
boxplot(MntFruits ~ Response) # Comparing with target variable

#Analyzing
par(mfrow = c(2,1))

noout_MntFruits <- remove_outlier2(MntFruits)
noout_MntFruits2 <- noout_MntFruits[!is.na(noout_MntFruits)]
hist(noout_MntFruits)
out_MntFruits <- outlier2(MntFruits)
out_MntFruits2 <- out_MntFruits[!is.na(out_MntFruits)]
hist(out_MntFruits2)
max(noout_MntFruits2)
min(noout_MntFruits2)

boxplot(noout_MntFruits2)
boxplot(out_MntFruits2)

out_MntFruits2_box <- df[df$MntFruits > max(noout_MntFruits2),c("MntFruits", "Response")]
boxplot(out_MntFruits2_box$MntFruits ~ out_MntFruits2_box$Response)
noout_MntFruits2_box <- df[df$MntFruits <= max(noout_MntFruits2), c("MntFruits", "Response")]
boxplot(noout_MntFruits2_box$MntFruits ~ noout_MntFruits2_box$Response)

length(out_MntFruits2) / nrow(df)
#2.9% outliers

prop.table(table(noout_MntFruits2_box$Response))
#14.6% of positive Response
prop.table(table(out_MntFruits2_box$Response))
#29,7% of positive Response

## Transforming into bins to run chis-square test

q_MntFruits <- quantile(MntFruits, probs = c(0.1, 0.2, 0.3, 0.4, 0.5,
                                       0.6, 0.7, 0.8, 0.9, 1))
q_MntFruits

df_MntFruits <- df[, c('MntFruits', 'Response')]

df_MntFruits$fx_MntFruits <- cut(df_MntFruits$MntFruits,
                           breaks= c(-10000000,
                                     q_MntFruits[1],
                                     q_MntFruits[2],
                                     q_MntFruits[3],
                                     q_MntFruits[4],
                                     q_MntFruits[5],
                                     q_MntFruits[6],
                                     q_MntFruits[7],
                                     q_MntFruits[8],
                                     q_MntFruits[9],
                                     q_MntFruits[10]),
                           labels=c('<= 10%', '<= 20%', '<= 30%', '<= 40%', '<= 50%',
                                   '<= 60%', '<= 70%', '<= 80%', '<= 90%', '<= 100%'),
                           right=T)

CrossTable(df_MntFruits$fx_MntFruits, df_MntFruits$Response, chisq = T)

## KS test

par(mfrow = c(1,1))

ks.test(df_MntFruits[df_MntFruits$Response == 0,]$MntFruits,
        df_MntFruits[df_MntFruits$Response == 1,]$MntFruits)
plot(ecdf(df_MntFruits[df_MntFruits$Response == 0,]$MntFruits),
     xlim = range(c(df_MntFruits[df_MntFruits$Response == 0,]$MntFruits,
                    df_MntFruits[df_MntFruits$Response == 1,]$MntFruits)), col = 'red')
plot(ecdf(df_MntFruits[df_MntFruits$Response == 1,]$MntFruits),
     add = TRUE, lty = "dashed", col = 'blue')

#Conclusion: significant low p-value for chi-square test and ks test


### Analyzing the feature distribution, regarding the target variable ###

MntFruits_quant <- quantile(df$MntFruits, probs = c(0.2, 0.3, 0.4, 0.5,
                                                0.6, 0.7, 0.8, 0.9, 1))

df$faixa_MntFruits <- as.numeric(cut(df$MntFruits,
                                   breaks= c(unique(MntFruits_quant)),
                                   labels=c(1, 2, 3, 4, 5, 6, 7, 8),
                                   right=F))

df$faixa_MntFruits[is.na(df$faixa_MntFruits)] <- 8

hist(df$faixa_MntFruits)

MntFruits_faixa <- df %>%
  group_by(faixa_MntFruits) %>%
  summarise(obs = n(), MntFruits = mean(MntFruits), Response = sum(Response),
            pct_Response = sum(Response)/n())

hist(MntFruits_faixa$pct_Response)
par(mfrow = c(1,1))
plot(MntFruits_faixa$MntFruits, MntFruits_faixa$pct_Response)
abline(h=sum(Response) / nrow(df), col="red")

feature <- c()
p_chisq_test <- c()
p_ks_test <- c()
min_value <- c()
max_value <- c()
min_no_outlier <- c()
max_no_outlier <- c()

p_chisq_test <- CrossTable(df_MntFruits$fx_MntFruits, df_MntFruits$Response, chisq = T)$chisq[3]$p.value
df_stats12 <- data.frame('MntFruits', p_chisq_test)

df_stats12

colnames(df_stats12) <- c('feature', 'p-value')

df_stats <- rbind(df_stats, df_stats12)


##########################################################################################


##fruits_perc

hist(fruits_perc)
boxplot(fruits_perc)
boxplot(fruits_perc ~ Response) # Comparing with target variable

#Analyzing
par(mfrow = c(2,1))

noout_fruits_perc <- remove_outlier2(fruits_perc)
noout_fruits_perc2 <- noout_fruits_perc[!is.na(noout_fruits_perc)]
hist(noout_fruits_perc)
out_fruits_perc <- outlier2(fruits_perc)
out_fruits_perc2 <- out_fruits_perc[!is.na(out_fruits_perc)]
hist(out_fruits_perc2)
max(noout_fruits_perc2)
min(noout_fruits_perc2)

boxplot(noout_fruits_perc2)
boxplot(out_fruits_perc2)

out_fruits_perc2_box <- df[df$fruits_perc > max(noout_fruits_perc2),c("fruits_perc", "Response")]
boxplot(out_fruits_perc2_box$fruits_perc ~ out_fruits_perc2_box$Response)
noout_fruits_perc2_box <- df[df$fruits_perc <= max(noout_fruits_perc2), c("fruits_perc", "Response")]
boxplot(noout_fruits_perc2_box$fruits_perc ~ noout_fruits_perc2_box$Response)

length(out_fruits_perc2) / nrow(df)
#2.1% outliers

prop.table(table(noout_fruits_perc2_box$Response))
#14.9% of positive Response
prop.table(table(out_fruits_perc2_box$Response))
#19,6% of positive Response

## Transforming into bins to run chis-square test

q_fruits_perc <- quantile(fruits_perc, probs = c(0.1, 0.2, 0.3, 0.4, 0.5,
                                             0.6, 0.7, 0.8, 0.9, 1))
q_fruits_perc

df_fruits_perc <- df[, c('fruits_perc', 'Response')]

df_fruits_perc$fx_fruits_perc <- cut(df_fruits_perc$fruits_perc,
                                 breaks= c(-10000000,
                                           q_fruits_perc[1],
                                           q_fruits_perc[2],
                                           q_fruits_perc[3],
                                           q_fruits_perc[4],
                                           q_fruits_perc[5],
                                           q_fruits_perc[6],
                                           q_fruits_perc[7],
                                           q_fruits_perc[8],
                                           q_fruits_perc[9],
                                           q_fruits_perc[10]),
                                 labels=c('<= 10%', '<= 20%', '<= 30%', '<= 40%', '<= 50%',
                                          '<= 60%', '<= 70%', '<= 80%', '<= 90%', '<= 100%'),
                                 right=T)

CrossTable(df_fruits_perc$fx_fruits_perc, df_fruits_perc$Response, chisq = T)

## KS test

par(mfrow = c(1,1))

ks.test(df_fruits_perc[df_fruits_perc$Response == 0,]$fruits_perc,
        df_fruits_perc[df_fruits_perc$Response == 1,]$fruits_perc)
plot(ecdf(df_fruits_perc[df_fruits_perc$Response == 0,]$fruits_perc),
     xlim = range(c(df_fruits_perc[df_fruits_perc$Response == 0,]$fruits_perc,
                    df_fruits_perc[df_fruits_perc$Response == 1,]$fruits_perc)), col = 'red')
plot(ecdf(df_fruits_perc[df_fruits_perc$Response == 1,]$fruits_perc),
     add = TRUE, lty = "dashed", col = 'blue')

#Conclusion: significant low p-value for chi-square test and ks test


### Analyzing the feature distribution, regarding the target variable ###

fruits_perc_quant <- quantile(df$fruits_perc, probs = c(0, 0.1, 0.2, 0.3, 0.4, 0.5,
                                                    0.6, 0.7, 0.8, 0.9, 1))

df$faixa_fruits_perc <- as.numeric(cut(df$fruits_perc,
                                     breaks= c(unique(fruits_perc_quant)),
                                     labels=c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                                     right=F))

df$faixa_fruits_perc[is.na(df$faixa_fruits_perc)] <- 9

hist(df$faixa_fruits_perc)

fruits_perc_faixa <- df %>%
  group_by(faixa_fruits_perc) %>%
  summarise(obs = n(), fruits_perc = mean(fruits_perc), Response = sum(Response),
            pct_Response = sum(Response)/n())

hist(fruits_perc_faixa$pct_Response)
par(mfrow = c(1,1))
plot(fruits_perc_faixa$fruits_perc, fruits_perc_faixa$pct_Response)
abline(h=sum(Response) / nrow(df), col="red")

feature <- c()
p_chisq_test <- c()
p_ks_test <- c()
min_value <- c()
max_value <- c()
min_no_outlier <- c()
max_no_outlier <- c()

p_chisq_test <- CrossTable(df_fruits_perc$fx_fruits_perc, df_fruits_perc$Response, chisq = T)$chisq[3]$p.value
df_stats13 <- data.frame('fruits_perc', p_chisq_test)

df_stats13

colnames(df_stats13) <- c('feature', 'p-value')

df_stats <- rbind(df_stats, df_stats13)


##########################################################################################


##MntMeatProducts

hist(MntMeatProducts)
boxplot(MntMeatProducts)
boxplot(MntMeatProducts ~ Response) # Comparing with target variable

#Analyzing
par(mfrow = c(2,1))

noout_MntMeatProducts <- remove_outlier2(MntMeatProducts)
noout_MntMeatProducts2 <- noout_MntMeatProducts[!is.na(noout_MntMeatProducts)]
hist(noout_MntMeatProducts)
out_MntMeatProducts <- outlier2(MntMeatProducts)
out_MntMeatProducts2 <- out_MntMeatProducts[!is.na(out_MntMeatProducts)]
hist(out_MntMeatProducts2)
max(noout_MntMeatProducts2)
min(noout_MntMeatProducts2)

boxplot(noout_MntMeatProducts2)
boxplot(out_MntMeatProducts2)

out_MntMeatProducts2_box <- df[df$MntMeatProducts > max(noout_MntMeatProducts2),c("MntMeatProducts", "Response")]
boxplot(out_MntMeatProducts2_box$MntMeatProducts ~ out_MntMeatProducts2_box$Response)
noout_MntMeatProducts2_box <- df[df$MntMeatProducts <= max(noout_MntMeatProducts2), c("MntMeatProducts", "Response")]
boxplot(noout_MntMeatProducts2_box$MntMeatProducts ~ noout_MntMeatProducts2_box$Response)

length(out_MntMeatProducts2) / nrow(df)
#1.8% outliers

prop.table(table(noout_MntMeatProducts2_box$Response))
#14.4% of positive Response
prop.table(table(out_MntMeatProducts2_box$Response))
#51.3% of positive Response

## Transforming into bins to run chis-square test

q_MntMeatProducts <- quantile(MntMeatProducts, probs = c(0.1, 0.2, 0.3, 0.4, 0.5,
                                               0.6, 0.7, 0.8, 0.9, 1))
q_MntMeatProducts

df_MntMeatProducts <- df[, c('MntMeatProducts', 'Response')]

df_MntMeatProducts$fx_MntMeatProducts <- cut(df_MntMeatProducts$MntMeatProducts,
                                   breaks= c(-10000000,
                                             q_MntMeatProducts[1],
                                             q_MntMeatProducts[2],
                                             q_MntMeatProducts[3],
                                             q_MntMeatProducts[4],
                                             q_MntMeatProducts[5],
                                             q_MntMeatProducts[6],
                                             q_MntMeatProducts[7],
                                             q_MntMeatProducts[8],
                                             q_MntMeatProducts[9],
                                             q_MntMeatProducts[10]),
                                   labels=c('<= 10%', '<= 20%', '<= 30%', '<= 40%', '<= 50%',
                                            '<= 60%', '<= 70%', '<= 80%', '<= 90%', '<= 100%'),
                                   right=T)

CrossTable(df_MntMeatProducts$fx_MntMeatProducts, df_MntMeatProducts$Response, chisq = T)

## KS test

par(mfrow = c(1,1))

ks.test(df_MntMeatProducts[df_MntMeatProducts$Response == 0,]$MntMeatProducts,
        df_MntMeatProducts[df_MntMeatProducts$Response == 1,]$MntMeatProducts)
plot(ecdf(df_MntMeatProducts[df_MntMeatProducts$Response == 0,]$MntMeatProducts),
     xlim = range(c(df_MntMeatProducts[df_MntMeatProducts$Response == 0,]$MntMeatProducts,
                    df_MntMeatProducts[df_MntMeatProducts$Response == 1,]$MntMeatProducts)), col = 'red')
plot(ecdf(df_MntMeatProducts[df_MntMeatProducts$Response == 1,]$MntMeatProducts),
     add = TRUE, lty = "dashed", col = 'blue')

#Conclusion: significant low p-value for chi-square test and ks test


### Analyzing the feature distribution, regarding the target variable ###

MntMeatProducts_quant <- quantile(df$MntMeatProducts, probs = c(0, 0.1, 0.2, 0.3, 0.4, 0.5,
                                                      0.6, 0.7, 0.8, 0.9, 1))

df$faixa_MntMeatProducts <- as.numeric(cut(df$MntMeatProducts,
                                      breaks= c(unique(MntMeatProducts_quant)),
                                      labels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                                      right=F))

df$faixa_MntMeatProducts[is.na(df$faixa_MntMeatProducts)] <- 10

hist(df$faixa_MntMeatProducts)

MntMeatProducts_faixa <- df %>%
  group_by(faixa_MntMeatProducts) %>%
  summarise(obs = n(), MntMeatProducts = mean(MntMeatProducts), Response = sum(Response),
            pct_Response = sum(Response)/n())

hist(MntMeatProducts_faixa$pct_Response)
par(mfrow = c(1,1))
plot(MntMeatProducts_faixa$MntMeatProducts, MntMeatProducts_faixa$pct_Response)
abline(h=sum(Response) / nrow(df), col="red")

feature <- c()
p_chisq_test <- c()
p_ks_test <- c()
min_value <- c()
max_value <- c()
min_no_outlier <- c()
max_no_outlier <- c()

p_chisq_test <- CrossTable(df_MntMeatProducts$fx_MntMeatProducts, df_MntMeatProducts$Response, chisq = T)$chisq[3]$p.value
df_stats14 <- data.frame('MntMeatProducts', p_chisq_test)

df_stats14

colnames(df_stats14) <- c('feature', 'p-value')

df_stats <- rbind(df_stats, df_stats14)


##########################################################################################


##meat_perc

hist(meat_perc)
boxplot(meat_perc)
boxplot(meat_perc ~ Response) # Comparing with target variable

#Analyzing
par(mfrow = c(2,1))

noout_meat_perc <- remove_outlier2(meat_perc)
noout_meat_perc2 <- noout_meat_perc[!is.na(noout_meat_perc)]
hist(noout_meat_perc)
out_meat_perc <- outlier2(meat_perc)
out_meat_perc2 <- out_meat_perc[!is.na(out_meat_perc)]
hist(out_meat_perc2)
max(noout_meat_perc2)
min(noout_meat_perc2)

boxplot(noout_meat_perc2)
boxplot(out_meat_perc2)

out_meat_perc2_box <- df[df$meat_perc > max(noout_meat_perc2),c("meat_perc", "Response")]
boxplot(out_meat_perc2_box$meat_perc ~ out_meat_perc2_box$Response)
noout_meat_perc2_box <- df[df$meat_perc <= max(noout_meat_perc2), c("meat_perc", "Response")]
boxplot(noout_meat_perc2_box$meat_perc ~ noout_meat_perc2_box$Response)

length(out_meat_perc2) / nrow(df)
#0.3% outliers

prop.table(table(noout_meat_perc2_box$Response))
#15% of positive Response
prop.table(table(out_meat_perc2_box$Response))
#33.3% of positive Response

## Transforming into bins to run chis-square test

q_meat_perc <- quantile(meat_perc, probs = c(0.1, 0.2, 0.3, 0.4, 0.5,
                                                         0.6, 0.7, 0.8, 0.9, 1))
q_meat_perc

df_meat_perc <- df[, c('meat_perc', 'Response')]

df_meat_perc$fx_meat_perc <- cut(df_meat_perc$meat_perc,
                                             breaks= c(-10000000,
                                                       q_meat_perc[1],
                                                       q_meat_perc[2],
                                                       q_meat_perc[3],
                                                       q_meat_perc[4],
                                                       q_meat_perc[5],
                                                       q_meat_perc[6],
                                                       q_meat_perc[7],
                                                       q_meat_perc[8],
                                                       q_meat_perc[9],
                                                       q_meat_perc[10]),
                                             labels=c('<= 10%', '<= 20%', '<= 30%', '<= 40%', '<= 50%',
                                                      '<= 60%', '<= 70%', '<= 80%', '<= 90%', '<= 100%'),
                                             right=T)

CrossTable(df_meat_perc$fx_meat_perc, df_meat_perc$Response, chisq = T)

## KS test

par(mfrow = c(1,1))

ks.test(df_meat_perc[df_meat_perc$Response == 0,]$meat_perc,
        df_meat_perc[df_meat_perc$Response == 1,]$meat_perc)
plot(ecdf(df_meat_perc[df_meat_perc$Response == 0,]$meat_perc),
     xlim = range(c(df_meat_perc[df_meat_perc$Response == 0,]$meat_perc,
                    df_meat_perc[df_meat_perc$Response == 1,]$meat_perc)), col = 'red')
plot(ecdf(df_meat_perc[df_meat_perc$Response == 1,]$meat_perc),
     add = TRUE, lty = "dashed", col = 'blue')

#Conclusion: significant low p-value for chi-square test and ks test


### Analyzing the feature distribution, regarding the target variable ###

meat_perc_quant <- quantile(df$meat_perc, probs = c(0, 0.1, 0.2, 0.3, 0.4, 0.5,
                                                                0.6, 0.7, 0.8, 0.9, 1))

df$faixa_meat_perc <- as.numeric(cut(df$meat_perc,
                                           breaks= c(unique(meat_perc_quant)),
                                           labels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                                           right=F))

df$faixa_meat_perc[is.na(df$faixa_meat_perc)] <- 10

hist(df$faixa_meat_perc)

meat_perc_faixa <- df %>%
  group_by(faixa_meat_perc) %>%
  summarise(obs = n(), meat_perc = mean(meat_perc), Response = sum(Response),
            pct_Response = sum(Response)/n())

hist(meat_perc_faixa$pct_Response)
par(mfrow = c(1,1))
plot(meat_perc_faixa$meat_perc, meat_perc_faixa$pct_Response)
abline(h=sum(Response) / nrow(df), col="red")

feature <- c()
p_chisq_test <- c()
p_ks_test <- c()
min_value <- c()
max_value <- c()
min_no_outlier <- c()
max_no_outlier <- c()

p_chisq_test <- CrossTable(df_meat_perc$fx_meat_perc, df_meat_perc$Response, chisq = T)$chisq[3]$p.value
df_stats15 <- data.frame('meat_perc', p_chisq_test)

df_stats15

colnames(df_stats15) <- c('feature', 'p-value')

df_stats <- rbind(df_stats, df_stats15)


##########################################################################################


##MntFishProducts

hist(MntFishProducts)
boxplot(MntFishProducts)
boxplot(MntFishProducts ~ Response) # Comparing with target variable

#Analyzing
par(mfrow = c(2,1))

noout_MntFishProducts <- remove_outlier2(MntFishProducts)
noout_MntFishProducts2 <- noout_MntFishProducts[!is.na(noout_MntFishProducts)]
hist(noout_MntFishProducts)
out_MntFishProducts <- outlier2(MntFishProducts)
out_MntFishProducts2 <- out_MntFishProducts[!is.na(out_MntFishProducts)]
hist(out_MntFishProducts2)
max(noout_MntFishProducts2)
min(noout_MntFishProducts2)

boxplot(noout_MntFishProducts2)
boxplot(out_MntFishProducts2)

out_MntFishProducts2_box <- df[df$MntFishProducts > max(noout_MntFishProducts2),c("MntFishProducts", "Response")]
boxplot(out_MntFishProducts2_box$MntFishProducts ~ out_MntFishProducts2_box$Response)
noout_MntFishProducts2_box <- df[df$MntFishProducts <= max(noout_MntFishProducts2), c("MntFishProducts", "Response")]
boxplot(noout_MntFishProducts2_box$MntFishProducts ~ noout_MntFishProducts2_box$Response)

length(out_MntFishProducts2) / nrow(df)
#2.6% outliers

prop.table(table(noout_MntFishProducts2_box$Response))
#14.8% of positive Response
prop.table(table(out_MntFishProducts2_box$Response))
#22.4% of positive Response

## Transforming into bins to run chis-square test

q_MntFishProducts <- quantile(MntFishProducts, probs = c(0.1, 0.2, 0.3, 0.4, 0.5,
                                             0.6, 0.7, 0.8, 0.9, 1))
q_MntFishProducts

df_MntFishProducts <- df[, c('MntFishProducts', 'Response')]

df_MntFishProducts$fx_MntFishProducts <- cut(df_MntFishProducts$MntFishProducts,
                                 breaks= c(-10000000,
                                           q_MntFishProducts[1],
                                           q_MntFishProducts[2],
                                           q_MntFishProducts[3],
                                           q_MntFishProducts[4],
                                           q_MntFishProducts[5],
                                           q_MntFishProducts[6],
                                           q_MntFishProducts[7],
                                           q_MntFishProducts[8],
                                           q_MntFishProducts[9],
                                           q_MntFishProducts[10]),
                                 labels=c('<= 10%', '<= 20%', '<= 30%', '<= 40%', '<= 50%',
                                          '<= 60%', '<= 70%', '<= 80%', '<= 90%', '<= 100%'),
                                 right=T)

CrossTable(df_MntFishProducts$fx_MntFishProducts, df_MntFishProducts$Response, chisq = T)

## KS test

par(mfrow = c(1,1))

ks.test(df_MntFishProducts[df_MntFishProducts$Response == 0,]$MntFishProducts,
        df_MntFishProducts[df_MntFishProducts$Response == 1,]$MntFishProducts)
plot(ecdf(df_MntFishProducts[df_MntFishProducts$Response == 0,]$MntFishProducts),
     xlim = range(c(df_MntFishProducts[df_MntFishProducts$Response == 0,]$MntFishProducts,
                    df_MntFishProducts[df_MntFishProducts$Response == 1,]$MntFishProducts)), col = 'red')
plot(ecdf(df_MntFishProducts[df_MntFishProducts$Response == 1,]$MntFishProducts),
     add = TRUE, lty = "dashed", col = 'blue')

#Conclusion: significant low p-value for chi-square test and ks test


### Analyzing the feature distribution, regarding the target variable ###

MntFishProducts_quant <- quantile(df$MntFishProducts, probs = c(0, 0.1, 0.2, 0.3, 0.4, 0.5,
                                                    0.6, 0.7, 0.8, 0.9, 1))

df$faixa_MntFishProducts <- as.numeric(cut(df$MntFishProducts,
                                     breaks= c(unique(MntFishProducts_quant)),
                                     labels=c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                                     right=F))

df$faixa_MntFishProducts[is.na(df$faixa_MntFishProducts)] <- 9

hist(df$faixa_MntFishProducts)

MntFishProducts_faixa <- df %>%
  group_by(faixa_MntFishProducts) %>%
  summarise(obs = n(), MntFishProducts = mean(MntFishProducts), Response = sum(Response),
            pct_Response = sum(Response)/n())

hist(MntFishProducts_faixa$pct_Response)
par(mfrow = c(1,1))
plot(MntFishProducts_faixa$MntFishProducts, MntFishProducts_faixa$pct_Response)
abline(h=sum(Response) / nrow(df), col="red")

feature <- c()
p_chisq_test <- c()
p_ks_test <- c()
min_value <- c()
max_value <- c()
min_no_outlier <- c()
max_no_outlier <- c()

p_chisq_test <- CrossTable(df_MntFishProducts$fx_MntFishProducts, df_MntFishProducts$Response, chisq = T)$chisq[3]$p.value
df_stats16 <- data.frame('MntFishProducts', p_chisq_test)

df_stats16

colnames(df_stats16) <- c('feature', 'p-value')

df_stats <- rbind(df_stats, df_stats16)


##########################################################################################


##fish_perc

hist(fish_perc)
boxplot(fish_perc)
boxplot(fish_perc ~ Response) # Comparing with target variable

#Analyzing
par(mfrow = c(2,1))

noout_fish_perc <- remove_outlier2(fish_perc)
noout_fish_perc2 <- noout_fish_perc[!is.na(noout_fish_perc)]
hist(noout_fish_perc)
out_fish_perc <- outlier2(fish_perc)
out_fish_perc2 <- out_fish_perc[!is.na(out_fish_perc)]
hist(out_fish_perc2)
max(noout_fish_perc2)
min(noout_fish_perc2)

boxplot(noout_fish_perc2)
boxplot(out_fish_perc2)

out_fish_perc2_box <- df[df$fish_perc > max(noout_fish_perc2),c("fish_perc", "Response")]
boxplot(out_fish_perc2_box$fish_perc ~ out_fish_perc2_box$Response)
noout_fish_perc2_box <- df[df$fish_perc <= max(noout_fish_perc2), c("fish_perc", "Response")]
boxplot(noout_fish_perc2_box$fish_perc ~ noout_fish_perc2_box$Response)

length(out_fish_perc2) / nrow(df)
#2% outliers

prop.table(table(noout_fish_perc2_box$Response))
#15% of positive Response
prop.table(table(out_fish_perc2_box$Response))
#???% of positive Response

## Transforming into bins to run chis-square test

q_fish_perc <- quantile(fish_perc, probs = c(0.1, 0.2, 0.3, 0.4, 0.5,
                                             0.6, 0.7, 0.8, 0.9, 1))
q_fish_perc

df_fish_perc <- df[, c('fish_perc', 'Response')]

df_fish_perc$fx_fish_perc <- cut(df_fish_perc$fish_perc,
                                 breaks= c(-10000000,
                                           q_fish_perc[1],
                                           q_fish_perc[2],
                                           q_fish_perc[3],
                                           q_fish_perc[4],
                                           q_fish_perc[5],
                                           q_fish_perc[6],
                                           q_fish_perc[7],
                                           q_fish_perc[8],
                                           q_fish_perc[9],
                                           q_fish_perc[10]),
                                 labels=c('<= 10%', '<= 20%', '<= 30%', '<= 40%', '<= 50%',
                                          '<= 60%', '<= 70%', '<= 80%', '<= 90%', '<= 100%'),
                                 right=T)

CrossTable(df_fish_perc$fx_fish_perc, df_fish_perc$Response, chisq = T)

## KS test

par(mfrow = c(1,1))

ks.test(df_fish_perc[df_fish_perc$Response == 0,]$fish_perc,
        df_fish_perc[df_fish_perc$Response == 1,]$fish_perc)
plot(ecdf(df_fish_perc[df_fish_perc$Response == 0,]$fish_perc),
     xlim = range(c(df_fish_perc[df_fish_perc$Response == 0,]$fish_perc,
                    df_fish_perc[df_fish_perc$Response == 1,]$fish_perc)), col = 'red')
plot(ecdf(df_fish_perc[df_fish_perc$Response == 1,]$fish_perc),
     add = TRUE, lty = "dashed", col = 'blue')

#Conclusion: significant low p-value for chi-square test and ks test


### Analyzing the feature distribution, regarding the target variable ###

fish_perc_quant <- quantile(df$fish_perc, probs = c(0, 0.1, 0.2, 0.3, 0.4, 0.5,
                                                    0.6, 0.7, 0.8, 0.9, 1))

df$faixa_fish_perc <- as.numeric(cut(df$fish_perc,
                                     breaks= c(unique(fish_perc_quant)),
                                     labels=c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                                     right=F))

df$faixa_fish_perc[is.na(df$faixa_fish_perc)] <- 9

hist(df$faixa_fish_perc)

fish_perc_faixa <- df %>%
  group_by(faixa_fish_perc) %>%
  summarise(obs = n(), fish_perc = mean(fish_perc), Response = sum(Response),
            pct_Response = sum(Response)/n())

hist(fish_perc_faixa$pct_Response)
par(mfrow = c(1,1))
plot(fish_perc_faixa$fish_perc, fish_perc_faixa$pct_Response)
abline(h=sum(Response) / nrow(df), col="red")

feature <- c()
p_chisq_test <- c()
p_ks_test <- c()
min_value <- c()
max_value <- c()
min_no_outlier <- c()
max_no_outlier <- c()

p_chisq_test <- CrossTable(df_fish_perc$fx_fish_perc, df_fish_perc$Response, chisq = T)$chisq[3]$p.value
df_stats17 <- data.frame('fish_perc', p_chisq_test)

df_stats17

colnames(df_stats17) <- c('feature', 'p-value')

df_stats <- rbind(df_stats, df_stats17)


##########################################################################################


##MntSweetProducts

hist(MntSweetProducts)
boxplot(MntSweetProducts)
boxplot(MntSweetProducts ~ Response) # Comparing with target variable

#Analyzing
par(mfrow = c(2,1))

noout_MntSweetProducts <- remove_outlier2(MntSweetProducts)
noout_MntSweetProducts2 <- noout_MntSweetProducts[!is.na(noout_MntSweetProducts)]
hist(noout_MntSweetProducts)
out_MntSweetProducts <- outlier2(MntSweetProducts)
out_MntSweetProducts2 <- out_MntSweetProducts[!is.na(out_MntSweetProducts)]
hist(out_MntSweetProducts2)
max(noout_MntSweetProducts2)
min(noout_MntSweetProducts2)

boxplot(noout_MntSweetProducts2)
boxplot(out_MntSweetProducts2)

out_MntSweetProducts2_box <- df[df$MntSweetProducts > max(noout_MntSweetProducts2),c("MntSweetProducts", "Response")]
boxplot(out_MntSweetProducts2_box$MntSweetProducts ~ out_MntSweetProducts2_box$Response)
noout_MntSweetProducts2_box <- df[df$MntSweetProducts <= max(noout_MntSweetProducts2), c("MntSweetProducts", "Response")]
boxplot(noout_MntSweetProducts2_box$MntSweetProducts ~ noout_MntSweetProducts2_box$Response)

length(out_MntSweetProducts2) / nrow(df)
#2.8% outliers

prop.table(table(noout_MntSweetProducts2_box$Response))
#14.8% of positive Response
prop.table(table(out_MntSweetProducts2_box$Response))
#21.3% of positive Response

## Transforming into bins to run chis-square test

q_MntSweetProducts <- quantile(MntSweetProducts, probs = c(0.1, 0.2, 0.3, 0.4, 0.5,
                                             0.6, 0.7, 0.8, 0.9, 1))
q_MntSweetProducts

df_MntSweetProducts <- df[, c('MntSweetProducts', 'Response')]

df_MntSweetProducts$fx_MntSweetProducts <- cut(df_MntSweetProducts$MntSweetProducts,
                                 breaks= c(-10000000,
                                           q_MntSweetProducts[1],
                                           q_MntSweetProducts[2],
                                           q_MntSweetProducts[3],
                                           q_MntSweetProducts[4],
                                           q_MntSweetProducts[5],
                                           q_MntSweetProducts[6],
                                           q_MntSweetProducts[7],
                                           q_MntSweetProducts[8],
                                           q_MntSweetProducts[9],
                                           q_MntSweetProducts[10]),
                                 labels=c('<= 10%', '<= 20%', '<= 30%', '<= 40%', '<= 50%',
                                          '<= 60%', '<= 70%', '<= 80%', '<= 90%', '<= 100%'),
                                 right=T)

CrossTable(df_MntSweetProducts$fx_MntSweetProducts, df_MntSweetProducts$Response, chisq = T)

## KS test

par(mfrow = c(1,1))

ks.test(df_MntSweetProducts[df_MntSweetProducts$Response == 0,]$MntSweetProducts,
        df_MntSweetProducts[df_MntSweetProducts$Response == 1,]$MntSweetProducts)
plot(ecdf(df_MntSweetProducts[df_MntSweetProducts$Response == 0,]$MntSweetProducts),
     xlim = range(c(df_MntSweetProducts[df_MntSweetProducts$Response == 0,]$MntSweetProducts,
                    df_MntSweetProducts[df_MntSweetProducts$Response == 1,]$MntSweetProducts)), col = 'red')
plot(ecdf(df_MntSweetProducts[df_MntSweetProducts$Response == 1,]$MntSweetProducts),
     add = TRUE, lty = "dashed", col = 'blue')

#Conclusion: significant low p-value for chi-square test and ks test


### Analyzing the feature distribution, regarding the target variable ###

MntSweetProducts_quant <- quantile(df$MntSweetProducts, probs = c(0, 0.1, 0.2, 0.3, 0.4, 0.5,
                                                    0.6, 0.7, 0.8, 0.9, 1))

df$faixa_MntSweetProducts <- as.numeric(cut(df$MntSweetProducts,
                                     breaks= c(unique(MntSweetProducts_quant)),
                                     labels=c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                                     right=F))

df$faixa_MntSweetProducts[is.na(df$faixa_MntSweetProducts)] <- 9

hist(df$faixa_MntSweetProducts)

MntSweetProducts_faixa <- df %>%
  group_by(faixa_MntSweetProducts) %>%
  summarise(obs = n(), MntSweetProducts = mean(MntSweetProducts), Response = sum(Response),
            pct_Response = sum(Response)/n())

hist(MntSweetProducts_faixa$pct_Response)
par(mfrow = c(1,1))
plot(MntSweetProducts_faixa$MntSweetProducts, MntSweetProducts_faixa$pct_Response)
abline(h=sum(Response) / nrow(df), col="red")

feature <- c()
p_chisq_test <- c()
p_ks_test <- c()
min_value <- c()
max_value <- c()
min_no_outlier <- c()
max_no_outlier <- c()

p_chisq_test <- CrossTable(df_MntSweetProducts$fx_MntSweetProducts, df_MntSweetProducts$Response, chisq = T)$chisq[3]$p.value
df_stats18 <- data.frame('MntSweetProducts', p_chisq_test)

df_stats18

colnames(df_stats18) <- c('feature', 'p-value')

df_stats <- rbind(df_stats, df_stats18)


##########################################################################################


##sweet_perc

hist(sweet_perc)
boxplot(sweet_perc)
boxplot(sweet_perc ~ Response) # Comparing with target variable

#Analyzing
par(mfrow = c(2,1))

noout_sweet_perc <- remove_outlier2(sweet_perc)
noout_sweet_perc2 <- noout_sweet_perc[!is.na(noout_sweet_perc)]
hist(noout_sweet_perc)
out_sweet_perc <- outlier2(sweet_perc)
out_sweet_perc2 <- out_sweet_perc[!is.na(out_sweet_perc)]
hist(out_sweet_perc2)
max(noout_sweet_perc2)
min(noout_sweet_perc2)

boxplot(noout_sweet_perc2)
boxplot(out_sweet_perc2)

out_sweet_perc2_box <- df[df$sweet_perc > max(noout_sweet_perc2),c("sweet_perc", "Response")]
boxplot(out_sweet_perc2_box$sweet_perc ~ out_sweet_perc2_box$Response)
noout_sweet_perc2_box <- df[df$sweet_perc <= max(noout_sweet_perc2), c("sweet_perc", "Response")]
boxplot(noout_sweet_perc2_box$sweet_perc ~ noout_sweet_perc2_box$Response)

length(out_sweet_perc2) / nrow(df)
#2.2% outliers

prop.table(table(noout_sweet_perc2_box$Response))
#15% of positive Response
prop.table(table(out_sweet_perc2_box$Response))
#16.3% of positive Response

## Transforming into bins to run chis-square test

q_sweet_perc <- quantile(sweet_perc, probs = c(0.1, 0.2, 0.3, 0.4, 0.5,
                                                           0.6, 0.7, 0.8, 0.9, 1))
q_sweet_perc

df_sweet_perc <- df[, c('sweet_perc', 'Response')]

df_sweet_perc$fx_sweet_perc <- cut(df_sweet_perc$sweet_perc,
                                               breaks= c(-10000000,
                                                         q_sweet_perc[1],
                                                         q_sweet_perc[2],
                                                         q_sweet_perc[3],
                                                         q_sweet_perc[4],
                                                         q_sweet_perc[5],
                                                         q_sweet_perc[6],
                                                         q_sweet_perc[7],
                                                         q_sweet_perc[8],
                                                         q_sweet_perc[9],
                                                         q_sweet_perc[10]),
                                               labels=c('<= 10%', '<= 20%', '<= 30%', '<= 40%', '<= 50%',
                                                        '<= 60%', '<= 70%', '<= 80%', '<= 90%', '<= 100%'),
                                               right=T)

CrossTable(df_sweet_perc$fx_sweet_perc, df_sweet_perc$Response, chisq = T)

## KS test

par(mfrow = c(1,1))

ks.test(df_sweet_perc[df_sweet_perc$Response == 0,]$sweet_perc,
        df_sweet_perc[df_sweet_perc$Response == 1,]$sweet_perc)
plot(ecdf(df_sweet_perc[df_sweet_perc$Response == 0,]$sweet_perc),
     xlim = range(c(df_sweet_perc[df_sweet_perc$Response == 0,]$sweet_perc,
                    df_sweet_perc[df_sweet_perc$Response == 1,]$sweet_perc)), col = 'red')
plot(ecdf(df_sweet_perc[df_sweet_perc$Response == 1,]$sweet_perc),
     add = TRUE, lty = "dashed", col = 'blue')

#Conclusion: significant low p-value for chi-square test and ks test


### Analyzing the feature distribution, regarding the target variable ###

sweet_perc_quant <- quantile(df$sweet_perc, probs = c(0, 0.1, 0.2, 0.3, 0.4, 0.5,
                                                                  0.6, 0.7, 0.8, 0.9, 1))

df$faixa_sweet_perc <- as.numeric(cut(df$sweet_perc,
                                            breaks= c(unique(sweet_perc_quant)),
                                            labels=c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                                            right=F))

df$faixa_sweet_perc[is.na(df$faixa_sweet_perc)] <- 9

hist(df$faixa_sweet_perc)

sweet_perc_faixa <- df %>%
  group_by(faixa_sweet_perc) %>%
  summarise(obs = n(), sweet_perc = mean(sweet_perc), Response = sum(Response),
            pct_Response = sum(Response)/n())

hist(sweet_perc_faixa$pct_Response)
par(mfrow = c(1,1))
plot(sweet_perc_faixa$sweet_perc, sweet_perc_faixa$pct_Response)
abline(h=sum(Response) / nrow(df), col="red")

feature <- c()
p_chisq_test <- c()
p_ks_test <- c()
min_value <- c()
max_value <- c()
min_no_outlier <- c()
max_no_outlier <- c()

p_chisq_test <- CrossTable(df_sweet_perc$fx_sweet_perc, df_sweet_perc$Response, chisq = T)$chisq[3]$p.value
df_stats19 <- data.frame('sweet_perc', p_chisq_test)

df_stats19

colnames(df_stats19) <- c('feature', 'p-value')

df_stats <- rbind(df_stats, df_stats19)


##########################################################################################


##MntGoldProds

hist(MntGoldProds)
boxplot(MntGoldProds)
boxplot(MntGoldProds ~ Response) # Comparing with target variable

#Analyzing
par(mfrow = c(2,1))

noout_MntGoldProds <- remove_outlier2(MntGoldProds)
noout_MntGoldProds2 <- noout_MntGoldProds[!is.na(noout_MntGoldProds)]
hist(noout_MntGoldProds)
out_MntGoldProds <- outlier2(MntGoldProds)
out_MntGoldProds2 <- out_MntGoldProds[!is.na(out_MntGoldProds)]
hist(out_MntGoldProds2)
max(noout_MntGoldProds2)
min(noout_MntGoldProds2)

boxplot(noout_MntGoldProds2)
boxplot(out_MntGoldProds2)

out_MntGoldProds2_box <- df[df$MntGoldProds > max(noout_MntGoldProds2),c("MntGoldProds", "Response")]
boxplot(out_MntGoldProds2_box$MntGoldProds ~ out_MntGoldProds2_box$Response)
noout_MntGoldProds2_box <- df[df$MntGoldProds <= max(noout_MntGoldProds2), c("MntGoldProds", "Response")]
boxplot(noout_MntGoldProds2_box$MntGoldProds ~ noout_MntGoldProds2_box$Response)

length(out_MntGoldProds2) / nrow(df)
#2.1% outliers

prop.table(table(noout_MntGoldProds2_box$Response))
#14.8% of positive Response
prop.table(table(out_MntGoldProds2_box$Response))
#23.9% of positive Response

## Transforming into bins to run chis-square test

q_MntGoldProds <- quantile(MntGoldProds, probs = c(0.1, 0.2, 0.3, 0.4, 0.5,
                                               0.6, 0.7, 0.8, 0.9, 1))
q_MntGoldProds

df_MntGoldProds <- df[, c('MntGoldProds', 'Response')]

df_MntGoldProds$fx_MntGoldProds <- cut(df_MntGoldProds$MntGoldProds,
                                   breaks= c(-10000000,
                                             q_MntGoldProds[1],
                                             q_MntGoldProds[2],
                                             q_MntGoldProds[3],
                                             q_MntGoldProds[4],
                                             q_MntGoldProds[5],
                                             q_MntGoldProds[6],
                                             q_MntGoldProds[7],
                                             q_MntGoldProds[8],
                                             q_MntGoldProds[9],
                                             q_MntGoldProds[10]),
                                   labels=c('<= 10%', '<= 20%', '<= 30%', '<= 40%', '<= 50%',
                                            '<= 60%', '<= 70%', '<= 80%', '<= 90%', '<= 100%'),
                                   right=T)

CrossTable(df_MntGoldProds$fx_MntGoldProds, df_MntGoldProds$Response, chisq = T)

## KS test

par(mfrow = c(1,1))

ks.test(df_MntGoldProds[df_MntGoldProds$Response == 0,]$MntGoldProds,
        df_MntGoldProds[df_MntGoldProds$Response == 1,]$MntGoldProds)
plot(ecdf(df_MntGoldProds[df_MntGoldProds$Response == 0,]$MntGoldProds),
     xlim = range(c(df_MntGoldProds[df_MntGoldProds$Response == 0,]$MntGoldProds,
                    df_MntGoldProds[df_MntGoldProds$Response == 1,]$MntGoldProds)), col = 'red')
plot(ecdf(df_MntGoldProds[df_MntGoldProds$Response == 1,]$MntGoldProds),
     add = TRUE, lty = "dashed", col = 'blue')

#Conclusion: significant low p-value for chi-square test and ks test


### Analyzing the feature distribution, regarding the target variable ###

MntGoldProds_quant <- quantile(df$MntGoldProds, probs = c(0, 0.1, 0.2, 0.3, 0.4, 0.5,
                                                      0.6, 0.7, 0.8, 0.9, 1))

df$faixa_MntGoldProds <- as.numeric(cut(df$MntGoldProds,
                                      breaks= c(unique(MntGoldProds_quant)),
                                      labels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                                      right=F))

df$faixa_MntGoldProds[is.na(df$faixa_MntGoldProds)] <- 10

hist(df$faixa_MntGoldProds)

MntGoldProds_faixa <- df %>%
  group_by(faixa_MntGoldProds) %>%
  summarise(obs = n(), MntGoldProds = mean(MntGoldProds), Response = sum(Response),
            pct_Response = sum(Response)/n())

hist(MntGoldProds_faixa$pct_Response)
par(mfrow = c(1,1))
plot(MntGoldProds_faixa$MntGoldProds, MntGoldProds_faixa$pct_Response)
abline(h=sum(Response) / nrow(df), col="red")

feature <- c()
p_chisq_test <- c()
p_ks_test <- c()
min_value <- c()
max_value <- c()
min_no_outlier <- c()
max_no_outlier <- c()

p_chisq_test <- CrossTable(df_MntGoldProds$fx_MntGoldProds, df_MntGoldProds$Response, chisq = T)$chisq[3]$p.value
df_stats20 <- data.frame('MntGoldProds', p_chisq_test)

df_stats20

colnames(df_stats20) <- c('feature', 'p-value')

df_stats <- rbind(df_stats, df_stats20)


##########################################################################################


##total_spent

hist(total_spent)
boxplot(total_spent)
boxplot(total_spent ~ Response) # Comparing with target variable

#Analyzing
par(mfrow = c(2,1))

noout_total_spent <- remove_outlier2(total_spent)
noout_total_spent2 <- noout_total_spent[!is.na(noout_total_spent)]
hist(noout_total_spent)
out_total_spent <- outlier2(total_spent)
out_total_spent2 <- out_total_spent[!is.na(out_total_spent)]
hist(out_total_spent2)
max(noout_total_spent2)
min(noout_total_spent2)

boxplot(noout_total_spent2)
boxplot(out_total_spent2)

out_total_spent2_box <- df[df$total_spent > max(noout_total_spent2),c("total_spent", "Response")]
boxplot(out_total_spent2_box$total_spent ~ out_total_spent2_box$Response)
noout_total_spent2_box <- df[df$total_spent <= max(noout_total_spent2), c("total_spent", "Response")]
boxplot(noout_total_spent2_box$total_spent ~ noout_total_spent2_box$Response)

length(out_total_spent2) / nrow(df)
#0.2% outliers

prop.table(table(noout_total_spent2_box$Response))
#14.8% of positive Response
prop.table(table(out_total_spent2_box$Response))
#80% of positive Response

## Transforming into bins to run chis-square test

q_total_spent <- quantile(total_spent, probs = c(0.1, 0.2, 0.3, 0.4, 0.5,
                                                   0.6, 0.7, 0.8, 0.9, 1))
q_total_spent

df_total_spent <- df[, c('total_spent', 'Response')]

df_total_spent$fx_total_spent <- cut(df_total_spent$total_spent,
                                       breaks= c(-10000000,
                                                 q_total_spent[1],
                                                 q_total_spent[2],
                                                 q_total_spent[3],
                                                 q_total_spent[4],
                                                 q_total_spent[5],
                                                 q_total_spent[6],
                                                 q_total_spent[7],
                                                 q_total_spent[8],
                                                 q_total_spent[9],
                                                 q_total_spent[10]),
                                       labels=c('<= 10%', '<= 20%', '<= 30%', '<= 40%', '<= 50%',
                                                '<= 60%', '<= 70%', '<= 80%', '<= 90%', '<= 100%'),
                                       right=T)

CrossTable(df_total_spent$fx_total_spent, df_total_spent$Response, chisq = T)

## KS test

par(mfrow = c(1,1))

ks.test(df_total_spent[df_total_spent$Response == 0,]$total_spent,
        df_total_spent[df_total_spent$Response == 1,]$total_spent)
plot(ecdf(df_total_spent[df_total_spent$Response == 0,]$total_spent),
     xlim = range(c(df_total_spent[df_total_spent$Response == 0,]$total_spent,
                    df_total_spent[df_total_spent$Response == 1,]$total_spent)), col = 'red')
plot(ecdf(df_total_spent[df_total_spent$Response == 1,]$total_spent),
     add = TRUE, lty = "dashed", col = 'blue')

#Conclusion: significant low p-value for chi-square test and ks test


### Analyzing the feature distribution, regarding the target variable ###

total_spent_quant <- quantile(df$total_spent, probs = c(0, 0.1, 0.2, 0.3, 0.4, 0.5,
                                                          0.6, 0.7, 0.8, 0.9, 1))

df$faixa_total_spent <- as.numeric(cut(df$total_spent,
                                        breaks= c(unique(total_spent_quant)),
                                        labels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                                        right=F))

df$faixa_total_spent[is.na(df$faixa_total_spent)] <- 10

hist(df$faixa_total_spent)

total_spent_faixa <- df %>%
  group_by(faixa_total_spent) %>%
  summarise(obs = n(), total_spent = mean(total_spent), Response = sum(Response),
            pct_Response = sum(Response)/n())

hist(total_spent_faixa$pct_Response)
par(mfrow = c(1,1))
plot(total_spent_faixa$total_spent, total_spent_faixa$pct_Response)
abline(h=sum(Response) / nrow(df), col="red")

feature <- c()
p_chisq_test <- c()
p_ks_test <- c()
min_value <- c()
max_value <- c()
min_no_outlier <- c()
max_no_outlier <- c()

p_chisq_test <- CrossTable(df_total_spent$fx_total_spent, df_total_spent$Response, chisq = T)$chisq[3]$p.value
df_stats21 <- data.frame('total_spent', p_chisq_test)

df_stats21

colnames(df_stats21) <- c('feature', 'p-value')

df_stats <- rbind(df_stats, df_stats21)


##########################################################################################


##NumDealsPurchases

hist(NumDealsPurchases)
boxplot(NumDealsPurchases)
boxplot(NumDealsPurchases ~ Response) # Comparing with target variable

#Analyzing
par(mfrow = c(2,1))

noout_NumDealsPurchases <- remove_outlier2(NumDealsPurchases)
noout_NumDealsPurchases2 <- noout_NumDealsPurchases[!is.na(noout_NumDealsPurchases)]
hist(noout_NumDealsPurchases)
out_NumDealsPurchases <- outlier2(NumDealsPurchases)
out_NumDealsPurchases2 <- out_NumDealsPurchases[!is.na(out_NumDealsPurchases)]
hist(out_NumDealsPurchases2)
max(noout_NumDealsPurchases2)
min(noout_NumDealsPurchases2)

boxplot(noout_NumDealsPurchases2)
boxplot(out_NumDealsPurchases2)

out_NumDealsPurchases2_box <- df[df$NumDealsPurchases > max(noout_NumDealsPurchases2),c("NumDealsPurchases", "Response")]
boxplot(out_NumDealsPurchases2_box$NumDealsPurchases ~ out_NumDealsPurchases2_box$Response)
noout_NumDealsPurchases2_box <- df[df$NumDealsPurchases <= max(noout_NumDealsPurchases2), c("NumDealsPurchases", "Response")]
boxplot(noout_NumDealsPurchases2_box$NumDealsPurchases ~ noout_NumDealsPurchases2_box$Response)

length(out_NumDealsPurchases2) / nrow(df)
#1.4% outliers

prop.table(table(noout_NumDealsPurchases2_box$Response))
#14.8% of positive Response
prop.table(table(out_NumDealsPurchases2_box$Response))
#29% of positive Response

## Transforming into bins to run chis-square test

q_NumDealsPurchases <- quantile(NumDealsPurchases, probs = c(0.4,
                                                 0.6, 0.8, 0.9, 1))
q_NumDealsPurchases

df_NumDealsPurchases <- df[, c('NumDealsPurchases', 'Response')]

df_NumDealsPurchases$fx_NumDealsPurchases <- cut(df_NumDealsPurchases$NumDealsPurchases,
                                     breaks= c(-10000000,
                                               q_NumDealsPurchases[1],
                                               q_NumDealsPurchases[2],
                                               q_NumDealsPurchases[3],
                                               q_NumDealsPurchases[4],
                                               q_NumDealsPurchases[5]),
                                     labels=c('<= 40%',
                                              '<= 60%', '<= 80%', '<= 90%', '<= 100%'),
                                     right=T)

CrossTable(df_NumDealsPurchases$fx_NumDealsPurchases, df_NumDealsPurchases$Response, chisq = T)

## KS test

par(mfrow = c(1,1))

ks.test(df_NumDealsPurchases[df_NumDealsPurchases$Response == 0,]$NumDealsPurchases,
        df_NumDealsPurchases[df_NumDealsPurchases$Response == 1,]$NumDealsPurchases)
plot(ecdf(df_NumDealsPurchases[df_NumDealsPurchases$Response == 0,]$NumDealsPurchases),
     xlim = range(c(df_NumDealsPurchases[df_NumDealsPurchases$Response == 0,]$NumDealsPurchases,
                    df_NumDealsPurchases[df_NumDealsPurchases$Response == 1,]$NumDealsPurchases)), col = 'red')
plot(ecdf(df_NumDealsPurchases[df_NumDealsPurchases$Response == 1,]$NumDealsPurchases),
     add = TRUE, lty = "dashed", col = 'blue')

#Conclusion: significant low p-value for chi-square test and ks test


### Analyzing the feature distribution, regarding the target variable ###

NumDealsPurchases_quant <- quantile(df$NumDealsPurchases, probs = c(0.4,
                                                        0.6, 0.8, 0.9, 1))

df$faixa_NumDealsPurchases <- as.numeric(cut(df$NumDealsPurchases,
                                       breaks= c(unique(NumDealsPurchases_quant)),
                                       labels=c(1, 2, 3, 4),
                                       right=F))

df$faixa_NumDealsPurchases[is.na(df$faixa_NumDealsPurchases)] <- 4

hist(df$faixa_NumDealsPurchases)

NumDealsPurchases_faixa <- df %>%
  group_by(faixa_NumDealsPurchases) %>%
  summarise(obs = n(), NumDealsPurchases = mean(NumDealsPurchases), Response = sum(Response),
            pct_Response = sum(Response)/n())

hist(NumDealsPurchases_faixa$pct_Response)
par(mfrow = c(1,1))
plot(NumDealsPurchases_faixa$NumDealsPurchases, NumDealsPurchases_faixa$pct_Response)
abline(h=sum(Response) / nrow(df), col="red")

feature <- c()
p_chisq_test <- c()
p_ks_test <- c()
min_value <- c()
max_value <- c()
min_no_outlier <- c()
max_no_outlier <- c()

p_chisq_test <- CrossTable(df_NumDealsPurchases$fx_NumDealsPurchases, df_NumDealsPurchases$Response, chisq = T)$chisq[3]$p.value
df_stats22 <- data.frame('NumDealsPurchases', p_chisq_test)

df_stats22

colnames(df_stats22) <- c('feature', 'p-value')

df_stats <- rbind(df_stats, df_stats22)


##########################################################################################


##NumWebPurchases
  
hist(NumWebPurchases)
boxplot(NumWebPurchases)
boxplot(NumWebPurchases ~ Response) # Comparing with target variable

#Analyzing
par(mfrow = c(2,1))

noout_NumWebPurchases <- remove_outlier2(NumWebPurchases)
noout_NumWebPurchases2 <- noout_NumWebPurchases[!is.na(noout_NumWebPurchases)]
hist(noout_NumWebPurchases)
out_NumWebPurchases <- outlier2(NumWebPurchases)
out_NumWebPurchases2 <- out_NumWebPurchases[!is.na(out_NumWebPurchases)]
hist(out_NumWebPurchases2)
max(noout_NumWebPurchases2)
min(noout_NumWebPurchases2)

boxplot(noout_NumWebPurchases2)
boxplot(out_NumWebPurchases2)

out_NumWebPurchases2_box <- df[df$NumWebPurchases > max(noout_NumWebPurchases2),c("NumWebPurchases", "Response")]
boxplot(out_NumWebPurchases2_box$NumWebPurchases ~ out_NumWebPurchases2_box$Response)
noout_NumWebPurchases2_box <- df[df$NumWebPurchases <= max(noout_NumWebPurchases2), c("NumWebPurchases", "Response")]
boxplot(noout_NumWebPurchases2_box$NumWebPurchases ~ noout_NumWebPurchases2_box$Response)

length(out_NumWebPurchases2) / nrow(df)
#0.1% outliers

prop.table(table(noout_NumWebPurchases2_box$Response))
#15% of positive Response
prop.table(table(out_NumWebPurchases2_box$Response))
#29% of positive Response

## Transforming into bins to run chis-square test

q_NumWebPurchases <- quantile(NumWebPurchases, probs = c(0.1, 0.3, 0.4,
                                                             0.6, 0.7, 0.8, 0.9, 1))
q_NumWebPurchases

df_NumWebPurchases <- df[, c('NumWebPurchases', 'Response')]

df_NumWebPurchases$fx_NumWebPurchases <- cut(df_NumWebPurchases$NumWebPurchases,
                                                 breaks= c(-10000000,
                                                           q_NumWebPurchases[1],
                                                           q_NumWebPurchases[2],
                                                           q_NumWebPurchases[3],
                                                           q_NumWebPurchases[4],
                                                           q_NumWebPurchases[5],
                                                           q_NumWebPurchases[6],
                                                           q_NumWebPurchases[7],
                                                           q_NumWebPurchases[8]),
                                                 labels=c('<= 10%', '<= 30%', '<= 40%',
                                                          '<= 60%', '<= 70%', '<= 80%', '<= 90%', '<= 100%'),
                                                 right=T)

CrossTable(df_NumWebPurchases$fx_NumWebPurchases, df_NumWebPurchases$Response, chisq = T)

## KS test

par(mfrow = c(1,1))

ks.test(df_NumWebPurchases[df_NumWebPurchases$Response == 0,]$NumWebPurchases,
        df_NumWebPurchases[df_NumWebPurchases$Response == 1,]$NumWebPurchases)
plot(ecdf(df_NumWebPurchases[df_NumWebPurchases$Response == 0,]$NumWebPurchases),
     xlim = range(c(df_NumWebPurchases[df_NumWebPurchases$Response == 0,]$NumWebPurchases,
                    df_NumWebPurchases[df_NumWebPurchases$Response == 1,]$NumWebPurchases)), col = 'red')
plot(ecdf(df_NumWebPurchases[df_NumWebPurchases$Response == 1,]$NumWebPurchases),
     add = TRUE, lty = "dashed", col = 'blue')

#Conclusion: significant low p-value for chi-square test and ks test


### Analyzing the feature distribution, regarding the target variable ###

NumWebPurchases_quant <- quantile(df$NumWebPurchases, probs = c(0, 0.1, 0.3, 0.4,
                                                                    0.6, 0.7, 0.8, 0.9, 1))

df$faixa_NumWebPurchases <- as.numeric(cut(df$NumWebPurchases,
                                             breaks= c(unique(NumWebPurchases_quant)),
                                             labels=c(1, 2, 3, 4, 5, 6, 7, 8),
                                             right=F))

df$faixa_NumWebPurchases[is.na(df$faixa_NumWebPurchases)] <- 10

hist(df$faixa_NumWebPurchases)

NumWebPurchases_faixa <- df %>%
  group_by(faixa_NumWebPurchases) %>%
  summarise(obs = n(), NumWebPurchases = mean(NumWebPurchases), Response = sum(Response),
            pct_Response = sum(Response)/n())

hist(NumWebPurchases_faixa$pct_Response)
par(mfrow = c(1,1))
plot(NumWebPurchases_faixa$NumWebPurchases, NumWebPurchases_faixa$pct_Response)
abline(h=sum(Response) / nrow(df), col="red")

feature <- c()
p_chisq_test <- c()
p_ks_test <- c()
min_value <- c()
max_value <- c()
min_no_outlier <- c()
max_no_outlier <- c()

p_chisq_test <- CrossTable(df_NumWebPurchases$fx_NumWebPurchases, df_NumWebPurchases$Response, chisq = T)$chisq[3]$p.value
df_stats23 <- data.frame('NumWebPurchases', p_chisq_test)

df_stats23

colnames(df_stats23) <- c('feature', 'p-value')

df_stats <- rbind(df_stats, df_stats23)


##########################################################################################


##web_perc

hist(web_perc)
boxplot(web_perc)
boxplot(web_perc ~ Response) # Comparing with target variable

#Analyzing
par(mfrow = c(2,1))

noout_web_perc <- remove_outlier2(web_perc)
noout_web_perc2 <- noout_web_perc[!is.na(noout_web_perc)]
hist(noout_web_perc)
out_web_perc <- outlier2(web_perc)
out_web_perc2 <- out_web_perc[!is.na(out_web_perc)]
hist(out_web_perc2)
max(noout_web_perc2)
min(noout_web_perc2)

boxplot(noout_web_perc2)
boxplot(out_web_perc2)

out_web_perc2_box <- df[df$web_perc > max(noout_web_perc2),c("web_perc", "Response")]
boxplot(out_web_perc2_box$web_perc ~ out_web_perc2_box$Response)
noout_web_perc2_box <- df[df$web_perc <= max(noout_web_perc2), c("web_perc", "Response")]
boxplot(noout_web_perc2_box$web_perc ~ noout_web_perc2_box$Response)

length(out_web_perc2) / nrow(df)
#0.2% outliers

prop.table(table(noout_web_perc2_box$Response))
#15% of positive Response
prop.table(table(out_web_perc2_box$Response))
#???% of positive Response

## Transforming into bins to run chis-square test

q_web_perc <- quantile(web_perc, probs = c(0.1, 0.2, 0.3, 0.4, 0.5,
                                                         0.6, 0.7, 0.8, 0.9, 1))
q_web_perc

df_web_perc <- df[, c('web_perc', 'Response')]

df_web_perc$fx_web_perc <- cut(df_web_perc$web_perc,
                                             breaks= c(-10000000,
                                                       q_web_perc[1],
                                                       q_web_perc[2],
                                                       q_web_perc[3],
                                                       q_web_perc[4],
                                                       q_web_perc[5],
                                                       q_web_perc[6],
                                                       q_web_perc[7],
                                                       q_web_perc[8],
                                                       q_web_perc[9],
                                                       q_web_perc[10]),
                                             labels=c('<= 10%', '<= 20%', '<= 30%', '<= 40%', '<= 50%',
                                                      '<= 60%', '<= 70%', '<= 80%', '<= 90%', '<= 100%'),
                                             right=T)

CrossTable(df_web_perc$fx_web_perc, df_web_perc$Response, chisq = T)

## KS test

par(mfrow = c(1,1))

ks.test(df_web_perc[df_web_perc$Response == 0,]$web_perc,
        df_web_perc[df_web_perc$Response == 1,]$web_perc)
plot(ecdf(df_web_perc[df_web_perc$Response == 0,]$web_perc),
     xlim = range(c(df_web_perc[df_web_perc$Response == 0,]$web_perc,
                    df_web_perc[df_web_perc$Response == 1,]$web_perc)), col = 'red')
plot(ecdf(df_web_perc[df_web_perc$Response == 1,]$web_perc),
     add = TRUE, lty = "dashed", col = 'blue')

#Conclusion: not significant high p-value for chi-square test and ks test


### Analyzing the feature distribution, regarding the target variable ###

web_perc_quant <- quantile(df$web_perc, probs = c(0, 0.1, 0.2, 0.3, 0.4, 0.5,
                                                                0.6, 0.7, 0.8, 0.9, 1))

df$faixa_web_perc <- as.numeric(cut(df$web_perc,
                                           breaks= c(unique(web_perc_quant)),
                                           labels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                                           right=F))

df$faixa_web_perc[is.na(df$faixa_web_perc)] <- 10

hist(df$faixa_web_perc)

web_perc_faixa <- df %>%
  group_by(faixa_web_perc) %>%
  summarise(obs = n(), web_perc = mean(web_perc), Response = sum(Response),
            pct_Response = sum(Response)/n())

hist(web_perc_faixa$pct_Response)
par(mfrow = c(1,1))
plot(web_perc_faixa$web_perc, web_perc_faixa$pct_Response)
abline(h=sum(Response) / nrow(df), col="red")

feature <- c()
p_chisq_test <- c()
p_ks_test <- c()
min_value <- c()
max_value <- c()
min_no_outlier <- c()
max_no_outlier <- c()

p_chisq_test <- CrossTable(df_web_perc$fx_web_perc, df_web_perc$Response, chisq = T)$chisq[3]$p.value
df_stats24 <- data.frame('web_perc', p_chisq_test)

df_stats24

colnames(df_stats24) <- c('feature', 'p-value')

df_stats <- rbind(df_stats, df_stats24)


##########################################################################################


##NumCatalogPurchases

hist(NumCatalogPurchases)
boxplot(NumCatalogPurchases)
boxplot(NumCatalogPurchases ~ Response) # Comparing with target variable

#Analyzing
par(mfrow = c(2,1))

noout_NumCatalogPurchases <- remove_outlier2(NumCatalogPurchases)
noout_NumCatalogPurchases2 <- noout_NumCatalogPurchases[!is.na(noout_NumCatalogPurchases)]
hist(noout_NumCatalogPurchases)
out_NumCatalogPurchases <- outlier2(NumCatalogPurchases)
out_NumCatalogPurchases2 <- out_NumCatalogPurchases[!is.na(out_NumCatalogPurchases)]
hist(out_NumCatalogPurchases2)
max(noout_NumCatalogPurchases2)
min(noout_NumCatalogPurchases2)

boxplot(noout_NumCatalogPurchases2)
boxplot(out_NumCatalogPurchases2)

out_NumCatalogPurchases2_box <- df[df$NumCatalogPurchases > max(noout_NumCatalogPurchases2),c("NumCatalogPurchases", "Response")]
boxplot(out_NumCatalogPurchases2_box$NumCatalogPurchases ~ out_NumCatalogPurchases2_box$Response)
noout_NumCatalogPurchases2_box <- df[df$NumCatalogPurchases <= max(noout_NumCatalogPurchases2), c("NumCatalogPurchases", "Response")]
boxplot(noout_NumCatalogPurchases2_box$NumCatalogPurchases ~ noout_NumCatalogPurchases2_box$Response)

length(out_NumCatalogPurchases2) / nrow(df)
#0.2% outliers

prop.table(table(noout_NumCatalogPurchases2_box$Response))
#15% of positive Response
prop.table(table(out_NumCatalogPurchases2_box$Response))
#???% of positive Response

## Transforming into bins to run chis-square test

q_NumCatalogPurchases <- quantile(NumCatalogPurchases, probs = c(0.3, 0.5, 0.7, 0.8, 0.9, 1))
q_NumCatalogPurchases

df_NumCatalogPurchases <- df[, c('NumCatalogPurchases', 'Response')]

df_NumCatalogPurchases$fx_NumCatalogPurchases <- cut(df_NumCatalogPurchases$NumCatalogPurchases,
                               breaks= c(-10000000,
                                         q_NumCatalogPurchases[1],
                                         q_NumCatalogPurchases[2],
                                         q_NumCatalogPurchases[3],
                                         q_NumCatalogPurchases[4],
                                         q_NumCatalogPurchases[5],
                                         q_NumCatalogPurchases[6]),
                               labels=c('<= 30%', '<= 50%', '<= 70%', '<= 80%', '<= 90%', '<= 100%'),
                               right=T)

CrossTable(df_NumCatalogPurchases$fx_NumCatalogPurchases, df_NumCatalogPurchases$Response, chisq = T)

## KS test

par(mfrow = c(1,1))

ks.test(df_NumCatalogPurchases[df_NumCatalogPurchases$Response == 0,]$NumCatalogPurchases,
        df_NumCatalogPurchases[df_NumCatalogPurchases$Response == 1,]$NumCatalogPurchases)
plot(ecdf(df_NumCatalogPurchases[df_NumCatalogPurchases$Response == 0,]$NumCatalogPurchases),
     xlim = range(c(df_NumCatalogPurchases[df_NumCatalogPurchases$Response == 0,]$NumCatalogPurchases,
                    df_NumCatalogPurchases[df_NumCatalogPurchases$Response == 1,]$NumCatalogPurchases)), col = 'red')
plot(ecdf(df_NumCatalogPurchases[df_NumCatalogPurchases$Response == 1,]$NumCatalogPurchases),
     add = TRUE, lty = "dashed", col = 'blue')

#Conclusion: not significant high p-value for chi-square test and ks test


### Analyzing the feature distribution, regarding the target variable ###

NumCatalogPurchases_quant <- quantile(df$NumCatalogPurchases, probs = c(0.3, 0.5, 0.7, 0.8, 0.9, 1))

df$faixa_NumCatalogPurchases <- as.numeric(cut(df$NumCatalogPurchases,
                                    breaks= c(unique(NumCatalogPurchases_quant)),
                                    labels=c(1, 2, 3, 4, 5),
                                    right=F))

df$faixa_NumCatalogPurchases[is.na(df$faixa_NumCatalogPurchases)] <- 5

hist(df$faixa_NumCatalogPurchases)

NumCatalogPurchases_faixa <- df %>%
  group_by(faixa_NumCatalogPurchases) %>%
  summarise(obs = n(), NumCatalogPurchases = mean(NumCatalogPurchases), Response = sum(Response),
            pct_Response = sum(Response)/n())

hist(NumCatalogPurchases_faixa$pct_Response)
par(mfrow = c(1,1))
plot(NumCatalogPurchases_faixa$NumCatalogPurchases, NumCatalogPurchases_faixa$pct_Response)
abline(h=sum(Response) / nrow(df), col="red")

feature <- c()
p_chisq_test <- c()
p_ks_test <- c()
min_value <- c()
max_value <- c()
min_no_outlier <- c()
max_no_outlier <- c()

p_chisq_test <- CrossTable(df_NumCatalogPurchases$fx_NumCatalogPurchases, df_NumCatalogPurchases$Response, chisq = T)$chisq[3]$p.value
df_stats25 <- data.frame('NumCatalogPurchases', p_chisq_test)

df_stats25

colnames(df_stats25) <- c('feature', 'p-value')

df_stats <- rbind(df_stats, df_stats25)


##########################################################################################


##catalog_perc

hist(catalog_perc)
boxplot(catalog_perc)
boxplot(catalog_perc ~ Response) # Comparing with target variable

#Analyzing
par(mfrow = c(2,1))

noout_catalog_perc <- remove_outlier2(catalog_perc)
noout_catalog_perc2 <- noout_catalog_perc[!is.na(noout_catalog_perc)]
hist(noout_catalog_perc)
out_catalog_perc <- outlier2(catalog_perc)
out_catalog_perc2 <- out_catalog_perc[!is.na(out_catalog_perc)]
hist(out_catalog_perc2)
max(noout_catalog_perc2)
min(noout_catalog_perc2)

boxplot(noout_catalog_perc2)
boxplot(out_catalog_perc2)

out_catalog_perc2_box <- df[df$catalog_perc > max(noout_catalog_perc2),c("catalog_perc", "Response")]
boxplot(out_catalog_perc2_box$catalog_perc ~ out_catalog_perc2_box$Response)
noout_catalog_perc2_box <- df[df$catalog_perc <= max(noout_catalog_perc2), c("catalog_perc", "Response")]
boxplot(noout_catalog_perc2_box$catalog_perc ~ noout_catalog_perc2_box$Response)

length(out_catalog_perc2) / nrow(df)
#0.5% outliers

prop.table(table(noout_catalog_perc2_box$Response))
#15% of positive Response
prop.table(table(out_catalog_perc2_box$Response))
#9% of positive Response

## Transforming into bins to run chis-square test

q_catalog_perc <- quantile(catalog_perc, probs = c(0.1, 0.2, 0.3, 0.4, 0.5,
                                                                 0.6, 0.7, 0.8, 0.9, 1))
q_catalog_perc

df_catalog_perc <- df[, c('catalog_perc', 'Response')]

df_catalog_perc$fx_catalog_perc <- cut(df_catalog_perc$catalog_perc,
                                                     breaks= c(-10000000,
                                                               q_catalog_perc[3],
                                                               q_catalog_perc[4],
                                                               q_catalog_perc[5],
                                                               q_catalog_perc[6],
                                                               q_catalog_perc[7],
                                                               q_catalog_perc[8],
                                                               q_catalog_perc[9],
                                                               q_catalog_perc[10]),
                                                     labels=c('<= 30%', '<= 40%', '<= 50%',
                                                              '<= 60%', '<= 70%', '<= 80%', '<= 90%', '<= 100%'),
                                                     right=T)

CrossTable(df_catalog_perc$fx_catalog_perc, df_catalog_perc$Response, chisq = T)

## KS test

par(mfrow = c(1,1))

ks.test(df_catalog_perc[df_catalog_perc$Response == 0,]$catalog_perc,
        df_catalog_perc[df_catalog_perc$Response == 1,]$catalog_perc)
plot(ecdf(df_catalog_perc[df_catalog_perc$Response == 0,]$catalog_perc),
     xlim = range(c(df_catalog_perc[df_catalog_perc$Response == 0,]$catalog_perc,
                    df_catalog_perc[df_catalog_perc$Response == 1,]$catalog_perc)), col = 'red')
plot(ecdf(df_catalog_perc[df_catalog_perc$Response == 1,]$catalog_perc),
     add = TRUE, lty = "dashed", col = 'blue')

#Conclusion: significant low p-value for chi-square test and ks test


### Analyzing the feature distribution, regarding the target variable ###

catalog_perc_quant <- quantile(df$catalog_perc, probs = c(0.3, 0.4, 0.5,
                                                                        0.6, 0.7, 0.8, 0.9, 1))

df$faixa_catalog_perc <- as.numeric(cut(df$catalog_perc,
                                               breaks= c(unique(catalog_perc_quant)),
                                               labels=c(1, 2, 3, 4, 5, 6, 7),
                                               right=F))

df$faixa_catalog_perc[is.na(df$faixa_catalog_perc)] <- 7

hist(df$faixa_catalog_perc)

catalog_perc_faixa <- df %>%
  group_by(faixa_catalog_perc) %>%
  summarise(obs = n(), catalog_perc = mean(catalog_perc), Response = sum(Response),
            pct_Response = sum(Response)/n())

hist(catalog_perc_faixa$pct_Response)
par(mfrow = c(1,1))
plot(catalog_perc_faixa$catalog_perc, catalog_perc_faixa$pct_Response)
abline(h=sum(Response) / nrow(df), col="red")

feature <- c()
p_chisq_test <- c()
p_ks_test <- c()
min_value <- c()
max_value <- c()
min_no_outlier <- c()
max_no_outlier <- c()

p_chisq_test <- CrossTable(df_catalog_perc$fx_catalog_perc, df_catalog_perc$Response, chisq = T)$chisq[3]$p.value
df_stats26 <- data.frame('catalog_perc', p_chisq_test)

df_stats26

colnames(df_stats26) <- c('feature', 'p-value')

df_stats <- rbind(df_stats, df_stats26)


##########################################################################################


##NumStorePurchases

hist(NumStorePurchases)
boxplot(NumStorePurchases)
boxplot(NumStorePurchases ~ Response) # Comparing with target variable

#Analyzing
par(mfrow = c(2,1))

noout_NumStorePurchases <- remove_outlier2(NumStorePurchases)
noout_NumStorePurchases2 <- noout_NumStorePurchases[!is.na(noout_NumStorePurchases)]
hist(noout_NumStorePurchases)
out_NumStorePurchases <- outlier2(NumStorePurchases)
out_NumStorePurchases2 <- out_NumStorePurchases[!is.na(out_NumStorePurchases)]
hist(out_NumStorePurchases2)
max(noout_NumStorePurchases2)
min(noout_NumStorePurchases2)

boxplot(noout_NumStorePurchases2)
boxplot(out_NumStorePurchases2)

out_NumStorePurchases2_box <- df[df$NumStorePurchases > max(noout_NumStorePurchases2),c("NumStorePurchases", "Response")]
boxplot(out_NumStorePurchases2_box$NumStorePurchases ~ out_NumStorePurchases2_box$Response)
noout_NumStorePurchases2_box <- df[df$NumStorePurchases <= max(noout_NumStorePurchases2), c("NumStorePurchases", "Response")]
boxplot(noout_NumStorePurchases2_box$NumStorePurchases ~ noout_NumStorePurchases2_box$Response)

length(out_NumStorePurchases2) / nrow(df)
#0% outliers

prop.table(table(noout_NumStorePurchases2_box$Response))
#15% of positive Response
prop.table(table(out_NumStorePurchases2_box$Response))
#0% of positive Response

## Transforming into bins to run chis-square test

q_NumStorePurchases <- quantile(NumStorePurchases, probs = c(0.1, 0.2, 0.3, 0.4, 0.5,
                                                                 0.6, 0.7, 0.8, 0.9, 1))
q_NumStorePurchases

df_NumStorePurchases <- df[, c('NumStorePurchases', 'Response')]

df_NumStorePurchases$fx_NumStorePurchases <- cut(df_NumStorePurchases$NumStorePurchases,
                                                     breaks= c(-10000000,
                                                               q_NumStorePurchases[1],
                                                               q_NumStorePurchases[3],
                                                               q_NumStorePurchases[4],
                                                               q_NumStorePurchases[5],
                                                               q_NumStorePurchases[6],
                                                               q_NumStorePurchases[7],
                                                               q_NumStorePurchases[8],
                                                               q_NumStorePurchases[9],
                                                               q_NumStorePurchases[10]),
                                                     labels=c('<= 10%', '<= 30%', '<= 40%', '<= 50%',
                                                              '<= 60%', '<= 70%', '<= 80%', '<= 90%', '<= 100%'),
                                                     right=T)

CrossTable(df_NumStorePurchases$fx_NumStorePurchases, df_NumStorePurchases$Response, chisq = T)

## KS test

par(mfrow = c(1,1))

ks.test(df_NumStorePurchases[df_NumStorePurchases$Response == 0,]$NumStorePurchases,
        df_NumStorePurchases[df_NumStorePurchases$Response == 1,]$NumStorePurchases)
plot(ecdf(df_NumStorePurchases[df_NumStorePurchases$Response == 0,]$NumStorePurchases),
     xlim = range(c(df_NumStorePurchases[df_NumStorePurchases$Response == 0,]$NumStorePurchases,
                    df_NumStorePurchases[df_NumStorePurchases$Response == 1,]$NumStorePurchases)), col = 'red')
plot(ecdf(df_NumStorePurchases[df_NumStorePurchases$Response == 1,]$NumStorePurchases),
     add = TRUE, lty = "dashed", col = 'blue')

#Conclusion: significant low p-value for chi-square test and ks test


### Analyzing the feature distribution, regarding the target variable ###

NumStorePurchases_quant <- quantile(df$NumStorePurchases, probs = c(0, 0.1, 0.3, 0.4, 0.5,
                                                                        0.6, 0.7, 0.8, 0.9, 1))

df$faixa_NumStorePurchases <- as.numeric(cut(df$NumStorePurchases,
                                               breaks= c(unique(NumStorePurchases_quant)),
                                               labels=c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                                               right=F))

df$faixa_NumStorePurchases[is.na(df$faixa_NumStorePurchases)] <- 9

hist(df$faixa_NumStorePurchases)

NumStorePurchases_faixa <- df %>%
  group_by(faixa_NumStorePurchases) %>%
  summarise(obs = n(), NumStorePurchases = mean(NumStorePurchases), Response = sum(Response),
            pct_Response = sum(Response)/n())

hist(NumStorePurchases_faixa$pct_Response)
par(mfrow = c(1,1))
plot(NumStorePurchases_faixa$NumStorePurchases, NumStorePurchases_faixa$pct_Response)
abline(h=sum(Response) / nrow(df), col="red")

feature <- c()
p_chisq_test <- c()
p_ks_test <- c()
min_value <- c()
max_value <- c()
min_no_outlier <- c()
max_no_outlier <- c()

p_chisq_test <- CrossTable(df_NumStorePurchases$fx_NumStorePurchases, df_NumStorePurchases$Response, chisq = T)$chisq[3]$p.value
df_stats27 <- data.frame('NumStorePurchases', p_chisq_test)

df_stats27

colnames(df_stats27) <- c('feature', 'p-value')

df_stats <- rbind(df_stats, df_stats27)


##########################################################################################


##store_perc

hist(store_perc)
boxplot(store_perc)
boxplot(store_perc ~ Response) # Comparing with target variable

#Analyzing
par(mfrow = c(2,1))

noout_store_perc <- remove_outlier2(store_perc)
noout_store_perc2 <- noout_store_perc[!is.na(noout_store_perc)]
hist(noout_store_perc)
out_store_perc <- outlier2(store_perc)
out_store_perc2 <- out_store_perc[!is.na(out_store_perc)]
hist(out_store_perc2)
max(noout_store_perc2)
min(noout_store_perc2)

boxplot(noout_store_perc2)
boxplot(out_store_perc2)

out_store_perc2_box <- df[df$store_perc > max(noout_store_perc2),c("store_perc", "Response")]
boxplot(out_store_perc2_box$store_perc ~ out_store_perc2_box$Response)
noout_store_perc2_box <- df[df$store_perc <= max(noout_store_perc2), c("store_perc", "Response")]
boxplot(noout_store_perc2_box$store_perc ~ noout_store_perc2_box$Response)

length(out_store_perc2) / nrow(df)
#2.1% outliers

prop.table(table(noout_store_perc2_box$Response))
#15.2% of positive Response
prop.table(table(out_store_perc2_box$Response))
#???% of positive Response

## Transforming into bins to run chis-square test

q_store_perc <- quantile(store_perc, probs = c(0.1, 0.2, 0.3, 0.4, 0.5,
                                                                 0.6, 0.7, 0.8, 0.9, 1))
q_store_perc

df_store_perc <- df[, c('store_perc', 'Response')]

df_store_perc$fx_store_perc <- cut(df_store_perc$store_perc,
                                                     breaks= c(-10000000,
                                                               q_store_perc[1],
                                                               q_store_perc[2],
                                                               q_store_perc[3],
                                                               q_store_perc[4],
                                                               q_store_perc[5],
                                                               q_store_perc[6],
                                                               q_store_perc[7],
                                                               q_store_perc[8],
                                                               q_store_perc[9],
                                                               q_store_perc[10]),
                                                     labels=c('<= 10%', '<= 20%', '<= 30%', '<= 40%', '<= 50%',
                                                              '<= 60%', '<= 70%', '<= 80%', '<= 90%', '<= 100%'),
                                                     right=T)

CrossTable(df_store_perc$fx_store_perc, df_store_perc$Response, chisq = T)

## KS test

par(mfrow = c(1,1))

ks.test(df_store_perc[df_store_perc$Response == 0,]$store_perc,
        df_store_perc[df_store_perc$Response == 1,]$store_perc)
plot(ecdf(df_store_perc[df_store_perc$Response == 0,]$store_perc),
     xlim = range(c(df_store_perc[df_store_perc$Response == 0,]$store_perc,
                    df_store_perc[df_store_perc$Response == 1,]$store_perc)), col = 'red')
plot(ecdf(df_store_perc[df_store_perc$Response == 1,]$store_perc),
     add = TRUE, lty = "dashed", col = 'blue')

#Conclusion: significant low p-value for chi-square test and ks test


### Analyzing the feature distribution, regarding the target variable ###

store_perc_quant <- quantile(df$store_perc, probs = c(0, 0.1, 0.2, 0.3, 0.4, 0.5,
                                                                        0.6, 0.7, 0.8, 0.9, 1))

df$faixa_store_perc <- as.numeric(cut(df$store_perc,
                                               breaks= c(unique(store_perc_quant)),
                                               labels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                                               right=F))

df$faixa_store_perc[is.na(df$faixa_store_perc)] <- 10

hist(df$faixa_store_perc)

store_perc_faixa <- df %>%
  group_by(faixa_store_perc) %>%
  summarise(obs = n(), store_perc = mean(store_perc), Response = sum(Response),
            pct_Response = sum(Response)/n())

hist(store_perc_faixa$pct_Response)
par(mfrow = c(1,1))
plot(store_perc_faixa$store_perc, store_perc_faixa$pct_Response)
abline(h=sum(Response) / nrow(df), col="red")

feature <- c()
p_chisq_test <- c()
p_ks_test <- c()
min_value <- c()
max_value <- c()
min_no_outlier <- c()
max_no_outlier <- c()

p_chisq_test <- CrossTable(df_store_perc$fx_store_perc, df_store_perc$Response, chisq = T)$chisq[3]$p.value
df_stats28 <- data.frame('store_perc', p_chisq_test)

df_stats28

colnames(df_stats28) <- c('feature', 'p-value')

df_stats <- rbind(df_stats, df_stats28)


##########################################################################################


##total_purchases

hist(total_purchases)
boxplot(total_purchases)
boxplot(total_purchases ~ Response) # Comparing with target variable

#Analyzing
par(mfrow = c(2,1))

noout_total_purchases <- remove_outlier2(total_purchases)
noout_total_purchases2 <- noout_total_purchases[!is.na(noout_total_purchases)]
hist(noout_total_purchases)
out_total_purchases <- outlier2(total_purchases)
out_total_purchases2 <- out_total_purchases[!is.na(out_total_purchases)]
hist(out_total_purchases2)
max(noout_total_purchases2)
min(noout_total_purchases2)

boxplot(noout_total_purchases2)
boxplot(out_total_purchases2)

out_total_purchases2_box <- df[df$total_purchases > max(noout_total_purchases2),c("total_purchases", "Response")]
boxplot(out_total_purchases2_box$total_purchases ~ out_total_purchases2_box$Response)
noout_total_purchases2_box <- df[df$total_purchases <= max(noout_total_purchases2), c("total_purchases", "Response")]
boxplot(noout_total_purchases2_box$total_purchases ~ noout_total_purchases2_box$Response)

length(out_total_purchases2) / nrow(df)
#0% outliers

prop.table(table(noout_total_purchases2_box$Response))
#15% of positive Response
prop.table(table(out_total_purchases2_box$Response))
#0% of positive Response

## Transforming into bins to run chis-square test

q_total_purchases <- quantile(total_purchases, probs = c(0.1, 0.2, 0.3, 0.4, 0.5,
                                               0.6, 0.7, 0.8, 0.9, 1))
q_total_purchases

df_total_purchases <- df[, c('total_purchases', 'Response')]

df_total_purchases$fx_total_purchases <- cut(df_total_purchases$total_purchases,
                                   breaks= c(-10000000,
                                             q_total_purchases[1],
                                             q_total_purchases[2],
                                             q_total_purchases[3],
                                             q_total_purchases[4],
                                             q_total_purchases[5],
                                             q_total_purchases[6],
                                             q_total_purchases[7],
                                             q_total_purchases[8],
                                             q_total_purchases[9],
                                             q_total_purchases[10]),
                                   labels=c('<= 10%', '<= 20%', '<= 30%', '<= 40%', '<= 50%',
                                            '<= 60%', '<= 70%', '<= 80%', '<= 90%', '<= 100%'),
                                   right=T)

CrossTable(df_total_purchases$fx_total_purchases, df_total_purchases$Response, chisq = T)

## KS test

par(mfrow = c(1,1))

ks.test(df_total_purchases[df_total_purchases$Response == 0,]$total_purchases,
        df_total_purchases[df_total_purchases$Response == 1,]$total_purchases)
plot(ecdf(df_total_purchases[df_total_purchases$Response == 0,]$total_purchases),
     xlim = range(c(df_total_purchases[df_total_purchases$Response == 0,]$total_purchases,
                    df_total_purchases[df_total_purchases$Response == 1,]$total_purchases)), col = 'red')
plot(ecdf(df_total_purchases[df_total_purchases$Response == 1,]$total_purchases),
     add = TRUE, lty = "dashed", col = 'blue')

#Conclusion: significant low p-value for chi-square test and ks test


### Analyzing the feature distribution, regarding the target variable ###

total_purchases_quant <- quantile(df$total_purchases, probs = c(0, 0.1, 0.2, 0.3, 0.4, 0.5,
                                                      0.6, 0.7, 0.8, 0.9, 1))

df$faixa_total_purchases <- as.numeric(cut(df$total_purchases,
                                      breaks= c(unique(total_purchases_quant)),
                                      labels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                                      right=F))

df$faixa_total_purchases[is.na(df$faixa_total_purchases)] <- 10

hist(df$faixa_total_purchases)

total_purchases_faixa <- df %>%
  group_by(faixa_total_purchases) %>%
  summarise(obs = n(), total_purchases = mean(total_purchases), Response = sum(Response),
            pct_Response = sum(Response)/n())

hist(total_purchases_faixa$pct_Response)
par(mfrow = c(1,1))
plot(total_purchases_faixa$total_purchases, total_purchases_faixa$pct_Response)
abline(h=sum(Response) / nrow(df), col="red")

feature <- c()
p_chisq_test <- c()
p_ks_test <- c()
min_value <- c()
max_value <- c()
min_no_outlier <- c()
max_no_outlier <- c()

p_chisq_test <- CrossTable(df_total_purchases$fx_total_purchases, df_total_purchases$Response, chisq = T)$chisq[3]$p.value
df_stats29 <- data.frame('total_purchases', p_chisq_test)

df_stats29

colnames(df_stats29) <- c('feature', 'p-value')

df_stats <- rbind(df_stats, df_stats29)


##########################################################################################


##spent_per_purchase

hist(spent_per_purchase)
boxplot(spent_per_purchase)
boxplot(spent_per_purchase ~ Response) # Comparing with target variable

#Analyzing
par(mfrow = c(2,1))

noout_spent_per_purchase <- remove_outlier2(spent_per_purchase)
noout_spent_per_purchase2 <- noout_spent_per_purchase[!is.na(noout_spent_per_purchase)]
hist(noout_spent_per_purchase)
out_spent_per_purchase <- outlier2(spent_per_purchase)
out_spent_per_purchase2 <- out_spent_per_purchase[!is.na(out_spent_per_purchase)]
hist(out_spent_per_purchase2)
max(noout_spent_per_purchase2)
min(noout_spent_per_purchase2)

boxplot(noout_spent_per_purchase2)
boxplot(out_spent_per_purchase2)

out_spent_per_purchase2_box <- df[df$spent_per_purchase > max(noout_spent_per_purchase2),c("spent_per_purchase", "Response")]
boxplot(out_spent_per_purchase2_box$spent_per_purchase ~ out_spent_per_purchase2_box$Response)
noout_spent_per_purchase2_box <- df[df$spent_per_purchase <= max(noout_spent_per_purchase2), c("spent_per_purchase", "Response")]
boxplot(noout_spent_per_purchase2_box$spent_per_purchase ~ noout_spent_per_purchase2_box$Response)

length(out_spent_per_purchase2) / nrow(df)
#0.8% outliers

prop.table(table(noout_spent_per_purchase2_box$Response))
#14.9% of positive Response
prop.table(table(out_spent_per_purchase2_box$Response))
#33.3% of positive Response

## Transforming into bins to run chis-square test

q_spent_per_purchase <- quantile(spent_per_purchase, probs = c(0.1, 0.2, 0.3, 0.4, 0.5,
                                                         0.6, 0.7, 0.8, 0.9, 1))
q_spent_per_purchase

df_spent_per_purchase <- df[, c('spent_per_purchase', 'Response')]

df_spent_per_purchase$fx_spent_per_purchase <- cut(df_spent_per_purchase$spent_per_purchase,
                                             breaks= c(-10000000,
                                                       q_spent_per_purchase[1],
                                                       q_spent_per_purchase[2],
                                                       q_spent_per_purchase[3],
                                                       q_spent_per_purchase[4],
                                                       q_spent_per_purchase[5],
                                                       q_spent_per_purchase[6],
                                                       q_spent_per_purchase[7],
                                                       q_spent_per_purchase[8],
                                                       q_spent_per_purchase[9],
                                                       q_spent_per_purchase[10]),
                                             labels=c('<= 10%', '<= 20%', '<= 30%', '<= 40%', '<= 50%',
                                                      '<= 60%', '<= 70%', '<= 80%', '<= 90%', '<= 100%'),
                                             right=T)

CrossTable(df_spent_per_purchase$fx_spent_per_purchase, df_spent_per_purchase$Response, chisq = T)

## KS test

par(mfrow = c(1,1))

ks.test(df_spent_per_purchase[df_spent_per_purchase$Response == 0,]$spent_per_purchase,
        df_spent_per_purchase[df_spent_per_purchase$Response == 1,]$spent_per_purchase)
plot(ecdf(df_spent_per_purchase[df_spent_per_purchase$Response == 0,]$spent_per_purchase),
     xlim = range(c(df_spent_per_purchase[df_spent_per_purchase$Response == 0,]$spent_per_purchase,
                    df_spent_per_purchase[df_spent_per_purchase$Response == 1,]$spent_per_purchase)), col = 'red')
plot(ecdf(df_spent_per_purchase[df_spent_per_purchase$Response == 1,]$spent_per_purchase),
     add = TRUE, lty = "dashed", col = 'blue')

#Conclusion: significant low p-value for chi-square test and ks test


### Analyzing the feature distribution, regarding the target variable ###

spent_per_purchase_quant <- quantile(df$spent_per_purchase, probs = c(0, 0.1, 0.2, 0.3, 0.4, 0.5,
                                                                0.6, 0.7, 0.8, 0.9, 1))

df$faixa_spent_per_purchase <- as.numeric(cut(df$spent_per_purchase,
                                           breaks= c(unique(spent_per_purchase_quant)),
                                           labels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                                           right=F))

df$faixa_spent_per_purchase[is.na(df$faixa_spent_per_purchase)] <- 10

hist(df$faixa_spent_per_purchase)

spent_per_purchase_faixa <- df %>%
  group_by(faixa_spent_per_purchase) %>%
  summarise(obs = n(), spent_per_purchase = mean(spent_per_purchase), Response = sum(Response),
            pct_Response = sum(Response)/n())

hist(spent_per_purchase_faixa$pct_Response)
par(mfrow = c(1,1))
plot(spent_per_purchase_faixa$spent_per_purchase, spent_per_purchase_faixa$pct_Response)
abline(h=sum(Response) / nrow(df), col="red")

feature <- c()
p_chisq_test <- c()
p_ks_test <- c()
min_value <- c()
max_value <- c()
min_no_outlier <- c()
max_no_outlier <- c()

p_chisq_test <- CrossTable(df_spent_per_purchase$fx_spent_per_purchase, df_spent_per_purchase$Response, chisq = T)$chisq[3]$p.value
df_stats30 <- data.frame('spent_per_purchase', p_chisq_test)

df_stats30

colnames(df_stats30) <- c('feature', 'p-value')

df_stats <- rbind(df_stats, df_stats30)


##########################################################################################


##NumWebVisitsMonth

hist(NumWebVisitsMonth)
boxplot(NumWebVisitsMonth)
boxplot(NumWebVisitsMonth ~ Response) # Comparing with target variable

#Analyzing
par(mfrow = c(2,1))

noout_NumWebVisitsMonth <- remove_outlier2(NumWebVisitsMonth)
noout_NumWebVisitsMonth2 <- noout_NumWebVisitsMonth[!is.na(noout_NumWebVisitsMonth)]
hist(noout_NumWebVisitsMonth)
out_NumWebVisitsMonth <- outlier2(NumWebVisitsMonth)
out_NumWebVisitsMonth2 <- out_NumWebVisitsMonth[!is.na(out_NumWebVisitsMonth)]
hist(out_NumWebVisitsMonth2)
max(noout_NumWebVisitsMonth2)
min(noout_NumWebVisitsMonth2)

boxplot(noout_NumWebVisitsMonth2)
boxplot(out_NumWebVisitsMonth2)

out_NumWebVisitsMonth2_box <- df[df$NumWebVisitsMonth > max(noout_NumWebVisitsMonth2),c("NumWebVisitsMonth", "Response")]
boxplot(out_NumWebVisitsMonth2_box$NumWebVisitsMonth ~ out_NumWebVisitsMonth2_box$Response)
noout_NumWebVisitsMonth2_box <- df[df$NumWebVisitsMonth <= max(noout_NumWebVisitsMonth2), c("NumWebVisitsMonth", "Response")]
boxplot(noout_NumWebVisitsMonth2_box$NumWebVisitsMonth ~ noout_NumWebVisitsMonth2_box$Response)

length(out_NumWebVisitsMonth2) / nrow(df)
#0.4% outliers

prop.table(table(noout_NumWebVisitsMonth2_box$Response))
#15% of positive Response
prop.table(table(out_NumWebVisitsMonth2_box$Response))
#??% of positive Response

## Transforming into bins to run chis-square test

q_NumWebVisitsMonth <- quantile(NumWebVisitsMonth, probs = c(0.1, 0.2, 0.3, 0.4, 0.5,
                                                               0.6, 0.7, 0.8, 0.9, 1))
q_NumWebVisitsMonth

df_NumWebVisitsMonth <- df[, c('NumWebVisitsMonth', 'Response')]

df_NumWebVisitsMonth$fx_NumWebVisitsMonth <- cut(df_NumWebVisitsMonth$NumWebVisitsMonth,
                                                   breaks= c(-10000000,
                                                             q_NumWebVisitsMonth[1],
                                                             q_NumWebVisitsMonth[2],
                                                             q_NumWebVisitsMonth[3],
                                                             q_NumWebVisitsMonth[4],
                                                             q_NumWebVisitsMonth[6],
                                                             q_NumWebVisitsMonth[8],
                                                             q_NumWebVisitsMonth[9],
                                                             q_NumWebVisitsMonth[10]),
                                                   labels=c('<= 10%', '<= 20%', '<= 30%', '<= 40%',
                                                            '<= 60%', '<= 80%', '<= 90%', '<= 100%'),
                                                   right=T)

CrossTable(df_NumWebVisitsMonth$fx_NumWebVisitsMonth, df_NumWebVisitsMonth$Response, chisq = T)

## KS test

par(mfrow = c(1,1))

ks.test(df_NumWebVisitsMonth[df_NumWebVisitsMonth$Response == 0,]$NumWebVisitsMonth,
        df_NumWebVisitsMonth[df_NumWebVisitsMonth$Response == 1,]$NumWebVisitsMonth)
plot(ecdf(df_NumWebVisitsMonth[df_NumWebVisitsMonth$Response == 0,]$NumWebVisitsMonth),
     xlim = range(c(df_NumWebVisitsMonth[df_NumWebVisitsMonth$Response == 0,]$NumWebVisitsMonth,
                    df_NumWebVisitsMonth[df_NumWebVisitsMonth$Response == 1,]$NumWebVisitsMonth)), col = 'red')
plot(ecdf(df_NumWebVisitsMonth[df_NumWebVisitsMonth$Response == 1,]$NumWebVisitsMonth),
     add = TRUE, lty = "dashed", col = 'blue')

#Conclusion: significant low p-value for chi-square test and ks test


### Analyzing the feature distribution, regarding the target variable ###

NumWebVisitsMonth_quant <- quantile(df$NumWebVisitsMonth, probs = c(0, 0.1, 0.2, 0.3, 0.4,
                                                                      0.6, 0.8, 0.9, 1))

df$faixa_NumWebVisitsMonth <- as.numeric(cut(df$NumWebVisitsMonth,
                                              breaks= c(unique(NumWebVisitsMonth_quant)),
                                              labels=c(1, 2, 3, 4, 5, 6, 7, 8),
                                              right=F))

df$faixa_NumWebVisitsMonth[is.na(df$faixa_NumWebVisitsMonth)] <- 8

hist(df$faixa_NumWebVisitsMonth)

NumWebVisitsMonth_faixa <- df %>%
  group_by(faixa_NumWebVisitsMonth) %>%
  summarise(obs = n(), NumWebVisitsMonth = mean(NumWebVisitsMonth), Response = sum(Response),
            pct_Response = sum(Response)/n())

hist(NumWebVisitsMonth_faixa$pct_Response)
par(mfrow = c(1,1))
plot(NumWebVisitsMonth_faixa$NumWebVisitsMonth, NumWebVisitsMonth_faixa$pct_Response)
abline(h=sum(Response) / nrow(df), col="red")

feature <- c()
p_chisq_test <- c()
p_ks_test <- c()
min_value <- c()
max_value <- c()
min_no_outlier <- c()
max_no_outlier <- c()

p_chisq_test <- CrossTable(df_NumWebVisitsMonth$fx_NumWebVisitsMonth, df_NumWebVisitsMonth$Response, chisq = T)$chisq[3]$p.value
df_stats31 <- data.frame('NumWebVisitsMonth', p_chisq_test)

df_stats31

colnames(df_stats31) <- c('feature', 'p-value')

df_stats <- rbind(df_stats, df_stats31)


##########################################################################################


##Complain

hist(Complain)
boxplot(Complain)
boxplot(Complain ~ Response) # Comparing with target variable

#Analyzing
par(mfrow = c(2,1))

noout_Complain <- remove_outlier2(Complain)
noout_Complain2 <- noout_Complain[!is.na(noout_Complain)]
hist(noout_Complain)
out_Complain <- outlier2(Complain)
out_Complain2 <- out_Complain[!is.na(out_Complain)]
hist(out_Complain2)
max(noout_Complain2)
min(noout_Complain2)

boxplot(noout_Complain2)
boxplot(out_Complain2)

out_Complain2_box <- df[df$Complain > max(noout_Complain2),c("Complain", "Response")]
boxplot(out_Complain2_box$Complain ~ out_Complain2_box$Response)
noout_Complain2_box <- df[df$Complain <= max(noout_Complain2), c("Complain", "Response")]
boxplot(noout_Complain2_box$Complain ~ noout_Complain2_box$Response)

length(out_Complain2) / nrow(df)
#0.9% outliers

prop.table(table(noout_Complain2_box$Response))
#15% of positive Response
prop.table(table(out_Complain2_box$Response))
#14.3% of positive Response

## Transforming into bins to run chis-square test

#q_Complain <- quantile(Complain, probs = c(0.1, 0.2, 0.3, 0.4, 0.5,
#                                                               0.6, 0.7, 0.8, 0.9, 1))
#q_Complain

df_Complain <- df[, c('Complain', 'Response')]

#df_Complain$fx_Complain <- cut(df_Complain$Complain,
#                                                   breaks= c(-10000000,
#                                                             q_Complain[1],
#                                                             q_Complain[2],
#                                                             q_Complain[3],
#                                                             q_Complain[4],
#                                                             q_Complain[5],
#                                                             q_Complain[6],
#                                                             q_Complain[7],
#                                                             q_Complain[8],
#                                                             q_Complain[9],
#                                                             q_Complain[10]),
#                                                   labels=c('<= 10%', '<= 20%', '<= 30%', '<= 40%', '<= 50%',
#                                                            '<= 60%', '<= 70%', '<= 80%', '<= 90%', '<= 100%'),
#                                                   right=T)

CrossTable(df_Complain$Complain, df_Complain$Response, chisq = T)

## KS test

par(mfrow = c(1,1))

ks.test(df_Complain[df_Complain$Response == 0,]$Complain,
        df_Complain[df_Complain$Response == 1,]$Complain)
plot(ecdf(df_Complain[df_Complain$Response == 0,]$Complain),
     xlim = range(c(df_Complain[df_Complain$Response == 0,]$Complain,
                    df_Complain[df_Complain$Response == 1,]$Complain)), col = 'red')
plot(ecdf(df_Complain[df_Complain$Response == 1,]$Complain),
     add = TRUE, lty = "dashed", col = 'blue')

#Conclusion: significant low p-value for chi-square test and ks test


### Analyzing the feature distribution, regarding the target variable ###

Complain_quant <- quantile(df$Complain, probs = c(0, 0.1, 0.2, 0.3, 0.4, 0.5,
                                                                      0.6, 0.7, 0.8, 0.9, 1))

df$faixa_Complain <- as.numeric(cut(df$Complain,
                                              breaks= c(unique(Complain_quant)),
                                              labels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                                              right=F))

df$faixa_Complain[is.na(df$faixa_Complain)] <- 10

hist(df$faixa_Complain)

Complain_faixa <- df %>%
  group_by(faixa_Complain) %>%
  summarise(obs = n(), Complain = mean(Complain), Response = sum(Response),
            pct_Response = sum(Response)/n())

hist(Complain_faixa$pct_Response)
par(mfrow = c(1,1))
plot(Complain_faixa$Complain, Complain_faixa$pct_Response)
abline(h=sum(Response) / nrow(df), col="red")

hist(Complain_faixa$pct_Response)
par(mfrow = c(1,1))
plot(Complain_faixa$Complain, Complain_faixa$pct_Response)
abline(h=sum(Response) / nrow(df), col="red")

feature <- c()
p_chisq_test <- c()
p_ks_test <- c()
min_value <- c()
max_value <- c()
min_no_outlier <- c()
max_no_outlier <- c()

p_chisq_test <- CrossTable(df_Complain$Complain, df_Complain$Response, chisq = T)$chisq[3]$p.value
df_stats33 <- data.frame('Complain', p_chisq_test)

df_stats33

colnames(df_stats33) <- c('feature', 'p-value')

df_stats <- rbind(df_stats, df_stats33)


##########################################################################################


##total_campaigns

hist(total_campaigns)
boxplot(total_campaigns)
boxplot(total_campaigns ~ Response) # Comparing with target variable

#Analyzing
par(mfrow = c(2,1))

noout_total_campaigns <- remove_outlier2(total_campaigns)
noout_total_campaigns2 <- noout_total_campaigns[!is.na(noout_total_campaigns)]
hist(noout_total_campaigns)
out_total_campaigns <- outlier2(total_campaigns)
out_total_campaigns2 <- out_total_campaigns[!is.na(out_total_campaigns)]
hist(out_total_campaigns2)
max(noout_total_campaigns2)
min(noout_total_campaigns2)

boxplot(noout_total_campaigns2)
boxplot(out_total_campaigns2)

out_total_campaigns2_box <- df[df$total_campaigns > max(noout_total_campaigns2),c("total_campaigns", "Response")]
boxplot(out_total_campaigns2_box$total_campaigns ~ out_total_campaigns2_box$Response)
noout_total_campaigns2_box <- df[df$total_campaigns <= max(noout_total_campaigns2), c("total_campaigns", "Response")]
boxplot(noout_total_campaigns2_box$total_campaigns ~ noout_total_campaigns2_box$Response)

length(out_total_campaigns2) / nrow(df)
#2.5% outliers

prop.table(table(noout_total_campaigns2_box$Response))
#13.3% of positive Response
prop.table(table(out_total_campaigns2_box$Response))
#82.8% of positive Response

## Transforming into bins to run chis-square test

#q_total_campaigns <- quantile(total_campaigns, probs = c(0.8, 1))
#q_total_campaigns

df_total_campaigns <- df[, c('total_campaigns', 'Response')]

#df_total_campaigns$fx_total_campaigns <- cut(df_total_campaigns$total_campaigns,
#                               breaks= c(-10000000,
#                                         q_total_campaigns[1],
#                                         q_total_campaigns[2]),
#                               labels=c('<= 80%', '<= 100%'),
#                               right=T)

CrossTable(df_total_campaigns$total_campaigns, df_total_campaigns$Response, chisq = T)

## KS test

par(mfrow = c(1,1))

ks.test(df_total_campaigns[df_total_campaigns$Response == 0,]$total_campaigns,
        df_total_campaigns[df_total_campaigns$Response == 1,]$total_campaigns)
plot(ecdf(df_total_campaigns[df_total_campaigns$Response == 0,]$total_campaigns),
     xlim = range(c(df_total_campaigns[df_total_campaigns$Response == 0,]$total_campaigns,
                    df_total_campaigns[df_total_campaigns$Response == 1,]$total_campaigns)), col = 'red')
plot(ecdf(df_total_campaigns[df_total_campaigns$Response == 1,]$total_campaigns),
     add = TRUE, lty = "dashed", col = 'blue')

#Conclusion: significant low p-value for chi-square test


### Analyzing the feature distribution, regarding the target variable ###

total_campaigns_quant <- quantile(df$total_campaigns, probs = c(0.8, 1))

df$faixa_total_campaigns <- as.numeric(cut(df$total_campaigns,
                                    breaks= c(unique(total_campaigns_quant)),
                                    labels=c(1),
                                    right=F))

df$faixa_total_campaigns[is.na(df$faixa_total_campaigns)] <- 1

hist(df$faixa_total_campaigns)

total_campaigns_faixa <- df %>%
  group_by(faixa_total_campaigns) %>%
  summarise(obs = n(), total_campaigns = mean(total_campaigns), Response = sum(Response),
            pct_Response = sum(Response)/n())

hist(total_campaigns_faixa$pct_Response)
par(mfrow = c(1,1))
plot(total_campaigns_faixa$total_campaigns, total_campaigns_faixa$pct_Response)
abline(h=sum(Response) / nrow(df), col="red")

feature <- c()
p_chisq_test <- c()
p_ks_test <- c()
min_value <- c()
max_value <- c()
min_no_outlier <- c()
max_no_outlier <- c()

p_chisq_test <- CrossTable(df_total_campaigns$total_campaigns, df_total_campaigns$Response, chisq = T)$chisq[3]$p.value
df_stats34 <- data.frame('total_campaigns', p_chisq_test)

df_stats34

colnames(df_stats34) <- c('feature', 'p-value')

df_stats <- rbind(df_stats, df_stats34)


##########################################################################################


##AcceptedCmp1

hist(AcceptedCmp1)
boxplot(AcceptedCmp1)
boxplot(AcceptedCmp1 ~ Response) # Comparing with target variable

#Analyzing
par(mfrow = c(2,1))

noout_AcceptedCmp1 <- remove_outlier2(AcceptedCmp1)
noout_AcceptedCmp12 <- noout_AcceptedCmp1[!is.na(noout_AcceptedCmp1)]
hist(noout_AcceptedCmp1)
out_AcceptedCmp1 <- outlier2(AcceptedCmp1)
out_AcceptedCmp12 <- out_AcceptedCmp1[!is.na(out_AcceptedCmp1)]
hist(out_AcceptedCmp12)
max(noout_AcceptedCmp12)
min(noout_AcceptedCmp12)

boxplot(noout_AcceptedCmp12)
boxplot(out_AcceptedCmp12)

out_AcceptedCmp12_box <- df[df$AcceptedCmp1 > max(noout_AcceptedCmp12),c("AcceptedCmp1", "Response")]
boxplot(out_AcceptedCmp12_box$AcceptedCmp1 ~ out_AcceptedCmp12_box$Response)
noout_AcceptedCmp12_box <- df[df$AcceptedCmp1 <= max(noout_AcceptedCmp12), c("AcceptedCmp1", "Response")]
boxplot(noout_AcceptedCmp12_box$AcceptedCmp1 ~ noout_AcceptedCmp12_box$Response)

length(out_AcceptedCmp12) / nrow(df)
#6.4% outliers

prop.table(table(noout_AcceptedCmp12_box$Response))
#12.2% of positive Response
prop.table(table(out_AcceptedCmp12_box$Response))
#55.6% of positive Response

## Transforming into bins to run chis-square test

#q_AcceptedCmp1 <- quantile(AcceptedCmp1, probs = c(0.1, 0.2, 0.3, 0.4, 0.5,
#                                                         0.6, 0.7, 0.8, 0.9, 1))
#q_AcceptedCmp1

df_AcceptedCmp1 <- df[, c('AcceptedCmp1', 'Response')]

#df_AcceptedCmp1$fx_AcceptedCmp1 <- cut(df_AcceptedCmp1$AcceptedCmp1,
#                                             breaks= c(-10000000,
#                                                       q_AcceptedCmp1[7],
#                                                       q_AcceptedCmp1[9],
#                                                       q_AcceptedCmp1[10]),
#                                             labels=c('<= 70%', '<= 90%', '<= 100%'),
#                                             right=T)

CrossTable(df_AcceptedCmp1$AcceptedCmp1, df_AcceptedCmp1$Response, chisq = T)

## KS test

par(mfrow = c(1,1))

ks.test(df_AcceptedCmp1[df_AcceptedCmp1$Response == 0,]$AcceptedCmp1,
        df_AcceptedCmp1[df_AcceptedCmp1$Response == 1,]$AcceptedCmp1)
plot(ecdf(df_AcceptedCmp1[df_AcceptedCmp1$Response == 0,]$AcceptedCmp1),
     xlim = range(c(df_AcceptedCmp1[df_AcceptedCmp1$Response == 0,]$AcceptedCmp1,
                    df_AcceptedCmp1[df_AcceptedCmp1$Response == 1,]$AcceptedCmp1)), col = 'red')
plot(ecdf(df_AcceptedCmp1[df_AcceptedCmp1$Response == 1,]$AcceptedCmp1),
     add = TRUE, lty = "dashed", col = 'blue')

#Conclusion: significant low p-value for chi-square test


### Analyzing the feature distribution, regarding the target variable ###

AcceptedCmp1_quant <- quantile(df$AcceptedCmp1, probs = c(0.7, 0.9, 1))

df$faixa_AcceptedCmp1 <- as.numeric(cut(df$AcceptedCmp1,
                                           breaks= c(unique(AcceptedCmp1_quant)),
                                           labels=c(1, 2, 3),
                                           right=F))

df$faixa_AcceptedCmp1[is.na(df$faixa_AcceptedCmp1)] <- 10

hist(df$faixa_AcceptedCmp1)

AcceptedCmp1_faixa <- df %>%
  group_by(faixa_AcceptedCmp1) %>%
  summarise(obs = n(), AcceptedCmp1 = mean(AcceptedCmp1), Response = sum(Response),
            pct_Response = sum(Response)/n())

hist(AcceptedCmp1_faixa$pct_Response)
par(mfrow = c(1,1))
plot(AcceptedCmp1_faixa$AcceptedCmp1, AcceptedCmp1_faixa$pct_Response)
abline(h=sum(Response) / nrow(df), col="red")

feature <- c()
p_chisq_test <- c()
p_ks_test <- c()
min_value <- c()
max_value <- c()
min_no_outlier <- c()
max_no_outlier <- c()

p_chisq_test <- CrossTable(df_AcceptedCmp1$AcceptedCmp1, df_AcceptedCmp1$Response, chisq = T)$chisq[3]$p.value
df_stats35 <- data.frame('AcceptedCmp1', p_chisq_test)

df_stats35

colnames(df_stats35) <- c('feature', 'p-value')

df_stats <- rbind(df_stats, df_stats35)


##########################################################################################


##AcceptedCmp2

hist(AcceptedCmp2)
boxplot(AcceptedCmp2)
boxplot(AcceptedCmp2 ~ Response) # Comparing with target variable

#Analyzing
par(mfrow = c(2,1))

noout_AcceptedCmp2 <- remove_outlier2(AcceptedCmp2)
noout_AcceptedCmp22 <- noout_AcceptedCmp2[!is.na(noout_AcceptedCmp2)]
hist(noout_AcceptedCmp2)
out_AcceptedCmp2 <- outlier2(AcceptedCmp2)
out_AcceptedCmp22 <- out_AcceptedCmp2[!is.na(out_AcceptedCmp2)]
hist(out_AcceptedCmp22)
max(noout_AcceptedCmp22)
min(noout_AcceptedCmp22)

boxplot(noout_AcceptedCmp22)
boxplot(out_AcceptedCmp22)

out_AcceptedCmp22_box <- df[df$AcceptedCmp2 > max(noout_AcceptedCmp22),c("AcceptedCmp2", "Response")]
boxplot(out_AcceptedCmp22_box$AcceptedCmp2 ~ out_AcceptedCmp22_box$Response)
noout_AcceptedCmp22_box <- df[df$AcceptedCmp2 <= max(noout_AcceptedCmp22), c("AcceptedCmp2", "Response")]
boxplot(noout_AcceptedCmp22_box$AcceptedCmp2 ~ noout_AcceptedCmp22_box$Response)

length(out_AcceptedCmp22) / nrow(df)
#1.4% outliers

prop.table(table(noout_AcceptedCmp22_box$Response))
#14.3% of positive Response
prop.table(table(out_AcceptedCmp22_box$Response))
#67% of positive Response

## Transforming into bins to run chis-square test

#q_AcceptedCmp2 <- quantile(AcceptedCmp2, probs = c(0.1, 0.2, 0.3, 0.4, 0.5,
#                                                         0.6, 0.7, 0.8, 0.9, 1))
#q_AcceptedCmp2

df_AcceptedCmp2 <- df[, c('AcceptedCmp2', 'Response')]

#df_AcceptedCmp2$fx_AcceptedCmp2 <- cut(df_AcceptedCmp2$AcceptedCmp2,
#                                             breaks= c(-10000000,
#                                                       q_AcceptedCmp2[7],
#                                                       q_AcceptedCmp2[9],
#                                                       q_AcceptedCmp2[10]),
#                                             labels=c('<= 70%', '<= 90%', '<= 100%'),
#                                             right=T)

CrossTable(df_AcceptedCmp2$AcceptedCmp2, df_AcceptedCmp2$Response, chisq = T)

## KS test

par(mfrow = c(1,1))

ks.test(df_AcceptedCmp2[df_AcceptedCmp2$Response == 0,]$AcceptedCmp2,
        df_AcceptedCmp2[df_AcceptedCmp2$Response == 1,]$AcceptedCmp2)
plot(ecdf(df_AcceptedCmp2[df_AcceptedCmp2$Response == 0,]$AcceptedCmp2),
     xlim = range(c(df_AcceptedCmp2[df_AcceptedCmp2$Response == 0,]$AcceptedCmp2,
                    df_AcceptedCmp2[df_AcceptedCmp2$Response == 1,]$AcceptedCmp2)), col = 'red')
plot(ecdf(df_AcceptedCmp2[df_AcceptedCmp2$Response == 1,]$AcceptedCmp2),
     add = TRUE, lty = "dashed", col = 'blue')

#Conclusion: significant low p-value for chi-square test


### Analyzing the feature distribution, regarding the target variable ###

AcceptedCmp2_quant <- quantile(df$AcceptedCmp2, probs = c(0.7, 0.9, 1))

df$faixa_AcceptedCmp2 <- as.numeric(cut(df$AcceptedCmp2,
                                        breaks= c(unique(AcceptedCmp2_quant)),
                                        labels=c(1, 2, 3),
                                        right=F))

df$faixa_AcceptedCmp2[is.na(df$faixa_AcceptedCmp2)] <- 10

hist(df$faixa_AcceptedCmp2)

AcceptedCmp2_faixa <- df %>%
  group_by(faixa_AcceptedCmp2) %>%
  summarise(obs = n(), AcceptedCmp2 = mean(AcceptedCmp2), Response = sum(Response),
            pct_Response = sum(Response)/n())

hist(AcceptedCmp2_faixa$pct_Response)
par(mfrow = c(1,1))
plot(AcceptedCmp2_faixa$AcceptedCmp2, AcceptedCmp2_faixa$pct_Response)
abline(h=sum(Response) / nrow(df), col="red")

feature <- c()
p_chisq_test <- c()
p_ks_test <- c()
min_value <- c()
max_value <- c()
min_no_outlier <- c()
max_no_outlier <- c()

p_chisq_test <- CrossTable(df_AcceptedCmp2$AcceptedCmp2, df_AcceptedCmp2$Response, chisq = T)$chisq[3]$p.value
df_stats36 <- data.frame('AcceptedCmp2', p_chisq_test)

df_stats36

colnames(df_stats36) <- c('feature', 'p-value')

df_stats <- rbind(df_stats, df_stats36)


##########################################################################################


##AcceptedCmp3

hist(AcceptedCmp3)
boxplot(AcceptedCmp3)
boxplot(AcceptedCmp3 ~ Response) # Comparing with target variable

#Analyzing
par(mfrow = c(2,1))

noout_AcceptedCmp3 <- remove_outlier2(AcceptedCmp3)
noout_AcceptedCmp32 <- noout_AcceptedCmp3[!is.na(noout_AcceptedCmp3)]
hist(noout_AcceptedCmp3)
out_AcceptedCmp3 <- outlier2(AcceptedCmp3)
out_AcceptedCmp32 <- out_AcceptedCmp3[!is.na(out_AcceptedCmp3)]
hist(out_AcceptedCmp32)
max(noout_AcceptedCmp32)
min(noout_AcceptedCmp32)

boxplot(noout_AcceptedCmp32)
boxplot(out_AcceptedCmp32)

out_AcceptedCmp32_box <- df[df$AcceptedCmp3 > max(noout_AcceptedCmp32),c("AcceptedCmp3", "Response")]
boxplot(out_AcceptedCmp32_box$AcceptedCmp3 ~ out_AcceptedCmp32_box$Response)
noout_AcceptedCmp32_box <- df[df$AcceptedCmp3 <= max(noout_AcceptedCmp32), c("AcceptedCmp3", "Response")]
boxplot(noout_AcceptedCmp32_box$AcceptedCmp3 ~ noout_AcceptedCmp32_box$Response)

length(out_AcceptedCmp32) / nrow(df)
#7.3% outliers

prop.table(table(noout_AcceptedCmp32_box$Response))
#12.5% of positive Response
prop.table(table(out_AcceptedCmp32_box$Response))
#47% of positive Response

## Transforming into bins to run chis-square test

#q_AcceptedCmp3 <- quantile(AcceptedCmp3, probs = c(0.1, 0.2, 0.3, 0.4, 0.5,
#                                                         0.6, 0.7, 0.8, 0.9, 1))
#q_AcceptedCmp3

df_AcceptedCmp3 <- df[, c('AcceptedCmp3', 'Response')]

#df_AcceptedCmp3$fx_AcceptedCmp3 <- cut(df_AcceptedCmp3$AcceptedCmp3,
#                                             breaks= c(-10000000,
#                                                       q_AcceptedCmp3[7],
#                                                       q_AcceptedCmp3[9],
#                                                       q_AcceptedCmp3[10]),
#                                             labels=c('<= 70%', '<= 90%', '<= 100%'),
#                                             right=T)

CrossTable(df_AcceptedCmp3$AcceptedCmp3, df_AcceptedCmp3$Response, chisq = T)

## KS test

par(mfrow = c(1,1))

ks.test(df_AcceptedCmp3[df_AcceptedCmp3$Response == 0,]$AcceptedCmp3,
        df_AcceptedCmp3[df_AcceptedCmp3$Response == 1,]$AcceptedCmp3)
plot(ecdf(df_AcceptedCmp3[df_AcceptedCmp3$Response == 0,]$AcceptedCmp3),
     xlim = range(c(df_AcceptedCmp3[df_AcceptedCmp3$Response == 0,]$AcceptedCmp3,
                    df_AcceptedCmp3[df_AcceptedCmp3$Response == 1,]$AcceptedCmp3)), col = 'red')
plot(ecdf(df_AcceptedCmp3[df_AcceptedCmp3$Response == 1,]$AcceptedCmp3),
     add = TRUE, lty = "dashed", col = 'blue')

#Conclusion: significant low p-value for chi-square test


### Analyzing the feature distribution, regarding the target variable ###

AcceptedCmp3_quant <- quantile(df$AcceptedCmp3, probs = c(0.7, 0.9, 1))

df$faixa_AcceptedCmp3 <- as.numeric(cut(df$AcceptedCmp3,
                                        breaks= c(unique(AcceptedCmp3_quant)),
                                        labels=c(1, 2, 3),
                                        right=F))

df$faixa_AcceptedCmp3[is.na(df$faixa_AcceptedCmp3)] <- 10

hist(df$faixa_AcceptedCmp3)

AcceptedCmp3_faixa <- df %>%
  group_by(faixa_AcceptedCmp3) %>%
  summarise(obs = n(), AcceptedCmp3 = mean(AcceptedCmp3), Response = sum(Response),
            pct_Response = sum(Response)/n())

hist(AcceptedCmp3_faixa$pct_Response)
par(mfrow = c(1,1))
plot(AcceptedCmp3_faixa$AcceptedCmp3, AcceptedCmp3_faixa$pct_Response)
abline(h=sum(Response) / nrow(df), col="red")

feature <- c()
p_chisq_test <- c()
p_ks_test <- c()
min_value <- c()
max_value <- c()
min_no_outlier <- c()
max_no_outlier <- c()

p_chisq_test <- CrossTable(df_AcceptedCmp3$AcceptedCmp3, df_AcceptedCmp3$Response, chisq = T)$chisq[3]$p.value
df_stats37 <- data.frame('AcceptedCmp3', p_chisq_test)

df_stats37

colnames(df_stats37) <- c('feature', 'p-value')

df_stats <- rbind(df_stats, df_stats37)


##########################################################################################


##AcceptedCmp4

hist(AcceptedCmp4)
boxplot(AcceptedCmp4)
boxplot(AcceptedCmp4 ~ Response) # Comparing with target variable

#Analyzing
par(mfrow = c(2,1))

noout_AcceptedCmp4 <- remove_outlier2(AcceptedCmp4)
noout_AcceptedCmp42 <- noout_AcceptedCmp4[!is.na(noout_AcceptedCmp4)]
hist(noout_AcceptedCmp4)
out_AcceptedCmp4 <- outlier2(AcceptedCmp4)
out_AcceptedCmp42 <- out_AcceptedCmp4[!is.na(out_AcceptedCmp4)]
hist(out_AcceptedCmp42)
max(noout_AcceptedCmp42)
min(noout_AcceptedCmp42)

boxplot(noout_AcceptedCmp42)
boxplot(out_AcceptedCmp42)

out_AcceptedCmp42_box <- df[df$AcceptedCmp4 > max(noout_AcceptedCmp42),c("AcceptedCmp4", "Response")]
boxplot(out_AcceptedCmp42_box$AcceptedCmp4 ~ out_AcceptedCmp42_box$Response)
noout_AcceptedCmp42_box <- df[df$AcceptedCmp4 <= max(noout_AcceptedCmp42), c("AcceptedCmp4", "Response")]
boxplot(noout_AcceptedCmp42_box$AcceptedCmp4 ~ noout_AcceptedCmp42_box$Response)

length(out_AcceptedCmp42) / nrow(df)
#7.4% outliers

prop.table(table(noout_AcceptedCmp42_box$Response))
#13.2% of positive Response
prop.table(table(out_AcceptedCmp42_box$Response))
#37.8% of positive Response

## Transforming into bins to run chis-square test

#q_AcceptedCmp4 <- quantile(AcceptedCmp4, probs = c(0.1, 0.2, 0.3, 0.4, 0.5,
#                                                         0.6, 0.7, 0.8, 0.9, 1))
#q_AcceptedCmp4

df_AcceptedCmp4 <- df[, c('AcceptedCmp4', 'Response')]

#df_AcceptedCmp4$fx_AcceptedCmp4 <- cut(df_AcceptedCmp4$AcceptedCmp4,
#                                             breaks= c(-10000000,
#                                                       q_AcceptedCmp4[7],
#                                                       q_AcceptedCmp4[9],
#                                                       q_AcceptedCmp4[10]),
#                                             labels=c('<= 70%', '<= 90%', '<= 100%'),
#                                             right=T)

CrossTable(df_AcceptedCmp4$AcceptedCmp4, df_AcceptedCmp4$Response, chisq = T)

## KS test

par(mfrow = c(1,1))

ks.test(df_AcceptedCmp4[df_AcceptedCmp4$Response == 0,]$AcceptedCmp4,
        df_AcceptedCmp4[df_AcceptedCmp4$Response == 1,]$AcceptedCmp4)
plot(ecdf(df_AcceptedCmp4[df_AcceptedCmp4$Response == 0,]$AcceptedCmp4),
     xlim = range(c(df_AcceptedCmp4[df_AcceptedCmp4$Response == 0,]$AcceptedCmp4,
                    df_AcceptedCmp4[df_AcceptedCmp4$Response == 1,]$AcceptedCmp4)), col = 'red')
plot(ecdf(df_AcceptedCmp4[df_AcceptedCmp4$Response == 1,]$AcceptedCmp4),
     add = TRUE, lty = "dashed", col = 'blue')

#Conclusion: significant low p-value for chi-square test


### Analyzing the feature distribution, regarding the target variable ###

AcceptedCmp4_quant <- quantile(df$AcceptedCmp4, probs = c(0.7, 0.9, 1))

df$faixa_AcceptedCmp4 <- as.numeric(cut(df$AcceptedCmp4,
                                        breaks= c(unique(AcceptedCmp4_quant)),
                                        labels=c(1, 2, 3),
                                        right=F))

df$faixa_AcceptedCmp4[is.na(df$faixa_AcceptedCmp4)] <- 10

hist(df$faixa_AcceptedCmp4)

AcceptedCmp4_faixa <- df %>%
  group_by(faixa_AcceptedCmp4) %>%
  summarise(obs = n(), AcceptedCmp4 = mean(AcceptedCmp4), Response = sum(Response),
            pct_Response = sum(Response)/n())

hist(AcceptedCmp4_faixa$pct_Response)
par(mfrow = c(1,1))
plot(AcceptedCmp4_faixa$AcceptedCmp4, AcceptedCmp4_faixa$pct_Response)
abline(h=sum(Response) / nrow(df), col="red")

feature <- c()
p_chisq_test <- c()
p_ks_test <- c()
min_value <- c()
max_value <- c()
min_no_outlier <- c()
max_no_outlier <- c()

p_chisq_test <- CrossTable(df_AcceptedCmp4$AcceptedCmp4, df_AcceptedCmp4$Response, chisq = T)$chisq[3]$p.value
df_stats38 <- data.frame('AcceptedCmp4', p_chisq_test)

df_stats38

colnames(df_stats38) <- c('feature', 'p-value')

df_stats <- rbind(df_stats, df_stats38)


##########################################################################################


##AcceptedCmp5

hist(AcceptedCmp5)
boxplot(AcceptedCmp5)
boxplot(AcceptedCmp5 ~ Response) # Comparing with target variable

#Analyzing
par(mfrow = c(2,1))

noout_AcceptedCmp5 <- remove_outlier2(AcceptedCmp5)
noout_AcceptedCmp52 <- noout_AcceptedCmp5[!is.na(noout_AcceptedCmp5)]
hist(noout_AcceptedCmp5)
out_AcceptedCmp5 <- outlier2(AcceptedCmp5)
out_AcceptedCmp52 <- out_AcceptedCmp5[!is.na(out_AcceptedCmp5)]
hist(out_AcceptedCmp52)
max(noout_AcceptedCmp52)
min(noout_AcceptedCmp52)

boxplot(noout_AcceptedCmp52)
boxplot(out_AcceptedCmp52)

out_AcceptedCmp52_box <- df[df$AcceptedCmp5 > max(noout_AcceptedCmp52),c("AcceptedCmp5", "Response")]
boxplot(out_AcceptedCmp52_box$AcceptedCmp5 ~ out_AcceptedCmp52_box$Response)
noout_AcceptedCmp52_box <- df[df$AcceptedCmp5 <= max(noout_AcceptedCmp52), c("AcceptedCmp5", "Response")]
boxplot(noout_AcceptedCmp52_box$AcceptedCmp5 ~ noout_AcceptedCmp52_box$Response)

length(out_AcceptedCmp52) / nrow(df)
#7.3% outliers

prop.table(table(noout_AcceptedCmp52_box$Response))
#11.8% of positive Response
prop.table(table(out_AcceptedCmp52_box$Response))
#56% of positive Response

## Transforming into bins to run chis-square test

#q_AcceptedCmp5 <- quantile(AcceptedCmp5, probs = c(0.1, 0.2, 0.3, 0.4, 0.5,
#                                                         0.6, 0.7, 0.8, 0.9, 1))
#q_AcceptedCmp5

df_AcceptedCmp5 <- df[, c('AcceptedCmp5', 'Response')]

#df_AcceptedCmp5$fx_AcceptedCmp5 <- cut(df_AcceptedCmp5$AcceptedCmp5,
#                                             breaks= c(-10000000,
#                                                       q_AcceptedCmp5[7],
#                                                       q_AcceptedCmp5[9],
#                                                       q_AcceptedCmp5[10]),
#                                             labels=c('<= 70%', '<= 90%', '<= 100%'),
#                                             right=T)

CrossTable(df_AcceptedCmp5$AcceptedCmp5, df_AcceptedCmp5$Response, chisq = T)

## KS test

par(mfrow = c(1,1))

ks.test(df_AcceptedCmp5[df_AcceptedCmp5$Response == 0,]$AcceptedCmp5,
        df_AcceptedCmp5[df_AcceptedCmp5$Response == 1,]$AcceptedCmp5)
plot(ecdf(df_AcceptedCmp5[df_AcceptedCmp5$Response == 0,]$AcceptedCmp5),
     xlim = range(c(df_AcceptedCmp5[df_AcceptedCmp5$Response == 0,]$AcceptedCmp5,
                    df_AcceptedCmp5[df_AcceptedCmp5$Response == 1,]$AcceptedCmp5)), col = 'red')
plot(ecdf(df_AcceptedCmp5[df_AcceptedCmp5$Response == 1,]$AcceptedCmp5),
     add = TRUE, lty = "dashed", col = 'blue')

#Conclusion: significant low p-value for chi-square test


### Analyzing the feature distribution, regarding the target variable ###

AcceptedCmp5_quant <- quantile(df$AcceptedCmp5, probs = c(0.7, 0.9, 1))

df$faixa_AcceptedCmp5 <- as.numeric(cut(df$AcceptedCmp5,
                                        breaks= c(unique(AcceptedCmp5_quant)),
                                        labels=c(1, 2, 3),
                                        right=F))

df$faixa_AcceptedCmp5[is.na(df$faixa_AcceptedCmp5)] <- 10

hist(df$faixa_AcceptedCmp5)

AcceptedCmp5_faixa <- df %>%
  group_by(faixa_AcceptedCmp5) %>%
  summarise(obs = n(), AcceptedCmp5 = mean(AcceptedCmp5), Response = sum(Response),
            pct_Response = sum(Response)/n())

hist(AcceptedCmp5_faixa$pct_Response)
par(mfrow = c(1,1))
plot(AcceptedCmp5_faixa$AcceptedCmp5, AcceptedCmp5_faixa$pct_Response)
abline(h=sum(Response) / nrow(df), col="red")

feature <- c()
p_chisq_test <- c()
p_ks_test <- c()
min_value <- c()
max_value <- c()
min_no_outlier <- c()
max_no_outlier <- c()

p_chisq_test <- CrossTable(df_AcceptedCmp5$AcceptedCmp5, df_AcceptedCmp5$Response, chisq = T)$chisq[3]$p.value
df_stats39 <- data.frame('AcceptedCmp5', p_chisq_test)

df_stats39

colnames(df_stats39) <- c('feature', 'p-value')

df_stats <- rbind(df_stats, df_stats39)

test <- df_stats[-c(36), ]

write.csv(test, 'C:/Users/Guilherme Lupatelli/Desktop/Git Repository/Project 2 - Marketing Campaign Acceptance Prediction/df_chisquare.csv')


##########################################################################################