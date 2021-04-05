library(dplyr)
library(tidyverse)
library(ggplot2)
library(lme4) # poisson, gamma regression
library(ggcorrplot)
library(rstan) 
library(brms)
library(psych) #to get some extended summary statistics

# first let's do some EDA
setwd("~/STA723/Case Study 4/Case-Study-4")
data = read.csv(file = 'data.csv') %>% 
  select(-Obs)

colnames(data) <- c("HF_coef", "efficiency", "Fgas_rate",
                    "Sgas_rate", "Sgas_size", "Sgas_temp")

summary(data)

# boxplot and find possible outlier
par(mfrow=c(1,6))

for(i in 1:6) {
  boxplot(data[,i], main = names(data)[i])
}

data1 <- data[-c(4, 17),]

# correlation plot before deleting outlier
correlations <- cor(data[,1:6])
ggcorrplot(corr = correlations, method = "circle", 
           type = "upper", lab = TRUE,
           ggtheme = theme_bw)

correlations <- cor(data1[,1:6])
ggcorrplot(corr = correlations, method = "circle", 
           type = "upper", lab = TRUE,
           ggtheme = theme_bw)


data1 %>%
  ggplot(mapping = aes(x = Sgas_rate, y = HF_coef)) +
  geom_point(color = "black") +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ x) +
  theme_bw(base_size = 16)




# # Frequentist and Bayesian modeling 
# fit <- lm(HF_coef ~ Sgas_rate + Fgas_rate + Sgas_size + Sgas_temp, data = data) #Create the linear regression
# summary(fit) #Review the results
# 
# plot(fit$residuals)
# 
# plot(cooks.distance(fit), pch = 16, col = "blue")
# 
# 
# fit1 <- lm(HF_coef ~ Sgas_rate + Fgas_rate + Sgas_size + Sgas_temp, data = data1) #Create the linear regression
# summary(fit1) #Review the results
# 
# plot(fit1$residuals)



bprior = prior_string("normal(0,10)", class = "b")

brm1 <- brm(HF_coef ~ Sgas_rate + Fgas_rate + Sgas_size + Sgas_temp, 
            data = data1, family = gaussian, verbose = FALSE, 
            warmup = 500, chains = 2,iter = 5000, prior = bprior)

summary(brm1)
posterior_summary(brm1)

dens = pp_check(brm1, nsamples = 50, 'dens_overlay')
dens

pp <- predict(brm1)
data1$predict <- pp[,1]

pred <- posterior_predict(brm1, 
                          newdata = data.frame(Sgas_rate = 116.9, 
                                               Fgas_rate = 172.1, 
                                               Sgas_size = 43.89, 
                                               Sgas_temp = 220.36))

hist(pred, breaks = 100, 
     main="Histogram for Estimated Y1",
     xlab="Predicted Value", ylab="Frequency")


data2 <- data1 %>% 
  mutate(HF_index = case_when(
    HF_coef > 100 ~ 1,
    HF_coef <= 100 ~ 0,
  ))


# not sure about this!!!!!!!!!!!!!!!!!
# fit3 <- glm(formula = HF_index ~ Sgas_rate + Fgas_rate + Sgas_size + Sgas_temp, 
#               data = data2, 
#               family = binomial(link = "logit"))
# summary(fit3)


bprior2 = prior_string("normal(0,10)", class = "b")

brm2 <- brm(HF_index ~ Sgas_rate + Fgas_rate + Sgas_size + Sgas_temp, 
            data = data2, family = bernoulli(link = "logit"), verbose = FALSE, 
            warmup = 500, chains = 2, iter = 5000, prior = bprior2)

summary(brm2)

posterior_summary(brm2)

dens = pp_check(brm2, nsamples = 50, 'dens_overlay')
dens

pp <- predict(brm2)


bprior3 = prior_string("normal(0,10)", class = "b")

brm3 <- brm(efficiency ~ Sgas_rate + Fgas_rate + Sgas_size + Sgas_temp, 
            data = data1, family = gaussian, verbose = FALSE, 
            warmup = 500, chains = 2,iter = 5000, prior = bprior3)

summary(brm3)
posterior_summary(brm3)

dens = pp_check(brm3, nsamples = 50, 'dens_overlay')
dens

pp <- predict(brm3)





















# fit2 <- glm(log(HF_coef) ~ 1 + Fgas_rate, data = data, family = gaussian(link = "identity"))
# summary(fit2)
# 
# 
# M1 <- lmer(
#   formula = as.formula(log(HF_coef) ~ Fgas_rate + Sgas_rate + (1|Sgas_size) + Sgas_temp),
#   data = data
# )
# coef(M1)
# 
# 
# 
# 
