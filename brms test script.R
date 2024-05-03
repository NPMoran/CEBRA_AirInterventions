### Testing brms ----


#install.packages('devtools')
library(devtools)

#install.packages('installr')
library(installr)
install.Rtools(check = TRUE, check_r_update = TRUE, GUI = TRUE)
find_rtools()

#install.packages("rstan", repos = "https://cloud.r-project.org/", dependencies = TRUE)
remove.packages('rstan')
remove.packages('StanHeaders')
install.packages("StanHeaders", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
install.packages("rstan", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
library(StanHeaders)
library(rstan)

example(stan_model, package = "rstan", run.dontrun = TRUE)

#brms
#install.packages("brms")
library(brms)

#Building a test dataset with:
# - (x1) continuous fixed effect factor that has no effect on y, and a strong effect on y2 
# - (r1) categorical random effect which has high assocaited variance

set.seed(253)
testdat <- NULL
testdat$y <- rnorm(n = 100, mean = 5, sd = 4)
testdat$x1 <- runif(n = 100, min = 1, max = 2)
testdat$r1 <- "A"
testdat <- as.data.frame(testdat)
set.seed(182)
testdat2 <- NULL
testdat2$y <- rnorm(n = 100, mean = 8, sd = 4)
testdat2$x1 <- runif(n = 100, min = 1, max = 2)
testdat2$r1 <- "B"
testdat2 <- as.data.frame(testdat2)
testdat <- rbind(testdat, testdat2)
testdat$y2 <- testdat$y*0.4*testdat$x1
testdat

#Simple model using lme4
#library(lme4); library(lmerTest); library(performance)
#lme_test <- lmer(y ~ 1 + x1 + (1|r1), data=testdat)
#summary(lme_test)
#confint(lme_test)
#r2_nakagawa(lme_test)          
#plot(lme_test)
#
#library(lme4); library(lmerTest)
#lme_test2 <- lmer(y2 ~ 1 + x1 + (1|r1), data=testdat)
#summary(lme_test2)
#confint(lme_test2)
#r2_nakagawa(lme_test2)
#plot(lme_test2)

#Rerunning using brms
#preliminary model specifications
adapt_delta_value <- 0.9999
max_treedepth_value <- 20
iterations <- 5000
burnin <- 1000
thinning <- 2

#for saving model
filename <- paste0("models/brms.smd.base",
                   "sharedcontrol_",
                   iterations,"iter_",
                   burnin,"burnin_",
                   thinning,"thin_",
                   adapt_delta_value,"delta_",
                   max_treedepth_value,"treedepth.RData")

brms_test <- brm(y  ~ 1 + x1 + (1|r1), 
                 data   = testdat, 
                 family = gaussian(),
                 control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                 chains = 4, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
summary(brms_test)
ranef(brms_test)
fixef(brms_test)
plot(brms_test)
r2_bayes(brms_test)

brms_test2 <- brm(y2  ~ 1 + x1 + (1|r1), 
                  data   = testdat, 
                  family = gaussian(),
                  control = list(adapt_delta = adapt_delta_value, max_treedepth = max_treedepth_value),
                  chains = 4, cores = 4, iter = iterations, warmup = burnin, thin = thinning)
summary(brms_test2)
ranef(brms_test2)
fixef(brms_test2)
plot(brms_test2)
r2_bayes(brms_test2)