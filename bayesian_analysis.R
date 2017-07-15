rm(list = ls())
load('./data/Data.RData')
rtg <- read.csv("./data/AMA28_VideoRatingsALL.csv")

library(dplyr)
library(ggplot2)
library(mgcv)
library(readr)
library(rstan)
library(rstanarm) # https://cran.r-project.org/web/packages/rstanarm/rstanarm.pdf
library(tidyr)

source('bayesian_analysis_functions.R')
## The models themselves take a substantial time to run. The results
## of the models are stored in this .Rdata file.
load('./bayesian_results/model_posetrior_reults.RData')

options(mc.cores = parallel::detectCores())

Data <- tbl_df(Data)
Data$Mov[!is.na(Data$SocMov)] <- as.character(Data$SocMov[!is.na(Data$SocMov)])
Data$Mov[!is.na(Data$NonSocMov)] <- as.character(Data$NonSocMov[!is.na(Data$NonSocMov)])
rtg$VideoFileName <- as.character(rtg$VideoFileName)
rtg$Mov <- rtg$VideoFileName

tmp <- left_join(Data, rtg, c("Mov"))

# Compute lagged variables
Data <- 
  tmp %>%
  group_by(MMU, Day) %>%
  arrange(Trial) %>%
  mutate(Arousal.lag = lag(Arousal, 1),
         Valence.lag = lag(Valence, 1),
         Dominance.lag = lag(Dominance, 1),
         Submission.lag = lag(Submission, 1),
         Closeness.lag = lag(Closeness, 1),
         Novelty.lag = lag(Novelty, 1),
         Interactions.lag = lag(Interactions, 1),
         Choice.lag = lag(Choice, 1))

## Mean imputation of missing data
table(is.na(Data$Pupil),Data$MMU)
Data$Pupil[is.na(Data$Pupil)&Data$MMU=='33825'&Data$CS==1] <- mean(Data$Pupil[Data$MMU=='33825'&Data$CS==1],na.rm=T)
Data$Pupil[is.na(Data$Pupil)&Data$MMU=='33825'&Data$CNS==1] <- mean(Data$Pupil[Data$MMU=='33825'&Data$CNS==1],na.rm=T)
Data$Pupil[is.na(Data$Pupil)&Data$MMU=='34700'&Data$CS==1] <- mean(Data$Pupil[Data$MMU=='34700'&Data$CS==1],na.rm=T)
Data$Pupil[is.na(Data$Pupil)&Data$MMU=='34700'&Data$CNS==1] <- mean(Data$Pupil[Data$MMU=='34700'&Data$CNS==1],na.rm=T)

table(is.na(Data$SocFix),Data$MMU)
Data$SocFix[is.na(Data$SocFix)&Data$MMU=='33089'&Data$CS==1] <- mean(Data$SocFix[Data$MMU=='33089'&Data$CS==1],na.rm=T)
Data$SocFix[is.na(Data$SocFix)&Data$MMU=='33825'&Data$CS==1] <- mean(Data$SocFix[Data$MMU=='33825'&Data$CS==1],na.rm=T)

table(is.na(Data$NonSocFix),Data$MMU)
Data$NonSocFix[is.na(Data$NonSocFix)&Data$MMU=='33089'&Data$CS==1] <- mean(Data$NonSocFix[Data$MMU=='33089'&Data$CS==1],na.rm=T)
Data$NonSocFix[is.na(Data$NonSocFix)&Data$MMU=='33825'&Data$CS==1] <- mean(Data$NonSocFix[Data$MMU=='33825'&Data$CS==1],na.rm=T)

# One obs where Pupil == 0, replace with mean pupil diam for that MMU
Data$Pupil[Data$Pupil==0] <- mean(Data$Pupil[Data$MMU=='33089'&Data$CNS==1],na.rm=T)

# Mean center and scale all (continuous) variables. Mean center binary variables.
Data$SFminusNSF <- Data$SocFix - Data$NonSocFix
Data$Day_c <- as.vector(scale(Data$Day))
Data$Trial_c <- as.vector(scale(Data$Trial))
Data$SocFix_c <- as.vector(scale(Data$SocFix))
Data$NonSocFix_c <- as.vector(scale(Data$NonSocFix))
Data$SFminusNSF_c <- as.vector(scale(Data$SFminusNSF))
Data$Choice_c <- as.vector(scale(Data$Choice, scale = FALSE))
Data$Valence_c <- as.vector(scale(Data$Valence))
Data$Arousal_c <- as.vector(scale(Data$Arousal))
Data$Arousal_c <- as.vector(scale(Data$Arousal))
Data$Dominance_c <- as.vector(scale(Data$Dominance))
Data$Submission_c <- as.vector(scale(Data$Submission))
Data$Closeness_c <- as.vector(scale(Data$Closeness))
Data$Interactions_c <- as.vector(scale(Data$Interactions))
Data$Novelty_c <- as.vector(scale(Data$Novelty))
Data$Valence.lag_c <- as.vector(scale(Data$Valence.lag))
Data$Arousal.lag_c <- as.vector(scale(Data$Arousal.lag))
Data$Dominance.lag_c <- as.vector(scale(Data$Dominance.lag))
Data$Submission.lag_c <- as.vector(scale(Data$Submission.lag))
Data$Closeness.lag_c <- as.vector(scale(Data$Closeness.lag))
Data$Interactions.lag_c <- as.vector(scale(Data$Interactions.lag))
Data$Novelty.lag_c <- as.vector(scale(Data$Novelty.lag))
Data$Choice.lag_c <- as.vector(scale(Data$Choice.lag, scale = FALSE))

Data$Choice.lag <- as.factor(Data$Choice.lag)
Data$Choice <- as.factor(Data$Choice)

## Run the models. Note that this takes substantial time (~8 hours)
# mod.choice <- stan_glmer(Choice ~ Day_c + Choice.lag*(Valence.lag_c + Arousal.lag_c  +
#                            Dominance.lag_c + Submission.lag_c + Closeness.lag_c + Interactions.lag_c + Novelty.lag_c) +
#                            (Day_c + Choice.lag*(Valence.lag_c + Arousal.lag_c  +
#                               Dominance.lag_c + Submission.lag_c + Closeness.lag_c + Interactions.lag_c + Novelty.lag_c) | MMU),
#                          family = binomial(), data = Data,
#                          prior_intercept = normal(2.5),
#                          prior = normal(2.5), iter = 2000,
#                          seed = 1234, chains = 4, adapt_delta = 0.99999)
# 
# mod.pupil <- stan_lmer(Pupil ~ SFminusNSF_c + Trial_c + Day_c + Choice*(Valence_c + Arousal_c +
#                           Dominance_c + Submission_c + Closeness_c + Interactions_c + Novelty_c) +
#                           (SFminusNSF_c + Trial_c + Day_c + Choice*(Valence_c + Arousal_c +
#                            Dominance_c + Submission_c + Closeness_c + Interactions_c + Novelty_c)| MMU),
#                         data = Data, prior_intercept = normal(location = 70, scale = 10), iter = 2000,
#                         prior = normal(scale = 2.5), seed = 1234, chains = 4, adapt_delta = 0.99999)
# 
# stanMod = list(
#   mod.choice, mod.pupil
# )
#save(stanMod, file = "ChoicePupilModel_Interaction.RData")

post <- shift_draws(draws1[,c("b[(Intercept) MMU:33825]",
                              "b[(Intercept) MMU:33089]",
                              "b[(Intercept) MMU:32992]",
                              "b[(Intercept) MMU:34700]")])

draws1 <- as.matrix(stanMod[[1]])
draws2 <- as.matrix(stanMod[[2]])

post <- shift_draws(draws1[,c("Choice.lag1", "b[Choice.lag1 MMU:33825]",
                              "b[Choice.lag1 MMU:33089]", "b[Choice.lag1 MMU:32992]",
                              "b[Choice.lag1 MMU:34700]")])

cbind(mean = colMeans(post),
      t(apply(post, 2, quantile, c(0.05,0.95))),
      t(apply(post,2,quantile,c(0.25,0.75))),
      "Pr(theta>0)" = apply(post, 2, function(x){ mean(x>0) }))

posterior.choice.nonsocial <- lapply(c("Valence.lag_c", "Arousal.lag_c", "Dominance.lag_c", "Submission.lag_c", 
                                      "Closeness.lag_c", "Interactions.lag_c", "Novelty.lag_c"),
                                    function(x){
                                      post <- shift_draws(GetColumns(draws1, x, interaction = "Choice.lag1", incl=FALSE))
                                      cbind(mean=colMeans(post),t(apply(post,2,quantile,c(0.05,0.95))),t(apply(post,2,quantile,c(0.25,0.75))),
                                                                                                         "Pr(theta>0)"=apply(post,2,function(x){mean(x>0)}))
                                    })

posterior.choice.social <- lapply(c("Valence.lag_c", "Arousal.lag_c", "Dominance.lag_c", "Submission.lag_c", 
                                   "Closeness.lag_c", "Interactions.lag_c", "Novelty.lag_c"),
                                  function(x){
                                   post <- shift_draws(CollapseInteraction(GetColumns(draws1, x, 
                                                                                      interaction = "Choice.lag1", incl=T), 
                                                                           field = x, interaction = "Choice.lag1"))
                                   cbind(mean=colMeans(post),t(apply(post,2,quantile,c(0.05,0.95))),t(apply(post,2,quantile,c(0.25,0.75))),
                                         "Pr(theta>0)"=apply(post,2,function(x){mean(x>0)}))
                                 })
posterior.pupil.nonsocial <- lapply(c("Valence_c", "Arousal_c", "Dominance_c", "Submission_c", 
                                      "Closeness_c", "Interactions_c", "Novelty_c"),
                          function(x){
                            post <- shift_draws(GetColumns(draws2, x, interaction = "Choice1", incl=FALSE))
                            cbind(mean=colMeans(post),t(apply(post,2,quantile,c(0.05,0.95))),t(apply(post,2,quantile,c(0.25,0.75))),
                                  "Pr(theta>0)"=apply(post,2,function(x){mean(x>0)}))
                          })

posterior.pupil.social <- lapply(c("Valence_c", "Arousal_c", "Dominance_c", "Submission_c", 
                                      "Closeness_c", "Interactions_c", "Novelty_c"),
                                    function(x){
                                      post <- shift_draws(CollapseInteraction(GetColumns(draws2, x, 
                                                                                         interaction = "Choice1", incl=T), 
                                                                              field = x, interaction = "Choice1"))
                                      cbind(mean=colMeans(post),t(apply(post,2,quantile,c(0.05,0.95))),t(apply(post,2,quantile,c(0.25,0.75))),
                                            "Pr(theta>0)"=apply(post,2,function(x){mean(x>0)}))
                                    })

(posterior.choice.social <- data.frame(do.call(rbind, posterior.choice.social)))
(posterior.choice.nonsocial <- data.frame(do.call(rbind, posterior.choice.nonsocial)))
(posterior.pupil.nonsocial <- data.frame(do.call(rbind, posterior.pupil.nonsocial)))
(posterior.pupil.social <- data.frame(do.call(rbind, posterior.pupil.social)))
posterior.choice.social[,1:5] <- exp(posterior.choice.social[,1:5])
posterior.choice.nonsocial[,1:5] <- exp(posterior.choice.nonsocial[,1:5])
posterior.choice.social$Par <- rep(c("Valence.lag_c", "Arousal.lag_c", 
                              "Dominance.lag_c", "Submission.lag_c", "Closeness.lag_c", "Interactions.lag_c", "Novelty.lag_c"), each = 5)
posterior.choice.social$Unit <- rep(c("Mean","32992","33089","33825","34700"), times = 7)
posterior.choice.social$Unit <- factor(posterior.choice.social$Unit, 
                                levels=c("Mean","32992","33089","33825","34700"))

posterior.choice.nonsocial$Par <- rep(c("Valence.lag_c", "Arousal.lag_c", 
                                     "Dominance.lag_c", "Submission.lag_c", "Closeness.lag_c", "Interactions.lag_c", "Novelty.lag_c"), each = 5)
posterior.choice.nonsocial$Unit <- rep(c("Mean","32992","33089","33825","34700"), times = 7)
posterior.choice.nonsocial$Unit <- factor(posterior.choice.nonsocial$Unit, 
                                       levels=c("Mean","32992","33089","33825","34700"))

posterior.pupil.social$Par <- rep(c("Valence_c", "Arousal_c",
                             "Dominance_c", "Submission_c", "Closeness_c", "Interactions_c", "Novelty_c"), each = 5)
posterior.pupil.social$Unit <- rep(c("Mean","32992","33089","33825","34700"), times = 7)
posterior.pupil.social$Unit <- factor(posterior.pupil.social$Unit, 
                               levels=c("Mean","32992","33089","33825","34700"))
posterior.pupil.nonsocial$Par <- rep(c("Valence_c", "Arousal_c",
                                    "Dominance_c", "Submission_c", "Closeness_c", "Interactions_c", "Novelty_c"), each = 5)
posterior.pupil.nonsocial$Unit <- rep(c("Mean","32992","33089","33825","34700"), times = 7)
posterior.pupil.nonsocial$Unit <- factor(posterior.pupil.nonsocial$Unit, 
                               levels=c("Mean","32992","33089","33825","34700"))

posterior.choice.social$Par <- gsub(".lag_c","",posterior.choice.social$Par)
posterior.choice.social %>%
  ggplot(aes(x=Par,y=mean,color=Unit,group=Unit)) + 
  geom_hline(yintercept = 1, lty=2, col='grey') + 
  geom_linerange(aes(ymin=X25., ymax=X75.),lwd=1.25,pos=position_dodge(.6)) +   
  geom_linerange(aes(ymin=X5., ymax=X95.), pos=position_dodge(.6)) +   
  geom_point(bg="white", pch=21, pos = position_dodge(.6), size=3) +
  ylab("Odds ratio for choosing \n social over non-social video") + xlab("Previous Video's Content") + 
  coord_trans(y = "log", limy = c(.25,9)) +
  scale_y_continuous(breaks = c(0.5,0.75, 1, 1.25, 1.5, 2, 3, 6, 10)) +  
  scale_color_manual(values=c("darkgrey",col)) + theme_bw() + 
  ggtitle("After Social Video")

write_csv(posterior.choice.social, path = "~./bayesian_results/SocialChoice_AfterSocial_coefs.csv")

posterior.choice.nonsocial$Par <- gsub(".lag_c","",posterior.choice.nonsocial$Par)
posterior.choice.nonsocial %>%
  ggplot(aes(x=Par,y=mean,color=Unit,group=Unit)) + geom_hline(yintercept = 1, lty=2, col='grey') + 
  geom_linerange(aes(ymin=X25., ymax=X75.),lwd=1.25,pos=position_dodge(.6)) +   
  geom_linerange(aes(ymin=X5., ymax=X95.), pos=position_dodge(.6)) +   
  geom_point(bg="white", pch=21, pos = position_dodge(.6),size=3) +
  ylab("Odds ratio for choosing \n social over non-social video") + xlab("Previous Video's Content") +  
  xlab("Previous Video's Content") + 
  coord_trans(y = "log", limy = c(.25,9)) + 
  scale_y_continuous(breaks = c(0.5,0.75, 1, 1.25, 1.5, 2, 3, 6, 10)) +  
  scale_color_manual(values=c("darkgrey",col)) + theme_bw() + ggtitle("After Non-Social Video")

write_csv(posterior.choice.nonsocial, path = "./bayesian_results/SocialChoice_AfterNonSocial_coefs.csv")

posterior.pupil.social %>% filter(Par != "(Intercept)") %>%
  ggplot(aes(x=Par,y=mean,color=Unit,group=Unit)) + geom_hline(yintercept = 0, lty=2, col='grey') + 
  geom_linerange(aes(ymin=X25., ymax=X75.),lwd=1.25,pos=position_dodge(.6)) +   
  geom_linerange(aes(ymin=X5., ymax=X95.), pos=position_dodge(.6)) +   
  geom_point(bg="white", pch=21, pos = position_dodge(.6),size=3) +
  ylab("Std. Diff.") + xlab("Covariate") + 
  scale_color_manual(values=c("darkgrey",col)) +
  theme_bw()  + ggtitle("During Social Video") + ylim(c(-5,25))

write_csv(posterior.pupil.social, path = "./bayesian_results/ChangeInPupil_DuringSocial_coefs.csv")

posterior.pupil.nonsocial %>% filter(Par != "(Intercept)") %>%
  ggplot(aes(x=Par,y=mean,color=Unit,group=Unit)) + geom_hline(yintercept = 0, lty=2, col='grey') + 
  geom_linerange(aes(ymin=X25., ymax=X75.),lwd=1.25,pos=position_dodge(.6)) +   
  geom_linerange(aes(ymin=X5., ymax=X95.), pos=position_dodge(.6)) +   
  geom_point(bg="white", pch=21, pos = position_dodge(.6),size=3) +
  ylab("Std. Diff.") + xlab("Covariate") + 
  scale_color_manual(values=c("darkgrey",col)) +
  theme_bw()  + ggtitle("During Non-Social Video") + ylim(c(-5,25))

write_csv(posterior.pupil.nonsocial, path = "./bayesian_results/ChangeInPupil_DuringNonSocial_coefs.csv")