## Michael Black
## January 2020
## Images for linear regression lecture

#install.packages("pacman")
pacman::p_load(dplyr, ggplot2, wooldridge, openxlsx)

setwd("/Volumes/GoogleDrive/My Drive/Teaching/AGEC 317 Spring 2020/Module 3_ Simple Linear Regression/Lecture")

################################################################################
## For lecture
################################################################################
data('mtcars')
mtcars <- mtcars %>%
  select(wt, mpg)
write.xlsx(mtcars, "cars.xlsx")

# Scatter
raw<-ggplot(mtcars, aes(x=wt, y=mpg)) + 
  geom_point(aes(size = 2), show.legend = FALSE)+
  labs(x = "Weight, thousands lbs", y = "MPG", title = "MPG vs Vehicle Weight: Raw Data")+
  theme_classic()
# Remove the confidence interval
linear<-ggplot(mtcars, aes(x=wt, y=mpg)) + 
  geom_point(aes(size = 2), show.legend = FALSE)+
  geom_smooth(method=lm, se=FALSE)+
  labs(x = "Weight, thousands lbs", y = "MPG", title = "MPG vs Vehicle Weight: Linear Regression")+
  theme_classic()
# Different line
linear2<-ggplot(mtcars, aes(x=wt, y=mpg)) + 
  geom_point(aes(size = 2), show.legend = FALSE)+
  geom_smooth(method=lm, se=FALSE)+
  labs(x = "Weight, thousands lbs", y = "MPG", title = "MPG vs Vehicle Weight: Linear Regression")+
  theme_classic()+
  geom_abline(intercept = 37, slope = -5, size = 2, color = "red")
# Horizontal line at mean, with residuals
mtcars <- mtcars %>%
  mutate(mpgm = mean(mpg),
         mpgr = mpg - mpgm)
hori_resid <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_segment(aes(xend = wt, yend = mpgm), alpha = .2) +  
  geom_point(size = 3) +
  geom_point(aes(y = mpgm), shape = 1, size = 2) +
  geom_hline(yintercept = 20.09, size = 1, color = "red") +
  labs(x = "Weight, thousands lbs", y = "MPG", title = "MPG vs Vehicle Weight: Plotting Residuals")+
  theme_classic()
# Horizontal line at mean
linear3<-ggplot(mtcars, aes(x=wt, y=mpg)) + 
  geom_point(aes(size = 2), show.legend = FALSE)+
  #geom_smooth(method=lm, se=FALSE)+
  labs(x = "Weight, thousands lbs", y = "MPG", title = "MPG vs Vehicle Weight: Linear Regression")+
  theme_classic()+
  geom_abline(intercept = 21, slope = 0, size = 2, color = "red")
# Horizontal line at 200
mtcars <- mtcars %>%
  mutate(mpg200 = 200,
         mpgr = mpg - mpg200)
woah_resid <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_segment(aes(xend = wt, yend = mpg200), alpha = .2) +  
  geom_point(size = 3) +
  geom_point(aes(y = mpg200), shape = 1, size = 2) +
  geom_hline(yintercept = 200, size = 1, color = "red") +
  labs(x = "Weight, thousands lbs", y = "MPG", title = "MPG vs Vehicle Weight: Plotting Residuals")+
  theme_classic()
# Loess method
nonlinear<-ggplot(mtcars, aes(x=wt, y=mpg)) + 
  geom_point(aes(size = 2), show.legend = FALSE)+
  geom_smooth(se = FALSE)+
  labs(x = "Weight, thousands lbs", y = "MPG", title = "MPG vs Vehicle Weight: Non-linear Regression")+
  theme_classic()
# Plot residuals
fit <- lm(mpg ~ wt, data = mtcars)
mtcars$predicted <- predict(fit)
mtcars$residuals <- residuals(fit)
resids <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_smooth(method = "lm", se = FALSE) + 
  geom_segment(aes(xend = wt, yend = predicted), alpha = .2) +  
  geom_point(size = 3) +
  geom_point(aes(y = predicted), shape = 1, size = 2) +
  labs(x = "Weight, thousands lbs", y = "MPG", title = "MPG vs Vehicle Weight: Plotting Residuals")+
  theme_classic()

ggsave("raw.png", plot = raw)
ggsave("linear.png", plot = linear)
ggsave("linear2.png", plot = linear2)
ggsave("linear3.png", plot = linear3)
ggsave("nonlinear.png", plot = nonlinear)
ggsave("resids.png", plot = resids)
ggsave("hori_resid.png", plot = hori_resid)
ggsave("woah_resid.png", plot = woah_resid)
################################################################################
## For problem set
################################################################################
setwd("/Volumes/GoogleDrive/My Drive/Teaching/AGEC 317 Spring 2020/Module 3_ Simple Linear Regression/PS3")
data('econmath')
econmath <- head(econmath, 25)
econmath <- econmath %>%
  select(study, work, colgpa, score, age)
#write.xlsx(econmath, "PS3.xlsx")

# Decide which regressions to run
study <- lm(score ~ study, data = econmath)
work <- lm(score ~ work, data = econmath)
colgpa <- lm(score ~ colgpa, data = econmath)
age <- lm(score ~ age, data = econmath)

summary(study)
summary(work)
summary(colgpa)
summary(age)