library(dplyr)
library(ggplot2)
bee<-read.csv("/Users/samwiselimpavido/Desktop/Statistics/6. Poisson/Eulaema.csv", h=T)%>%
  mutate(
    SA = as.factor(SA),
    SU = as.factor(SU),
    method = as.factor(method)
  )
str(bee)


pairs(bee)

ggplot(bee,aes(method, Eulaema_nigrita)) +
  geom_point()
ggplot(bee,aes(effort, Eulaema_nigrita)) +
  geom_point()

# If we do exp(effort), are the really high values outliers than we should not use?

# Temporary a priori model
m1<-glm(Eulaema_nigrita~lu_het+forest.+Tseason+Pseason+effort, data=bee,family="poisson")
summary(m1)
# Overdispersed
library(MASS)
m2<-glm.nb(Eulaema_nigrita~lu_het+forest.+Tseason+Pseason+effort, data=bee)
summary(m2)
#Removing lu_het
m3<-glm.nb(Eulaema_nigrita~forest.+Tseason+Pseason+effort, data=bee)
summary(m3)
#Tried altitude instead of climate variables
m4<-glm.nb(Eulaema_nigrita~forest.+altitude+effort, data=bee)
summary(m4)

#I want to see the backward selection
m5<-glm.nb(Eulaema_nigrita~lu_het+forest.+altitude+Tseason+Pseason+effort+MAP+MAT+method, data=bee)#...
summary(m5)
library(car)
vif(m5)
library(ppcor)
predictors <- bee[, c("lu_het","forest.","Tseason","Pseason","effort","MAP","MAT","altitude")]
predictors_clean <- na.omit(predictors)
pcor_results <- pcor(predictors_clean)
pcor_results$estimate
#...
m6<-glm.nb(Eulaema_nigrita~forest.+Tseason+Pseason+effort+MAP, data=bee)
# Reasonably similar to a priori model
summary(m6)
predictors <- bee[, c("Tseason","Pseason","MAP","MAT")]
predictors_clean <- na.omit(predictors)
pcor_results <- pcor(predictors_clean)
pcor_results$estimate

m7<-glm.nb(Eulaema_nigrita~forest.+Tseason+Pseason+effort, data=bee)
#Kept one varibale for precipitation and one for temperature
summary(m7)



#Final model
m<-glm.nb(Eulaema_nigrita~forest.+Tseason+Pseason+effort, data=bee)
summary(m)
plot(m)
coefs<-coef(m)

# y_hat = 3.6357552-1.3740454forest-0.0008547Tseason+0.0167939Pseason+0.3857495effort
# y     = 37.93049+0.2530811forest+0.9991457Tseason+1.016936Pseason+1.470716effort

# For a unit increase in forest, abundance decreases of exp(-1.37) that is 75%, or multiplied by 0.25
# For a unit increase in Tseason, abundance decreases of exp(-0.001) that is 1%, or multiplied by 0.99
# For a unit increase in Pseason, abundance increases of exp(-0.02) that is 1%, or multiplied by 1.02
# For a unit increase in effort, abundance increases of exp(-0.39) that is 47%, or multiplied by 1.47

# When we say about one unit change is a change of 1. For forest cover: the 75% decrease happen
# going from 0 to 1 cover. What about the change for a unit change of 0.1?
# 0.25^0.1 = 0.87. In this case the decrease is 13% for every 0.1 increase in forest cover.
# It matches that 10 times this change is 75%. It would be 0.87^10=0.25.

r2=1-(m$deviance/m$null.deviance) # 0.3413617
overdispersion=208.14/173 # 1.203121

means <- summary(m)$coefficients[,1]
back.means <- exp(means)
std.errs <- summary(m)$coefficients[,2]
back.se.up <- exp(means+std.errs)
back.se.down <- exp(means-std.errs)
CV.up<-means+1.96*std.errs
CV.down<-means-1.96*std.errs
back.CV.up<-exp(means+1.96*std.errs)
back.CV.down<-exp(means-1.96*std.errs)
df <- data.frame(
  term = names(back.means),
  estimate = back.means,
  lower = back.CV.down,
  upper = back.CV.up
)

library(broom)
tidy_m <- tidy(m, conf.int = TRUE, conf.level = 0.95) # Alternative, different CIs than custom made
# Above is still not back transformed
confint(m) #Alternative for CIs, different CIs than custom made
confint.default(m) #Also for the CIs, same as custom calculated, but higher rounding precision

# Plot effect sizes back transformed
df$term <- as.factor(df$term)
ggplot(df%>%
         filter(term!="(Intercept)")# %>%
       # mutate(label = paste0(
       #   round(estimate, 2), " [",round(lower, 2), ", ", round(upper, 2), "]"))
       , aes(x = estimate, y = term)) +
  geom_point(size=3) +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0) +
  geom_vline(xintercept = 1, linetype = "dashed") + 
  scale_x_continuous(limits = c(0, 2.01), breaks = seq(0, 2, by = 0.25))+
  scale_y_discrete(labels = c("Sampling effort",
                              "Forest cover",
                              "Precipitaion\nseasonality",
                              "Temperature\nseasonality"))+
  #geom_text(aes(label = label, x = upper + 0.05), hjust = 0, size = 3) +
  theme_classic() +
  labs(x = "Effect size", y = "") +
  theme(axis.text.x= element_text(color="gray20", size = 11),
        axis.text.y= element_text(color="gray20", size = 10, margin = margin(r=10)),
        axis.line = element_line(colour = "gray20"),
        axis.ticks = element_line(colour = "gray20"),
        axis.title.x = element_text(colour = "gray14", size = 12, margin = margin(t=10)),
        plot.margin = margin(5, 60, 5, 5))




# Plotting effect of forest holding other predictors constant
forest_seq <- seq(min(bee$forest.), max(bee$forest.), length.out = 100)
newdata <- data.frame(
  forest. = forest_seq,
  Tseason = mean(bee$Tseason),
  Pseason = mean(bee$Pseason),
  effort = mean(bee$effort)
)
pred <- predict(m, newdata, type = "link", se.fit = TRUE)
newdata$y_pred <- exp(pred$fit)
newdata$y_lower <- exp(pred$fit - 1.96*pred$se.fit)
newdata$y_upper <- exp(pred$fit + 1.96*pred$se.fit)
plot(bee$forest., bee$Eulaema_nigrita,
     xlab = "Forest Cover",
     ylab = "Counts",
     ylim=c(0,200))
lines(newdata$forest., newdata$y_pred, col = "black", lwd = 2)
lines(newdata$forest., newdata$y_lower, col = "black", lty = 2)
lines(newdata$forest., newdata$y_upper, col = "black", lty = 2)

# You can use type = "response" to get the predicted mean counts:
# But if you also want confidence intervals, R does not automatically provide
# the standard errors on the response scale, so they can be inaccurate.

# Plotting effect of forest using the "averaging marginal effects" approach:
forest_seq <- seq(min(bee$forest.), max(bee$forest.), length.out = 100)
marginal_preds <- numeric(length(forest_seq))
marginal_lower <- numeric(length(forest_seq))
marginal_upper <- numeric(length(forest_seq))
for (i in seq_along(forest_seq)) {
  f <- forest_seq[i]
  temp <- bee
  temp$forest. <- f
  pred <- predict(m, newdata = temp, type = "link", se.fit = TRUE)
  link_lower <- pred$fit - 1.96 * pred$se.fit
  link_upper <- pred$fit + 1.96 * pred$se.fit
  marginal_preds[i] <- mean(exp(pred$fit))
  marginal_lower[i] <- mean(exp(link_lower))
  marginal_upper[i] <- mean(exp(link_upper))
}
plot(bee$forest., bee$Eulaema_nigrita, pch=20, las=1,
     col = rgb(0, 0, 0, 0.2),
     xlab = "Forest Cover",
     ylab = expression("Counts of  " * italic("E. nigrita")))
lines(forest_seq, marginal_preds, type = "l",lwd = 2)
lines(forest_seq, marginal_lower, lty = 2, col = "black")
lines(forest_seq, marginal_upper, lty = 2, col = "black")

# At 1, it is 31.95886 At 0, it is 126.27914 in the fixed way
# At 1, it is 24.15750. At 0, it is 95.45362 in the AME way
# They both comply with the 75% rule!
# It would not work if instead of +, I used *


#Basic Predicted vs Observed Plot
bee$y_pred <- predict(m, type = "response")
plot(bee$Eulaema_nigrita, bee$y_pred, xlim=c(0,300), ylim=c(0,300), las=1,
     xlab = "Observed counts",
     ylab = "Predicted counts")
abline(0, 1, lwd = 2)
# Still quite a lot of variation



# Questions:
# I am confused about how to deal with sampling effort
# The bad model fit, is it related to 1-r^2 being 66%?
# I am always a bit confused what per unit change is equivalent to, quantified