library(dplyr)
library(ggplot2)

maple<-read.csv("/Users/samwiselimpavido/Desktop/hbr_maples.csv", header=T)
maple <- maple %>%
  filter(!is.na(watershed),
         !is.na(elevation),
         !is.na(stem_length),
         !is.na(stem_dry_mass)) %>%
  mutate(
    watershed = as.factor(watershed),
    elevation = as.factor(elevation))

ggplot(maple, aes(x = watershed, y = stem_length, colour = elevation)) +
  geom_boxplot(width = 0.5) +
  labs(
    x = "Watershed",
    y = "Stem Length (mm)"
  ) +
  theme_classic()

ggplot(maple, aes(x = watershed, y = stem_dry_mass, colour = elevation)) +
  geom_boxplot(width = 0.5) +
  labs(
    x = "Watershed",
    y = "Stem Dry Mass (mg)"
  ) +
  theme_classic()


table_summary <- maple %>%
  group_by(watershed, elevation) %>%
  summarise(
    mean_length = mean(stem_length),
    se_length = sd(stem_length) / sqrt(n()),
    mean_mass = 1000*mean(stem_dry_mass),
    se_mass = 1000*sd(stem_dry_mass) / sqrt(n()),
    .groups = "drop"
  )
watershed_summary <- maple %>%
  group_by(watershed) %>%
  summarise(
    mean_length = mean(stem_length),
    se_length = sd(stem_length) / sqrt(n()),
    mean_mass = 1000*mean(stem_dry_mass),
    se_mass = 1000*sd(stem_dry_mass) / sqrt(n()),
    .groups = "drop"
  )

#Stem Length
m<-lm(stem_length~elevation*watershed, data=maple)
par(mfrow=c(2,2))
plot(m)
par(mfrow=c(1,1))
anova(m)
summary(m)


tot_SS = 189+2857+1+47410
189/tot_SS
2857/tot_SS
47410/tot_SS
1/tot_SS
0.003745764+0.05662247+1.981886e-05


#Stem Dry Mass
maple<-maple%>%
  mutate(stem_dry_mass=stem_dry_mass*1000)
m2<-lm(stem_dry_mass~elevation*watershed, data=maple)
par(mfrow=c(2,2))
plot(m2)
par(mfrow=c(1,1))
anova(m2)
summary(m2)


tot_SS = 582.2+6506.3+61+17732.8
582.2/tot_SS
6506.3/tot_SS
61/tot_SS
17732.8/tot_SS
0.02339816+0.2614831+0.002451542
