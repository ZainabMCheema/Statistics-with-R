red=read.csv('radiation.csv.')
head(red)

mean_damage <- tapply(red$damage, red$treatment, mean)

library(dplyr)

stderr_damage <- red %>%
  group_by(treatment) %>%
  summarize(stderr = sd(damage) / sqrt(n()))

# Print mean damage and standard errors
print(mean_damage)
print(stderr_damage)


library(ggplot2)

ggplot(red, aes(x = exposure, y = damage, color = treatment)) +
  geom_point() +
  labs(x = "Exposure", y = "Damage", color = "Treatment") +
  ggtitle("Exposure vs. Damage by Treatment") +
  theme_minimal()



lm_model <- lm(damage ~ treatment * exposure, data = red)

# Summary of the linear model
summary(lm_model)


# Summary of the linear model
summary(lm_model)
red