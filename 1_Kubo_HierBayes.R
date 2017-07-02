load("data.RData")
d

summary(glm(mean.Y ~ 1 + Age + Age:X, data = d))