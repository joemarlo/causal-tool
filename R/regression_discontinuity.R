set.seed(40)
n <- 200
age <- rnorm(n, 50, 12)
eligible <- (age > 50)


y_0 <- age + 0 + rnorm(n, 0, 5)
y_1 <- age + 5 + rnorm(n, 0, 5)

# scale the numbers between 0 and 30
x <- c(y_0, y_1)
scaled <- ((x - min(x)) / (max(x) - min(x))) * 100
rm(x)
y_0 <- scaled[0:length(y_0)]
y_1 <- scaled[(length(y_0)+1):(length(y_0) + length(y_1))]

mean(y_1 - y_0)
range(c(y_0, y_1))
plot(age, y_0, ylab = "y")
points(age, y_1, col = 'red')

fullA <- data.frame(age, y_0, y_1)
obsA <- data.frame(age, eligible, y = y_0 * (eligible == 0) + y_1 * eligible)

library(tidyverse)
obsA %>%
  ggplot(aes(x = age, y = y, color = as.logical(eligible))) +
  geom_smooth(method = 'lm') +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = c("blue", "red")) +
  labs(title = "World A from researcher perspective",
       x = "Age",
       y = "Health",
       color = "Eligibility")


model_A_lm_all <- lm(y ~ age + eligible, data = obsA)
model_A_int_all <- lm(y ~ age * eligible, data = obsA)
model_A_quad_all <- lm(y ~ age * eligible + I(age^2) * eligible, data = obsA)

model_A_lm <- lm(y ~ age + eligible, data = obsA[obsA$age >= 47 & obsA$age <= 53,])
model_A_int <- lm(y ~ age * eligible, data = obsA[obsA$age >= 47 & obsA$age <= 53,])
model_A_quad <- lm(y ~ age * eligible + I(age^2) * eligible, data = obsA[obsA$age >= 47 & obsA$age <= 53,])

data.frame(Model = c("Linear model", "Linear model w/interaction", 'Quadratic model'),
           `All` = c(coef(model_A_lm_all)[['eligibleTRUE']], 
                     coef(model_A_int_all)[['eligibleTRUE']] + (20 * coef(model_A_int_all)[['age:eligibleTRUE']]),
                     coef(model_A_quad_all)[['eligibleTRUE']]
                     + (20 * coef(model_A_quad_all)[['age:eligibleTRUE']])
                     + (20^2 * coef(model_A_quad_all)[['eligibleTRUE:I(age^2)']])),
           `Observable` = c(coef(model_A_lm)[['eligibleTRUE']], 
                            coef(model_A_int)[['eligibleTRUE']] + (20 * coef(model_A_int)[['age:eligibleTRUE']]),
                            coef(model_A_quad)[['eligibleTRUE']]
                            + (20 * coef(model_A_quad)[['age:eligibleTRUE']])
                            + (20^2 * coef(model_A_quad)[['eligibleTRUE:I(age^2)']])))

