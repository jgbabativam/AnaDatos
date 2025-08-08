library(pacman)
p_load(tidyverse, modelr, broom, gt, gtsummary)

datos <- heights

modelo1 <- lm(income ~ education, data = datos)

tbl_regression(modelo1, intercept = TRUE)

glimpse(datos)
class(datos$sex)

levels(datos$sex)

modelo2 <- lm(income ~ sex, data = datos)

tbl_regression(modelo2, intercept = TRUE) |> 
  add_glance_table(include = c(r.squared, p.value))

class(datos$marital)
levels(datos$marital)


datos$marital <- relevel(datos$marital, ref = "married")

modelo3 <- lm(income ~ education + sex + marital, data = datos)

tbl_regression(modelo3, intercept = TRUE) |> 
  add_glance_table(include = c(r.squared, p.value))


par(mfrow = c(2, 2))
plot(modelo3)

plot(modelo3, 4)

p_load(performance)

check_model(modelo3)
check_collinearity(modelo3)
check_outliers(modelo3)
check_autocorrelation(modelo3)
check_heteroscedasticity(modelo3)
check_normality(modelo3)

