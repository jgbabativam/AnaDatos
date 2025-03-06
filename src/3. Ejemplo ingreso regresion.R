

library(pacman)
p_load(tidyverse, modelr, broom, gt, gtsummary)

datos <- heights

modelo1 <- lm(income ~ education, data = datos)

tbl_regression(modelo1, intercept = TRUE)
