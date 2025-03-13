library(tidyverse)
library(haven)
library(psych)

url <- "https://github.com/jgbabativam/AnaDatos/raw/main/datos/DASS21.sav"
dass <- read_sav(url)

lapply(dass, function(x) attributes(x)$label)


glimpse(dass)


dass |> 
  ggplot(aes(x = DEPRESION, y = ESTRES)) +
  geom_point() +
  facet_wrap(~as_factor(sexo))


vars <- dass |> select(DEPRESION, ANSIEDAD, ESTRES)

cor(vars)
pairs.panels(vars, main="Matriz de correlaciones")


dass |> 
  ggplot(aes(x = as_factor(sexo), y = ESTRES, fill = as_factor(sexo))) +
  geom_boxplot() +
  theme_classic()


vars <- dass |> select(starts_with("P1_"), DEPRESION)
pairs.panels(vars, main="Matriz de correlaciones")


############# Regresion

mod_dep <- lm(DEPRESION ~ P1_2, data = dass)

tbl_regression(mod_dep, intercept = TRUE) |> 
  add_glance_table(include = c(r.squared, p.value))


glimpse(dass)

class(dass$sexo)

dass$sexo <- as_factor(dass$sexo)
class(dass$sexo)
levels(dass$sexo)

mod_dep <- lm(DEPRESION ~ P1_2 + sexo, data = dass)

tbl_regression(mod_dep, intercept = TRUE) |> 
  add_glance_table(include = c(r.squared, p.value))


#### Más de dos categorías
#### Sexo y Educ -- B0
### cambiar categoría de referencia





