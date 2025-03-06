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





