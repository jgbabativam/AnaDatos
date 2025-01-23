library(pacman)

p_load(tidyverse, haven, skimr)

url <- "https://github.com/jgbabativam/AnaDatos/raw/main/datos/ENSIN.sav"
ensin <- read_sav(url)

glimpse(as_factor(ensin))
View(as_factor(ensin))


ggplot(ensin, aes(x = Peso, y = Estatura)) +
  geom_point() +
  labs(title = "Peso vs Estatura") +
  theme_bw()










