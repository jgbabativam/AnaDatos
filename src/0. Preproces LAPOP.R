rm(list = ls())

library(pacman)


p_load(tidyverse, janitor, corrplot, haven,
       devtools, FactoMineR, factoextra)

colombia <- read_stata("data/COL_2021_LAPOP.dta")

glimpse(colombia)

datos <- colombia |> 
         mutate(region = factor(estratopri, levels = 811:816,
                                labels = c("Atlántica",
                                           "Bogotá D.C.",
                                           "Central",
                                           "Oriental",
                                           "Pacífica",
                                           "Antiguos Territorios Nacionales")),
         sexo = as_factor(sexi),
         edad = factor(case_when(
                     between(q2, 18, 25) ~ 1,
                     between(q2, 26, 35) ~ 2,
                     between(q2, 36, 45) ~ 3,
                     between(q2, 46, 55) ~ 4,
                     between(q2, 56,100) ~ 5,
                     TRUE ~ NA
         ), levels = 1:5, labels = c("18 a 25 años", "26 a 35 años", "36 a 45 años", "46 a 55 años", "56 años o más")),
         educacion = factor(ifelse(edr %in% c(0, 1, NA), 1, edr), levels = 1:3,
                            labels = c("Máximo Primaria (incompleta o completa)", 
                                       "Secundaria (incompleta o completa)",
                                       "Universitaria o superior (incompleta o completa)"))
         ) |> 
         filter(!is.na(region)) |> 
         mutate(
#Cuestionario B: Alguna gente dice que en ciertas circunstancias se justificaría que los militares de este país tomen el poder
#                por un golpe de Estado. En su opinión se justificaría que hubiera un golpe de Estado por los militares           
           justifica_golpe = ifelse(jc13 == 1 | jc13covid == 1, 1, 0),
         )


datos$jc13covid


devtools::install_github("https://github.com/arcruz0/paqueteadp")

library(paqueteadp)

data("lapop") 
