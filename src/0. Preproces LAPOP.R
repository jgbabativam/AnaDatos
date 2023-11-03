rm(list = ls())

library(pacman)


p_load(tidyverse, janitor, corrplot, haven,
       devtools, FactoMineR, factoextra, VIM)

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
                     TRUE ~ NA), 
                     levels = 1:5, 
                     labels = c("18 a 25 años", 
                                "26 a 35 años", 
                                "36 a 45 años", 
                                "46 a 55 años", "56 años o más")),
         educacion = factor(ifelse(edr %in% c(0, 1, NA), 1, edr), levels = 1:3,
                            labels = c("Máximo Primaria (incompleta o completa)", 
                                       "Secundaria (incompleta o completa)",
                                       "Universitaria o superior (incompleta o completa)"))
         ) |> 
         filter(!is.na(region)) |> 
         mutate(
           jc13 = ifelse(is.na(jc13) & !is.na(jc13covid), 0, jc13),
           jc13covid = ifelse(!is.na(jc13) & is.na(jc13covid), 0, jc13covid),
           just_golpe = ifelse(jc13 == 1 | jc13covid == 1, 1, 0),
           just_cierre_cong = ifelse(jc15a == 1, 1, 0),
           conf_gobiero_nal = 5 - anestg,
           conf_instit = b2,
           conf_alcaldia = b32,
           conf_fuerzas_mil = b12,
           conf_policia = b18,
           conf_elecciones = b47a,
           conf_medios = b37,
           prot_derechos = b3,
           sat_democracia = 5 - pn4,
           orgullo_sistema = b4
         ) |> 
     select(idnum, region, sexo, edad, educacion, starts_with("just"), starts_with("conf"), prot_derechos, sat_democracia, orgullo_sistema) |> 
     mutate(across(where(is.numeric),~as.integer(.)))

library(mice)
plaus <- mice(datos |> 
              select(-idnum), m = 5, maxit = 5, method = "pmm", seed = 26052013)

df_colombia <- complete(plaus, action = 1)

dir.create("output")
save(df_colombia, file = "output/df_colombia.rds")
