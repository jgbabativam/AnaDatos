library(pacman)
p_load(tidyverse, janitor,
       FactoMineR, factoextra, Factoshiny, 
       skimr, corrplot, psych, gt, gtsummary, haven)

url <- "https://github.com/jgbabativam/AnaDatos/raw/main/datos/RESUMEN.sav"
datos <- read_sav(url) |> as_factor()

datos <- datos |> 
  column_to_rownames(var = "DPTO")

res <- PCA(datos, scale.unit = T, graph = F)
fviz_screeplot(res, addlabels = TRUE, ylim = c(0, 60))

fviz_pca_var(res, axes = c(3, 5),
             col.var="contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

fviz_pca_ind(res, col.ind = "cos2", axes = c(3, 5),
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)


fviz_pca_biplot(res, repel = TRUE, col.ind = "blue", col.var = "red")


abs(res$var$cor[,1])/sum(abs(res$var$cor[,1]))


index <- as.data.frame(res$ind$coord[,1]) |> 
  rownames_to_column("DPTO") |> 
  rename(Indice = `res$ind$coord[, 1]`) |> 
  mutate(reesc = round(exp(Indice)/(1+exp(Indice))*100, 1)) |> 
  arrange(-reesc)












