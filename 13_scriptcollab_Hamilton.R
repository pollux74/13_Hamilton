# Script R - Hamilton

## Packages utilisés enlever le # devant la deucième ligne si vous vouler installer les packages
Pac <- c("ggplot2","data.table", "psy", "gridExtra", "lme4")
#for (i in Pac) { install.packages(i, character.only = TRUE)}

# Définir le répertoire de travail
setwd("/home/wasabee/R/0_Data_bases/D2")

# Charger les fichiers CSV
gp <- read.csv2("H_groupe.csv", sep=" ")
scl <- read.csv2("autoeval.csv", sep=" ")
ham <- read.csv2("hdrs.csv", sep=" ")

########## Data management  

###### Fusionner les colonnes 16A & 16B du score de Hamilton puis supprimer les anciennes colonnes

ham$HAMD16 <- ifelse(is.na(ham$HAMD16B), ham$HAMD16A, ham$HAMD16B)
ham <- subset(ham, select = -c(HAMD16B, HAMD16A)) # Supprimer les colonnes HAMD16A et HAMD16B après la fusion
ham$HS <- rowSums(subset(ham, select = HAMD1:HAMD16), na.rm = TRUE)# Calculer le score global de Hamilton

###### Calculer le sous-score dépression de SCL90
cols_list <- list(
  D = c(5, 14, 15, 20, 22, 26, 29, 30, 31, 32, 54, 71, 79),
  Som = c(1, 4, 12, 27, 42, 48, 49, 52, 53, 56, 58, 40),
  Obs = c(9, 10, 28, 38, 3, 45, 46, 51, 55, 65),
  Hos = c(11, 24, 63, 67, 74, 81),
  Phob = c(13, 25, 47, 70, 75, 82, 50),
  Para = c(8, 18, 43, 68, 76, 83),
  Psycho = c(7, 16, 35, 62, 77, 84, 85, 87, 90, 88),
  Div = c(19, 44, 59, 60, 64, 66, 89),
  Sens = c(6, 21, 34, 36, 37, 41, 61, 69, 73),
  Anx = c(2, 17, 23, 33, 39, 57, 72, 78, 80, 86)
)

for (name in names(cols_list)) {
  cols <- paste0("Q", cols_list[[name]])
  scl[cols] <- lapply(scl[cols], function(x) as.numeric(as.character(x)))
  scl[[paste0("HS_", name)]] <- rowSums(scl[cols], na.rm = TRUE)
}


###### Constitution d'un dataframe global au format large (db_large)

library(data.table) # Charger la bibliothèque data.table pour utiliser la fonction dcast


ham <- merge(gp, as.data.frame(ham), by="NUMERO", all.x = T, all.y = T) # Fusionner les données `ham` et `gp` pour obtenir un dataframe complet
ham_large <- dcast(as.data.table(ham), NUMERO ~ VISIT, value.var = c(paste0("HAMD", (1:16)), "HS")) # Transformer les données `ham` en format large avec `dcast`

db_large <- merge(gp, as.data.frame(ham_large), by="NUMERO", all.x = T, all.y = T) # Fusionner les données du groupe `gp` avec les données `ham_large`
scl_large <- dcast(as.data.table(scl), NUMERO ~ VISIT, value.var = c(paste0("Q", 1:90), "HS_D", "HS_Som", "HS_Obs", "HS_Hos", "HS_Phob", "HS_Para", "HS_Psycho", "HS_Div", "HS_Sens", "HS_Anx"), fill = NA) # Transformer les données `scl` en format large avec `dcast`
db_large <- merge(as.data.frame(scl_large), db_large, by="NUMERO", all.x = T, all.y = T) # Fusionner `scl_large` et `db_large` pour obtenir un dataframe complet

sum(is.na(ham))
sum(is.na(db_large))
sum(is.na(ham_large))
sum(is.na(scl_large))
sum(is.na(scl))

sum(is.na(ham_large)) / (nrow(ham_large) * ncol(ham_large)) * 100
sum(is.na(db_large)) / (nrow(db_large) * ncol(db_large)) * 100

sum(is.na(db_large[col_ham_J0])) / (nrow(db_large[col_ham_J0]) * ncol(db_large[col_ham_J0])) * 100
sum(is.na(db_large[col_ham_J56])) / (nrow(db_large[col_ham_J56]) * ncol(db_large[col_ham_J56])) * 100


###### Description des données 

######### scl-Questionnaire

# Boucle pour afficher les distributions des réponses pour chaque question (Q1 à Q90)


for (i in 1:90) {
  col_scl_J0 <- paste0("Q", i, "_J0")
  tab <- table(db_large[[col_scl_J0]], deparse.level = 2) 
  print(i)
  print(tab)
}
# Calculer le nombre de valeurs manquantes dans le dataframe `scl`
sum(is.na(scl))

round(100 * (sum(is.na(scl)) / (nrow(scl) * ncol(scl))), 2) # % de données manquantes
table(scl$Q1)
round(100 * (length(which(scl_j0 == 0)) / (nrow(scl_j0) * ncol(scl_j0))), 1) ## % réponse 0 J0
round(100 * (length(which(scl_j0 == 4)) / (nrow(scl_j0) * ncol(scl_j0))), 1) ## % réponse 4 J0

#### Données manquantes
# Trouver les indices des valeurs manquantes pour les colonnes HAMD1 à HAMD16 pour la visite J0
which(is.na(db_large[paste0("HAMD", 1:16, "_J0")]))
db_large[1, paste0("HAMD", 1:16, "_J0")] # Afficher les données manquantes pour la première observation


################################################  Question 1 : validation du score HDRS

################ Histogrammes des réponses aux questions à J0 & J58 du score de Hamilton

# Créer les noms des colonnes pour les scores à J0 et J56
col_ham_J0 <- paste0("HAMD", 1:16, "_J0")
col_ham_J56 <- paste0("HAMD", 1:16, "_J56")




library(ggplot2)
library(gridExtra)

# Créer une liste de graphiques pour chaque item HAMD à J0


fhist_item <- function(i, j) {
  col_ham <- paste0("HAMD", i, "_", j)
  ggplot(data = db_large, aes(x = factor(.data[[col_ham]], levels = c(0, 1, 2, 3, 4, NA)))) +
    geom_bar(fill = ifelse(j == "J0", "#69b3a2", "#ffb347"), color = "#e9ecef", alpha = 0.9) +
    ggtitle(paste("Item", i, "à", j)) +
    theme_classic() +
    theme(plot.title = element_text(size = 15)) +
    scale_x_discrete(drop = FALSE)
}

plist_J0 <- lapply(1:16, function(i) fhist_item(i, "J0"))
plist_J56 <- lapply(1:16, function(i) fhist_item(i, "J56"))

grid.arrange(grobs = plist_J0, ncol = 4, nrow =4)
grid.arrange(grobs = plist_J56, ncol = 4, nrow =4)



#### Diagramme des valeurs propres et analayse factorielle
# Charger la bibliothèque psy pour les analyses psychométriques
library(psy)

# Afficher le diagramme des valeurs propres pour HDRS à J0 et les items spécifiques
scree.plot(db_large[, col_ham_J0], simu = 20, title = "Diagramme des valeurs propres HDRS J0")
#### Analyse factorielle à 3 facteurs avec rotation varimax à J0
fact_j0 <-factanal(na.omit(db_large[,col_ham_J0]), factors = 4)
print(fact_j0, cutoff=0)

write.csv2(round(as.data.frame(unclass(fact_j0$loadings)), digits = 3), file = "/home/wasabee/M2/Devoirs/D2/loadings_j0f4.csv", row.names = TRUE)

# Afficher le diagramme des valeurs propres pour HDRS à J56 et les items spécifiques
scree.plot(db_large[, col_ham_J56], simu = 20, title = "Diagramme des valeurs propres HDRS J56")
#### Analyse factorielle à 3 facteurs avec rotation varimax à J56
fact_j56 <-factanal(na.omit(db_large[,col_ham_J56]), factors = 3)
print(fact_j56, cutoff=0)

write.csv2(round(as.data.frame(unclass(fact_j56$loadings)), digits = 3), file = "/home/wasabee/M2/Devoirs/D2/loadings_j56.csv", row.names = TRUE)
plot(sort(as.numeric(fact_j0$loadings[,2])))





###################### Sous-score spécifique dépression
# Créer des vecteurs pour les scores spécifiques (questions sélectionnées) à J0 et J56
col_af1_J0 <- paste0("HAMD", c(12,14,16), "_J0")
scree.plot(db_large[, col_af1_J0], simu = 20, title = "Diagramme des valeurs propres HDRS J0, item spécifiques")
col_af2_J0 <- paste0("HAMD", c(4,5,13,15), "_J0")
scree.plot(db_large[, col_af2_J0], simu = 20, title = "Diagramme des valeurs propres HDRS J0, item spécifiques")
col_af3_J0 <- paste0("HAMD", c(1,2,3,6,7,8,9,10), "_J0")
scree.plot(db_large[, col_af3_J0], simu = 20, title = "Diagramme des valeurs propres HDRS J0, item spécifiques")

col_af1_J56 <- paste0("HAMD", c(1,3,4,5,7,8,12, 16), "_J56")
scree.plot(db_large[, col_af1_J56], simu = 20, title = "Diagramme des valeurs propres HDRS J56, factor1")

col_af2_J56 <- paste0("HAMD", c(2,6,9,10,11,13,15), "_J56")
scree.plot(db_large[, col_af2_J56], simu = 20, title = "Diagramme des valeurs propres HDRS J56, factor2")



################ Cronbach alpha sous échelle items 1,3,4,7,8,10,12 

cronbach(db_large[, col_af1_J56])
cronbach(db_large[, col_af2_J56])



###################### Validité concourante & divergente




###### Modifier les noms des sous-scores pour correspondre à la description
SS_SCL <- c("HS_D", "HS_Som", "HS_Obs", "HS_Hos", "HS_Phob", "HS_Para", "HS_Psyc", "HS_Div", "HS_Sens", "HS_Anx")

# Initialiser les tableaux de corrélation pour J0 et J56
correlations_J0 <- data.frame(Sous_Score = SS_SCL, Correlation = numeric(length(SS_SCL)))
correlations_J56 <- data.frame(Sous_Score = SS_SCL, Correlation = numeric(length(SS_SCL)))

# Calculer les corrélations pour J0
for (i in seq_along(SS_SCL)) {
  col_name <- paste0(SS_SCL[i], "_J0")
  if (col_name %in% names(db_large)) {
    corr_value <- cor(db_large[[col_name]], db_large$HS_J0, use = "complete.obs", method = "pearson")
    correlations_J0[i, "Correlation"] <- round(corr_value, 3)
  }
}

# Calculer les corrélations pour J56
for (i in seq_along(SS_SCL)) {
  col_name <- paste0(SS_SCL[i], "_J56")
  if (col_name %in% names(db_large)) {
    corr_value <- cor(db_large[[col_name]], db_large$HS_J0, use = "complete.obs", method = "pearson")
    correlations_J56[i, "Correlation"] <- round(corr_value, 3)
  }
}

correlation_table <- merge(correlations_J0, correlations_J56, by = "Sous_Score", suffixes = c("_J0", "_J56"))
correlation_table$Sous_Score <- c("Dépression", "Somatisation", "Symptômes obsessionnels", "Hostilité", "Phobies", "Traits paranoïaques", "Traits psychotiques", "Symptômes divers", "Sensitivité interpersonnelle", "Anxiété")
print(correlation_table)

colnames(correlation_table) <- c("Sous-Score", "Corrélation J0", "Corrélation J56")

write.csv(correlation_table, "/home/wasabee/M2/Devoirs/D2/correlation_table.csv", row.names = FALSE)




#######################################################################################################################################################################
############################################### Question 2 Model mixtes ###############################################################################################
#######################################################################################################################################################################


######### Repartition de la moyenne du score de HAMD au cours du temps ##################################
# Extraire les jours de visite en tant que valeurs numériques
ham$J.VISIT <- as.numeric(sub("J", "", ham$VISIT))

# Calculer la moyenne du score HAMD pour chaque visite
d1 <- aggregate(ham$HS, by = list(ham$VISIT), FUN = mean)
names(d1) <- c("Date", "Mean")

# Définir l'ordre des dates de visite
d1$Date <- factor(d1$Date, levels = paste0("J", c(0, 4, 7, 14, 21, 28, 42, 56)))

# Calculer l'écart-type et le nombre d'observations pour chaque visite
d2 <- aggregate(ham$HS, by = list(ham$VISIT), FUN = sd)
d3 <- aggregate(ham$HS, by = list(ham$VISIT), FUN = length)

# Calculer l'erreur standard de la moyenne
d1$sem <- d2$x / sqrt(d3$x)
pd <- position_dodge(0.1)

# Créer un graphique de la moyenne du score HAMD avec les barres d'erreur
p <- ggplot(d1, aes(x = Date, y = Mean, group = 1)) +
  geom_errorbar(aes(ymin = Mean - sem, ymax = Mean + sem), colour = "black", width = .2, position = pd) +
  geom_line(position = pd) +
  geom_point(position = pd, size = 3, shape = 21) +
  theme_classic()
p

##### En fagot
# Créer un graphique en ligne pour chaque sujet montrant l'évolution du score HAMD
p <- ggplot(ham, aes(x = J.VISIT, y = HS, group = NUMERO)) +
  geom_line(alpha = 0.4) +
  theme_classic() + labs(y = "Score de dépression", x = "Durée de suivi en jours")
p

##### Par groupes
# Créer un graphique avec une courbe lissée pour chaque groupe
# Les groupes sont définis par la variable `GROUPE` (binaire)
ggplot(ham, aes(x = J.VISIT, y = HS, group = NUMERO, colour = GROUPE > 0)) +
  geom_smooth(aes(group = GROUPE > 0, fill = GROUPE > 0)) +
  guides(fill = F) +
  theme_classic() +
  scale_color_discrete(name = "GROUPE", labels = c("0", "1"))

## Analyse factorielle rotation varimax x facteurs

#### BAZARD
# Réorganiser les données pour que les dates soient triées par ordre croissant
d1 <- d1[order(as.numeric(sub("J", "", d1$Date))), ]
scl_j0 <- subset(scl, VISIT == "J0") # Sélectionner les observations de `scl` pour la visite J0
