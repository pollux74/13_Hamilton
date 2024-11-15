# Script R - Hamilton

## Packages utilisés enlever le # devant la deucième ligne si vous vouler installer les packages
Pac <- c("ggplot2","data.table", "psy", "gridExtra")
#for (i in Pac) { install.packages(i, character.only = TRUE)}
for (i in Pac) {  library(i, character.only = TRUE)}

setwd("/home/wasabee/R/0_Data_bases/D2")
gp <- read.csv2("H_groupe.csv", sep=" ")
scl <- read.csv2("autoeval.csv", sep=" ")
ham <- read.csv2("hdrs.csv", sep=" ")


########## Data management  



###### Fusionner les colonnes 16A & 16B du score de Hamilton puis supprimer les ancienne colonnes
ham$HAMD16 <- ifelse(is.na(ham$HAMD16B)==T,ham$HAMD16A,ham$HAMD16B)
ham <- subset(ham, select = -c(HAMD16B, HAMD16A))

###### Score globale de Hamilton par colonnes
ham$HS <- rowSums(subset(ham, select = HAMD1:HAMD16), na.rm = TRUE)

###### Constitution d'un dataframe globale au format large (db_large)
######## J'utilise dcast() de data.table parcque je trouvait reshape2 trop compliqué
ham_large <- dcast(as.data.table(ham), NUMERO ~ VISIT, value.var = c(paste0("HAMD", (1:16)), "HS"), verbose=TRUE)
db_large <- merge(gp, as.data.frame(ham_large), by="NUMERO", all.x = T, all.y = T)
View(ham_large)
scl_large <- dcast(as.data.table(scl), NUMERO~VISIT, value.var = c(paste0("Q",1:90)), verbose = T, fill = NA)
View(scl_large)
db_large <- merge(as.data.frame(scl_large), db_large, by="NUMERO", all.x = T, all.y = T) ## Format Large SCL, HAM
View(db_large)
ham <- merge(gp, as.data.frame(ham), by="NUMERO", all.x = T, all.y = T)



scl_j0 <- subset(scl, VISIT=="J0")

col_scl_J0 <- NULL
for (i in 1:90) {
  
  col_scl_J0 <- paste0("Q",i,"_J0")
  tab <- table(db_large[[col_scl_J0]], deparse.level = 2) 
  print(i)
  print(tab)
}





###### Description des données 

######### scl-Questionnaire
sum(is.na(scl))
round(100*(sum(is.na(scl))/(nrow(scl)*ncol(scl))),2) # % de données manquantes

table(scl$Q1)

round(100*(length(which(scl_j0==0))/(nrow(scl_j0)*ncol(scl_j0))),1) ## % réponse 0 J0
round(100*(length(which(scl_j0==4))/(nrow(scl_j0)*ncol(scl_j0))),1) ## % réponse 4 J0



#### Question 1 : validation du score SCL90 à J0 & J90

################ Histogrammes des réponses aux questions à J0 du score de Hamilton
plist <- list()

for (i in 1:16) {
  col_ham_J0_i <- paste0("HAMD",i,"_J0")
  plist[[i]] <- ggplot(data=db_large, aes(x=.data[[col_ham_J0_i]]))+
    geom_bar( binwidth=1, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
    theme_classic()+
    theme(plot.title = element_text(size=15)  )+
    scale_x_continuous(limits = c(0, 4))
}
grid.arrange(grobs=plist, ncol=4, nrow=4)

plist2 <- list()
for (i in 1:16) {
  col_ham_J56 <- paste0("HAMD",i,"_J56")
  plist2[[i]] <- ggplot(data=db_large, aes(x=.data[[col_ham_J56]]))+
    geom_bar( binwidth=1, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
    theme_classic()+
    theme(plot.title = element_text(size=15)  )+
    scale_x_continuous(limits = c(0, 4))
}
grid.arrange(grobs=plist2, ncol=4, nrow=4)





#########  Répartition de la moyenne du score de HAMD au cours du temps ##################################
# Moyenne de HS par VISIT
ham$J.VISIT <- as.numeric(sub("J", "", ham$VISIT))

d1 <- aggregate(ham$HS, by=list(ham$VISIT), FUN=mean)
names(d1) <- c("Date", "Mean")

d1$Date <- factor(d1$Date, levels = paste0("J", c(0, 4, 7, 14, 21, 28, 42, 56)))


d2 <- aggregate(ham$HS, by=list(ham$VISIT), FUN=sd)
d3 <- aggregate(ham$HS, by=list(ham$VISIT), FUN=length)
d1$sem <- d2$x/sqrt(d3$x)
pd <- position_dodge(0.1)

p <- ggplot(d1, aes(x=Date, y=Mean, group = 1))+
  geom_errorbar(aes(ymin=Mean-sem, ymax=Mean+sem), colour="black", width=.2, position=pd)+
  geom_line(position = pd)+
  geom_point(position = pd, size=3, shape=21)+
  theme_classic() 
p 


#####  En fagot
p <- ggplot(ham, aes(x=J.VISIT, y=HS, group = NUMERO)) +
  geom_line(alpha = 0.4) + 
  theme_classic() + labs(y = "Score de dépression", x = "Durée de suivie en jours") 
p 

#####  Par groupes
ggplot(ham, aes(x=J.VISIT, y=HS, group = NUMERO, colour = GROUPE>0))+
  geom_smooth(aes(group = GROUPE>0, fill = GROUPE>0))+
  guides(fill=F)+
  theme_classic()+
  scale_color_discrete(name="GROUPE", labels=c("0", "1"))






## Analyse factorielle rotation varimax x facteurs



#### BAZARD
d1 <- d1[order(as.numeric(sub("J", "", d1$Date))), ]