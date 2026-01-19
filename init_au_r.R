################
#  Exercice 1  #
################
notes = c(12,8,15,10,9,14,7,11,16,13,5,18,10,12,9,14,6,17)

#la longueur
length(notes)

#5 premiers valeurs
head(notes, 5)

#moyenne
mean(notes)  # 11.44444

#mediane
median(notes)  # 11.5

#variance
var(notes)  #14.26144

#ecart type
sd(notes) # 3.77

#quantiles
quantile(notes,0.25)  # 9
quantile(notes,0.5)  # 11.5
quantile(notes,0.75)  # 14

#proportion des notes >=10
prop_10 <- mean(notes >= 10)
prop_10  # 0.6666667

#proportion des notes >=14
prop_14 <- mean(notes >= 14)
prop_14  # 0.3333333

#histogramme
hist(notes,
     main = "Histogramme des notes",
     xlab= "Notes",
     col = "lightblue",
     breaks = 10)
#boxplot
boxplot(notes,
        main = "Boîte à moustaches des notes",
        col = "lightgreen",
        horizontal = TRUE)

################
#  Exercice 2  #
################

#creation du data frame
etudiants <- data.frame(
  nom = c("Alice", "Bob", "Claire", "David", "Emma"),
  age = c(20, 22, 21, 23, 20),
  note = c(15.5, 12.0, 16.5, 14.0, 17.5)
)

# Affichage du data frame
print(etudiants) #print in the terminal
View(etudiants) #see the table in another file

#moyenne et écart-type de la variable note
# Moyenne des notes
moyenne_note <- mean(etudiants$note)
moyenne_note  # 15.1

# Écart-type des notes
ecart_type_note <- sd(etudiants$note)
ecart_type_note  # 2.162175

# Note du troisième étudiant
note_troisieme <- etudiants$note[3]
note_troisieme  #

################
#  Exercice 3  #
################

#creation du data frame
classe <- data.frame(
  groupe = c('A','A','A','B','B','B','B','A'),
  note = c(12, 9, 15, 8, 11, 14, 10, 16),
  revise = c(3.5, 1.0, 4.0, 1.5, 2.0, 3.0, 2.0, 4.5)
)

View(classe)

#structure de classe
str(classe)

#résumé statistique
summary(classe)

#l’élément de la 2e ligne et 3e colonne.
tt_element <-classe[2,3]
tt_element  # 1.0

#Extraire la colonne note de deux façons différentes.
notes1 <- classe$note
notes1

notes2 <- classe[, "note"]  # ou classe[, 2] si on prend l’indice de colonne
notes2

#dataframe avec les etudiants du froupe A
dataA = classe[classe$groupe == "A",]
View(dataA)
print(dataA)

#Ajouter une variable booleenne admis (TRUE si note ≥ 10, FALSE sinon)
classe$admis <- classe$note >= 10

#Calculer la moyenne des notes du groupe A et du groupe B (utiliser subset ou [ ,]).
# Moyenne des notes du groupe A
moyenne_A <- mean(subset(classe, groupe == "A")$note)
moyenne_A # 13

# Moyenne des notes du groupe B
moyenne_B <- mean(subset(classe, groupe == "B")$note)
moyenne_B # 10.75

#Trier les lignes par ordre décroissant de note (fonction order) et afficher les 3 meilleurs
# Trier Classe par note décroissante
classe_triee <- classe[order(classe$note, decreasing = TRUE), ]
classe_triee[1:3, ]

################
#  Exercice 4  #
################

#creattion du data frame
sexe = c(rep('F',14), rep('H',16))
abo =c('Mensuel','Annuel','Mensuel','Mensuel','Annuel','Mensuel','Annuel','Mensuel','Mensuel',
'Annuel','Mensuel','Mensuel','Annuel','Mensuel','Annuel','Mensuel','Mensuel','Annuel','Mensuel',
'Mensuel','Annuel','Mensuel','Mensuel','Mensuel','Annuel','Mensuel','Annuel','Mensuel','Mensuel','Annuel')
sport = data.frame(sexe, abo)
View(sport)
str(sport)

# Conversion en facteurs
sport$sexe <- as.factor(sport$sexe)
sport$abo  <- as.factor(sport$abo)
str(sport)

# Tableau de contingence
table_sport <- table(sport$sexe, sport$abo)
print(table_sport)

# Tableau des fréquences (proportion)
freq <- prop.table(table_sport) * 100
print(freq)  #La majorité des personnes ont une abonnement mensuel

# Diagramme en barres empilées
barplot(table_sport,
        main = "Répartition des abonnements par sexe",
        xlab = "Sexe",
        ylab = "Nombre de personnes",
        col = c("skyblue", "salmon"),
        legend = TRUE)

# Test du chi-deux d'indépendance
chi2 <- chisq.test(table_sport)
chi2
#H0 (hypothèse nulle) : sexe et type d’abonnement sont indépendants.
#H1 (hypothèse alternative) : sexe et type d’abonnement sont associés.
#Ici, p-value = 1 > 0.05
#on ne rejette pas H0, donc il n’y a pas de preuve significative que le sexe influence le type d’abonnement.

################
#  Exercice 5  #
################

# charger le jeu de données mtcars
data(mtcars)
View(mtcars)

#résumé statistique
summary(mtcars)

#mediane de mpg
median(mtcars$mpg)  # 19.2

#voiture avec plus des cheveaux
voit_max_cheveaux = mtcars[which.max(mtcars$hp), ]
voit_max_cheveaux

#coefficient de corrélation entre mpg et hp 
covariance = cor(mtcars$mpg, mtcars$hp)  #-0.776 forte corrrelation négative
covariance #Relation forte et significative, lus la puissance augmente, plus la consommation augmente (mpg diminue)

#Créer voitures_economiques : Sélectionner les colonnes mpg, cyl et hp
voitures_economiques = mtcars[ , c("mpg", "cyl", "hp")]
head(voitures_economiques, 5)
                
#les voitures avec 4 cylindres 
voitures_4cyl <- sum(voitures_economiques$cyl == 4)
voitures_4cyl

################
#  Exercice 6  #
################

# charger le jeu de données iris
data(iris)
View(iris)

#dimension d'iris
dim(iris)
head(iris)
View(iris)
#data frame iris setosa
iris_setosa = subset(iris,Species == "setosa")
head(iris_setosa)
dim(iris_setosa)

#moyenne de Sepal.Length pour chaque espèce (aggregate ou tapply)
avg__length_chaque_esp = tapply(iris$Sepal.Length, iris$Species, mean)
avg__length_chaque_esp
# aggregate(Sepal.Length ~ Species, data = iris.mean)

#plot de Sepal.Length vs Petal.Length par espèce d'iris
# Définir des couleurs pour chaque espèce
colors <- c("setosa" = "red", "versicolor" = "green", "virginica" = "blue")

# Tracer le nuage de points
plot(iris$Petal.Length, iris$Sepal.Length,
     col = colors[iris$Species],   # couleur selon l'espèce
     pch = 19,                     # points pleins
     xlab = "Petal Length",
     ylab = "Sepal Length",
     main = "Sepal.Length vs Petal.Length par espèce")

# Ajouter une légende
legend("topleft", legend = levels(iris$Species), col = colors, pch = 19)

#la corrélation entre Sepal.Length et Petal.Length
cor_all <- cor(iris$Sepal.Length, iris$Petal.Length)
cor_all


# Corrélation uniquement pour l'espèce versicolor
iris_versicolor <- subset(iris, Species == "versicolor")
cor_versicolor <- cor(iris_versicolor$Sepal.Length, iris_versicolor$Petal.Length)
cor_versicolor

################
#  Exercice 7  #
################

#repetoire actuel du travail
getwd()

#modifier le repetoire
setwd("~/Bureau/L3/S6/Analyse_de_donnees/TD1")

# Lire le fichier mesures.txt
mesures <- read.table("mesures.txt", header = TRUE)
View(mesures)
str(mesures)

#change the class of variable taile to numeric
mesures$taille = as.numeric(mesures$taille)

#creer un vecteur imc
imc = mesures$poids / (mesures$taille /100)^2
imc
#ajouter imc au mesures
mesures$imc = imc

#exporter le data frame
write.csv(mesures, "mesures_out.csv", row.names = FALSE)


################
#  Exercice 8  #
################

x = c(10, 12, NA, 9, 15, NA, 11)
y = c(3.2, NA, 4.1, 2.9, 3.8, 3.5, NA)
d = data.frame(x, y)
View(d)

# Compter les valeurs manquantes par colonne
colSums(is.na(d))

# Moyenne de x en ignorant les NA
mean_x <- mean(d$x, na.rm = TRUE)
mean_x  # 11.4

# Supprimer les lignes contenant au moins un NA
d2 <- na.omit(d)
d2

# Copier d pour ne pas modifier l'original
d_imputed <- d
# Remplacer les NA dans x par la moyenne
d_imputed$x[is.na(d_imputed$x)] <- mean_x
d_imputed

mean(d_imputed$x)   # Moyenne
sd(d_imputed$x)     # Écart-type

x_orig <- d$x
x_imputed <- d_imputed$x

# Mettre deux graphiques côte à côte
par(mfrow = c(1, 2))  # 1 ligne, 2 colonnes

plot(x_orig, d$y,
     main = "y en fonction de x (avec NA)",
     xlab = "x",
     ylab = "y",
     pch = 19,
     col = "red")

plot(x_imputed, d$y,
     main = "y en fonction de x (x NA remplacés par la moyenne)",
     xlab = "x",
     ylab = "y",
     pch = 19,
     col = "blue")


# Réinitialiser la disposition des plots pour la suite
par(mfrow = c(1, 1))

