Budget = c(40,52,60,45,70,55,62,48,66,58,72,50)
CA = c(130,134,138,132,142,136,139,133,141,137,144,135)

ecom = matrix(c(Budget,CA), nrow =12, ncol=2, byrow =F)
ecom = as.data.frame(ecom)
colnames(ecom)[1] = "Budget"
colnames(ecom)[2] = "CA"
ecom

#################################
#         PARTIE 1 
#################################

#Question 1 
moy_bud = mean(ecom[,1])  # = 56.5
moy_ca = mean(ecom[,2])  # = 136.75

var_bud=var(ecom[,1])   # = 99.90
var_ca=var(ecom[,2])   # = 18.02

ecart_type_bud=sqrt(var_bud)   # = 9.99
ecart_type_ca=sqrt(var_ca)   # = 4.24

moy_bud
moy_ca
var_bud
var_ca
ecart_type_bud
ecart_type_ca

#Question 2
med_bud= median(ecom[,1]) # = 56.5
Q1_bud=quantile(ecom[,1],0.25) # = 49.5
Q3_bud=quantile(ecom[,1],0.75) # = 63

med_ca = median(ecom[,2]) #  = 136.5
Q1_ca=quantile(ecom[,2],0.25) #   = 133.75
Q3_ca=quantile(ecom[,2],0.75) #   = 139.5

med_bud
med_ca
Q1_bud
Q1_ca
Q3_bud
Q3_ca

#Question 3
#Budget
boxplot(ecom$Budget,
        main = "Boxplot of the amount spent",
        ylab = "Budget")

#CA
boxplot(ecom$CA,
        main = "Boxplot of the Chiffre d'affaires gotten",
        ylab = "Chiffre d'affaires")
par(mfrow = c(1, 2))

#La ligne noir est la moyen
#La partie en bas est le premier quatile
#La partie en haut est la troisième quartile

#################################
#         PARTIE 2 
#################################

#############################################
#         Etude de la Variable "CA" 
#############################################

#Question 4
#P(X>=143) = 1-P(X<=143)
mu = 135
sigma = sqrt(16)

# Probabilité que X > 143
prob = 1 - pnorm(143, mean = mu, sd = sigma)
prob  #0.022

#Question 5
#P(128<= X <= 132)
prob_interval = pnorm(132, mean = mu, sd = sigma) - pnorm(128, mean = mu, sd = sigma)
prob_interval  #0.186


#Question 6
#H0 : (les performances de shopNow ont la meme moyen que les sites de e-commerce)
#H1 : (les performances de shopNow sont superiures en moyenne)
#Comme la variance de la population est connue, on utilise la loi normale
n = 12
xbar = mean(ecom$CA)
Z = (xbar - mu) / (sigma / sqrt(n))
Z #1.51

#valeur critique alpha= 1%
z_crit = qnorm(0.99)  # = 2.326
z_crit

#Z est Inferiuer a z_crit, on ne rejette pas H0

#############################################
#         Etude de la Variable "Budget" 
#############################################

#Question 7
mu_chapeau = mean(ecom$Budget)
mu_chapeau  #56.5

sigma_chapeau = sd(ecom$Budget)
sigma_chapeau  #9.99

#Question 8
n = length(ecom$Budget)
xbar = mean(ecom$Budget)
s = sd(ecom$Budget)

alpha = 0.05
t_val = qt(1 - alpha/2, df = n-1)

IC_lower = xbar - t_val * s/sqrt(n)
IC_upper = xbar + t_val * s/sqrt(n)

c(IC_lower, IC_upper) #[50.14 , 62.85]

#Question 9
#Comme la varianc n'est pas connu et ecart type <30 on vas utilisée la loi de student avec n-1 degré de liberté
#H0 : μ=60 (le budget est compatible)
#H1 : μ !=60 (le budget n'est pas compatible)
n = length(ecom$Budget)
xbar = mean(ecom$Budget)
s = sd(ecom$Budget)
mu0 = 60
alpha = 0.05

t_stat = (xbar - mu0) / (s / sqrt(n))
t_crit = qt(1 - alpha/2, df = n-1)

t_stat
t_crit
summary(t_stat)

#t.test(ecom$Budget, mu=60)

#t_stat < t_crit
#Au risque de 5%, on ne  rejette pas H0
#Le Budget n'est pas compatible avec la recommandation
#En pratique : Elle est plus elevée que la moyen



#################################
#         PARTIE 3 
#################################
#Question 10
#Variable dépendante : Chiffre d'affaires
#Variable explicative : Budget
#Le chiffre d'affaires est ce qu'on essaye de predire d'après un Budget publicitaire

#Question 11
plot(ecom$Budget, ecom$CA,
     main = "Diagramme de dispersion : Chiffre d'affaire vs Budget",
     xlab = "Budget",
     ylab = "CA obtenus",
     pch = 19, col = "blue")
abline(lm(CA ~ Budget, data=ecom), col="red", lwd=2)

#En regardant le scatter plot, on peut vérifier si les points suggèrent une relation linéaire.
#dans ce cas, les points semblent légèrement croissants : plus de budget sont associées à un CA légèrement plus élevé
#Cela justifie l’utilisation d’une régression linéaire simple.

#Question 12
r = cor(ecom$Budget, ecom$CA)
r
#test de correlation
cor_test = cor.test(ecom$Budget, ecom$CA, method="pearson", conf.level = 0.95)
cor_test$estimate     # coefficient de corrélation r
cor_test$p.value      # p-value
cor_test$conf.int     # intervalle de confiance à 95% 


#Test de correlation : 
#H0= « cor = 0 » (aucune corrélation entre Budget et CA)
#H1: « cor != 0 » (il existe une corrélation)

# p_value = 1.318773e-10 < 0.05, donc h0 est rejetté. 

#Intervalle de confiance :
#[0.9743442;0.99809704]

#Conclusion: la correlation est 0.99 qui nous dit la correlation est fort probable car elle est proche de 1

#Question 13
# Ajustement du modèle
modele = lm(CA ~ Budget, data = ecom)

# Affichage du résumé complet
summary(modele)

#Question 14
coef(modele)       # beta0 et beta1
# Equation de la droite : 112.921292 + 0.421747.Budget


plot(ecom$Budget, ecom$CA,
     main="CA vs Budget avec droite de régression",
     xlab="Budget",
     ylab="CA obtenus",
     pch=19, col="blue")
abline(modele, col="red", lwd=2)

#Question 15
# D'après le summary(modele), l'écart type résiduel est de :
# 0.5263312
sigma_resid = summary(modele)$sigma
sigma_resid

#Question 16
summary(modele)$r.squared
# D'après le summary(modele), R² vaut :
# 0.9860265
# C'est une valeur très proche de 1, signifiant que le modèle semble bien adapté.

#environ 98% de la variance des chiffre d'affaire est expliquée par le budget de publication
#Le reste (2%) est dû à d’autres facteurs non inclus dans le modèle 
#Donc, relation modérée, mais le modèle capte une partie de l’effet de la budget sur les CA.


#Question 17
# Nouvelle valeur de X
nouveau_X = data.frame(Budget = 76)

# Prédiction ponctuelle
predict(modele, newdata = nouveau_X) #environ 144 
#c'est à dire la prediction pour un Budget de 76k vas nous donner un CA de 144k


# Intervalle de prédiction (variation) pour une nouvelle observation
# Cet intervalle inclut la variabilité des résidus et donc montre où un nouveau point peut se situer
predict(modele, newdata = nouveau_X, interval = "prediction", level = 0.95)
#[143.5;146.3]  en moyen:144.9

# Intervalle de confiance pour la moyenne prédite
# Cet intervalle montre où se situe la valeur moyenne de Y pour X = 76k
predict(modele, newdata = nouveau_X, interval = "confidence", level = 0.95)
#[144.2;145.7] en moyen:144.9

