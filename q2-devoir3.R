library(readr)
library(car)

florida = read_csv("devoir3/florida.csv")

# graphique de Buchanan vs Bush
plot(florida$Bush, florida$Buchanan,
     xlab = "Votes Bush", ylab = "Votes Buchanan", main = "Votes : Buchanan vs Bush")

# modèle de régression
modele = lm(Buchanan ~ Bush, data = florida)
abline(modele, col = 'red', lwd =2)

# test de l'observation aberrante (H0 : la donnée n'est pas aberrante, HA : la donnée est aberrante)
outlierTest(modele)
#    rstudent unadjusted p-value Bonferroni p
# 50 24.08014         8.6246e-34   5.7785e-32

# La valeur-p de Bonferonni du test effectué est largement inférieure à 0.05, alors on rejette l'hypothèse nulle et
# on confirme à 95% que la 50e donnée (PALM BEACH) est une valeure aberrante.



# On peut aussi lister et afficher les ri pour voir les valeurs qui sont en dehors de [-2,2] et
# possiblement les autres valeurs aberrantes, puisque les plus grands ri sont suceptibles d'être des valeurs aberrantes
ri = rstudent(modele)

# graphique des ri
plot(ri, xlab = "Index", ylab = "Résidus studentisés",
     main = "Graphique des Résidus studentisés")

# afficher les deux premiers ri
(sort(abs(ri), decreasing = TRUE))[1:2]
#    50        13 
# 24.080144  3.280922 

# la 13e donnée a le deuxième résidu standarisé le plus élevé, alors le compté de DADE a une valeur inhabituelle, qu'il faut tester
# pour vérifier si elle est peut être considérée comme une valeur aberrante.


# test des valeurs aberrantes pour les deux premières observations
# (H0 : la donnée n'est pas aberrante, HA : la donnée est aberrante)
outlierTest(modele, n.max = 2, cutoff = Inf)
# rstudent unadjusted p-value Bonferroni p
# 50 24.080144         8.6246e-34   5.7785e-32
# 13 -3.280922         1.6772e-03   1.1237e-01

# La valeur-p de Bonferonni du test effectué pour le comté DADE est 0.112 > 0.05, alors on ne rejette pas l'hypothèse nulle et
# on confirme à 95% que le compté DADE n'est pas une observation aberrante significative.



# On pourrait envisager d'effectuer une transformation logarithme à cause de la grande échelle des données.
# transformations log
florida$Bush_log = log(florida$Bush)
florida$Buchanan_log = log(florida$Buchanan)

# graphique de Buchanan_log vs Bush_log
plot(florida$Bush_log, florida$Buchanan_log,
     xlab = "Votes Bush_log", ylab = "Votes Buchanan_log", main = "Votes : Buchanan_log vs Bush_log")

# la transformation des données à l'échelle logarithmique a un effet de réduction de l'échelle. Ce changement a permis
# de supprimer l'effet de "valeurs aberrantes sur le graphique de "points isolés" qu'on avait sur le graphique sans transformation.
# On observe aussi une relation presque linéaire entre les deux variables à l'échelle logarithmique.

# vérifier si le compté de PALM BEACH est une observation aberrante
modele_log = lm(Buchanan_log ~ Bush_log, data = florida)
abline(modele_log, col = 'orange', lwd =2)
outlierTest(modele_log)

# rstudent unadjusted p-value Bonferroni p
# 50 4.066282         0.00013325    0.0089278

# H0 : la donnée n'est pas aberrante / HA : la donnée est aberrante.
# La valeur-p de Bonferonni du test effectué est 0.0089 < 0.05, alors on rejette l'hypothèse nulle et
# on confirme à 95% que la 50e donnée (PALM BEACH) est toujours une valeure aberrante après la transformation des données.

# On pourrait conclure en disant que la transformation des données a permis de réduire l'effet de la valeur aberrante sur le graphique, mais pas statistiquement.






























