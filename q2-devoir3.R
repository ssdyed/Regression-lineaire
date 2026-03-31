library(readr)
library(car)

florida = read_csv("OneDrive - Universite de Montreal/h26/stt2400/devoir3/florida.csv")

# graphique de Buchanan vs Bush
plot(florida$Bush, florida$Buchanan,
     xlab = "Votes Bush", ylab = "Votes Buchanan", main = "Votes : Buchanan vs Bush")

# modèle de régression
modele = lm(Buchanan ~ Bush, data = florida)
abline(modele, col = 'red', lwd =2)

# test de l'observation aberrante (H0 : la donnée n'est pas aberrante, HA : la donnée est aberrante)
outlierTest(modele)
# La valeur-p du test effectué est largement inférieure à 0.05, alors on rejette l'hypothèse nulle et
# on confirme à 95% que la 50e donnée (PALM BEACH) est une valeure aberrante.


# On peut aussi lister et afficher les ri pour voir les valeurs qui sont en dehors de [-2,2] et
# possiblement les autres valeurs aberrantes, puisque les plus grands ri sont suceptibles d'être des valeurs aberrantes
ri = rstudent(modele)
print(ri)

# afficher les deux premiers ri
(sort(abs(ri), decreasing = TRUE))[1:2]
# la 13e donnée a le deuxième résidu standarisé le plus élevé, alors le compté de DADE a une valeur inhabituelle

# graphique des ri
plot(ri, xlab = "Index", ylab = "Résidus studentisés",
     main = "Graphique des Résidus studentisés")

# test des deux premières observations aberrantes
outlierTest(modele, n.max = 2, cutoff = Inf)
# La valeur-p de Bonferonni du test effectué pour le comté DADE est 0.112 > 0.05, alors on ne rejette pas l'hypothèse nulle et
# on confirme à 95% que la DADE n'est pas une observation aberrante significative.




















