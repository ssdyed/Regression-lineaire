library(readr)
library(car)

# importattion des données
stopping = read_csv("devoir3/stopping.csv")

# a. graphique de distance vs vitesse
plot(stopping$Speed, stopping$Distance,
     xlab = "Vitesse (en mph)", ylab = "Distan
     ce (en pieds)", main = "Distance vs Vitesse")

# Le graphique nous montre que la relation entre la distance et la vitesse n'est
# pas linéaire. À partir d'une certaine vitesse, on remarque la distance augmente de
# manière accélérée quand la vitesse augmente ; ce qui suggère un modèle quadratique.

# regression lineaire
modele_reg = lm(Distance ~ Speed, data = stopping)
residualPlot(modele_reg)
# Le graphique des résidus montre un patron en U, ce qui confirme la non-linéarité et la conclusion de la question (a).


# b. 
# 1. régression quadratique avec variance constante
stopping$vitesse_carre = stopping$Speed^2
modele_quad = lm(Distance ~ Speed + vitesse_carre, data = stopping)
summary(modele_quad)
residualPlot(modele_quad)

# On remarque que les points ne sont pas distribués de manière aléatoire sur le graphique et que l'évolution des 
# points forme un entonnoir. La variance n'est donc pas constante et on ne peut donc pas se fier aux résultats fournis
# par le modele quadratique avec variance constante.

# 3. test de variance non constante :  dépend de la moyenne
ncvTest(modele_quad)

# 4. test de variance non constante :  dépend de la vitesse
ncvTest(modele_quad, ~ Speed)

# 5. test de variance non constante :  dépend de la vitesse et la vitesse au carré
ncvTest(modele_quad, ~ Speed + vitesse_carre)

# Pour chacun des tests de variance non constance, on a comme hypothèse nulle que la variance est constante et l'alternative est qu'elle n'est pas constante.
# Dans chaque test effectué, on trouve une p-valeur largement inférieure à 0.05, alors on rejette automatiquement l'hypothèse nulle.
# Cela confirme la remarque effectuée ci-dessus avec le graphique des résidus qui montre que la variance n'est pas constante.

# Est-ce que l’utilisation de la vitesse^2 aide à expliquer la distance d’arrêt ?
# Oui, la vitesse au carré aide à expliquer la distance d'arrêt. Le test-t associé
# à la vitesse carré donne t = 5,033 avec avec une valeur-p de (4.83e-06)<0.05, ce qui est fortement significatif.


# c. regression quadratique avec pondération
wi = 1 / (stopping$Speed)
modele_quad_pond = lm(Distance ~ Speed + vitesse_carre, data = stopping, weights = wi)
residualPlot(modele_quad_pond)

# comparaison des deux modèles avec le graphique des résidus studentisés
par(mfrow = c(2,2))
residualPlot(modele_quad) ; residualPlot(modele_quad_pond)
par(mfrow = c(1,1))

# Les résidus sont plus uniformément dispersés autour de zéro dans le modèle pondéré. 
# Cela annule l'effet d'entonnoir etrend le modèle avec pondération plus efficace.

# comparaison avec le modèle quadratique et le modele quadratique pondéré
summary(modele_quad)$coefficients
summary(modele_quad_pond)$coefficients

# Les estimateurs dans les deux cas changent légèrement. Par contre, on remarque que les écarts 
# types dans le modèle pondéré sont plus petits que ceux dans le modèle normal.
# Cela suggère que le modèle pondéré corrige l'hétéroscédasticité et rend nos estimateurs plus efficaces.




