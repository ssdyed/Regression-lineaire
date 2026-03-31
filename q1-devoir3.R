library(readr)
library(car)

# importattion des données
stopping = read_csv("OneDrive - Universite de Montreal/h26/stt2400/devoir3/stopping.csv")

# a. graphique de distance vs vitesse
plot(stopping$Speed, stopping$Distance,
     xlab = "Vitesse (en mph)", ylab = "Distance (en pieds)", main = "Distance vs Vitesse")

# Le graphique nous montre que la relation entre la distance et la vitesse n'est
# pas linéaire. À partir de la vitesse 20, on remarque la distance augmente de
# manière accélérée quand la vitesse augmente ; ce qui suggère un modèle quadratique.


# b. 
# 1. régression quadratique avec variance constante
stopping$vitesse_carre = stopping$Speed^2
modele_quad = lm(Distance ~ Speed + vitesse_carre, data = stopping)
summary(modele_quad)


# 2. graphique des résidus vs valeurs ajustées
stopping$residus = residuals(modele_quad)
stopping$ajustees = fitted.values(modele_quad)
plot(stopping$ajustees, stopping$residus, 
     xlab = "Valeurs ajustées", ylab = "Résidus",
     main = "Résidus vs Valeus ajustées")

# On remarque que les points ne sont pas distribués de manière aléatoire sur le graphique.
# La variance n'est donc pas constante, on ne peut donc pâs se fier aux résultats trouvées
# avec le modele quadratique avec variance constante.

# 3. test de variance non constante :  dépend de la moyenne
ncvTest(modele_quad)

# 4. test de variance non constante :  dépend de la vitesse
ncvTest(modele_quad, ~ Speed)

# 5. test de variance non constante :  dépend de la vitesse et la vitesse au carré
ncvTest(modele_quad, ~ Speed + vitesse_carre)

# Pour chaque test de variance non constance, on a comme hypothèse nulle que la variance est constante.
# Dans chacun des test effectués, on trouve une p-valeur largement petite, alors on rejette automatiquement l'hypothèse nulle.
# Cela confirme la remarque effectuée ci-dessus avec le graphique des résidus qui montre que la variance n'est pas constante.

# Est-ce que l’utilisation de la vitesse^2 aide à expliquer la distance d’arrêt?
# Oui, la vitesse au carré aide à expliquer la distance d'arrêt. Le test-t associé
# à la vitesse carré donne t = 5,033 avec avec p<0.05, ce qui est fortement significatif.


# c. regression quadratique avec pondération
wi = 1 / (stopping$vitesse_carre)
modele_quad_pond = lm(Distance ~ Speed + vitesse_carre, data = stopping, weights = wi)

# comparaison avec le modèle quadratique et le modele quadratique pondéré
summary(modele_quad)$coefficients
summary(modele_quad_pond)$coefficients

# Les estimateurs dans les deux cas changent légèrement. Par contre, on remarque que les écarts 
# types dans le modèle pondéré sont plus petits que ceux dans le modèle normal.
# Cela suggère que le modèle pondéré corrige l'hétéroscédasticité et rend nos estimateurs plus efficaces.




