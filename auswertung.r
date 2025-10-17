library(tidyverse)
library(ggplot2)

# Wir lesen die Daten ein
quali <- read.csv("Fruchtqualitaet_Gruppe 1.csv", header = TRUE)

# lassen uns eine Zusammenfassung ausgeben
summary(quali)

# Wandeln die Behandlung in einen Faktor um
quali$T.Behandlung <- as.factor(quali$T.Behandlung)

# machen einen ersten Plot
plot(quali$Aepfel_Sre ~ quali$T.Behandlung)


# Da wir nur 2 Behandlungsgruppen haben, können wir einen t-Test machen

## t-Test zum Vergleich zweier Datensätze -
# für einen t-Test zum Vergleich zweier Datensätze müssen die Daten in zwei Spalten nebeneinander  stehen. 
# Um das Format zu ändern nutzen wir die Funktion 'pivot_wider()' und speichern die neue formatieren Daten in einer neuen Variable mit Namen 'quali_w'
quali_w <- quali %>% 
  pivot_wider(
    id_cols = Wdh,
    names_from = T.Behandlung,
    values_from = c(Zucker_Sre, Aepfel_Sre)
   )


# zwei Datensätze mit gleicher Varianz vergleichen
t.test(quali_w$Aepfel_Sre_5, quali_w$Aepfel_Sre_20, var.equal = TRUE)

# Wenn der p-Wert kleiner 0.05 ist, ist die alternative Hypothese, dass die Differenz der
# Mittelwerte der beiden Gruppen nicht 0 ist wahr -> die Gruppen haben einen signifikant
# Unterschied.
# Das Konfidenzintervall bezieht sich auf den Unterschied der Mittelwert

# Testen ob die Varianzen der beiden Gruppen gleich sind (Varianzhomogenität/Homoskedastizität)

# Varianzhomogenitätstest der auf dem F-Test beruht
var.test(quali_w$Aepfel_Sre_5, quali_w$Aepfel_Sre_20)
# wenn der p-Wert kleiner als 0.05 ist, sind die Varianzen unterschiedlich

# Sollte das der Fall sein, kann man in der t.test Funktion 'var.equal = F' setzen
# zusätzlich zu den Mittelwerten werden jetzt auch die Varianzen geschätzt, deshalb
# sinkt die Zahl der Freiheitsgrade


##------  Q-Q plots -------
# zum visuellen Testen auf Normalverteilung der Residuen
qqnorm(quali_w$Aepfel_Sre_5)
qqline(quali_w$Aepfel_Sre_5, col = "red", lty = 3)

qqnorm(quali_w$Aepfel_Sre_20)
qqline(quali_w$Aepfel_Sre_20)

# wenn die Punkte annähernd auf der Linie liegen, insbesondere kein besonderes Muster
# aufweisen, sind die Residuen normalverteilt. Häufig gibt es ein paar Ausreißer am linken
# und rechten Ende, die nicht überbewertet werden sollten. Bei nur wenigen Datenpunkten ist die Analyse nicht sehr aussagekräftig.

# Abbildung der Ergebnisse


p <- ggplot(quali, aes(x=T.Behandlung, y= Aepfel_Sre)) + 
  labs(x = "Temperatur [°C]", y = "Säuregehalt [g/L]") +
  geom_boxplot() +   theme_grey()

  p
