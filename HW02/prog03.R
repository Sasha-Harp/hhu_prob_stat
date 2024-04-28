#Sortieren Sie den Datensatz ansteigend nach LengthOfTheme. Plotten Sie das
#sortierte array, einmal wie es steht, einmal exp-transformiert.

library(languageR)
verbs = verbs[order(verbs$LengthOfTheme),]
#Die Aufgabe war hier unklar - was muss geplottet werden? Sortiert wurde verbs, das nach typeof eine Liste ist.
#Ich habe angenommen, dass LoT gemeint war, da ich nicht wüsste, wie man den ganzen Datensatz sinnvoll plotten würde.
plot(verbs$LengthOfTheme)
plot(exp(verbs$LengthOfTheme))
