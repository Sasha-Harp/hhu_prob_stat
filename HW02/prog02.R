#Erweitern Sie den dataframe verbs um ein Merkmal ”Cross”, welches den Wert
#”True” annimmt, falls entweder AnimacyOfRec= ”animate” und AnimacyOfTheme=”inanimate”,
#oder umgekehrt AnimacyOfRec= ”inanimate” und AnimacyOfTheme=”animate”.

library(languageR)
#falls es mehr states für AoT und AoR geben würde, wäre es:
#verbs$Cross = (verbs$AnimacyOfTheme=="animate" & verbs$AnimacyOfRec=="inanimate") | (verbs$AnimacyOfTheme=="inanimate" & verbs$AnimacyOfRec=="animate")
verbs$Cross = verbs$AnimacyOfTheme != verbs$AnimacyOfRec
verbs