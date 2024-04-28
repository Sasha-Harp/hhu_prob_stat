#Erstellen Sie einen dataframe, der alle Zeilen von verbs enth¨alt, in denen das Verb
#weniger als 5 Buchstaben hat. Hier brauchen Sie einige zus¨atzliche Befehle, die wir
#nicht besprochen haben: nchar() gibt die L¨ange eines Wortes; allerdings m¨ussen
#Sie zun¨achst die levels zu strings transformieren (as.character()).

library(languageR)
#ich kann mich nicht dazu bringen, 'weniger als' mit 'höchstens' gleichzustellen:
verbs[nchar(as.character(verbs$Verb))<5,]
