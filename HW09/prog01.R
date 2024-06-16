#   LICENSE: zlib/libpng
#
#   Copyright (c) 2024 Dela Kleemeyer & Alexander Andrushchenko
#
#   This software is provided "as-is", without any express or implied warranty. In no event
#   will the authors be held liable for any damages arising from the use of this software.
#
#   Permission is granted to anyone to use this software for any purpose, including commercial
#   applications, and to alter it and redistribute it freely, subject to the following restrictions:
#
#     1. The origin of this software must not be misrepresented; you must not claim that you
#     wrote the original software. If you use this software in a product, an acknowledgment
#     in the product documentation would be appreciated but is not required.
#
#     2. Altered source versions must be plainly marked as such, and must not be misrepresented
#     as being the original software.
#
#     base. This notice may not be removed or altered from any source distribution.

#1. Andern Sie die Sch¨atzfunktion so, dass die Wahrscheinlichkeiten an einem festen Lexikon
#(z.B. array oder hashmap, r2r) abgespeichert werden, das dann von der score-Funktion
#aufgerufen wird.
markov_probs = c()

create_translator = function(korpus) {
  occurences = c()
  k = 0
  translator = c()
  for(s in korpus) {
    chars = c(strsplit(s, "")[[1]], "s")
    for(n in 1:length(chars)) {
      if(!(chars[n] %in% occurences)) {
        occurences = c(occurences, chars[n])
        translator[chars[n]] = k
        k = k + 1
      }
    }
  }
  return(translator)
}

markov_init = function(korpus) {
  translator = create_translator(korpus)
  base = length(translator)
  ret = rep(0, times = base*base)
  ns = rep(0, times = base)
  for(s in korpus) {
    chars = c("s", strsplit(s, "")[[1]], "s")
    for(n in 2:length(chars)) {
      key = translator[chars[n-1]]*base + translator[chars[n]]
      first = translator[chars[n-1]]
      ret[key+1] = ret[key+1] + 1
      ns[first+1] = ns[first+1] + 1
    }
  }
  for(n in 0:(base-1)) {
    for(k in 0:(base-1)) {
      ret[n*base+k+1] = ret[n*base+k+1]/ns[n+1]
    }
  }
  markov_probs <<- ret
}
markov_score = function(sentence) {
  translator = create_translator(c(sentence))
  base = length(translator)
  chars = c('s', strsplit(sentence, "")[[1]], 's')
  prob = 1
  for(n in 2:length(chars)) {
    key = translator[chars[n-1]]*base + translator[chars[n]]
    prob = prob * markov_probs[key + 1]
  }
  return(prob)
}

#2. Modifizieren Sie die Sch¨atz-Funktionen so, dass wir add-one smoothing haben. Testen
#Sie die beiden Varianten (maximum likelihoodL,add-one) auf folgenden Daten:
#  • Korpus (zum Sch¨atzen): {aaba,abbaa,abbca,abcca,baabc}
#• Worte (zum scoren): aacca,abcc,aabaabc
markov_addone_init = function(korpus) {
  translator = create_translator(korpus)
  base = length(translator)
  ret = rep(1, times = base*base)
  ns = rep(base, times = base)
  for(s in korpus) {
    chars = c("s", strsplit(s, "")[[1]], "s")
    for(n in 2:length(chars)) {
      key = translator[chars[n-1]]*base + translator[chars[n]]
      first = translator[chars[n-1]]
      ret[key+1] = ret[key+1] + 1
      ns[first+1] = ns[first+1] + 1
    }
  }
  for(n in 0:(base-1)) {
    for(k in 0:(base-1)) {
      ret[n*base+k+1] = ret[n*base+k+1]/ns[n+1]
    }
  }
  markov_probs <<- ret
}

korpus = c("aaba", "abbaa", "abbca", "abcca", "baabc")
score_words = c("aacca", "abcc", "aabaabc")
markov_addone_init(korpus)
print("add one:")
for(word in score_words) {
  print(markov_score(word))
}
markov_init(korpus)
print("not add one:")
for(word in score_words) {
  print(markov_score(word))
}

#3
#Im Schwellentest m¨ ussen wir H0, H1 komplett ausbuchstabieren. Wir sagen: H0 ist: die
#Wahrscheinlichkeiten der einzelnen Buchstaben sind unabh¨angig, H1 lautet: sie bilden
#eine Markov Kette erster Ordnung (mit add-one smoothing). Dann berechnen wir das
#likelihood-Verh¨altnis

schwellentest = function(lex) {
  translator = create_translator(c(lex))
  base = length(translator)
  probs = rep(0, times = base-1)
  chars = strsplit(lex, "")[[1]]
  for(n in 1:length(chars)) {
    probs[translator[chars[n]]+1] = probs[translator[chars[n]]+1] + 1
  }
  for(n in 1:length(probs)) {
    probs[n] = probs[n]/length(chars)
  }
  p_0 = 1
  for(char in chars) {
    p_0 = p_0 * probs[translator[char]+1]
  }
  markov_addone_init(c(lex))
  p_1 = markov_score(lex)
  return(p_0/p_1)
}
schwellentest("abababaababab") #H_0
schwellentest("abbaabbaabbaabbaabbaababb") #H_0
