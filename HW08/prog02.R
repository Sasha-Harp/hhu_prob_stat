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
#     3. This notice may not be removed or altered from any source distribution.

#1. Implementieren Sie eine Funktion, die f¨ur ein array von Worten (Eingabe-Variable!)
#die Markov-Wahrscheinlichkeiten (erster Ordnung) f¨ur diese Sprache nach MaximumLikelihood sch¨atzt.
markov_init = function(korpus) {
  translator = c(
    'a'=0,
    'b'=1,
    's'=2
  )
  ret = c(0,0,0,0,0,0,0,0,0)
  ns = c(0,0,0)
  for(s in korpus) {
    chars = c("s", strsplit(s, "")[[1]], "s")
    for(n in 2:length(chars)) {
      #im Dreiersystem
      key = translator[chars[n-1]]*3 + translator[chars[n]]
      first = translator[chars[n-1]]
      ret[key+1] = ret[key+1] + 1
      ns[first+1] = ns[first+1] + 1
    }
  }
  for(n in 0:2) {
    for(k in 0:2) {
      ret[n*3+k+1] = ret[n*3+k+1]/ns[n+1]
    }
  }
  return(ret)
}

#2. Implementieren Sie die Funktion score, die die Wahrscheinlichkeit eines beliebigen
#Eingabewortes nach diesen Wahrscheinlichkeiten berechnet.
markov_score = function(probs, sentence) {
  translator = c(
    'a'=0,
    'b'=1,
    's'=2
  )
  chars = c('s', strsplit(sentence, "")[[1]], 's')
  prod = 1
  for(n in 2:length(chars)) {
    key = translator[chars[n-1]]*3 + translator[chars[n]]
    prod = prod * probs[key + 1]
  }
  return(prod)
}

#3. Nehmen Sie folgendes Korpus: T = {aba, ababb, aabba, ababab, abbabab}. Sch¨atzen
#Sie die Wahrscheinlichkeiten nach Maximum Likelihood f¨ur eine Markov Kette
#erster Ordung.
probs = markov_init(c("aba", "ababb", "aabba", "ababab", "abbabab"))
print(probs)

#4. Scoren Sie mittels der gesch¨atzten Wahrscheinlichkeiten die Worte abba, aabbaa,
#aaabbbaaa, ababab.
print(markov_score(probs, "abba"))
print(markov_score(probs, "aabbaa"))
print(markov_score(probs, "aaabbbaaa"))
print(markov_score(probs, "ababab"))