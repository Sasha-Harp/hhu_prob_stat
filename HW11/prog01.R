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

safe_div = function(a, b) {
  if (a == 0 && b == 0) {
    return(0)
  }
  return(a/b)
}

safe_log2_mult = function(a, b) {
  if(a == 0 && b == 0) {
    return(0)
  }
  return(a * log2(b))
}

#1. Schreiben Sie ein Programm, welches als Eingabe nimmt: ein beliebiger Vektor,
#und dessen Entropie berechnet. Nutzen Sie:
#  x < − as.factor(x)
#xlev < − levels(x)

entropy <- function(vec) {
  levs = levels(as.factor(vec))
  probs = c()
  for(l in levs) {
    probs[l] = 0
  }
  for(v in vec) {
    probs[as.character(v)] = probs[as.character(v)] + 1
  }
  sum = 0
  for(l in levs) {
    probs[l] = probs[l] / length(vec)
    sum = sum - safe_log2_mult(probs[l], probs[l])
  }
  print(probs)
  return(sum)
}

#2. Schreiben Sie ein Programm, welches zwei beliebige Vektoren vec1, vec1 nimmt,
#und berechnet die bedingte Entropie H(vec1|vec2).

entropy2 <- function(vec1, vec2) {
  levs1 = levels(as.factor(vec1))
  levs2 = levels(as.factor(vec2))
  probs = c()
  for(l2 in levs2) {
    for(l1 in levs1) {
      probs[paste(l1, l2, sep="_")] = 0
    }
    probs[l2] = 0
  }
  for(n in 1:length(vec1)) {
    probs[paste(as.character(vec1[n]), as.character(vec2[n]), sep="_")] = probs[paste(as.character(vec1[n]), as.character(vec2[n]), sep="_")] + 1
    probs[as.character(vec2[n])] = probs[as.character(vec2[n])] + 1  
  }
  sum = 0
  for(l2 in levs2) {
    for(l1 in levs1) {
      sum = sum - safe_log2_mult(probs[paste(l1, l2, sep="_")], safe_div(probs[paste(l1, l2, sep="_")], probs[l2]))
    }
  }
  return(sum)
}

#3. Wir definieren den Informationsgewinn ¨uber X1 durch X2 wie folgt:
#  IG(X1|X2) = H(X1) − H(X1|X2)
#Dieser Wert sagt uns, wieviel Information wir gewonnen haben. Berechnen Sie,
#f.a. Merkmale X im Restaurantbeispiel, IG(Warten?|X)

info_win <- function(vec1, vec2) {
  return(entropy(vec1) - entropy2(vec1, vec2))
}
#Ich weiß nicht, welches Restaurantbeispiel hier gemeint ist, aber die anwendung wäre ungefähr:
#info_win(c("warten, "nicht warten", ...), c(1, 2, 3, 2, 3, ...))