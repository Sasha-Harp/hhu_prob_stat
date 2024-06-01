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

#Teil 1 Wir nehmen wieder an dass P(U) = P(F) = 0.5, P(K|U) = 0.4, P(K|F) = 0.5.
#Schreiben Sie eine Funktion f in R, die
#• als eine Eingabe eine beliebige Folge (array) ω von K,Z gibt (oder einfacher: 0,1)
#nimmt, und
#• als Ausgabe eine gleichlange Folge von aposteriori-Wahrscheinlichkeiten P(F|ωi)
#liefert, ωi die Folge der ersten i W¨ urfe.
#Wichtig: die Funktion soll linear in der Eingabe rechnen, also die Rechenkomplexit¨at
#soll linear in der Eingabel¨ange wachsen (siehe Skript). Insbesondere nutzen Sie bitte
#nicht die Binomialverteilung!

f = function(w) {
  ret = c()
  for (v in w) {
    if(length(ret) == 0) {
      ret = c(ret, 0.25 / (0.25 + (0.6-0.2*v)*0.5))
    } else {
      ret = c(ret, 
              ret[length(ret)] * 
                0.5 / 
                (0.5 * ret[length(ret)] + (0.6-0.2*v) * (1-ret[length(ret)])))
    }
  }
  return(ret)
}