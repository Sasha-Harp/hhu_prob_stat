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

#1. Schreiben Sie ein Programm, welches als Eingabe nimmt: zwei Vektoren vec1, vec2
#gleicher L¨ange mit Werten in {0, 1}, und als Ausgabe liefert: H(vec1|vec2) (die
# Wahrscheinlichkeiten von 0, 1 sind nat¨urlich die relativen H¨aufigkeiten im Vektor,
# genau wie im Datensatz)

P_v1_v2 = function(v1, v2) {
  p21 = sum(v2)/length(v2)
  p20 = 1-p21
  df = data.frame(v1, v2)
  nrows = nrow(df)
  p10_20 = sum(df$v1==0 & df$v2==0) / nrows
  p10_21 = sum(df$v1==0 & df$v2==1) / nrows
  p11_20 = sum(df$v1==1 & df$v2==0) / nrows
  p11_21 = sum(df$v1==1 & df$v2==1) / nrows
  return(-(p10_20*log2(p10_20/p20) 
           + p10_21*log2(p10_21/p21)
           + p11_20*log2(p11_20/p20)
           + p11_21*log2(p11_21/p21)))
}

#2. Stellen Sie sicher, dass das Programm f.a. wohlgeformten Eingaben (zwei gleichlange Vektoren ¨uber 0,1) eine Ausgabee liefert. Insbesondere f¨ur H(~v|~v) taucht
#log(0) auf; bei c(0, 0, 0) kann x/0 auftauchen.
#Die Regel ist: 0/0 := 0, und 0 · log(0) := 0. Bauen Sie Kontrollstrukturen die sich
#darum k¨ummern!

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

P_v1_v2_safe = function(v1, v2) {
  if (length(v1) != length(v2)) {
    stop("array lengths have to be equal")
  }
  p21 = sum(v2)/length(v2)
  p20 = 1-p21
  df = data.frame(v1, v2)
  nrows = nrow(df)
  p10_20 = sum(df$v1==0 & df$v2==0) / nrows
  p10_21 = sum(df$v1==0 & df$v2==1) / nrows
  p11_20 = sum(df$v1==1 & df$v2==0) / nrows
  p11_21 = sum(df$v1==1 & df$v2==1) / nrows
  return(-(safe_log2_mult(p10_20, safe_div(p10_20, p20))
           + safe_log2_mult(p10_21, safe_div(p10_21, p21))
           + safe_log2_mult(p11_20, safe_div(p11_20, p20))
           + safe_log2_mult(p11_21, safe_div(p11_21, p21))))
}
