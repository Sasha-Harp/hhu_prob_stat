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

#Bitte bearbeiten bis zum 6.5.2024
#1. Implementieren Sie eine Funktion Varianz() mit elementarer Arithmetik und Kontrollstrukturen, die f¨ur einen gegeben Zahlenvektor die Varianz berechnet! (sie
# d¨urfen aber benutzen Funktionen wie length(), nrow(), sum()).
#Achtung: R benutzt die Stichprobenvarianz, siehe Skript!
#2. Ebenso mit Standardabweichung!
#3. Ebenso mit Kovarianz. Beachten Sie: Funktion mit 2 Argumenten!
  

varianz = function(arr) {
  if (length(arr) == 0) {
    stop("calculating variance with a zero-length array will lead to division by zero")
  }
  avg = sum(arr)/length(arr)
  return(sum((arr-avg)^2)/length(arr))
}
stichprobenvarianz = function(arr) {
  if (length(arr) < 2) {
    stop("calculating the unbiased sample variance with a zero- or one-length array will lead to division by zero")
  }
  avg = sum(arr)/length(arr)
  return(sum((arr-avg)^2)/(length(arr))-1)
}
stdabweichung = function(arr) {
  return(varianz(arr)^(1/2))
}
covar = function(arr1, arr2) {
  if (length(arr1) != length(arr2)) {
    stop("calculating the covariance requires equal-sized arrays")
  }
  avg1 = sum(arr1)/length(arr1)
  avg2 = sum(arr2)/length(arr2)
  return(sum((arr1-avg1)*(arr2-avg2))/length(arr1))
}