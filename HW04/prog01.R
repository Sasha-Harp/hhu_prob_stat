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

#1 Bitte bearbeiten bis zum 14.5.2024
#Wir gehen ins Casino, und nehmen ein Beispiel aus der Sitzung: Roulette mit Farben
#und Ziffern.
#Nehmen Sie die folgenden Ereignisse und ordnen Sie sie Verteilungen zu. Dann
#schreiben Sie die Funktion, mit der Sie deren Wahrscheinlichkeit berechnen k¨onnen.
#1. Die Wahrscheinlichkeit, dass Sie bei 50 Spielen k mal gewinnen (von Roulette auf
#                                                                   Zahl, mit den Wahrscheinlichkeiten wie in der letzten Aufgabe).
aufg_1 <- function(k) {
  #Binomialverteilung
  return(dbinom(k, 50, 1/37))
}
plot(aufg_1(0:50))

#2. Die Wahrscheinlichkeit, dass Sie bei 20 Spielen kein einziges Mal gewinnen (von
#                                                                               Roulette auf Zahl, mit den Wahrscheinlichkeiten wie in der letzten Aufgabe).
aufg_2 <- function(obligatorische_variable) {
  #Binomialverteilung
  return(dbinom(0, 20, 1/37))
}
plot(aufg_2(0:50))

#3. Sie spielen Poker. Wie ist die Wahrscheinlichkeit, dass Sie im franz¨osischen Blatt
#(32 Karten) genau 3 Damen bekommen (f¨ur Ihre 5 Karten total)?
aufg_3 <- function(obligatorische_variable) {
  #Hypergeometric Distribution
  return(dhyper(3, 4, 32-4, 5))
}
plot(aufg_3(0:50))

#  4. Wie oft m¨ussten Sie Roulette auf Zahl spielen, bis Sie zu 90% sicher sind, einmal
#zu gewinnen?
#  Bitte schreiben Sie die passende Funktion in R (mit jeweils nur einer Variable, und
#                                                  geben Sie dazu Code welche die Funktion plottet (auf einer vern¨unftigen Skala).
aufg_4 <- function(obligatorische_variable) {
  #Geometric Distribution
  return(qgeom(0.9, 1/37))
}
plot(aufg_4(0:50))
