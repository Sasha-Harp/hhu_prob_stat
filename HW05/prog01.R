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

#Hilfsfunktionen:
sum_list = function(n) {
    vals = dbinom(0:n, n, 0.5)
    avg = mean(1:(n+1))
    sums = 1:(avg-1)
    for (x in 1:(avg-1)) {sums[x] = sum(vals[(avg-x):(avg+x)])}
    return(sums)
}

#Teil 1 Bitte bearbeiten bis zum 21.5.24
#Berechnen Sie, bei welchen Ergebnissen die Vertrauensgrenzen f¨ur die folgenden Binomialverteilungen liegen (bitte mit Code):
#a. n = 50, p = 0.5, Vertrauenskonstante c = 0.95
l = sum_list(50)
for(v in 1:length(l)) {
    if (l[v] > 0.95) {
        print(v)
        break
    }
}
#b. n = 200, p = 0.5, Vertrauenskonstante c = 0.99
l = sum_list(200)
for(v in 1:length(l)) {
    if (l[v] > 0.99) {
        print(v)
        break
    }
}
#c. Schreiben Sie ein Programm, dass n und c als Argumente nimmt (p = 0.5 ist gesetzt),
#und Ihnen die (minimale) Abweichung vom Erwartungswert, die ausserhalb der Vertrauensgrenze liegt, als Ausgabe liefert.
min_significant = function(n, c) {
    l = sum_list(n)
    for(v in 1:length(l)) {
        if (l[v] > c) {
            return(v)
        }
    }
}

#d. Implementieren Sie eine Funktion f(n, m), wobei n die Anzahl der M¨unzw¨urfe ist, m
#die Anzahl der W¨urfe von Zahl. Wir nehmen an die M¨unze ist fair, und die Ausgabe
#ist 1, falls das Ergebnis signifikant ist (c = 0.95), 0 andernfalls.
f = function(n, m) {
    avg = mean(0:n)
    if (min_significant(n, 0.95) > abs(avg-m)) {
        return(0)
    }
    return(1)
}


