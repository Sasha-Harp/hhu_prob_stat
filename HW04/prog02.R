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

#2 Bitte bearbeiten bis zum 14.5.2024
#Befehle f¨ur die Binomialverteilung sind dbinom; f¨ur Normalverteilung dnorm.
#1. Plotten Sie den Graphen f¨ur die Binomialverteilung, mit p = 0.5, und n = 200, 2000
#und 20000 (also drei Graphen).
plot(dbinom(0:200,200,0.5))
plot(dbinom(0:2000,2000,0.5))
plot(dbinom(0:20000,20000,0.5))

#2. F¨ur jeden dieser Graphen erzeugen Sie einen Graphen der Normalverteilung,
#der diesen Graphen bestm¨oglich approximiert; Sie m¨ussen also die Parameter µ, σ
#(Erwartungswert, Standardabweichung) entsprechen setzen.
#Tipp: Wenn Sie gut aufgepasst haben, kommen Sie mit ein bisschen Uberlegen/Nachschlagen ¨
#auf die korrekte L¨osung. Sonst m¨ussen Sie ein bisschen rumprobieren!
plot(dnorm(0:200, 0.5*200, sqrt(0.5*(1-0.5)*200)))
plot(dnorm(0:2000, 0.5*2000, sqrt(0.5*(1-0.5)*2000)))
plot(dnorm(0:20000, 0.5*20000, sqrt(0.5*(1-0.5)*20000)))
