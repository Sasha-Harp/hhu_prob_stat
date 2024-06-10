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

#Sie haben aus Restbest¨anden 10000 Coronatests bekommen, herzlichen Gl¨uckwunsch!
#  Tats¨achlich sind die aber bereits ¨alter. Sie m¨ochten zu 95% sicher sein, dass die (Typ
#                                                                                        II) Fehlerquote der Tests unter 1/50 liegt. Dazu nehmen Sie einen Coronaerkrankten,
#machen mit ihm 1000 Tests, 20 sind negativ. Sind Sie damit zufrieden?

# Epsilon = 1/50; q = 0.95, p = 1 - 0.95 = 0.05; H_0: f=0.02

p = 0.05
f = 0.02
r = sum(dhyper(0:20, 10000 * f, 10000 * (1-f), 1000))
#r = phyper(20, 10000 * f, 10000 * (1-f), 1000)
if (r < p) {
  print("H_0 zurückgewiesen => Wir sind zufrieden")
} else {
  print("H_0 nicht zurückgewiesen => Wir sind nicht zufrieden")
}
