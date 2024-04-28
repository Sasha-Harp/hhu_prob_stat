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

#Erweitern Sie den dataframe verbs um ein Merkmal ”Cross”, welches den Wert
#”True” annimmt, falls entweder AnimacyOfRec= ”animate” und AnimacyOfTheme=”inanimate”,
#oder umgekehrt AnimacyOfRec= ”inanimate” und AnimacyOfTheme=”animate”.

library(languageR)
#falls es mehr states für AoT und AoR geben würde, wäre es:
#verbs$Cross = (verbs$AnimacyOfTheme=="animate" & verbs$AnimacyOfRec=="inanimate") | (verbs$AnimacyOfTheme=="inanimate" & verbs$AnimacyOfRec=="animate")
verbs$Cross = verbs$AnimacyOfTheme != verbs$AnimacyOfRec
verbs