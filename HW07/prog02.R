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

#Teil 2 Machen Sie dasselbe f¨ ur Szenario 2: es gibt noch eine dritte Verteilung U2, mit
#den bekannten Parametern:
#  P(K|U2) = 0.7, P(Z|U2) = 0.3. Apriori-Wahrscheinlichkeiten sind:
#  P(F) = 0.45, P(U) = 0.45, P(U2) = 0.1
f2 = function(w) {
  ret = c()
  u_arr = c()
  u2_arr = c()
  for (v in w) {
    if(length(ret) == 0) {
      marg = 0.5*0.45 + (0.6-0.2*v)*0.45 + (0.3+0.4*v)*0.1
      ret = c(ret, 0.5*0.45 / marg)
      u_arr = c(ret, 0.4*0.45 / marg)
      u2_arr = c(ret, 0.7*0.1 / marg)
    } else {
      marg = 0.5 * ret[length(ret)] + (0.6-0.2*v) * u_arr[length(ret)] + (0.3+0.4*v)*u2_arr[length(ret)]
      ret = c(ret, 
              ret[length(ret)] * 0.5 / marg)
      u_arr = c(u_arr, 
                u_arr[length(u_arr)] * (0.6-0.2*v) / marg)
      u2_arr = c(u2_arr, 
                 u2_arr[length(u2_arr)] * (0.3+0.4*v) / marg)
    }
  }
  return(ret)
}