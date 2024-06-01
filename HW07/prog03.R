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

#functions from previous parts:
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

#Teil 3
#1. Was w¨are die k¨ urzeste Folge von W¨ urfen ω, mittels derer Sie P(F|ω) > 0.95
#bekommen in Szenario 1 (wie in Teil 1)?
#  2. Welches w¨are die k¨ urzeste Folge von W¨urfen ω, mittels derer Sie P(F|ω) > 0.95
#bekommen, aber diesmal in 2?

#es müsste noch gezeigt werden, dass es kein w_(n+1) und w'_(n+1) paar gibt mit w_n = w'_n, w_(n+1) =/= w'_(n+1) und P(F|w'_(n+1)) < P(F|w'_n), 
#sodass P(F|w'_(n+k)) > P(F|w_(n+k)), was aber mit den formeln relativ einfach sein sollte, ich hier aber nicht machen werde.
shortest_f_95 = function() {
  res = 0
  prev = c()
  count = 0
  while (res <= 0.95) {
    count = count + 1
    z = f(c(prev, 0))
    o = f(c(prev, 1))
    res = max(z[length(z)], o[length(o)])
    if (z[length(z)] > o[length(o)]) {
      prev = c(prev, 0)
    } else {
      prev = c(prev, 1)
    }
  }
  return(prev)
}
shortest_f2_95 = function() {
  res = 0
  prev = c()
  count = 0
  while (res <= 0.95) {
    count = count + 1
    z = f2(c(prev, 0))
    o = f2(c(prev, 1))
    res = max(z[length(z)], o[length(o)])
    if (z[length(z)] > o[length(o)]) {
      prev = c(prev, 0)
    } else {
      prev = c(prev, 1)
    }
  }
  return(prev)
}