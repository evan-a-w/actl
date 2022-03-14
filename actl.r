# r -> i^(n)
nom_to_eff <- function(nom, periods) { (1 + nom / periods)^periods - 1 }
eff_to_nom <- function(i, n) { n * ((1 + i)^(1/n) - 1) }

# 1 / a(1) = 1 - d = (1 - d^(m) / m)^m
nom_to_d <- function(dm, m) { 1 - (1 - dm/m)^m }

# 1 + i = e^del
del_to_i <- function(del) { exp(del) - 1 }
i_to_del <- function(i) { log(1 + i) }

# FORCE OF DISCOUNT
# 1 - d = exp(-d^(inf))
dinf_to_d <- function(dinf) { 1 - exp(-dinf) }
d_to_dinf <- function(d) { -log(1 - d) }

# 1 + i = A(1), 1 - d = (A(1))^-1, 1 / (1 - d) = a(1)
i_to_d <- function(i) { 1 - 1 / (i + 1) }
d_to_i <- function(d) { 1 / (1 - d) - 1 }

# v = 1 / (1 + i)
# v = d / i
i_to_v <- function(i) { 1 / (1 + i) }
v_to_i <- function(v) { 1 / v - 1 }

#  v = 1 / (1 / (1 - d)) = 1 - d
d_to_v <- function(d) { 1 - d } 
v_to_d <- function(v) { 1 - v }

# i = money interest, r = real interest, p(t) price index (p(0) = 1),
# pi. = inflation rate
# a(1) = 1 + i, a(0) = 1
# p(0) = 1, p(1) = 1 + pi.
# a(1) / p(1) = (1 + i) / (1 + pi.) = 1 + r
# r = (i - pi.) / (1 + pi.)
# (for effective rates)
r_from_pi_i <- function(i, pi.) { (i - pi.) / (1 + pi.) }


# YIELD = rate of interest st. NPV is 0 (IRR) given cash flows


# ANNUITY - i is always effective per annum
const_ann <- function(n, i, due=FALSE, p=1, age=NULL, m=0) {
  ip <- eff_to_nom(i, p)
  
  if (n == Inf) {
    if (due) {
      return(1 + 1 / ip)
    } else {
      return(1 / ip)
    }
  }  
  
  v <- 1 / (1 + i)
  v^m * (if (due) {
    dp <- ip / (1 + ip/p)
    (1 + ip/p) * (1 - v^n) / ip
  } else {
    (1 - v^n) / ip
  })
}

Ia <- function(n, i, p=1, P=1, Q=1, j=NULL, due=FALSE) {
  if (due) {
    return((1+i)*Ia(n, i, p=p, P=P, Q=Q, j=j))
  }
  s1p <- (1+i)*const_ann(n=1, i=i, p=p)
    
  # Geom
  if (!is.null(j)) {
    return(s1p*(1 - ((1+j)/(1+i))^n)/(i-j))
  }
  v <- 1 / (1 + i)
  an <- const_ann(n, i)
  s1p * (P*an + Q*(an-n*v^n)/i)
}

ann <- function(n, i, pi.=0, r=NULL, im, inc=FALSE
                , dec=FALSE, due=FALSE, p=1, P=NULL, Q=NULL, j=NULL
                ,s=FALSE, m=NULL) {
  if (!missing(pi.)) {
    i <- r_from_pi_i(i, pi.)
  } else if (!missing(r)) {
    i <- r
  }
  
  if (s) { (1 + i)^n } else { 1 } *
  if (inc) {
    Ia(n, i, P=P, Q=Q, p=p, j=j, due=due)
  } else if (dec) {
    Ia(n, i, P=n, Q=-1, p=p, j=j, due=due)
  } else {
    const_ann(n, i, due=due, p=p, m=m)
  }
}
