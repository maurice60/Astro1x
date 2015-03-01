source('constant.R')

# Mass of companion star and orbital radius
m21 <- function(P, r1, r2) 4*pi^2*r1*(r1+r2)^2/(G*P^2)
m22 <- function(m1, r1, r2) m1*r1/r2
findCompanion <- function(P, m1, r1) {
    g <- 30*AU
    repeat {
        a <- m21(P, r1, g)
        b <- m22(m1, r1, g)
        if (a/b < 1.01 && a/b > .99) return(c(a, g))
        if (a>b) g <- g <- g - .1*AU else g <- g + .1*AU
    }
}

# Radius of companion star
rC <- function(rA, lA, tA, lB, tB) rA*sqrt(lB/lA)*(tA/tB)^2