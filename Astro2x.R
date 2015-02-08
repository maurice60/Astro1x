source('constant.R')

# Distance of planet from pulsar given mass of pulsar and period
rp <- function(m, p) ((G*m*p^2)/(4*pi^2))^.33333333
# Mass of planet given mass of pulsar and two orbital radii
Mp <- function(Ms, rs, rp) rs * Ms / rp
