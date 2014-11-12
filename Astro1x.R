pc <- 3.09e16 # Parsec to m
Mpc <- 3.09e22 # Megaparsec in m
Ls <- 3.839e26 # Luminosity of the Sun
AU <- 1.496e11 # Astronomical unit in m
c <- 3e8 # Speed of light in a vacuum

# Degreees to radians / radians to degrees
rad <- function(d) d*pi/180
deg <- function(r) r*180/pi
# Luminosity given flux and distance
L <- function(f, D) f * 4 * pi * D^2
# Flux given luminosity and distance
f <- function(L, D) L / (4*pi*D^2)
# Distance given flux and luminosity
D <- function(f, L) sqrt(L/(4*pi*f))
# Distance given flux ratio
Dr <- function(D, r) D*sqrt(r)
# Size given distance and angle
S <- function(r, th) r*th
# Angle given distance and size
th <- function(r, S) S/r
# Distance given size and angle
Da <- function(S, th) S/th
# Distance given angular size ratio
Dar <- function(D, r) D*r
# Redshift 
z <- function(Wo, Wl) (Wo - Wl)/Wl
# Speed given redshift
v <- function(z) z * c

