pc <- 3.09e16 # Parsec to m
Mpc <- 3.09e22 # Megaparsec in m
Ls <- 3.839e26 # Luminosity of the Sun
AU <- 1.496e11 # Astronomical unit in m
c <- 3e8 # Speed of light in a vacuum
eV <- 1.6e-19 # electron volt J
ep <- 1e-3 # present day mean energy of photons eV
rho0 <- 5e-28 # present day density of universe
Ep <- 1.2e28*eV #Planck energy

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
# Size given distance and angle (angle in radians)
S <- function(r, th) r*th
# Angle given distance and size (angle in radians)
th <- function(r, S) S/r 
# Distance given size and angle (angle in radians)
Da <- function(S, th) S/th
# Distance given angular size ratio
Dar <- function(D, r) D*r
# Redshift 
z <- function(Wo, Wl) (Wo - Wl)/Wl
# Speed given redshift
v <- function(z) z * c
# Redshift given scale factor
zs <- function(at) (1-at)/at
# Scale factor given redshift
at <- function(z) 1/(1+z)
# Energy of photons at some time in the past given scale factor
et <- function(a) ep*eV/a
# scale factor given energy
ae <- function(e) ep*eV/e
# Density of universe given redshift
rho <- function(z) rho0*(1+z)^3
# Redshift given energy of typical photon
zsf <- function(e) zs(ae(e))
