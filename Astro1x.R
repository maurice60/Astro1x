source('constant.R')

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
# Age of universe given redshift and distance of object
At <- function(D, z) D/(c*z)
# Flux taking redshift into account
fz <- function(L, D, z) f(L, D)*at(z)^2
# gravitational potential energy
pe <- function(m1, m2, r) -(G*m1*m2)/r
# Beaming angle
theta <- function(v) sqrt(1-(v/c)^2)
# Observed flux
ofl <- function(thet) 4/thet^2
# Central mass given orbital velocity and radius. Radius from S above.
M <- function(v, r) v^2*r/G
# Mass given flux 
Mf <- function(f, D) 4*ms*4*pi*D^2*f/Ls
# Temperature at surface of planet
pT <- function(L, D) (L/(16*pi*sb*D^2))^.25
