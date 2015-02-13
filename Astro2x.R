source('constant.R')

# Distance of planet from pulsar given mass of pulsar and period
rp <- function(m, p) ((G*m*p^2)/(4*pi^2))^(1/3)
# Period of planet mass m2, star m1, distance D
pp <- function(m1, m2, D) sqrt(4*pi^2*D^3/(G*(m1+m2)))
# Mass of planet given mass of pulsar and two orbital radii
Mp <- function(Ms, rs, rp) rs * Ms / rp
# Star's wobble distance
ws <- function(m1, m2, D) D / (1 + m1/m2)
# Star's velocity given orbital radius and period
vs <- function(r, p) 2*pi*r/p
# Orbital radius of star given velocity and preiod
rs <- function(v, p) v*p/(2*pi)
# Doppler shift given velocity and wavelength
dop <- function(v, w) v*w/c
# Velocity given doppler shift and wavelength
vd <- function(ds, w) ds*c/w

# Radius of planet given star radius and dip in brightness
pR <- function(star, dip) star * sqrt(dip)
# Temperature at surface of planet given luminosity of star and distance
pT <- function(L, D) (L/(16*pi*sb*D^2))^.25

# Gravitational microlens angle due to passing r away from M
theta <- function(r, M) 4*G*M/(r*c^2)
# Einstein radius distance D mass M
er <- function(D, M) sqrt(4*G*M/(D*c^2))
# Mass given distance to far object and Einstein radius
eM <- function(D, e) D*c^2*e^2/(4*G)
