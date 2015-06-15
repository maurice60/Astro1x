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
# Central pressure in white dwarf radius R, density rho
pC <- function(rho, R) 2*pi*G*rho^2*R^2/3

# Orbital radius of gas cloud, velocity vg around a star of mass Ms (Ms >> m gas)
rG <- function(Ms, vg) G*Ms/vg^2
# Orbital radius of white dwarf, velocity vWD around a star of mass Ms (Ms ~ m WD)
rWD <- function(Ms, vWD) G*Ms/(4*vWD^2)

# The cumulative force of all the photons flooding out from an object of luminosity L and hitting one electron a distance D away
Fphot <- function(L, D) L * sigt / (4*pi*D^2*c)
# Eddington luminosity
LEdd <- function(mWD) 4*pi*G*mWD*mp*c/sigt

# Mass at Eddington lminosity
mEdd = function(L) L*sigt / (4*pi*G*mp*c)

# Redshift
z <- function(obs, lab) (obs - lab) / lab

# Speed of gas emitted wavelength e absorbed wavelength a
vGas <- function(e, a) (e - a) * c / e

# neutrino flux due to formation of neutron star D away
fN <- function(D) 1e57/(4*pi*D^2)
fQ <- function(n, D) n/(4*pi*D^2)
tO <- function(n, D) n*4*pi*D^2

# number of molecules w weight of substance Z atomic weight
nMol <- function(w, Z) w / Z

# number of interactions, m molecules f flux
nInt <- function(m, f) 1e-47 * m * f
xArea <- function(n, m, f) n / (m * f)
fInc <- function(n, m, a) n / (m * a)
