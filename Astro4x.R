source('constant.R')
# Redshift
z <- function(obs, lab) (obs - lab) / lab
# Cylindrical polar coordinates
ds <- function(r1, t1, z1, r2, t2, z2) sqrt((r1 - r2)^2 + r1^2*(t1 - t2)^2 + (z1 - z2)^2)

# Distance based on flux f2 against reference distance D flux d1
dStar <- function(D, f1, f2) D * sqrt(f1 / f2)

# Luminosity given surface brightness S, star radius r
Lsurf <- function(S, r) 4*pi*r^2*S
# Distance
Dsurf <- function(f, S, r) sqrt(Lsurf(S, r)/(4*pi*f))

load('obs.rda')
obs
plot(a_t~time, data = obs)

# Number of microstates q quanta, N states 
nM <- function(q, N) factorial(q + N - 1)/(factorial(q)*factorial(N-1))

