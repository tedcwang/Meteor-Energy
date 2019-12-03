#R script for the final paper
#Theodore Wang
#December 30, 2016
#
# clean things up
rm(list=ls())
setwd("/Users/Theo/Desktop/rfolder/Final Paper")
#
#
# load data
fireball <- read.csv("fireball.csv", header = T)
#
#
# exclude fireballs with excessive absence of data
remove <- complete.cases(fireball)
str(remove)
#
#
# summary of data
summary(fireball)
#
#
# reassigning variables
latitude <- fireball$Latitude..Deg.
longitude <- fireball$Longitude..Deg.
altitude <- fireball$Altitude..km.
velocity <- fireball$Velocity..km.s.
velocityx <- fireball$Velocity.Components..km.s...vx
velocityy <- fireball$Velocity.Components..km.s...vy
velocityz <- fireball$Velocity.Components..km.s...vz
radiated <- fireball$Total.Radiated.Energy..J.
impact <- fireball$Calculated.Total.Impact.Energy..kt.
#

# plot of latitude vs altitude and testing any relationships
plot(latitude, altitude, main="Latitude vs Altitude", xlab="Latitude (degrees)", ylab="Altitude (kilometers)", pch=20)
# plot of longitude vs altitude and testing any relationships
plot(longitude, altitude, main="Longitude vs Altitude", xlab="Longitude (degrees)", ylab="Altitude (kilometers)", pch=20)#
#
#
# plot of altitude vs radiated energy
plot(altitude, radiated, main="Altitude vs Radiated Energy", xlab="Altitude (kilometers)", ylab="Radiated Energy (joules)", pch=18)
alt <- lm(radiated~altitude)
abline(alt)
plot(altitude, radiated, ylim=c(0, 2.5e+13), main="Altitude vs Radiated Energy", xlab="Altitude (kilometers)", ylab="Radiated Energy (joules)", pch=18)
alt <- lm(radiated~altitude)
abline(alt)
plot(altitude, radiated, ylim=c(0, 0.5e+13), main="Altitude vs Radiated Energy", xlab="Altitude (kilometers)", ylab="Radiated Energy (joules)", pch=18)
alt <- lm(radiated~altitude)
abline(alt)
#
#
# plot of velocity vs radiated energy
plot(velocity, radiated, main="Velocity vs Radiated Energy", xlab="Velocity (km/s)", ylab="Radiated Energy (joules)", pch=18)
vel <- lm(radiated~velocity)
abline(vel)
plot(velocity, radiated, ylim=c(0, 2.5e+13), main="Velocity vs Radiated Energy", xlab="Velocity (km/s)", ylab="Radiated Energy (joules)", pch=18)
vel <- lm(radiated~velocity)
abline(vel)
plot(velocity, radiated, ylim=c(0, 0.5e+13), main="Velocity vs Radiated Energy", xlab="Velocity (km/s)", ylab="Radiated Energy (joules)", pch=18)
vel <- lm(radiated~velocity)
abline(vel)
#
#
# plot of velocity component x vs radiated energy
plot(velocityx, radiated, main="Velocity X vs Radiated Energy", xlab="Velocity X (km/s)", ylab="Radiated Energy (joules)", pch=18)
velx <- lm(radiated~velocityx)
abline(velx)
plot(velocityx, radiated, ylim=c(0, 2.5e+13), main="Velocity X vs Radiated Energy", xlab="Velocity X (km/s)", ylab="Radiated Energy (joules)", pch=18)
velx <- lm(radiated~velocityx)
abline(velx)
plot(velocityx, radiated, ylim=c(0, 0.5e+13), main="Velocity X vs Radiated Energy", xlab="Velocity X (km/s)", ylab="Radiated Energy (joules)", pch=18)
velx <- lm(radiated~velocityx)
abline(velx)
#
#
# plot of velocity component y vs radiated energy
plot(velocityy, radiated, main="Velocity Y vs Radiated Energy", xlab="Velocity Y (km/s)", ylab="Radiated Energy (joules)", pch=18)
vely <- lm(radiated~velocityy)
abline(vely)
plot(velocityy, radiated, ylim=c(0, 2.5e+13), main="Velocity Y vs Radiated Energy", xlab="Velocity Y (km/s)", ylab="Radiated Energy (joules)", pch=18)
vely <- lm(radiated~velocityy)
abline(vely)
plot(velocityy, radiated, ylim=c(0, 0.5e+13), main="Velocity Y vs Radiated Energy", xlab="Velocity Y (km/s)", ylab="Radiated Energy (joules)", pch=18)
vely <- lm(radiated~velocityy)
abline(vely)
#
#
# plot of velocity component z vs radiated energy
plot(velocityz, radiated, main="Velocity Z vs Radiated Energy", xlab="Velocity Z (km/s)", ylab="Radiated Energy (joules)", pch=18)
velz <- lm(radiated~velocityz)
abline(velz)
plot(velocityz, radiated, ylim=c(0, 2.5e+13), main="Velocity Z vs Radiated Energy", xlab="Velocity Z (km/s)", ylab="Radiated Energy (joules)", pch=18)
velz <- lm(radiated~velocityz)
abline(velz)
#
#
# plot of latitude vs radiated energy
plot(latitude, radiated, main="Latitude vs Radiated Energy", xlab="Latitude (degrees)", ylab="Radiated Energy (joules)", pch=18)
plot(latitude, radiated, ylim=c(0, 2.5e+13), main="Latitude vs Radiated Energy", xlab="Latitude (degrees)", ylab="Radiated Energy (joules)", pch=18)
plot(latitude, radiated, ylim=c(0, 0.5e+13), main="Latitude vs Radiated Energy", xlab="Latitude (degrees)", ylab="Radiated Energy (joules)", pch=18)
plot(latitude, radiated, ylim=c(0, 1e+12), main="Latitude vs Radiated Energy", xlab="Latitude (degrees)", ylab="Radiated Energy (joules)", pch=18)

#
#
# plot of longitude vs radiated energy
plot(longitude, radiated, main="Longitude vs Radiated Energy", xlab="Longitude (degrees)", ylab="Radiated Energy (joules)", pch=18)
plot(longitude, radiated, ylim=c(0, 2.5e+13), main="Longitude vs Radiated Energy", xlab="Longitude (degrees)", ylab="Radiated Energy (joules)", pch=18)
plot(longitude, radiated, ylim=c(0, 0.5e+13), main="Longitude vs Radiated Energy", xlab="Longitude (degrees)", ylab="Radiated Energy (joules)", pch=18)
plot(longitude, radiated, ylim=c(0, 1e+12), main="Longitude vs Radiated Energy", xlab="Longitude (degrees)", ylab="Radiated Energy (joules)", pch=18)
#
#
#
# BIC modeling radiated energy to possible influential factors
BIC(lm(radiated~1))
BIC(lm(radiated~altitude))
BIC(lm(radiated~velocity))
BIC(lm(radiated~velocityx))
BIC(lm(radiated~velocityy))
BIC(lm(radiated~velocityz))
BIC(lm(radiated~latitude))
BIC(lm(radiated~longitude))
#
#
# BIC modeling calculated impact energy to confirm
BIC(lm(impact~1))
BIC(lm(impact~altitude))
BIC(lm(impact~velocity))
BIC(lm(impact~velocityx))
BIC(lm(impact~velocityy))
BIC(lm(impact~velocityz))
BIC(lm(impact~latitude))
BIC(lm(impact~longitude))
#
#
# BIC modeling velocity to see if any factors influence overall velocity
BIC(lm(velocity~1))
BIC(lm(velocity~velocityx))
BIC(lm(velocity~velocityy))
BIC(lm(velocity~velocityz))
BIC(lm(velocity~altitude))
#
#
# end of script
