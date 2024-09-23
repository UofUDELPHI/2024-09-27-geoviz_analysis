
library(sf)
library(tmap)
library(INLA)

######################
# spatial regression
######################

scot <- st_read("data/scotlip/scotlip.shp")
# scot <- scot[-c(6, 8, 11), ]

tm_shape(scot) +
  tm_polygons(col = "orange", border.col = "white")

scot$cancer_SIR <- 100000 * scot$CANCER / scot$POP

cor.test(scot$AFF, scot$cancer_SIR)

scot$AFF_scaled <- scot$AFF / 10 # increase from 0% to 10% AFF

m1 <- glm(CANCER ~ AFF_scaled + offset(log(POP)), 
          data = scot, family = poisson(link = "log"))

summary(m1)
mean(sum(m1$residuals^2)) # 162.2338 / 172.026
# AFF model: 58.86549

exp(m1$coefficients["AFF_scaled"])


## map
Y <- tm_shape(scot) +
  tm_polygons(col = "cancer_SIR", style = "cont", 
              palette = "plasma", lwd = 0.25, border.col = "white")

X <- tm_shape(scot) +
  tm_polygons(col = "AFF", style = "cont", 
              palette = "plasma", lwd = 0.25, border.col = "white")

tmap_arrange(Y, X)

scot$error <- m1$residuals

e <- tm_shape(scot) +
  tm_polygons(col = "error", style = "cont", 
              palette = "-Spectral", lwd = 0.25, border.col = "white")

tmap_arrange(Y, e)




# spatial structure
nb <- poly2nb(scot)
nb2INLA("map.adj", nb)
g <- inla.read.graph(filename = "map.adj")

scot$idarea <- 1:nrow(scot)
scot$idarea_2 <- 1:nrow(scot)

m2 <- inla(CANCER ~ AFF_scaled + offset(log(POP)) +
             f(idarea, model = "slm", graph = g), 
           data = scot,  family = "poisson", 
           control.family = list(link = "log"),
           control.compute = list(dic = TRUE, waic = TRUE))

mean(sum(m2$residuals$deviance.residuals^2))
# Null model: 55.61381 / 59.0099
# AFF model: 58.68931 / 62.28554
m2$waic
# Null WAIC: 279.509
# AFF WAIC: 280.5297 / 300.3329

scot$error <- m2$residuals$deviance.residuals

e_spatial <- tm_shape(scot) +
  tm_polygons(col = "error", style = "cont", 
              palette = "-Spectral", lwd = 0.25, border.col = "white")

tm

exp(m2$summary.fixed$mean[2])

1 - inla.pmarginal(0.95, m2$marginals.fixed$AFF_scaled)












###############
# mess
library(nlme)
library(tidyr)
library(ggplot2)
B1.gls <- gls(CANCER ~ AFF_scaled + offset(log(POP)), data = scot)

col.slm = lagsarlm(CANCER ~ AFF_scaled + offset(log(POP)), 
                   data = lagsarlm, col.listw)

x <- ggplot(scot) +
  stat_sf_coordinates()

scot_coords <- st_coordinates(st_centroid(scot$geometry))
scot <- cbind(scot, scot_coords)

lme(CANCER ~ AFF_scaled, random = ~ 1 | idarea,
    correlation =
      corExp(form= ~ X + Y, nugget=T),
    scot)

scot$dummy <- rep(1, dim(scot)[1])

col.lme1 = lme(cancer_SIR ~ AFF_scaled, 
               random = ~ 1 | dummy,
               data = scot, method = "ML")

vgram <- Variogram(col.lme1, 
                   form = ~ Y + X,
                   data = scot)

plot(vgram)

vgram <- Variogram(col.lme1, 
                   form = ~ Y + X, 
                   data = scot, 
                   maxDist = 125000)

plot(vgram)


?corGaus

col.lme2 <- lme(cancer_SIR ~ AFF_scaled, 
                random = ~ 1 | dummy,
                data = scot, method = "ML",
                corr = corSpatial(value = c(125000, 0.4), 
                                  form = ~ Y + X, 
                                  type = 'spherical', 
                                  nugget = TRUE))
?corSpatial

vgram <- Variogram(col.lme2, 
                   form = ~ Y + X, 
                   data = scot, 
                   maxDist = 125000)

plot(vgram)



summary(col.lme1)



