#WAIC

setwd("C:/Users/hp/Desktop/Geostan")
library(readxl)
library(readr)
library(INLA)
library(sp)
library(raster)
library(rgeos)
library(rgdal)
library(sf)
library(leaflet)
library(viridis)
library(dplyr)
library(generics)
library(lattice)

values = seq(1,2, 0.1)
range = seq(4, 5.5, 0.1)

dt = read_excel('traintransfcoor.xlsx')
original = read_excel('train.xlsx')
tranformed = read_excel('traintransf.xlsx')

W = data.frame(0, 0, 0)
colnames(W) = c('Sigma','Range', 'Waic')
n <- dim(dt)[1]

dtt = read_excel('testtransfcoor.xlsx')
nt<- dim(dtt)[1]

sample = c(1:length(Latitude))
attach(dt)
library(RColorBrewer)
neigh = rep(0, n)
nn = levels(factor(dt$Neighborhood))
for (lvl in 1:length(nn)){
  for (row in 1:nrow(dt)){
    if (dt[row, 'Neighborhood'] == nn[lvl]){
      neigh[row] = lvl
    }
  }
}
pal <- colorNumeric(palette='inferno',  domain=neigh,
                    na.color = "transparent"
)

leaflet(dt) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircles(lng = ~Longitude, lat = ~Latitude, 
             color = ~ pal(neigh))%>%
  addScaleBar(position = c("bottomleft")) %>%
  addLegend("bottomright",
            pal = pal, values = neigh,
            title = "Neighborhood")


#Costruzione della mesh 
lat = c(Latitude, dtt$Latitude)
lon = c(Longitude, dtt$Longitude)
coo <- cbind(lat, lon)
mesh <- inla.mesh.2d(
  loc = coo, max.edge = c(0.01, 0.05),
  cutoff = 0.001
)

pal <- colorNumeric(palette="viridis",  domain=price,
                    na.color = "transparent"
)
leaflet(dt) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircles(lng = ~Longitude, lat = ~Latitude, 
             color = ~ pal(price))%>%
  addScaleBar(position = c("bottomleft")) %>%
  addLegend("bottomright",
            pal = pal, values = price,
            title = "Price")

for (v in values){
  for (p in range){
  sigma0 <- v
  size <- min(c(diff(range(mesh$loc[, 1])), diff(range(mesh$loc[, 2]))))
  range0 <- size/p
  kappa0 <- sqrt(8)/range0
  tau0 <- 1/(sqrt(4 * pi) * kappa0 * sigma0)
  spde <- inla.spde2.matern(mesh, B.tau = cbind(log(tau0), -1, +1),
                            B.kappa = cbind(log(kappa0), 0, -1), theta.prior.mean = c(0, 0),
                            theta.prior.prec = c(0.1, 1))
  
  indexs <- inla.spde.make.index("s", spde$n.spde)
  lengths(indexs)
  
  A <- inla.spde.make.A(mesh = mesh, loc = coo[sample,])
  Ap <- inla.spde.make.A(mesh = mesh, loc = coo[-sample,])
  
  stk.e <- inla.stack(
    tag = "est",
    data = list(y = dt$price),
    A = list(1, A),
    effects = list(data.frame(b0 = 1, x1 = dt$Overall.Qual, x2 = dt$Garage.Area, x3=dt$Total.Bsmt.SF,
                              x4 = dt$X1st.Flr.SF, x5=dt$Full.Bath,  x7=dt$Mas.Vnr.Area,
                              x8=dt$TotRms.AbvGrd, x9=dt$Fireplaces, x10=dt$Wood.Deck.SF, x11=dt$AgeofHouse
    ), s = indexs)
  )
  stk.p <- inla.stack(
    tag = "pred",
    data = list(price = NA),
    A = list(1, Ap),
    effects = list(data.frame(b0 = 1, x1 = dtt$Overall.Qual, x2 = dtt$Garage.Area, x3=dtt$Total.Bsmt.SF,
                              x4 = dtt$X1st.Flr.SF, x5=dtt$Full.Bath ,x7=dtt$Mas.Vnr.Area, 
                              x8=dtt$TotRms.AbvGrd, x9=dtt$Fireplaces,x10=dtt$Wood.Deck.SF, x11=dtt$AgeofHouse
    ),
    s = indexs
    )
  )
  
  
  join.stack <- inla.stack(stk.e, stk.p)
  
  formula <- y ~ 0 + b0 + x1 + x2 +x3 +x4+x5 +x7 +x8 +x9 + x10 + x11 +
    f(s, model = spde)
  
  res <- inla(formula,
              family = "gaussian",
              data = inla.stack.data(join.stack),
              control.predictor=list(A = inla.stack.A(join.stack), compute = TRUE), verbose=TRUE,
              control.compute  =list(cpo = TRUE, dic = TRUE, waic = TRUE, config=TRUE),
              control.fixed=list(expand.factor.strategy="model.matrix")
  )
  w = c(v, p , res$waic$waic)
  W = rbind(W, w)
  }
  }

plot(W[-1,3])
min(W[-1, 3])
which.min(W[, 3])
W[90,]

sigma = 1.5
range = 4.8 
# WAIC -5921.09 

sigma0 <- sigma
size <- min(c(diff(range(mesh$loc[, 1])), diff(range(mesh$loc[, 2]))))
range0 <- size/range
kappa0 <- sqrt(8)/range0
tau0 <- 1/(sqrt(4 * pi) * kappa0 * sigma0)
spde <- inla.spde2.matern(mesh, B.tau = cbind(log(tau0), -1, +1),
                          B.kappa = cbind(log(kappa0), 0, -1), theta.prior.mean = c(0, 0),
                          theta.prior.prec = c(0.1, 1))

indexs <- inla.spde.make.index("s", spde$n.spde)
lengths(indexs)

sample = c(1:length(Latitude))
A <- inla.spde.make.A(mesh = mesh, loc = coo[sample,])
Ap <- inla.spde.make.A(mesh = mesh, loc = coo[-sample,])

stk.e <- inla.stack(
  tag = "est",
  data = list(y = dt$price),
  A = list(1, A),
  effects = list(data.frame(b0 = 1, x1 = dt$Overall.Qual, x2 = dt$Garage.Area, x3=dt$Total.Bsmt.SF,
                            x4 = dt$X1st.Flr.SF, x5=dt$Full.Bath,  x7=dt$Mas.Vnr.Area,
                            x8=dt$TotRms.AbvGrd, x9=dt$Fireplaces, x10=dt$Wood.Deck.SF, x11=dt$AgeofHouse
  ), s = indexs)
)
stk.p <- inla.stack(
  tag = "pred",
  data = list(price = NA),
  A = list(1, Ap),
  effects = list(data.frame(b0 = 1, x1 = dtt$Overall.Qual, x2 = dtt$Garage.Area, x3=dtt$Total.Bsmt.SF,
                            x4 = dtt$X1st.Flr.SF, x5=dtt$Full.Bath ,x7=dtt$Mas.Vnr.Area, 
                            x8=dtt$TotRms.AbvGrd, x9=dtt$Fireplaces,x10=dtt$Wood.Deck.SF, x11=dtt$AgeofHouse
  ),
  s = indexs
  )
)


join.stack <- inla.stack(stk.e, stk.p)

formula <- y ~ 0 + b0 + x1 + x2 +x3 +x4+x5 +x7 +x8 +x9 + x10 + x11 +
  f(s, model = spde)

res <- inla(formula,
            family = "normal",
            data = inla.stack.data(join.stack, spde=spde),
            control.predictor=list(A = inla.stack.A(join.stack), compute = TRUE), verbose=TRUE,
            control.compute  =list(cpo = TRUE, dic = TRUE, waic = TRUE, config=TRUE),
            control.fixed=list(expand.factor.strategy="model.matrix")
)
summary(res)


result <- inla.spde2.result(res, "s", spde)
plot(result[["marginals.range.nominal"]][[1]], type = "l",
        main = "Nominal range, posterior density")
plot(result[["marginals.variance.nominal"]][[1]], type = "l",
      main = "Nominal variance, posterior density")


proj <- inla.mesh.projector(mesh, dims=c(100, 100))
index <- inla.stack.index(join.stack, "pred")$data
linpred.mean <- res[["summary.linear.predictor"]]$mean
linpred.sd <- res[["summary.linear.predictor"]]$sd


time = 1 : nrow(dt)
index <- inla.stack.index(join.stack, "est")$data
plot(time, price[time] , xlab = "Day", ylab = "Probability")
lines(time, res$summary.fitted.values$mean[index])
lines(time, res$summary.fitted.values$"0.025quant"[index], lty = 2)
lines(time, res$summary.fitted.values$"0.975quant"[index], lty = 2)


time = 1 : nrow(dtt)
indexpred <- inla.stack.index(join.stack, "pred")$data
plot(time, price[tail(nrow(dtt)):tail(1)] , xlab = "sample", ylab = "LogPrice")
lines(time, res$summary.fitted.values$mean[indexpred])
lines(time, res$summary.fitted.values$"0.025quant"[indexpred], lty = 2)
lines(time, res$summary.fitted.values$"0.975quant"[indexpred], lty = 2)

predictions = list(train = res$summary.fitted.values$mean[index], test = res$summary.fitted.values$mean[indexpred])

terr = res$summary.fitted.values$mean[index] - dt$price
err = res$summary.fitted.values$mean[indexpred] - dtt$price
mse = sum(err^2) / nt
plot(terr)
plot(err)

time = 1 : 60
index <- inla.stack.index(join.stack, "pred")$data
plot(time, price[tail(nrow(dtt)):tail(503)] , xlab = "sample", ylab = "LogPrice")
lines(time, res$summary.fitted.values$mean[index[time]])
lines(time, res$summary.fitted.values$"0.025quant"[index[time]], lty = 2)
lines(time, res$summary.fitted.values$"0.975quant"[index[time]], lty = 2)


m1.samp <- inla.posterior.sample(100, res)

library("ggplot2")
library("gridExtra")
res$marginals.fixed
# Posterior of coefficient of xs
plot1 <- ggplot(as.data.frame(res$marginals.fixed$x1)) + 
  geom_line(aes(x = x, y = y)) +
  ylab (expression(paste(pi, "(", "x", " | ", bold(y), ")")))
plot2 <- ggplot(as.data.frame(res$marginals.fixed$x2)) + 
  geom_line(aes(x = x, y = y)) +
  ylab (expression(paste(pi, "(", "x", " | ", bold(y), ")")))
plot3 <- ggplot(as.data.frame(res$marginals.fixed$x3)) + 
  geom_line(aes(x = x, y = y)) +
  ylab (expression(paste(pi, "(", "x", " | ", bold(y), ")")))
plot4 <- ggplot(as.data.frame(res$marginals.fixed$x4)) + 
  geom_line(aes(x = x, y = y)) +
  ylab (expression(paste(pi, "(", "x", " | ", bold(y), ")")))
plot6 <- ggplot(as.data.frame(res$marginals.fixed$x7)) + 
  geom_line(aes(x = x, y = y)) +
  ylab (expression(paste(pi, "(", "x", " | ", bold(y), ")")))
plot7 <- ggplot(as.data.frame(res$marginals.fixed$x8)) + 
  geom_line(aes(x = x, y = y)) +
  ylab (expression(paste(pi, "(", "x", " | ", bold(y), ")")))
plot8 <- ggplot(as.data.frame(res$marginals.fixed$x9)) + 
  geom_line(aes(x = x, y = y)) +
  ylab (expression(paste(pi, "(", "x", " | ", bold(y), ")")))
plot9 <- ggplot(as.data.frame(res$marginals.fixed$x10)) + 
  geom_line(aes(x = x, y = y)) +
  ylab (expression(paste(pi, "(", "x", " | ", bold(y), ")")))
plot10 <- ggplot(as.data.frame(res$marginals.fixed$x11)) + 
  geom_line(aes(x = x, y = y)) +
  ylab (expression(paste(pi, "(", "x", " | ", bold(y), ")")))

# Posterior of precision
plott <- ggplot(as.data.frame(res$marginals.hyperpar[[1]])) + 
  geom_line(aes(x = x, y = y)) +
  ylab (expression(paste(pi, "(", tau, " | ", bold(y), ")")))

grid.arrange(plot1, plot2,plot3, plot4, plot6, plot7, plot8,
             plot9, plot10, plott, nrow = 5)

grid.arrange(plot1, plot2, nrow = 2)
grid.arrange(plot3, plot4, nrow = 2)
grid.arrange(plot6, plot7, nrow = 2)
grid.arrange(plot8, plot9, nrow = 2)
grid.arrange(plot10, plott, nrow = 2)


trainPred = data.frame(res$summary.fitted.values$mean[index])
testPred = data.frame(res$summary.fitted.values$mean[indexpred])

write.csv2(trainPred, "C:/Users/hp/Desktop/Geostan/TrainPred.csv", row.names=FALSE)
write.csv2(testPred, "C:/Users/hp/Desktop/Geostan/TestPred.csv", row.names=FALSE)
