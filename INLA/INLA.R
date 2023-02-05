setwd("C:/Users/hp/Desktop/Geostan")
library(readxl)
library(readr)
library(INLA)
library(sp)
library(raster)
library(rgeos)
library(rgdal)
library(sf)

# Documentazione
# https://becarioprecario.bitbucket.io/inla-gitbook/ch-INLA.html
Data = read_excel('houseneighlatlong.xlsx')

neig = factor(Data$Neighborhood)
factors = unique(neig)
v = c()
for (f in factors){
  temp = which(Data$Neighborhood == f)
  if (is.null(v)) {
    v = temp
  }
  else{
    v = c(v, temp)
  }
}
  
#Scale Price
scaled = Data[, colnames(Data)[colnames(Data) != 'Neighborhood' & 
                               colnames(Data) != 'Latitude' &
                               colnames(Data) != 'Longitude' &
                                 colnames(Data) != 'dummy1_lc' &
                                 colnames(Data) != 'price']] 

log =  Data[, colnames(Data)[colnames(Data) == 'price']] 
coords = Data[, colnames(Data)[colnames(Data) == 'Latitude' |
                                 colnames(Data) == 'Longitude']] 

#No scaled price

# data = Data[, colnames(Data)[colnames(Data) != 'Neighborhood' & 
#                                colnames(Data) != 'Latitude' &
#                                colnames(Data) != 'Longitude'&
#                                colnames(Data) != 'price']] 
# coords = Data[, colnames(Data)[colnames(Data) == 'Latitude' |
#                                  colnames(Data) == 'Longitude' |
#                                  colnames(Data) == 'price']] 

Price = log10(log)
data = data.frame(scale(scaled))
data = cbind(Price, data, no_scale, coords)
attach(data)
n = nrow(data)
# sps = SpatialPoints(data[, c('Latitude','Longitude')], 
#                     proj4string = CRS("+proj=utm +zone=15"))
# spst <- spTransform(sps, CRS("+proj=longlat +datum=WGS84"))
# data[, c("long", "lat")] <- coordinates(spst)

library(leaflet)
library(viridis)


pal <- colorNumeric(palette="viridis",  domain=price,
                    na.color = "transparent"
)
leaflet(data) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircles(lng = ~Longitude, lat = ~Latitude, 
      color = ~ pal(price))%>%
  addScaleBar(position = c("bottomleft")) %>%
  addLegend("bottomright",
          pal = pal, values = price,
          title = "Price")

coo <- cbind(Longitude, Latitude)
mesh <- inla.mesh.2d(
  loc = coo, max.edge = c(0.01, 0.05),
  cutoff = 0.001
)

sample <- sample(c(TRUE, FALSE), nrow(data), replace=TRUE, prob=c(0.8,0.2))
dt  <- data[sample, ]
dtt   <- data[!sample, ]
plot(mesh)
points(coo, col = "red")
# coop = dtt[,c('Latitude','Longitude')]
spde <- inla.spde2.matern(mesh = mesh, alpha = 2, constr = TRUE)

indexs <- inla.spde.make.index("s", spde$n.spde)
lengths(indexs)

A <- inla.spde.make.A(mesh = mesh, loc = coo[sample,])
Ap <- inla.spde.make.A(mesh = mesh, loc = coo[!sample,])

stk.e <- inla.stack(
  tag = "est",
  data = list(y = dt$price),
  A = list(1, A),
  effects = list(data.frame(b0 = 1, x1 = dt$Overall.Qual, x2 = dt$Garage.Area, x3=dt$Total.Bsmt.SF,
                            x4 = dt$X1st.Flr.SF, x6=dt$AgeofHouse, x7=dt$Mas.Vnr.Area,
                            x8=dt$TotRms.AbvGrd, x9=dt$Fireplaces), s = indexs)
)

stk.p <- inla.stack(
  tag = "pred",
  data = list(price = NA),
  A = list(1, Ap),
  effects = list(data.frame(b0 = 1, x1 = dtt$Overall.Qual, x2 = dtt$Garage.Area, x3=dtt$Total.Bsmt.SF,
                            x4 = dtt$X1st.Flr.SF, x6=dtt$AgeofHouse, x7=dtt$Mas.Vnr.Area,
                            x8=dtt$TotRms.AbvGrd, x9=dtt$Fireplaces ),
                 s = indexs
  )
)

join.stack <- inla.stack(stk.e, stk.p)

formula <- y ~ 0 + b0 + x1 + x2 +x3 +x4+ x6 +x7 +x8 +x9 + f(s, model = spde)

res <- inla(formula,
            family = "gaussian",
            data = inla.stack.data(join.stack),
            control.predictor=list(A = inla.stack.A(join.stack), compute = TRUE), verbose=TRUE,
            control.compute  =list(cpo = TRUE, dic = TRUE, waic = TRUE, config=TRUE),
            control.fixed=list(expand.factor.strategy="model.matrix")
            )
summary(res)

library("ggplot2")
library("gridExtra")
res$marginals.fixed
# Posterior of coefficient of x1
plot1 <- ggplot(as.data.frame(res$marginals.fixed$x1)) + 
  geom_line(aes(x = x, y = y)) +
  ylab (expression(paste(pi, "(", "x", " | ", bold(y), ")")))

# Posterior of precision
plot2 <- ggplot(as.data.frame(res$marginals.hyperpar[[1]])) + 
  geom_line(aes(x = x, y = y)) +
  ylab (expression(paste(pi, "(", tau, " | ", bold(y), ")")))

grid.arrange(plot1, plot2, nrow = 2)


l = length(res$marginals.fixed)
v = rep(0, l)
j = 1
for (i in res$marginals.fixed){
  v[j] = inla.pmarginal(0, i)
  j= j+1
}

samp =inla.posterior.sample(100)


index.pred <- inla.stack.index(join.stack, "pred")$data

spde.est <- inla.spde2.result(inla = res, name = "spatial.field",
                              spde = spde, do.transf = TRUE)

inla.zmarginal(spde$marginals.variance.nominal[[1]])


pred_mean <- res$summary.fitted.values[index.pred, "mean"]
pred_sd <- res$summary.fitted.values[index.pred, "sd"]


pal_t <- colorNumeric(palette="viridis",  domain=dtt$price,
                    na.color = "transparent"
)
pal_f <- colorNumeric(palette="viridis",  domain=pred_mean,
                      na.color = "transparent"
)


leaflet(dtt) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircles(lng = ~Longitude, lat = ~Latitude, 
             color = ~ pal_t(price))%>%
  addScaleBar(position = c("bottomleft")) %>%
  addLegend("bottomright",
            pal = pal_t, values = dtt$price,
            title = "PriceT")

leaflet(dtt) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircles(lng = ~Longitude, lat = ~Latitude, 
             color = ~ pal_t(pred_mean))%>%
  addScaleBar(position = c("bottomleft")) %>%
  addLegend("bottomright",
            pal = pal_t, values = pred_mean,
            title = "PriceT")

r = res$residuals$deviance.residuals
plot(r)

plot(pred_mean - dtt$price)

err = sum((pred_mean - dtt$price)^2)
