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

# Documentazione
# https://becarioprecario.bitbucket.io/inla-gitbook/ch-INLA.html

#Preparazione dei dati
Data = read_excel('houseordinato.xlsx')
complete = read_excel('Amesconlatlong.xlsx')
n <- dim(complete)[1]
scaled = complete[, colnames(complete)[colnames(complete) == 'Overall.Qual' |
                                         colnames(complete) == 'Garage.Area' |
                                         colnames(complete) == 'Total.Bsmt.SF' |
                                         colnames(complete) == 'X1st.Flr.SF' |
                                         colnames(complete) == 'AgeofHouse' |
                                         colnames(complete) == 'Mas.Vnr.Area' |
                                         colnames(complete) == 'TotRms.AbvGrd' |
                                         colnames(complete) == 'Fireplaces' |
                                         colnames(complete) == 'Wood.Deck.SF'|
                                         colnames(complete) == 'Full.Bath']]
age = complete$Yr.Sold - complete$Year.Built

target =  complete[, colnames(complete)[colnames(complete) == 'price' ]]
coords = complete[, colnames(complete)[colnames(complete) == 'Latitude' |
                                 colnames(complete) == 'Longitude']]                                          

landcontour.name <- factor(complete$Land.Contour, labels=c('Bnk','HLS', 'Low', 'Lvl'))
i1_lc <- which(landcontour.name=='Bnk')
i2_lc <- which(landcontour.name=='HLS')
i3_lc <- which(landcontour.name=='Low')
i4_lc <- which(landcontour.name=='Lvl')
dummy1_lc <- matrix(rep(0,n),nrow=n)
dummy2_lc <- matrix(rep(0,n),nrow=n)
dummy3_lc <- matrix(rep(0,n),nrow=n)

centralair.name <- factor(complete$Central.Air, labels=c('N','Y'))
i1_ca <- which(centralair.name=='N')
i2_ca <- which(centralair.name=='Y')
dummy_ca <- matrix(rep(0,n),nrow=n)

condition1.name <- factor(complete$Condition.1, labels=c('Artery','Feedr','Norm','RRNn','RRAn','PosN','PosA','RRNe','RRAe'))
i1_con <- which(condition1.name=='Artery')
i2_con <- which(condition1.name=='Feedr')
i3_con <- which(condition1.name=='Norm')
i4_con <- which(condition1.name=='RRNn')
i5_con <- which(condition1.name=='RRAn')
i6_con <- which(condition1.name=='PosN')
i7_con <- which(condition1.name=='PosA')
i8_con <- which(condition1.name=='RRNe')
i9_con <- which(condition1.name=='RRAe')
dummy1_con <- matrix(rep(0,n),nrow=n)
dummy2_con <- matrix(rep(0,n),nrow=n)
dummy3_con <- matrix(rep(0,n),nrow=n)
dummy4_con <- matrix(rep(0,n),nrow=n)
dummy5_con <- matrix(rep(0,n),nrow=n)
dummy6_con <- matrix(rep(0,n),nrow=n)
dummy7_con <- matrix(rep(0,n),nrow=n)
dummy8_con <- matrix(rep(0,n),nrow=n)

DAT <- data.frame(dummy1_lc,dummy2_lc,dummy3_lc,dummy_ca,dummy1_con,dummy2_con,dummy3_con,dummy4_con,dummy5_con,dummy6_con,dummy7_con,dummy8_con)

DAT[i2_lc,1] <- 1
DAT[i3_lc,2] <- 1
DAT[i4_lc,3] <- 1

DAT[i2_ca,4] <- 1

DAT[i2_con,5] <- 1
DAT[i3_con,6] <- 1
DAT[i4_con,7] <- 1
DAT[i5_con,8] <- 1
DAT[i6_con,9] <- 1
DAT[i7_con,10] <- 1
DAT[i8_con,11] <- 1
DAT[i9_con,12] <- 1

dummy = DAT[, colnames(DAT)[colnames(DAT) == 'dummy1_lc' |
                                        colnames(DAT) == 'dummy2_lc'|
                                        colnames(DAT) == 'dummy3_lc' |
                                        colnames(DAT) == 'dummy2_con'|
                                        colnames(DAT) == 'dummy3_con'|
                                        colnames(DAT) == 'dummy4_con']]

# x1 = dtt$Overall.Qual, x2 = dtt$Garage.Area, x3=dtt$Total.Bsmt.SF,
# x4 = dtt$X1st.Flr.SF, x6=dtt$AgeofHouse, x7=dtt$Mas.Vnr.Area,
# x8=dtt$TotRms.AbvGrd, x9=dtt$Fireplaces

#Data visualisation
data = cbind(log10(target), scale(scaled), age, dummy, coords)
attach(data)

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

#Costruzione della mesh 
coo <- cbind(Longitude, Latitude)
mesh <- inla.mesh.2d(
  loc = coo, max.edge = c(0.01, 0.05),
  cutoff = 0.0001
)
plot(mesh)
points(coo, col = "red")

#Divisione train test
sample <- sample(c(TRUE, FALSE), nrow(data), replace=TRUE, prob=c(0.8,0.2))
dt  <- data[sample, ]
dtt   <- data[!sample, ]

#Costruzione della covariance function (matern covariance function) a partire dalla mesh
spde <- inla.spde2.matern(mesh = mesh, alpha = 1, constr = TRUE)
indexs <- inla.spde.make.index("s", spde$n.spde)
lengths(indexs)

A <- inla.spde.make.A(mesh = mesh, loc = coo[sample,])
Ap <- inla.spde.make.A(mesh = mesh, loc = coo[!sample,])

#Onestamente non so cosa faccia ma questo è l'oggetto che passi al comando INLA
stk.e <- inla.stack(
  tag = "est",
  data = list(y = dt$price),
  A = list(1, A),
  effects = list(data.frame(b0 = 1, x1 = dt$Overall.Qual, x2 = dt$Garage.Area, x3=dt$Total.Bsmt.SF,
                            x4 = dt$X1st.Flr.SF,  x7=dt$Mas.Vnr.Area,
                            x8=dt$TotRms.AbvGrd, x9=dt$Fireplaces, x10=dt$Wood.Deck.SF, x11=dt$age, 
                            x12=dt$dummy1_lc, x13=dt$dummy2_lc, x14=dt$dummy3_lc, x15=dt$dummy2_con, 
                            x16=dt$dummy3_con, x17=dt$dummy4_con), s = indexs)
)
stk.p <- inla.stack(
  tag = "pred",
  data = list(price = NA),
  A = list(1, Ap),
  effects = list(data.frame(b0 = 1, x1 = dtt$Overall.Qual, x2 = dtt$Garage.Area, x3=dtt$Total.Bsmt.SF,
                            x4 = dtt$X1st.Flr.SF, x7=dtt$Mas.Vnr.Area,
                            x8=dtt$TotRms.AbvGrd, x9=dtt$Fireplaces,x10=dtt$Wood.Deck.SF, x11=dtt$age,
                            x12=dtt$dummy1_lc, x13=dtt$dummy2_lc, x14=dtt$dummy3_lc, x15=dtt$dummy2_con, 
                            x16=dtt$dummy3_con, x17=dtt$dummy4_con),
                 s = indexs
  )
)

join.stack <- inla.stack(stk.e, stk.p)

#Modello lineare delle covariate
formula <- y ~ 0 + b0 + x1 + x2 +x3 +x4 +x7 +x8 +x9 + x10 + x11 + x12 +
  x13 + x14 + x15 +x16 +x17 + f(s, model = spde)

res <- inla(formula,
            family = "gaussian",
            data = inla.stack.data(join.stack),
            control.predictor=list(A = inla.stack.A(join.stack), compute = TRUE), verbose=TRUE,
            control.compute  =list(cpo = TRUE, dic = TRUE, waic = TRUE, config=TRUE),
            control.fixed=list(expand.factor.strategy="model.matrix")
)
summary(res)
# alpha=1, gaussian family, mesh: 0.01, 0.05, 0.0001 -> DIC: -6312.47, WAIC -> -5229.09


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



index.pred <- inla.stack.index(join.stack, "pred")$data

spde.est <- inla.spde2.result(inla = res, name = "spatial.field",
                              spde = spde, do.transf = TRUE)

#Previsione della media

pred_mean <- res$summary.fitted.values[index.pred, "mean"]
pred_sd <- res$summary.fitted.values[index.pred, "sd"]


pal_t <- colorNumeric(palette="viridis",  domain=dtt$price,
                      na.color = "transparent"
)

#Visualizzazione del test set

#Test price
leaflet(dtt) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircles(lng = ~Longitude, lat = ~Latitude, 
             color = ~ pal_t(price))%>%
  addScaleBar(position = c("bottomleft")) %>%
  addLegend("bottomright",
            pal = pal_t, values = dtt$price,
            title = "PriceT")

#Predicted price
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





########
##Tentativo 2 senza X11

spde2 <- inla.spde2.matern(mesh = mesh, alpha = 1, constr = TRUE)

indexs2 <- inla.spde.make.index("s", spde2$n.spde)
lengths(indexs2)

A2 <- inla.spde.make.A(mesh = mesh, loc = coo[sample,])
Ap2 <- inla.spde.make.A(mesh = mesh, loc = coo[!sample,])

stk.e2 <- inla.stack(
  tag = "est",
  data = list(y = dt$price),
  A = list(1, A2),
  effects = list(data.frame(b0 = 1, x1 = dt$Overall.Qual, x2 = dt$Garage.Area, x3=dt$Total.Bsmt.SF,
                            x4 = dt$X1st.Flr.SF,  x7=dt$Mas.Vnr.Area,
                            x8=dt$TotRms.AbvGrd, x9=dt$Fireplaces, x10=dt$Wood.Deck.SF, x11=dt$age, 
                            x12=dt$dummy1_lc, x13=dt$dummy2_lc, x14=dt$dummy3_lc, x15=dt$dummy2_con, 
                            x16=dt$dummy3_con, x17=dt$dummy4_con), s = indexs2)
)

stk.p2 <- inla.stack(
  tag = "pred",
  data = list(price = NA),
  A = list(1, Ap2),
  effects = list(data.frame(b0 = 1, x1 = dtt$Overall.Qual, x2 = dtt$Garage.Area, x3=dtt$Total.Bsmt.SF,
                            x4 = dtt$X1st.Flr.SF, x7=dtt$Mas.Vnr.Area,
                            x8=dtt$TotRms.AbvGrd, x9=dtt$Fireplaces,x10=dtt$Wood.Deck.SF, x11=dtt$age,
                            x12=dtt$dummy1_lc, x13=dtt$dummy2_lc, x14=dtt$dummy3_lc, x15=dtt$dummy2_con, 
                            x16=dtt$dummy3_con, x17=dtt$dummy4_con),
                 s = indexs2
  )
)

join.stack2 <- inla.stack(stk.e2, stk.p2)

formula2 <- y ~ 0 + b0 + x1 + x2 +x3 +x4 +x7 +x8 +x9 + x10 + x12 +
  x13 + x14 + x15 +x16 +x17 + f(s, model = spde)

res2 <- inla(formula2,
            family = "gaussian",
            data = inla.stack.data(join.stack2),
            control.predictor=list(A = inla.stack.A(join.stack2), compute = TRUE), verbose=TRUE,
            control.compute  =list(cpo = TRUE, dic = TRUE, waic = TRUE, config=TRUE),
            control.fixed=list(expand.factor.strategy="model.matrix")
)
summary(res2)
# alpha=1, gaussian family, mesh: 0.01, 0.05, 0.0001 -> DIC: -6641.51, WAIC -> -5856.85, NO X11
#con dummy DIC: -6256.07, WAIC -> -5317.92


plot1 <- ggplot(as.data.frame(res2$marginals.fixed$x1)) + 
  geom_line(aes(x = x, y = y)) +
  ylab (expression(paste(pi, "(", "x", " | ", bold(y), ")")))
plot2 <- ggplot(as.data.frame(res2$marginals.fixed$x2)) + 
  geom_line(aes(x = x, y = y)) +
  ylab (expression(paste(pi, "(", "x", " | ", bold(y), ")")))
plot3 <- ggplot(as.data.frame(res2$marginals.fixed$x3)) + 
  geom_line(aes(x = x, y = y)) +
  ylab (expression(paste(pi, "(", "x", " | ", bold(y), ")")))
plot4 <- ggplot(as.data.frame(res2$marginals.fixed$x4)) + 
  geom_line(aes(x = x, y = y)) +
  ylab (expression(paste(pi, "(", "x", " | ", bold(y), ")")))
plot7 <- ggplot(as.data.frame(res2$marginals.fixed$x8)) + 
  geom_line(aes(x = x, y = y)) +
  ylab (expression(paste(pi, "(", "x", " | ", bold(y), ")")))
plot8 <- ggplot(as.data.frame(res2$marginals.fixed$x9)) + 
  geom_line(aes(x = x, y = y)) +
  ylab (expression(paste(pi, "(", "x", " | ", bold(y), ")")))
plot9 <- ggplot(as.data.frame(res2$marginals.fixed$x10)) + 
  geom_line(aes(x = x, y = y)) +
  ylab (expression(paste(pi, "(", "x", " | ", bold(y), ")")))
plot10 <- ggplot(as.data.frame(res2$marginals.fixed$x11)) + 
  geom_line(aes(x = x, y = y)) +
  ylab (expression(paste(pi, "(", "x", " | ", bold(y), ")")))

# Posterior of precision
plott <- ggplot(as.data.frame(res2$marginals.hyperpar[[1]])) + 
  geom_line(aes(x = x, y = y)) +
  ylab (expression(paste(pi, "(", tau, " | ", bold(y), ")")))

grid.arrange(plot1, plot2,plot3, plot4, plot7, plot8,
             plot9, plot10, plott, nrow = 4)

grid.arrange(plot1, plot2, nrow = 2)
grid.arrange(plot3, plot4, nrow = 2)
grid.arrange(plot6, plot8, nrow = 2)
grid.arrange(plot9, plot10, nrow = 2)




index.pred2 <- inla.stack.index(join.stack2, "pred")$data

spde.est2 <- inla.spde2.result(inla = res2, name = "spatial.field",
                              spde = spde2, do.transf = TRUE)



pred_mean2 <- res$summary.fitted.values[index.pred2, "mean"]
pred_sd2 <- res$summary.fitted.values[index.pred2, "sd"]


pal_t <- colorNumeric(palette="viridis",  domain=dtt$price,
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
             color = ~ pal_t(pred_mean2))%>%
  addScaleBar(position = c("bottomleft")) %>%
  addLegend("bottomright",
            pal = pal_t, values = pred_mean2,
            title = "PriceT")

r = res2$residuals$deviance.residuals
plot(r)

plot(pred_mean2 - dtt$price)

err = 10^(sum((pred_mean2 - dtt$price)^2) / dim(dtt)[1])


#Tentatuvo 3
# alpha = 2


spde3 <- inla.spde2.matern(mesh = mesh, alpha = 2, constr = TRUE)

indexs3 <- inla.spde.make.index("s", spde3$n.spde)
lengths(indexs3)

A3 <- inla.spde.make.A(mesh = mesh, loc = coo[sample,])
Ap3 <- inla.spde.make.A(mesh = mesh, loc = coo[!sample,])

stk.e3 <- inla.stack(
  tag = "est",
  data = list(y = dt$price),
  A = list(1, A3),
  effects = list(data.frame(b0 = 1, x1 = dt$Overall.Qual, x2 = dt$Garage.Area, x3=dt$Total.Bsmt.SF,
                            x4 = dt$X1st.Flr.SF,  x7=dt$Mas.Vnr.Area,
                            x8=dt$TotRms.AbvGrd, x9=dt$Fireplaces, x10=dt$Wood.Deck.SF, x11=dt$age, 
                            x12=dt$dummy1_lc, x13=dt$dummy2_lc, x14=dt$dummy3_lc, x15=dt$dummy2_con, 
                            x16=dt$dummy3_con, x17=dt$dummy4_con), s = indexs3)
)

stk.p3 <- inla.stack(
  tag = "pred",
  data = list(price = NA),
  A = list(1, Ap3),
  effects = list(data.frame(b0 = 1, x1 = dtt$Overall.Qual, x2 = dtt$Garage.Area, x3=dtt$Total.Bsmt.SF,
                            x4 = dtt$X1st.Flr.SF, x7=dtt$Mas.Vnr.Area,
                            x8=dtt$TotRms.AbvGrd, x9=dtt$Fireplaces,x10=dtt$Wood.Deck.SF, x11=dtt$age,
                            x12=dtt$dummy1_lc, x13=dtt$dummy2_lc, x14=dtt$dummy3_lc, x15=dtt$dummy2_con, 
                            x16=dtt$dummy3_con, x17=dtt$dummy4_con),
                 s = indexs3
  )
)

join.stack3 <- inla.stack(stk.e3, stk.p3)

formula3 <- y ~ 0 + b0 + x1 + x2 +x3 +x4 +x7 +x8 +x9 + x10 + x11 + x12 +
  x13 + x14 + x15 +x16 +x17 + f(s, model = spde)

res3 <- inla(formula3,
             family = "gaussian",
             data = inla.stack.data(join.stack3),
             control.predictor=list(A = inla.stack.A(join.stack3), compute = TRUE), verbose=TRUE,
             control.compute  =list(cpo = TRUE, dic = TRUE, waic = TRUE, config=TRUE),
             control.fixed=list(expand.factor.strategy="model.matrix")
)
summary(res3)
# alpha=2, gaussian family, mesh: 0.01, 0.05, 0.0001 -> DIC: -6641.52, WAIC -> -5852.16
# alpha=2, gaussian family, mesh: 0.01, 0.05, 0.0001 -> DIC: -6312.23, WAIC -> -5239.11


plot1 <- ggplot(as.data.frame(res3$marginals.fixed$x1)) + 
  geom_line(aes(x = x, y = y)) +
  ylab (expression(paste(pi, "(", "x", " | ", bold(y), ")")))
plot2 <- ggplot(as.data.frame(res3$marginals.fixed$x2)) + 
  geom_line(aes(x = x, y = y)) +
  ylab (expression(paste(pi, "(", "x", " | ", bold(y), ")")))
plot3 <- ggplot(as.data.frame(res3$marginals.fixed$x3)) + 
  geom_line(aes(x = x, y = y)) +
  ylab (expression(paste(pi, "(", "x", " | ", bold(y), ")")))
plot4 <- ggplot(as.data.frame(res3$marginals.fixed$x4)) + 
  geom_line(aes(x = x, y = y)) +
  ylab (expression(paste(pi, "(", "x", " | ", bold(y), ")")))
plot7 <- ggplot(as.data.frame(res3$marginals.fixed$x8)) + 
  geom_line(aes(x = x, y = y)) +
  ylab (expression(paste(pi, "(", "x", " | ", bold(y), ")")))
plot8 <- ggplot(as.data.frame(res3$marginals.fixed$x9)) + 
  geom_line(aes(x = x, y = y)) +
  ylab (expression(paste(pi, "(", "x", " | ", bold(y), ")")))
plot9 <- ggplot(as.data.frame(res3$marginals.fixed$x10)) + 
  geom_line(aes(x = x, y = y)) +
  ylab (expression(paste(pi, "(", "x", " | ", bold(y), ")")))
plot10 <- ggplot(as.data.frame(res3$marginals.fixed$x11)) + 
  geom_line(aes(x = x, y = y)) +
  ylab (expression(paste(pi, "(", "x", " | ", bold(y), ")")))

# Posterior of precision
plott <- ggplot(as.data.frame(res3$marginals.hyperpar[[1]])) + 
  geom_line(aes(x = x, y = y)) +
  ylab (expression(paste(pi, "(", tau, " | ", bold(y), ")")))

grid.arrange(plot1, plot2,plot3, plot4, plot7, plot8,
             plot9, plot10, plott, nrow = 4)

grid.arrange(plot1, plot2, nrow = 2)
grid.arrange(plot3, plot4, nrow = 2)
grid.arrange(plot6, plot8, nrow = 2)
grid.arrange(plot9, plot10, nrow = 2)




index.pred3 <- inla.stack.index(join.stack3, "pred")$data

spde.est3 <- inla.spde2.result(inla = res3, name = "spatial.field",
                               spde = spde3, do.transf = TRUE)



pred_mean3 <- res$summary.fitted.values[index.pred3, "mean"]
pred_sd3 <- res$summary.fitted.values[index.pred3, "sd"]


pal_t <- colorNumeric(palette="viridis",  domain=dtt$price,
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
             color = ~ pal_t(pred_mean3))%>%
  addScaleBar(position = c("bottomleft")) %>%
  addLegend("bottomright",
            pal = pal_t, values = pred_mean3,
            title = "PriceT")

r = res3$residuals$deviance.residuals
plot(r)

plot(pred_mean3 - dtt$price)

err = 10^(sum((pred_mean3 - dtt$price)^2) / dim(dtt)[1])
err
