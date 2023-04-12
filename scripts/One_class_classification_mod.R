

require(devtools)

#Had to install spatial.tools package from the "spatial.tools_1.6.2.tar.gz file before installing Oneclass."
#install.packages("C:/Users/giil6243/Downloads/spatial.tools_1.6.2.tar.gz", repos = NULL, type='source')

#install_github('benmack/oneClass')


library(oneClass)


suppressWarnings(suppressMessages(library(oneClass)))

suppressWarnings(suppressMessages(library(doParallel)))
suppressWarnings(suppressMessages(library(raster)))
suppressWarnings(suppressMessages(library(RColorBrewer)))

cl <- makeCluster(detectCores()-1)
doParallel:::registerDoParallel(cl)



# data(bananas)
# options(repr.plot.width=7, repr.plot.height=1.65)
# plot(stack(bananas$y,bananas$x), nc=3)

# load packages
library(sf) # Simple Features for R
library(rnaturalearth) # World Map Data from Natural Earth
library(here) # A Simpler Way to Find Your Files
library(stars) # Spatiotemporal Arrays, Raster and Vector Data Cubes
library(plyr)
library(dplyr) # A Grammar of Data Manipulation
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(ggnewscale) # Multiple Fill and Color Scales in 'ggplot2'
library(scico) # Colour Palettes Based on the Scientific Colour-Maps
library(geobgu) # install from GitHub ("michaeldorman/geobgu")
library(ggrepel) # Automatically Position Non-Overlapping Text Labels with 'ggplot2'
library(devtools)
library(raster)


require(rgdal)
require(sp)



wd = "D:/Carbon_dynamics/UAS-processing/Hm_box1/training"
setwd(wd)
input =  stack(paste0(wd,"/","testing/texture/no_date/","Heyman-10-05-22-8-1_all_layers.tif"))

chm = stack("chm_resampled.tif")

input = stack(input,chm)

#writeRaster(input,"all_layers_with_chm.tif")

seed <- 123456
# tr.x <- bananas$tr[, -1]
# tr.y <- puFactor(bananas$tr[, 1], positive=1)
# set.seed(seed)
# tr.index <- createFolds(tr.y, k=10, returnTrain=TRUE)
# set.seed(seed)
# te.i <- sample(ncell(bananas$y), 1000)
# te.x <- extract(bananas$x, te.i)
# te.y <- extract(bananas$y, te.i)

training = read.csv("training_random_pts.csv")

train_var = data.frame(raster::extract(input, training[,c(3,4)] ))


#tr.x <- train_var[, c(-1,-2)]
tr.x <- train_var
tr.y <- puFactor(training[, 2], positive=1)
set.seed(seed)
tr.index <- createFolds(tr.y, k=10, returnTrain=TRUE)
ocsvm.fit <- trainOcc(x=tr.x, y=tr.y, method="ocsvm", index=tr.index)
ocsvm.fit.def <- ocsvm.fit # store for comparison
#x = brick(input[[1]],input[[2]],input[[3]])


ocsvm.pred <- predict(ocsvm.fit, input)

ocsvm.bin <- ocsvm.pred>0

options(repr.plot.width=14, repr.plot.height=3.5)
par(mfrow=c(1, 3))
hist(ocsvm.fit, ocsvm.pred, th=0, noWarnRasHist=TRUE)
plot(ocsvm.pred, col=brewer.pal(9, "RdBu"))

dev.new(width=3, height=3)
plot(ocsvm.pred>1, col=brewer.pal(9, "RdBu")[c(2,9)])

# dev.new(width=2, height=1)
# plot(ocsvm.pred>0, col=brewer.pal(9, "RdBu")[c(2,9)])

terra::writeRaster(ocsvm.pred>1,"pred_great_1-4-5-2023.tif")


tuneGrid <- expand.grid( sigma = seq(.1, 2, .1), nu = seq(.05, .5, .05) )
ocsvm.fit <- trainOcc(x=tr.x, y=tr.y, method="ocsvm", tuneGrid=tuneGrid, index=tr.index)


ocsvm.pred2 <- predict(ocsvm.fit, input)


dev.new(width=2, height=1)
plot(ocsvm.pred2>1, col=brewer.pal(9, "RdBu")[c(2,9)])

hist(ocsvm.fit, ocsvm.pred2, th=0, noWarnRasHist=TRUE)


# options(repr.plot.width=6, repr.plot.height=5)
# featurespace(ocsvm.fit, th=0)
dev.new(width=2, height=1)
#options(repr.plot.width=4, repr.plot.height=3)
par(mfrow=c(1, 2))
# trellis.par.set(caretTheme()) # nice colors from caret
plot(ocsvm.fit.def, plotType="level") # see ?plot.train for other plot types
plot(ocsvm.fit, plotType="level")

head(ocsvm.fit$results)
sort(ocsvm.fit, digits=2, by = 'puAuc', rows = 1:10, cols =1:8)

# model 63 has the highest puAUC

ocsvm.fit2 <- update(ocsvm.fit, modRow=63)
ocsvm.pred3 <- predict(ocsvm.fit2, input)
options(repr.plot.width=14, repr.plot.height=3.5)
par(mfrow=c(1, 3))
hist(ocsvm.fit, ocsvm.pred, th=0, noWarnRasHist=TRUE)
#plot(ocsvm.pred, col=brewer.pal(9, "RdBu"))

dev.new(width=2, height=1)
plot(ocsvm.pred3>0, col=brewer.pal(9, "RdBu")[c(2,9)])

#####

bsvm.fit <- trainOcc(x=tr.x, y=tr.y, index=tr.index)

dev.new(width=2, height=1)

options(repr.plot.width=14, repr.plot.height=3.5)
for (rank in 1:4) {
  bsvm.fit <- update(bsvm.fit, modRank=rank, metric="puF")
  bsvm.pred <- predict(bsvm.fit, input)
  par(mfrow=c(1, 3))
  hist(bsvm.fit, bsvm.pred, th=0, ylim=c(0,.15), noWarnRasHist=TRUE)
  plot(bsvm.pred, col=brewer.pal(9, "RdBu"))
  plot(bsvm.pred>0, col=brewer.pal(9, "RdBu")[c(2,9)])
}


dev.new(width=2, height=1)
plot(bsvm.pred, col=brewer.pal(9, "RdBu")[c(2,9)])




############evaluations with true images##
true_y = raster("rec_seed.tif")

clip_x = crop(input,true_y)

writeRaster(clip_x, "X_raster_extract.tif")
writeRaster(true_y,"Y_raster_extract.tif")


true_y[is.na(true_y[])] <- 0 

clip_x[is.na(clip_x[])] <- 0 


te.i <- sample(ncell(true_y), 1000)
te.x <- extract(clip_x, te.i)
te.y <- extract(true_y, te.i)

bsvm.ev <- evaluateOcc(ocsvm.fit, te.u=te.x, te.y=te.y, positive=1)

options(repr.plot.width=7, repr.plot.height=3.5)
th.sel <- 0
th.opt <- slot(bsvm.ev, "t")[which.max(slot(bsvm.ev, "kappa"))]

dev.new(width=2, height=1)
hist(ocsvm.fit, ocsvm.pred3, ylim=c(0, 0.5), col="grey", border=NA, noWarnRasHist=TRUE)
plot(bsvm.ev, add=TRUE, yLimits=c(0, 0.5))
abline(v=c(th.opt, th.sel), lwd=2)

evaluateAtTh(bsvm.ev, th=0)
######################################################
#########################################################

options(repr.plot.width=7, repr.plot.height=2.5)
par(mfrow=c(1,3), mar=c(4.1, 4.1, 1.1, 1.1))
plot(tr.x[tr.y=="pos", ], pch=16 )
plot(tr.x, pch=ifelse(tr.y=="pos", 16, 4) )
plot(te.x, pch=ifelse(te.y==1, 16, 1) )


ocsvm.fit <- trainOcc(x=tr.x, y=tr.y, method="ocsvm", index=tr.index)
ocsvm.fit.def <- ocsvm.fit # store for comparison


ocsvm.pred <- predict(ocsvm.fit, bananas$x)

ocsvm.bin <- ocsvm.pred>0

options(repr.plot.width=14, repr.plot.height=3.5)
par(mfrow=c(1, 3))
hist(ocsvm.fit, ocsvm.pred, th=0, noWarnRasHist=TRUE)
plot(ocsvm.pred, col=brewer.pal(9, "RdBu"))
plot(ocsvm.pred>0, col=brewer.pal(9, "RdBu")[c(2,9)])

options(repr.plot.width=2, repr.plot.height=1)
featurespace(ocsvm.fit, th=0)


tuneGrid <- expand.grid( sigma = seq(.1, 2, .1), nu = seq(.05, .5, .05) )
ocsvm.fit <- trainOcc(x=tr.x, y=tr.y, method="ocsvm", tuneGrid=tuneGrid, index=tr.index)
ocsvm.pred <- predict(ocsvm.fit, bananas$x)


options(repr.plot.width=4, repr.plot.height=3)
trellis.par.set(caretTheme()) # nice colors from caret
plot(ocsvm.fit.def, plotType="level") # see ?plot.train for other plot types
plot(ocsvm.fit, plotType="level")


options(repr.plot.width=14, repr.plot.height=3.5)
par(mfrow=c(1, 3))
hist(ocsvm.fit, ocsvm.pred, th=0, noWarnRasHist=TRUE)
plot(ocsvm.pred, col=brewer.pal(9, "RdBu"))
plot(ocsvm.pred>0, col=brewer.pal(9, "RdBu")[c(2,9)])



