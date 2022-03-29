###
# Project: Parks - Montes Post-Survey
# Data:    BRUVS, BOSS Habitat data
# Task:    Habitat modelling
# author:  Kingsley Griffin
# date:    Jan 22
##

library(reshape2)
library(mgcv)
library(ggplot2)
library(viridis)
library(raster)

# read in
habi  <- readRDS("data/tidy/merged_habitat.rds")                               # merged data from 'R/1_mergedata.R'
tifs  <- list.files("output/spatial_covariates/", "*.tif", full.names = TRUE)
preds <- stack(tifs)
preddf <- as.data.frame(preds, xy = TRUE, na.rm = TRUE)
# preddf$Depth <- preddf$Z * -1

# reduce predictor space to fit survey area
# preddf <- preddf[preddf$Depth > min(habi$Depth), ]
# preddf <- preddf[preddf$Depth < 200, ]
habisp <- SpatialPointsDataFrame(coords = cbind(habi$longitude.1, 
                                                habi$latitude.1), data = habi)
sbuff  <- buffer(habisp, 10000)

# # visualise patterns
# covs <- c("Depth", "slope", "roughness", "tpi", "tri", "detrended")             # all covariates
# habs <- c("kelps", "macroalgae", "sponge", "sand", "rock", "biogenic")          # all habitats
# habl <- habi[, colnames(habi) %in% c(covs, habs, "totalpts")]
# habl <- melt(habl, measure.vars = covs)
# habl <- melt(habl, measure.vars = habs)
# head(habl)
# colnames(habl) <- c("totalpts", "covariate", "value", "habitat", "count")
# ggplot(habl, aes(value, (count/totalpts) * 100)) + 
#   geom_point() + geom_smooth() + 
#   facet_grid(habitat ~ covariate, scales = "free")

# use formula from top model from '2_modelselect.R'
m_photic <- gam(cbind(photic.reef, Total.Sum - photic.reef) ~ 
                  s(layer_depth,     k = 5, bs = "cr")  + 
                  s(layer_detrended, k = 5, bs = "cr") + 
                  s(layer_roughness, k = 5, bs = "cr") +
                  s(layer_tpi, k = 5, bs = "cr"), 
                data = habi, method = "REML", family = binomial("logit"))
summary(m_photic)
gam.check(m_photic)
vis.gam(m_photic)

m_meso <- gam(cbind(mesophotic.reef, Total.Sum - mesophotic.reef) ~ 
                s(layer_depth,     k = 5, bs = "cr")  + 
                s(layer_detrended, k = 5, bs = "cr") + 
                s(layer_roughness, k = 5, bs = "cr") +
                s(layer_tpi, k = 5, bs = "cr"), 
              data = habi, method = "REML", family = binomial("logit"))
summary(m_meso)
gam.check(m_meso)
vis.gam(m_meso)

m_macro <- gam(cbind(biota.macroalgae, Total.Sum - biota.macroalgae) ~ 
                 s(layer_depth,     k = 5, bs = "cr")  + 
                 s(layer_detrended, k = 5, bs = "cr") + 
                 s(layer_roughness, k = 5, bs = "cr") +
                 s(layer_tpi, k = 5, bs = "cr"), 
          data = habi, method = "REML", family = binomial("logit"))
summary(m_macro)
gam.check(m_macro)
vis.gam(m_macro)

m_stony <- gam(cbind(biota.stony.corals, Total.Sum - biota.stony.corals) ~ 
                s(layer_depth,     k = 5, bs = "cr")  + 
                s(layer_detrended, k = 5, bs = "cr") + 
                s(layer_roughness, k = 5, bs = "cr"), 
              data = habi, method = "REML", family = binomial("logit"))
summary(m_stony)
gam.check(m_stony)
vis.gam(m_stony)

m_rock <- gam(cbind(biota.consolidated, Total.Sum - biota.consolidated) ~ 
                s(layer_depth,     k = 5, bs = "cr")  + 
                s(layer_detrended, k = 5, bs = "cr") + 
                s(layer_roughness, k = 5, bs = "cr") +
                s(layer_tpi, k = 5, bs = "cr"), 
              data = habi, method = "REML", family = binomial("logit"))
summary(m_rock)
gam.check(m_rock)
vis.gam(m_rock)

m_sand <- gam(cbind(biota.unconsolidated, Total.Sum - biota.unconsolidated) ~ 
                s(layer_depth,     k = 5, bs = "cr")  + 
                s(layer_detrended, k = 5, bs = "cr") + 
                s(layer_roughness, k = 5, bs = "cr") +
                s(layer_tpi, k = 5, bs = "cr"), 
              data = habi, method = "REML", family = binomial("logit"))
summary(m_sand)
gam.check(m_sand)
vis.gam(m_sand)

# predict, rasterise and plot
preddf <- cbind(preddf, 
                "pcoral" = predict(m_stony, preddf, type = "response"),
                "pmacro" = predict(m_macro, preddf, type = "response"),
                "pmeso" = predict(m_meso, preddf, type = "response"),
                "pphotic" = predict(m_photic, preddf, type = "response"),
                "prock" = predict(m_rock, preddf, type = "response"),
                "psand" = predict(m_sand, preddf, type = "response"))

prasts <- rasterFromXYZ(preddf, res = c(30, 30))
prasts$dom_tag <- which.max(prasts[[6:11]])
plot(prasts[[6:12]])

# categorise by dominant tag
preddf$dom_tag <- apply(preddf[10:13], 1,
                        FUN = function(x){names(which.max(x))})
preddf$dom_tag <- sub('.', '', preddf$dom_tag)
head(preddf)

# subset to 10km from sites only
sprast <- mask(prasts, sbuff)
sprast <- sprast[[6:12]]
plot(sprast)

# write to tifs to reduce file sizes for git
writeRaster(sprast, "output/spatial_predictions/layer.tif", 
            bylayer = TRUE, suffix = names(sprast), overwrite = TRUE)

# # tidy and output data - not keeping this, too big for git
# spreddf         <- as.data.frame(sprast, xy = TRUE, na.rm = TRUE)
# spreddf$dom_tag <- (names(spreddf)[8:13])[spreddf$dom_tag]
# 
# # saveRDS(preddf, "output/broad_habitat_predictions.rds")  
# saveRDS(spreddf, "output/site_habitat_predictions.rds")
