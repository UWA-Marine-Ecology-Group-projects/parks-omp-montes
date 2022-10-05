###
# Project: Parks - Montes
# Data:    BRUVS, BOSS Habitat data
# Task:    Model selection 
# author:  adapted from @beckyfisher/FSSgam
# date:    Jan 22

# Part 1-FSS modeling----

# libraries----
# detach("package:plyr", unload=TRUE)#will error - don't worry
library(tidyr)
library(dplyr)
# options(dplyr.width = Inf) #enables head() to display all coloums
library(mgcv)
library(MuMIn)
library(car)
library(doBy)
library(gplots)
library(RColorBrewer)
library(doParallel) #this can removed?
library(doSNOW)
library(gamm4)
library(RCurl) #needed to download data from GitHub
library(reshape2)

rm(list=ls())

# install fssgam package----
# devtools::install_github("beckyfisher/FSSgam_package") #run once
library(FSSgam)

# Bring in and format the data----
habi <- readRDS("data/tidy/broad_merged_habitat.rds")                           # merged data from 'R/1_mergedata.R'
colnames(habi)
habi <- melt(habi, measure.vars = c(9, 12, 15, 16, 33, 34))                     # collect all taxa tags for univariate stats

# Set predictor variables---
pred.vars <- c("depth_ga","detrended", 
               "roughness", "tpi") 

# Check for correlation of predictor variables- remove anything highly correlated (>0.95)---
round(cor(habi[ , pred.vars]), 2)
# dropping slope but should try trading slope and roughness

# # Review of individual predictors for even distribution---
# # Plot of likely transformations - Anna Cresswell loop
# par(mfrow = c(3, 2))
# for (i in pred.vars) {
#   x<-habi[ , i]
#   x = as.numeric(unlist(x))
#   hist((x))#Looks best
#   plot((x), main = paste(i))
#   hist(sqrt(x))
#   plot(sqrt(x))
#   hist(log(x + 1))
#   plot(log(x + 1))
# }
# 
# review and create cols for best transforms
habi <- habi %>%
  # mutate(logdepth = log(Depth)) %>%
  # # mutate(sqrttri = sqrt(tri)) %>%
  # mutate(sqrtrough = sqrt(roughness)) %>%
  # mutate(sqrtslope = sqrt(slope)) %>%
  rename(Taxa = variable) %>%
  rename(response = value)

# # Re-set the predictors for modeling----
# pred.vars <- c("Depth","roughness", "tpi", "detrended") # slope and roughness had high cov

# Check to make sure Response vector has not more than 80% zeros----
unique.vars     <- unique(as.character(habi$Taxa))
# unique.vars.use <- character()
# for(i in 1:length(unique.vars)){
#   temp.dat <- habi[which(habi$Taxa == unique.vars[i]),]
#   if(length(which(temp.dat$response == 0)) / nrow(temp.dat) < 0.8){
#     unique.vars.use <- c(unique.vars.use, unique.vars[i])}
# }
unique.vars.use <- unique.vars
# unique.vars.use <- unique.vars.use[11:12] # only macroalgae and sponge of interest. remove unknown, open water
# unique.vars.use     

# Run the full subset model selection----
outdir    <- ("output/fssgam-habitat_broad/") 
resp.vars <- unique.vars.use
use.dat   <- habi[habi$Taxa %in% c(unique.vars.use), ]
# factor.vars <- c("Status")# Status as a Factor with two levels
out.all <- list()
var.imp <- list()
name <- "montebello.synthesis"

# Loop through the FSS function for each Taxa----
for(i in 1:length(resp.vars)){
  use.dat <- habi[habi$Taxa == resp.vars[i],]
  # use.dat$Site <- as.factor(use.dat$Site)
  Model1  <- gam(cbind(response, (Total.Sum - response)) ~ s(depth_ga, bs = 'cr'),
                 family = binomial(link = "logit"),  data = use.dat)
  
  model.set <- generate.model.set(use.dat = use.dat,
                                  test.fit = Model1,
                                  pred.vars.cont = pred.vars,
                                  # pred.vars.fact=factor.vars,
                                  # linear.vars="Distance",
                                  # cyclic.vars = c("aspect"),
                                  max.predictors = 4,
                                  k = 5,
                                  cov.cutoff = 0.7
                                  # null.terms = "s(Site, bs='re')"
  )
  out.list <- fit.model.set(model.set,
                            max.models = 600,
                            parallel = T)
  names(out.list)
  
  out.list$failed.models # examine the list of failed models
  mod.table <- out.list$mod.data.out  # look at the model selection table
  mod.table <- mod.table[order(mod.table$AICc), ]
  mod.table$cumsum.wi <- cumsum(mod.table$wi.AICc)
  out.i     <- mod.table[which(mod.table$delta.AICc <= 2), ]
  out.all   <- c(out.all, list(out.i))
  var.imp   <- c(var.imp, list(out.list$variable.importance$aic$variable.weights.raw))
  
  # plot the best models
  for(m in 1:nrow(out.i)){
    best.model.name <- as.character(out.i$modname[m])
    
    png(file = paste(outdir, m, resp.vars[i], "mod_fits.png", sep = "_"))
    if(best.model.name != "null"){
      par(mfrow = c(3, 1), mar = c(9, 4, 3, 1))
      best.model = out.list$success.models[[best.model.name]]
      plot(best.model, all.terms = T, pages = 1, residuals = T, pch = 16)
      mtext(side = 2, text = resp.vars[i], outer = F)}  
    dev.off()
  }
}

# Model fits and importance---
names(out.all) <- resp.vars
names(var.imp) <- resp.vars
all.mod.fits <- do.call("rbind", out.all)
all.var.imp  <- do.call("rbind", var.imp)
write.csv(all.mod.fits[ , -2], file = paste(outdir, name, "all.mod.fits.csv", sep = "_"))
write.csv(all.var.imp,         file = paste(outdir, name, "all.var.imp.csv", sep = "_"))

# out.all
#  # Generic importance plots- - unsure why we're not getting any value for the other preds. internal m.cor exclusion?
# heatmap.2(all.var.imp, notecex = 0.4,  dendrogram = "none",
#           col = colorRampPalette(c("white", "yellow", "red"))(10),
#           trace = "none", key.title = "", keysize = 2,
#           notecol = "black", key = T,
#           sepcolor = "black", margins = c(20, 20), lhei = c(2, 6), Rowv = FALSE, Colv = FALSE)
