############################################################################
# Bioramble
# PCA - Part 3: In the trenches
# by Jesse Lipp
# Aug 19, 2015
############################################################################

# --------------------------------------------------------------------------
# Set up environment
# --------------------------------------------------------------------------
# clean-up
rm(list = ls())

# load libraries (install if needed)
if (!require(RCurl)) {
  install.packages("RCurl")
  library(RCurl)
}
if (!require(fields)) {
  install.packages("fields")
  library(fields)
}
if (!require(stringr)) {
  install.packages("stringr")
  library(stringr)
}
if (!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
}
if (!require(reshape2)) {
  install.packages("reshape2")
  library(reshape2)
}

# ----------------------------------------------------------
# Load data
# ----------------------------------------------------------
# data is a subset of the "Human Activity Recognition Using Smartphones Data Set"
# available at the UCI Machine Learning Repository
# https://archive.ics.uci.edu/ml/machine-learning-databases/00240/UCI%20HAR%20Dataset.zip

# the R script used to prepare the data is available on Github
# https://github.com/bioramble/pca/blob/master/pca_part3_data_preparation.R

# read data from Github
measurements <- read.table(text = getURL("https://raw.githubusercontent.com/bioramble/pca/master/pca_part3_measurements.txt"))
description <- read.table(text = getURL("https://raw.githubusercontent.com/bioramble/pca/master/pca_part3_description.txt"))

# ----------------------------------------------------------
# Step 1: Explore the data
# ----------------------------------------------------------
# what are the dimensions of the data?
dim(measurements)
# what type of data are the features?
table(sapply(measurements, class))
# are there missing values?
any(is.na(measurements))

# feature correlation before PCA
cor_m <- cor(measurements, method = "pearson")
# use only upper triangular matrix to avoid redundancy
upt_m <- cor_m[upper.tri(cor_m)]
# set up plotting area
par(mfrow = c(1, 2))
# plot correlations as histogram
hist(upt_m, prob = TRUE, 
     # scaling
     xlim = c(-1, 1), breaks = 40, 
     # color
     col = "grey70", 
     # labels
     xlab = "Feature Correlation", main = "", )
# create green-blue-yellow color scheme
greenblueyellow <- colorRampPalette(c("chartreuse", "blue", "yellow"))
# plot correlations as image
image.plot(cor_m, 
           # scaling
           axes = FALSE, 
           #color, 
           col = greenblueyellow(40), 
           # legend
           legend.shrink = 0.3, legend.mar = 7.5)

# ----------------------------------------------------------
# Step 2: Run PCA
# ----------------------------------------------------------
# features appear to be already centered & scaled but it can't hurt to do it again
pc <- prcomp(measurements, retx = TRUE, center = TRUE, scale. = TRUE)

# feature correlation after PCA
cor_r <- cor(pc$x, method = "pearson")
# use only upper triangular matrix to avoid redundancy
upt_r <- cor_r[upper.tri(cor_r)]
# set up plotting area
par(mfrow = c(1, 2))
# plot correlations as histogram
hist(upt_r, prob = TRUE, 
     # scaling
     xlim = c(-1, 1), ylim = c(0, 20), breaks = 40, 
     # color
     col = "grey70", 
     # labels
     xlab = "Feature Correlation", main = "", )
# plot correlations as image
image.plot(cor_r, 
           # scaling
           axes = FALSE, 
           #color, 
           col = greenblueyellow(40), 
           # legend
           legend.shrink = 0.3, legend.mar = 7.5)

# ----------------------------------------------------------
# Step 3: Interpret the results
# ----------------------------------------------------------
# draw a scree plot
par(mfrow = c(1, 1))
screeplot(pc, npc = 10, type = "l", main = "")

# show number of components to capture a given amount of variance
# calculate explained variance as cumulative sum
# sdev are the square roots of the variance
var_expl <- cumsum(pc$sdev^2) / sum(pc$sdev^2)
# plot explained variance
plot(c(0, var_expl), type = "l", lwd = 2, ylim = c(0, 1), 
     xlab = "Principal Components", ylab = "Variance explained")
# plot number of components needed to for common cut-offs of variance explained
vars <- c(0.8, 0.9, 0.95, 0.99)
for (v in vars) {
  npc <- which(var_expl > v)[1]
  lines(x = c(0, npc, npc), y = c(v, v, 0), lty = 3)
  text(x = npc, y = v - 0.05, labels = npc, pos = 4)
  points(x = npc, y = v)
}

# investigate separation of activites along first 8 PCs
# simple version using base R graphics
par(mfrow = c(2, 2))
for(p in seq(1, 8, by = 2)) {
  plot(pc$x[, p:(p+1)], pch = 16, col = as.numeric(description$activity_name))
}

# nicer version using ggplot2 graphics
proj <- pc$x[, 1:8]
# convert data into long form
proj <- melt(proj, value.name = "weight", varnames = c("id", "pc"))
proj$pc <- as.integer(str_replace(proj$pc, "PC", ""))
# plot odd-numbered PCs on x-axis and even-numbered PCs on y-axis
proj$coord <- ifelse(proj$pc %% 2 == 1, "x", "y")
# define groups of PCs to plot against each other
proj$group <- round(proj$pc / 2 + 0.1)
proj$group <- paste0("PC", (2*proj$group)-1, " vs. PC", 2*proj$group)
# reformat to have x and y column
proj <- dcast(proj, id + group ~ coord, value.var = "weight")
# add activity information
proj$Activity <- tolower(description$activity_name[proj$id])
# plot data as 2x2 facets
ggplot(proj, aes(x = x, y = y, col = Activity)) + 
  # scatterplot
  geom_point() +
  scale_color_manual(values = c("black", "red", "green", "blue", "cyan", "magenta")) + 
  # facets
  facet_wrap( ~ group, ncol = 2) + 
  # labels
  xlab("Principal Component") + ylab("Principal Component") +
  # appearance
  theme_bw(base_size = 18) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        legend.key = element_blank(), 
        panel.border = element_rect(color = "black")) +
  guides(color = guide_legend(override.aes = list(size = 5)))

# ----------------------------------------------------------
# Addendum: Understanding "prcomp"
# ----------------------------------------------------------
# investigate relationship to SVD
# perform singular value decomposition on centered and scaled data
sv <- svd(scale(measurements))
# compare eigenvector matrices of "prcomp" and "svd"
# prcomp stores eigenvectors in "rotation"
rot <- pc$rotation
dimnames(rot) = NULL
# svd stores right eigenvectors in matrix "v"
v <- sv$v
# check if the two matrices are equal
all.equal(rot, v)

# relationship between singular values and eigenvalues
all.equal(sv$d^2/(nrow(sv$u)-1), pc$sdev^2)

# manual projection of data
all.equal(pc$x, scale(measurements) %*% pc$rotation)

# calculate cumulative explained variance
var_expl <- cumsum(pc$sdev^2) / sum(pc$sdev^2)
# how many components are needed to explain 90% of variance
# in our case: 52
npc <- which(var_expl > 0.9)[1]
# projection of original data preserving 90% of variance
y90 <- pc$x[, 1:npc]
# this is the same as matrix multiplication with the first 52 eigenvectors
all.equal(y90, scale(measurements) %*% pc$rotation[, 1:npc])
