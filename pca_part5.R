############################################################################
# Bioramble
# PCA - Part 5: Eigenpets
# by Jesse Lipp
# Sep 1, 2015
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

# ----------------------------------------------------------
# Step 1: Prepare data
# ----------------------------------------------------------
# read data from Github
cats <- read.table(text = getURL("https://raw.githubusercontent.com/bioramble/pca/master/cat.csv"), sep = ",")
dogs <- read.table(text = getURL("https://raw.githubusercontent.com/bioramble/pca/master/dog.csv"), sep = ",")
# combine cats and dogs into single data frame
pets <- cbind(cats, dogs)

# check dimensions
dim(pets)
# check types of features
table(apply(pets, 2, class))
# check for missing values
any(is.na(pets))

# compute "average" pet
pet0 <- rowMeans(pets)
cat0 <- rowMeans(pets[, 1:80])
dog0 <- rowMeans(pets[, 81:160])

# what does the "average" pet look like?
# set up plotting area
par(mfrow = c(1, 3), mar = rep(0.5, 4))
# create grey scale color map
greys <- gray.colors(256, start = 0, end = 1)
# convenience function to plot pets
show_image <- function(v, n = 64, col = greys) {
  # transform image vector back to matrix
  m <- matrix(v, ncol = n, byrow = TRUE)
  # invert columns to obtain right orientation
  # plot using "image"
  image(m[, nrow(m):1], col = col, axes = FALSE)
}
# plot average pets
for (i in list(pet0, cat0, dog0)) {
  show_image(i)
}

# ----------------------------------------------------------
# Step 2: Run PCA
# ----------------------------------------------------------
# subtract average pet
pets0 <- pets - pet0
# run pca
pc <- prcomp(pets0, center = FALSE, scale. = FALSE)

# obtain unscaled eigenvectors of eigenfaces
u_unscaled <- as.matrix(pets0) %*% pc$rotation
# this turns out to be the same as the projection of the data stored in "pc$x"
all.equal(u_unscaled, pc$x)

# singular value decomposition of data
sv <- svd(pets0)
# left eigenvalues are stored in matrix "u"
u <- sv$u

# show equality of u_unscaled and u
library(MASS)
d <- sqrt(pc$sdev^2 * (nrow(pets0) - 1))
all.equal(d, sv$d)
u_pc <- pc$x %*% ginv(diag(d))
all.equal(u_pc, sv$u, tolerance = 1e-2)

# display first 6 eigenfaces
par(mfrow = c(1, 6), mar = rep(0.5, 4))
for (i in seq(6)) {
  show_image(pc$x[, i])
}

# ----------------------------------------------------------
# Step 3: Image reconstruction
# ----------------------------------------------------------
# a number of variance cut-offs
vars <- c(0.2, 0.5, 0.8, 0.9, 0.98, 1)
# calculate cumulative explained variance
var_expl <- cumsum(pc$sdev^2) / sum(pc$sdev^2)
# get the number of components that explain a given amount of variance
npc <- sapply(vars, function(v) which(var_expl >= v)[1])
par(mfrow = c(4, 6), mar = rep(0.5, 4))
# reconstruct four cats and four dogs
for (i in seq(79, 82)) {
  for (j in npc) {
    # project data using "j" principal components
    r <- pc$x[, 1:j] %*% t(pc$rotation[, 1:j])
    show_image(r[, i])
    text(x = 0.01, y = 0.05, pos = 4, labels = paste("v =", round(var_expl[j], 2)))
    text(x = 0.99, y = 0.05, pos = 2, labels = paste("pc =", j))
  }
}
