
# Cuda ------------------------------------------------------------------
#install.packages(gpuR)
# devtools::install_github('cdeterman/gpuR', ref = 'develop')
#library(pryr)
library("gpuR")
library(pryr)
A <- seq.int(from=0, to=999)
B <- seq.int(from=1000, to=1)
gpuA <- gpuVector(A)
gpuB <- gpuVector(B)
C <- A + B
gpuC <- gpuA + gpuB

all(C == gpuC)
vignette("gpuR")

