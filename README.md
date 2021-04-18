# dipr
Digital Image Processing with R

install.packages("BiocManager")
BiocManager::install("EBImage")
library(EBImage)
install.packages("devtools")
library(devtools)
install.Rtools() # only for Windows
devtools::install_github("tkatsuki/dipr")
library(dipr)
