library(tidyverse)
library(pheatmap)
library(viridisLite)
library(ggrepel)

library(pls)
library(TInPosition)
library(tidymodels)


require(devtools)
if(!require(GSVD)){
  devtools::install_github("derekbeaton/gsvd")
}
if(!require(GPLS)){
  devtools::install_github("derekbeaton/gpls",subdir = "Package")
}


## some functions, too.
source('./R/baby_plss.R')