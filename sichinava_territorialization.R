library(Cairo)
library(rgdal)
library(sp)
library(rgeos)
library(spdep)
library(maptools)
library(ggplot2)
library(nlme)
library(plyr)
library(dplyr)
library(gridExtra)
library(grid)
library(ggsn)
library(extrafont)
library(scales)
library(plm)
library(classInt)
library(RColorBrewer)
library(stargazer)


setwd("D:\\Dropbox\\My Projects\\Elections\\EGE article\\data\\repo")

### Source data processing script 
source('src/read.data.r')

###
source('src/lisa.r')

source('src/ols.r')


