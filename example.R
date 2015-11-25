#--- EXAMPLE ---#
# VERSION NOV 25 2015
# CHRISTIAN DANNE 
# DANNEC@TCD.IE 
#
# NOTES:
 
# Graph types:
# type=1 -> trace plots
# type=2-> autocorrelation plots
# type=3 -> density plots (plots the density of the entire MCMC sample against the 1st half and the second half of the sample)

#--- PREAMBLE ---#
rm(list = ls())
set.seed(12345)

#--- LIBRARIES ---#
# library("foreign")
library("grDevices")
library("ggplot2")
library("BMR")
library("grid")
library("gridExtra")

#--- READ FUNCTION ---# (change path to where you have stored "trace.R")
source("/home/christian/Mods and Macro/Data/trace.R", echo=T)

#--- READ DATA ---#
data(BMRVARData)

#--- MINNESOTA PRIOR ---#
prior<-c(0.9,0.95,0.95)
testbvarm <- BVARM(USMacroData[,2:4],prior,p=4,constant=T,irf.periods=20, keep=10000,burnin=5000,VType=1,  HP1=0.5,HP2=0.5,HP3=10)

plottrace.BVARM(testbvarm ,type=2, save=F)

#--- STEADY STATE PRIOR ---#
mycfp = c(0.9,0.95,0.95)
mypsi = c(3,6,5)
testbvars <- BVARS(USMacroData[,2:4],mypsi,mycfp,p=4,irf.periods=20,  keep=10000,burnin=5000,XiPsi=1,HP1=0.5,HP4=2,gamma=NULL)

plottrace.BVARS(testbvars ,type=2, save=F)

#--- WISHART PRIOR ---#
testbvarw <- BVARW(USMacroData[,2:4],cores=1,c(0.9,0.95,0.95),p=4,constant=T,  irf.periods=20,keep=10000,burnin=5000, XiBeta=4,XiSigma=1,gamma=4)

plottrace.BVARW(testbvarw ,type=2, save=F)


#--- END OF SCRIPT ---#
