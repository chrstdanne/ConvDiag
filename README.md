# ConvDiag
Graphical convergence diagnostics for BMR

#--- READ ME ---#
VERSION NOV 25 2015 
CHRISTIAN DANNE
DANNEC@TCD.IE

#--- DESCRIPTION ---#
The function provides some graphic diagnostic tools (trace plots, autocorrelation plots, and density plots) for BVARs of the BMR package by Keith O'Hara, available at http://bayes.squarespace.com/bmr/. You will have to install the packages listed below. In order to use the functions in combination with other packages change the corresponding lines that reads in the object containing the results of the VAR estimation in "trace.R".

#--- CONTENTS ---#
example.R: runs an example 
trace.R: contains the main functions. 

#--- USE ---#
To run the example code, change the path to where you have saved "trace.R" in the example file. 
Change "type=c(1,2,3)" in plottrace.*** for the different types of plot functions. 
 
# Graph types:
# type=1 -> trace plots
# type=2-> autocorrelation plots
# type=3 -> density plots (plots the density of the entire MCMC sample against the 1st half and the second half of the sample)
