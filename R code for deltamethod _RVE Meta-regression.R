

library(knitr);library(vctrs)
library(carData);library(car)
library(metafor)


#  Simple case Linear model
#  Scaling matches the more complex derivation at the mean
#  Evaluation at other points more complex
#  CIs not strictly symmetric

# Check on how important the approximation is;

#  Evaluated at the mean, zero correlation so should work

# Var(Eta) = Eta^2 x [ var(Y-hat)/Y-hat-bar^2 + Var(b-hat)/b-hat^2]# the variance using delta method

#  Use original paper results
Eta = -.3622
var.Y.hat = .4136 # SE
Y.hat.bar  = 51.843
var.b.hat = 0.7322^2
b.hat = 2.259

Var.Eta = Eta^2 * ((var.Y.hat/Y.hat.bar^2) + (var.b.hat/b.hat^2) )
Var.Eta^.5


#  Approximate SE

slope = -2.259
slope.SE = .7322
mean.P = 8.31
mean.Q = 51.843

# Extract the elasticity estimates 

slope*(mean.P/mean.Q)
# -0.3620988

slope.SE*(mean.P/mean.Q)
# 0.1173655

# So results match.  Also confirmed with H & L paper

#  Now start to collect information

#######################################
# Study 1
# Howe, C. W., & Linaweaver Jr, F. P. (1967). The impact of price on residential water demand and its relation to system design and price structure. Water Resources Research, 3(1), 13-32.

# All final models are based on author selected models

# Domestic dwelling inhouse use
# Linear model to get the elasticity and SE
#  Regression values Table 1# table 2?
#  Quantities for the 
SE=.339
d = .339^2# SE of the paper--for price?
est = -1.30# regression coefficient of the paper
names(est) <- c("b1")
# at means
P = 40.1 # sum of water & sewer charges, vary with usage
Q = 226# average quantity demadned, annually
  
deltaMethod(est, "b1*(P/Q)", vcov.=d)

#                   Estimate      SE     2.5 %    97.5 %
#  b1 * (40.1/226) 0.2306637 0.06015 0.1127719 0.3485556

SE*(P/Q)
# Note at the means this application = the same as direct scaling
# of the raw data

#  Income elasticity domestic indoor

#  Note income proxied by house price
#  big qualification

d = .585^2# se^2
est = 3.47
names(est) <- c("b1")
# at means
M = 20.8 # market value of houses in $1000s
Q = 226

deltaMethod(est, "b1*(M/Q)", vcov.=d)
#               Estimate         SE    2.5 %    97.5 %
#  b1 * (M/Q) 0.3193628 0.05384071 0.213837 0.4248887

#  People (household-size) elasticity
## Table 3
# 21 B

d = 12.2^2
est = -3.89
names(est) <- c("b1")
# at means
pop = 3.9
Q = 226

deltaMethod(est, "b1*(pop/Q)", vcov.=d)

# 13 B

d = 8.16^2
est = 29.1
names(est) <- c("b1")
# at means
pop = 3.7
Q = 236

deltaMethod(est, "b1*(pop/Q)", vcov.=d)

# 13 C

d = 8.46^2
est = 33.6
names(est) <- c("b1")
# at means
pop = 3.7
Q = 236

deltaMethod(est, "b1*(pop/Q)", vcov.=d)


#                  Estimate       SE      2.5 %    97.5 %
#  b1 * (pop/Q) -0.06712832 0.210531 -0.4797614 0.3455048


#  outdoor  demand dry area

#  Values can be taken directly from log log model
#  note log_10 for both sides gives the same interpretation and log_e

#  Dry Price, outdoor

#  elasticity = -0.703
#  SE .321

#  Dry Income, outdoor

#  elasticity = .429
#  SE .228

#  Wet Price, outdoor

#  elasticity = 1.57
# SE .190

#  Wet Income, outdoor

#  elasticity = 1.45
#  SE .306

##################################################################
# Study 2 

# Danielson, L. E. (1979). An analysis of residential demand for water using micro time‐series data. Water Resources Research, 15(4), 763-767.

# The elasticity estimates and associated SE values can be directly taken from the study
# All models are specified using double-log functional form

# study 3 
#  Billings (1982) 

#  Linear model
mean.Q = (26.3*-0.331)/-0.49 # where 26.3= mean.p, .331 = price coeff, -0.49= elasticity
mean.P = 26.3

mean.Q
mean.P
se = -0.331/3.2 # b/t, -0.33= price coeff, 3.2= t value
se
d = se^2
est = -0.331
names(est) <- c("b1")
deltaMethod(est, "b1*(mean.P/mean.Q)", vcov.=d)

##########

#  Study 4 
# Abrams et al. (2012). An econometric assessment of pricing Sydney’s residential water use. Economic Record, 88(280), 89-105.

# Getting the SE values for the model is difficult.  
# In this instance the pooled estimate is upwardly biased. 
# So take the weighted average estimates and apply the 
# Pooled SE to the weighted estimates.Seems similar
# probably a reasonable approximation

SE1 = abs((-0.009 -0.031))/(2*1.96) # find out why
SE1

SE2 = abs((-0.016 -0.053))/(2*1.96)# I take directly weighted average SR estimate
SE2

SE3 = abs((-0.027 -0.088))/(2*1.96)
SE3

SE4 = abs((-0.197 + 0.133))/(2*1.96)# for the LR the weighted average estimate as well
SE4

SE5 = abs((-0.338 + 0.228))/(2*1.96)
SE5

SE6 = abs((-0.562 + 0.381))/(2*1.96)
SE6

######
# Study 5
# Grafton et al. (2011). Determinants of residential water consumption: Evidence and analysis from a 10‐country household survey. Water Resources Research, 47(8).

#  Double log for price and income
#  For people take the average of the adult and child
#  SE is the same for both values
#  semi log so multiply by mean household pop
#  Children and adults not reported separately

mean.P= 1.770
mean.Q = 294 
mean.Person = 2.891   
semi.est = (.148+.082)/2
SE = .017
d= se^2

mean.Person*semi.est
mean.Person*SE
mean.P*SE
mean.P*semi.est


#############
# Study 6
# Hoffmann et al. (2006). Urban water demand with fixed volumetric charging in a large municipality: the case of Brisbane, Australia. Australian Journal of Agricultural and Resource Economics, 50(3), 347-359.

#  Note there is an aggregation bias issue in the all group results
#  So we need to use the separate results

#  The authors state a preference for the log-log results
#  The results are similar so probably no real value in working further

#  Owners long and short run price

d = diag(c(.084^2, .083^2),2,2)#SE values of the original study for SR & LR, respectively
d
z <- c(-.455, .486)# the coefficients for SR and LR values of the original study 
names(z) <- c("d1", "d2")
deltaMethod(z, "d1/(1-d2)", vcov.=d)
#  Longrun
#              Estimate        SE     2.5 %     97.5 %
# d1/(1 - d2) -0.885214 0.2171179 -1.310757 -0.4596708

#short-run
deltaMethod(z, "d1", vcov.=d)

#  Renters

d = diag(c(.099^2, .078^2),2,2)
d
z <- c(-.391, .502)
names(z) <- c("d1", "d2")
deltaMethod(z, "d1/(1-d2)", vcov.=d)
#  Longrun
#                Estimate        SE     2.5 %     97.5 %
#  d1/(1 - d2) -0.7851406 0.2337565 -1.243295 -0.3269863

#short
deltaMethod(z, "d1", vcov.=d)

#     Estimate    SE      2.5 %     97.5 %
#  d1   -0.391 0.099 -0.5850364 -0.1969636

#  Income effects

# direct from the paper

#  People effect

# direct from the paper

##  The linear results
# Price short linear
# Owners
d = 7.377^2 # SE of the paper
est = -59.888 # regression coefficienet of from the paper
names(est) <- c("b1")
# at means
P = .76
Q = 75.36

deltaMethod(est, "b1*(P/Q)", vcov.=d)
# owners short-run

#             Estimate        SE      2.5 %     97.5 %
#  b1 * (P/Q) -0.603966 0.0743965 -0.7497805 -0.4581516

#  Owners long run price
d = diag(c(0.0743965^2, .055^2),2,2)
d
z <- c(-0.603966, .616)
names(z) <- c("d1", "d2")
deltaMethod(z, "d1/(1-d2)", vcov.=d)

# Renters short run

d = 16.278^2 # SE of the paper
est = -34.165 # regression coefficienet of from the paper
names(est) <- c("b1")
# at means
P = .76
Q = 65.45

deltaMethod(est, "b1*(P/Q)", vcov.=d)

#               Estimate        SE         2.5 %        97.5 %
# b1 * (P/Q)     -0.3967212     0.1890188   -0.7671912  -0.02625113
#
#  Renters long run 
d = diag(c(0.1890188^2, 0.144^2),2,2)
d
z <- c(-0.3967212, 0.393)
names(z) <- c("d1", "d2")
deltaMethod(z, "d1/(1-d2)", vcov.=d)

#               Estimate        SE      2.5 %     97.5 %
 # d1/(1 - d2)  -0.6535769  0.3478639  -1.335378  0.02822379

# Income
# Owners
d = 7.377^2 # SE of the paper
est = -59.888 # regression coefficienet of from the paper
names(est) <- c("b1")
# at means
I = 12495.35
Q = 75.36

deltaMethod(est, "b1*(I/Q)", vcov.=d)

# Derive the full elasticity SE using the delta method

#  Assume the off diagonals are zero here.  I think this is plausible
#  but i am not 100% sure... any non-sero values will inflate SE
#  so maybe this is lower bound?????

# People (household-sze) Elasticity
#Owner-occupied
pop=2.57
Q=75.36
se = 1.317
se
d = se^2
est = 5.030
names(est) <- c("b1")

deltaMethod(est, "b1*(pop/Q)", vcov.=d)

##Renter households
pop=2.57
Q=65.45
se = 3.457
se
d = se^2
est = 10.685
names(est) <- c("b1")
deltaMethod(est, "b1*(pop/Q)", vcov.=d)

#  Create variance matrix based on data beta/t = SE, then square to get variance
#  First value is the relevant point estimate, or direct effect
#  Second value is the rho estimate.


d = diag(c((.021/4.0)^2,(.419/9.2)^2),2,2)
d

#  Create vector of point estimates (direct effect and rho) and give "names"
#  method seems to require this step.  not sure why.

z <- c(.021, .419)
names(z) <- c("d1", "d2")

# Estimate the relevant full marginal effect and SE

deltaMethod(z, "d1/(1-d2)", vcov.=d)

#               Estimate          SE
#  d1/(1 - d2) 0.03614458 0.009469927

###  check the result with direct scaling
#   With direct scaling, the t-stat will be unchanged, so t=4

# with delta t=0.03614458 / 0.009469927 = 3.8 
#  so it does not make much difference
#  maybe because the rho is quite precise.

# Further check, increase uncertaity for rho, so just stat sig


d = diag(c((.021/4.0)^2,(.419/2)^2),2,2)
d

#  Create vector of point estimates (direct effect and rho) and give "names"
#  method seems to require this step.  not sure why.

z <- c(.021, .419)
names(z) <- c("d1", "d2")

# Estimate the relevant full marginal effect and SE

deltaMethod(z, "d1/(1-d2)", vcov.=d)

#               Estimate         SE
# d1/(1 - d2) 0.03614458 0.01585926

#  yes t-values significantly.  So when rho only marginally significant,
#  the full effect is imprecise.  If rho estimated with precision then,
#  can largely ignore
####################


# Study 7
# Zaied and Binet (2015)

# price elasticity for lower block

mean.Q= 19.86
mean.P=0.39
se = -0.07/-1.70 # b/t
se
d = se^2
est = -0.07
names(est) <- c("b1")

deltaMethod(est, "b1*(mean.P/mean.Q)", vcov.=d)


#####
mean.Q= 19.86
mean.P=0.39
se = -3.56/-1.70 # b/t
se
d = se^2
est = -3.56
names(est) <- c("b1")

deltaMethod(est, "b1*(mean.P/mean.Q)", vcov.=d)

#Price elasticity for upper block (Long-run). Seems to have aggregation bias 
mean.Q= 150.61
mean.P=0.75
se = -74.30/-6.06 # b/t, mean.Q= (mean.P *b)/Eta
se
d = se^2
est = -74.30
names(est) <- c("b1")

deltaMethod(est, "b1*(mean.P/mean.Q)", vcov.=d)

#Price elasticity for Biannual, having seasonal effect
mean.Q= 19.86
mean.P=0.39
se = -99.3/-12.3 # b/t
se
d = se^2
est = -99.3
names(est) <- c("b1")
deltaMethod(est, "b1*(mean.P/mean.Q)", vcov.=d)

##Price elasticity for lower block (short-run). 

mean.Q= 19.86
mean.P=0.39
se = -0.2037/-0.03 # b/t
se
d = se^2
est = -0.2037
names(est) <- c("b1")

deltaMethod(est, "b1*(mean.P/mean.Q)", vcov.=d)

## People elasticity for lower block, LR
Q= 19.86
pop=73
se = 2.5/-1.76 # b/t
se
d = se^2
est = -0.44
names(est) <- c("b1")
deltaMethod(est, "b1*(pop/Q)", vcov.=d) 

##People elasticity for upper block, LR
Q= 150.61
pop=9
se = 2.845/1.53 # b/t
se
d = se^2
est = 2.845
names(est) <- c("b1")
deltaMethod(est, "b1*(pop/Q)", vcov.=d) 

##People elasticity for seasonal effects (lower block), LR
Q= 19.86
pop=73
se = -0.1183/-1.76 # b/t
se
d = se^2
est = -0.1183
names(est) <- c("b1")
deltaMethod(est, "b1*(pop/Q)", vcov.=d)

##Income elasticity for lower block, LR
Q= 19.86
M=73
se = 0.0025/6.75 # b/t
se
d = se^2
est = 0.0025
names(est) <- c("b1")
deltaMethod(est, "b1*(M/Q)", vcov.=d)

##Income elasticity for upper block, LR
Q= 150.6
M=1570
se = 0.022/3.46 # b/t
se
d = se^2
est = 0.022
names(est) <- c("b1")
deltaMethod(est, "b1*(M/Q)", vcov.=d)
#####################################
#####################################

# Study 8
##Suarez-Varela (2020)
#Table 13
#Linear model

mean.Q= 122.0
mean.P= 1.205
se = 0.470 # b/t
se
d = se^2
est = -46.45
names(est) <- c("b1")

deltaMethod(est, "b1*(mean.P/mean.Q)", vcov.=d)


#Log-linear model

mean.Q= 122.0
mean.P= 1.205
se = 0.00386 # b/t
se
d = se^2
est = -0.378
names(est) <- c("b1")

deltaMethod(est, "b1*mean.P", vcov.=d)


# Study 9
# Jegnie et al (2021) - IN PRESS

# Long-run and short-run SE estimates for Table 7

# Pooled demand model (Model 2)

d = diag(c(0.003^2, 0.00039945^2),2,2)#SE values of the original model for SR & lagged demand, respectively
d
z <- c(-0.124, 0.771)# the coefficients for SR and lagged demand values of the original model
names(z) <- c("d1", "d2")
deltaMethod(z, "d1/(1-d2)", vcov.=d)

#Winter demand model (Model 3)

d = diag(c(0.005^2, 0.001^2),2,2)#SE values of the original model for SR & lagged demand, respectively
d
z <- c(-0.094, 0.631)# the coefficients for SR and lagged demand values of the original model
names(z) <- c("d1", "d2")
deltaMethod(z, "d1/(1-d2)", vcov.=d)


#Summer demand model (Model 4)
d = diag(c(0.005^2, 0.001^2),2,2)#SE values of the original model for SR & LR, respectively
d
z <- c(-0.272, 0.694)# the coefficients for SR and LR values of the original model 
names(z) <- c("d1", "d2")
deltaMethod(z, "d1/(1-d2)", vcov.=d)

#July demand model (Model 5)

d = diag(c(0.012^2, 0.001^2),2,2)#SE values of the original model for SR & lagged demand, respectively
d
z <- c(-0.102, 0.669)# the coefficients for SR and lagged demand values of the original model
names(z) <- c("d1", "d2")
deltaMethod(z, "d1/(1-d2)", vcov.=d)


#Jan demand model (Model 6)

d = diag(c(0.009^2, 0.002^2),2,2)#SE values of the original model for SR & lagged demand, respectively
d
z <- c(-0.190, 0.679)# the coefficients for SR and lagged demand values of the original model
names(z) <- c("d1", "d2")
deltaMethod(z, "d1/(1-d2)", vcov.=d)


##Consumption groups
#Standard Customer Model

d = diag(c(0.003^2, 0.001^2),2,2)#SE values of the original model for SR & LR, respectively
d
z <- c(-0.123, 0.758)# the coefficients for SR and LR values of the original model 
names(z) <- c("d1", "d2")
deltaMethod(z, "d1/(1-d2)", vcov.=d)


#Pensioner model

d = diag(c(0.006^2, 0.001^2),2,2)#SE values of the original model for SR & LR, respectively
d
z <- c(-0.160, 0.829)# the coefficients for SR and LR values of the original model 
names(z) <- c("d1", "d2")
deltaMethod(z, "d1/(1-d2)", vcov.=d)


##State Senior Model

d = diag(c(0.009^2, 0.001^2),2,2)#SE values of the original model for SR & LR, respectively
d
z <- c(-0.140, 0.793)# the coefficients for SR and LR values of the original model 
names(z) <- c("d1", "d2")
deltaMethod(z, "d1/(1-d2)", vcov.=d)

##State and Cwlth-Senior Model

d = diag(c(0.017^2, 0.002^2),2,2)#SE values of the original model for SR & LR, respectively
d
z <- c(-0.072, 0.785)# the coefficients for SR and LR values of the original model 
names(z) <- c("d1", "d2")
deltaMethod(z, "d1/(1-d2)", vcov.=d)

########################################


#Model results for property groups
##House Model

d = diag(c(0.003^2, 0.000^2),2,2)#SE values of the original model for SR & LR, respectively
d
z <- c(-0.142, 0.775)# the coefficients for SR and LR values of the original model 
names(z) <- c("d1", "d2")
deltaMethod(z, "d1/(1-d2)", vcov.=d)


##Unit Model
d = diag(c(0.006^2, 0.001^2),2,2)#SE values of the original model for SR & LR, respectively
d
z <- c(-0.068, 0.778)# the coefficients for SR and LR values of the original model 
names(z) <- c("d1", "d2")
deltaMethod(z, "d1/(1-d2)", vcov.=d)


##Attached Dwellings Model

d = diag(c(0.03^2, 0.004^2),2,2)#SE values of the original model for SR & LR, respectively
d
z <- c(-0.054, 0.796)# the coefficients for SR and LR values of the original model 
names(z) <- c("d1", "d2")
deltaMethod(z, "d1/(1-d2)", vcov.=d)


##Flat Model

d = diag(c(0.079^2, 0.01^2),2,2)#SE values of the original model for SR & LR, respectively
d
z <- c(0.075, 0.912)# the coefficients for SR and LR values of the original model 
names(z) <- c("d1", "d2")
deltaMethod(z, "d1/(1-d2)", vcov.=d)


####### THE END #####

