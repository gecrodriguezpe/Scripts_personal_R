# Multivariate GARCH Example Enders
# Section 3.11

# Paquetes básicos econometria financiera
library(PerformanceAnalytics) 
library(quantmod)

# Paquetes para trabajar con m-garch
library(rmgarch)
library(mgarchBEKK)
library(VIRF)

# Paquetes series de tiempo
library(xts) 
library(urca) 
library(forecast)
library(aTSA) 
library(tseries)
library(MTS)

# Paquetes seres de tiempo multivariados
library(vars)
library(MTS)
library(FinTS) # Companion to Tsay Analysis of Financial Time Series

# Para hacer análisis de regresión
library(car) 

# Para importar bases de datos y funcionalidades generales en R
library(tidyverse) 
library(readxl) 
library(lubridate) 

# Preliminary analysis of each series ----

# In the current example we are going to work with the nominal exchage rates of
# British Pound, Swiss Franc and Euro

#Note: The example can be found in PAGE 169 Enders
setwd("~/Enders/Enders_personal/Datasets")

data = read_xls("ExRatesdaily.xls")
head(data)

glimpse(data)

data_xts = xts(data[-1], order.by = data$observation_date)
plot(data_xts)

### FIGURE 3.5
data$observation_date=as.Date(as.character(data$observation_date))
plot(data$observation_date,data$Euro,type="l",las=1,xaxs="i",yaxs="i",xlab="",ylab="",ylim=c(0.5,2.25),tck=.02)
lines(data$observation_date,data$Pound,col="steelblue4")
lines(data$observation_date,1/data$SW,col="steelblue1")
legend("topleft", xpd=TRUE, colnames(data)[-1], lty=c(1), col=c(1,"steelblue4","steelblue1"), bty='n', cex=0.75,ncol=1)

### PAGE 170
date = data$observation_date[-1]
eur = diff(log(data$Euro))
gbp = diff(log(data$Pound))
chf = diff(log(1/data$SW))
df = data.frame(eur,gbp,chf)
k=ncol(df)

par(mfrow=c(k,1))
for (i in 1:k) {
  plot(date,df[,i],type="l",las=1,xaxs="i",yaxs="i",xlab="",ylab="",tck=.02,ylim=c(-0.04,0.04),main=colnames(df)[i],col="steelblue4")
  print(mean(df[,i]))
}
acf.eur = acf(euro,lag=6)
acf.eur


# Constant Condidional Correlation (CCC) GARCH ----
library("rmgarch")

# Specify the sGARCH(1,1) that is going to be used for each nominal exchange rate
spec.FX = ugarchspec(mean.model=list(armaOrder=c(0,0)),
                     variance.model=list(garchOrder=c(1,1),model="sGARCH"))

# Method for creating a univariate multiple GARCH specification object 
spec.mgarch = multispec(replicate(k,spec.FX))

# Copula-GARCH Specification

## uspec: A uGARCHmultispec object created by calling multispec on a list of 
##        univariate GARCH specifications. The univariate GARCH specifications
##        doesn't necessarily have to be the same

## dccOrder: The DCC autoregressive order

## distribution.model: The Copula distribution model.
##                     multivariate Normal or Student Copula 

spec.cccgarch = cgarchspec(uspec=spec.mgarch, 
                           dccOrder=c(1,1), 
                           distribution.model=list(copula="mvnorm",time.varying=FALSE))

# Fiting a Copula-GARCH model

## spec: cgarchspec object
## data: A multivariate xts data object or one which can be coerced to such
## solver: the solver to use
## fit.control: Control arguments passed to the fitting routine
##              The ‘eval.se’ option determines whether standard errors are calculated

ccc.garch.FX = cgarchfit(spec.cccgarch, 
                         data=df, 
                         solver="solnp", 
                         fit.control=list(eval.se = TRUE))
ccc.garch.FX

# correlation matrix
cov_matrix_cc = ccc.garch.FX@mfit$Rt # Extracts the correlation matrix
colnames(cov_matrix_cc)=rownames(cov_matrix_cc)=colnames(df) # Correct naming for the correlation matrix
cov_matrix_cc # The same correlations that are found in Enders

# Note: The correlation matrix is constant since we are employing a CCC model
#       Therefore, the correlation parameters between the series are assumed constant

# Dynamic Condidional Correlation (DCC) GARCH ----

# Note: It is not covered in the main book but it is essential for multivariate volatility modelling
#       However DCC is explained in the supplementary manual of Enders

# A DCC model is a generalization of the CCC model so that the 
# conditional correlations vary over time 

# Copula-GARCH Specification
# The only difference with the Copula-GARCH for a CCC model is that
# in the case of a DCC model one has to specify a time varying conditional correlation
spec.dccgarch = cgarchspec(uspec=spec.mgarch, 
                           dccOrder=c(1,1),
                           distribution.model=list(copula="mvnorm", time.varying=TRUE))

# Fiting a Copula-GARCH model
dcc.garch.FX = cgarchfit(spec.dccgarch, 
                         data=df, 
                         solver="solnp", 
                         fit.control=list(eval.se = TRUE))
dcc.garch.FX

# conditional correlation matrices

# correlation matrix
Rt = rcor(dcc.garch.FX) # Extracts the correlation matrices (they change each day since they are conditional correlations from an DCC model)

# Note: We are not restraining  the conditional correlations to be constant
#       Therefore, day by day, their value is changing since those conditional correlation matrices
#       have been estimated though a DCC-GARCH model 

# Graph of the covariances
par(mfrow=c(k,1))
for (i in 1:k) {
  for (j in 1:k) {
    if (i>=j) {
      next
    } else {
      plot(Rt[i,j,],type="l",las=1,xaxs="i",yaxs="i",xlab="",ylab="",tck=.02,ylim=c(-0.2,1),main=paste(colnames(df)[i],"-",colnames(df)[j]),col="steelblue4")
      abline(h=0,lty=2)
    }
  }
}

# BEKK GARCH (MTS) ----

# Note: For estimating a BEKK11 model, one has to use the MTS packages

# library("MTS")
# bekk.FX = BEKK11(df[,1:2]) 
# We are selecting only the first two nominal exchange rates
# I.e. we are selecting euro and chf
# The main reason for only selecting two time series is that a bekk model is computationally very demanding
     
# Note: It is very likely that it would take a long time to estimate the model
#       and it is probable that it does not yield the value of some params
#       The issue with a Bekk11 model is that computationally is very demanding


# BEKK GARCH (mgarchBEKK) ----

# 
df2_xts = as.xts(data[-1], order.by = data$observation_date)
df2 = as.ts(df[, 1:2])

fit = BEKK(df2_xts[,1:2], order = c(1,1))
fit$est.params
VIRF(df2, 2223)




