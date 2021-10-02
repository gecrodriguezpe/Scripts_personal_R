#### Simulación de un modelo garch con regresores exógenos ####

library(rugarch)

data = cbind(rnorm(1000), rnorm(1000))

spec_non_exog = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                           mean.model = list(armaOrder = c(0,0)),
                           distribution.model = "norm")

spec_exog = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                  mean.model = list(armaOrder = c(0,0), external.regressors = matrix(data[,2])),
                  distribution.model = "norm")

garch_non_exog = ugarchfit(data = data[,1], spec_non_exog)
garch_non_exog

garch_exog = ugarchfit(data = data[,1], spec_exog)
garch_exog
