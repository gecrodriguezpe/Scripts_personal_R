library(rmgarch)
data(dji30ret)
Data = dji30ret[, 1:3, drop = FALSE]

vfit = varxfit(X=Data, p=3, exogen = NULL, robust = FALSE,
               gamma = 0.25, delta = 0.01, nc = 10, ns = 500, postpad = "constant")

uspec = ugarchspec(mean.model = list(armaOrder = c(0,0), include.mean = 
                                       FALSE), variance.model = list(garchOrder = c(1,1), model = "sGARCH"),
                   distribution.model = "norm")

spec = dccspec(uspec = multispec( replicate(3, uspec) ), VAR = TRUE,
               lag = 3, dccOrder = c(1,1), distribution = "mvnorm")

fit = dccfit(spec, data = Data, fit.control = list(eval.se=TRUE), 
             VAR.fit = vfit)

names(fit@model)

fit@model$varcoef

vfit$Bcoef
