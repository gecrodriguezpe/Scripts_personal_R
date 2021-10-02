# Paquetes básicos para hacer econometría financiera en R
library(rugarch) # para modelamiento garch univariado
library(PerformanceAnalytics) # Herramientas econométricas para el análisis de riesgo y desempeño financiero 
library(xts) # biblioteca más importante para trabajar series de tiempo en R
library(quantmod) # para hacer modelamiento cuantitativo financiero

# Para hacer análisis de series de tiempo (pruebas de raíz unitaria)
library(urca) # Para hacer pruebas de raíz unitaria y de cointegración en R
library(forecast) # En caso de querer utilizar autoarima para modelar la media del proceso
library(aTSA) # Librería para realizar el test ARCH-LM (de multiplicadores de Lagrange)
library(tseries) # Para realizar el test de Jarque Bera

# Para hacer análisis de regresión
library(car) # Permite realizar la qqplot para verificar el supuesto de normalidad en los residuales

# Para importar bases de datos y funcionalidades generales en R
library(tidyverse) # A mí juicio personal la biblioteca más importante de R 
library(readxl) # Para importar archivos .xlsx (excel) a R
library(lubridate) # Para manejar fechas y días en R

# Nota: Se recomienda fuertemente al lector ir leyendo la sección 3.10 del libro
#       del Enders mientras lee el presente Script

# 1. Análisis pre-estimación ----

# Ejemplo basado en la sección 3.10 del famoso libro de Walter Enders
# Applied time series econometrics 

# Definir el directorio de trabajo 
setwd("~/Scripts_R/scripts_personales")

# Se empleara la variable RATE que consiste en los retornos diarios totales 
# del NYSE 100 Index

# Nota: La fecha del archivo está mal por lo que toca construir un objeto de 
#       de tiempo desde R (Para ello se utiliza el paquete lubridate)

# La base de datos tiene el problema de que la variable RETURN originalmente
# se importa como variable character por lo que hay que transformarla a variable
# de tipo double (double es sinónimo de numérico en R). 
nyse = read_xlsx("NYSEReturns.38135430.xlsx") %>% 
       mutate(RETURN = suppressWarnings(as.double(RETURN))) 

# Visualizar base de datos 
glimpse(nyse) # Como se puede observar, las fechas originales del archivo están mal

# Creación de fecha de solo días hábiles entre semana utilizando lubridate

# Crear un objeto con todas las fechas desde el 3 de enero 2000 al 16 de julio de 2012
dias = seq(ymd("2000-01-03"), ymd("2012-07-16"), by = "days")
# Eliminar los dias que hacen parte de los fines de semana 
## 1: domingo, 2: lunes, 3: martes, 4: miercoles, 5: jueves, 6: viernes y 7: sábado
dias = dias[wday(dias) %in% 2:6] # eliminar los fines de semana del objeto dias

# objeto tipo Date
class(dias)

# Se agregan las fechas corregidas al data frame nyse
# Se agregan los retornos al cuadrado que sirven para hacer pruebas de heterocedasticidad condicional
# Además se eliminan los missing values de la base original 
# Para trabajar solo con los datos observados
nyse = nyse %>% 
       mutate(dias = dias, RATE2 = RATE^2) %>% 
       select(dias, RETURN, RATE, RATE2) %>% 
       drop_na()

# Creación de los objets xts (la serie de tiempo con la que se va a trabajar a lo largo del script)

# RETURN: Variable que contiene el índice de precios del NYSE 100
# RATE: Variable que contiene los retornos porcentuales del índice
nyse_xts = xts(nyse[-1], order.by = nyse$dias) 

class(nyse_xts)

## Recordar que xts utiliza el format internacional:
## ISO8601 para representar días y fechas

## Las observaciones van del 4 de Enero del 2000 al 16 de Julio del 2012
nyse_xts = nyse_xts["2000-01-04/2012-07-16"]

# 2. Modelamiento de la media ----

# El primer paso en modelar cualquier proceso GARCH es estimar el modelo de la media

# Info. adicional
## En un modelo para un activo financiero, modelar la media es de poco interés
## Los retornos de los activos tienden a comportarse como camintas aleatorias o
## como caminatas aleatorias con deriva
## Por tal motivo, el modelo de media de los retornos de los activos va a prover
## poca información desde la teoría financiera estándar
## (recuerden acá la hipótesis de los mercados financieros)

# 2.1 Gráficas de las series ----

# Gráfica del Índice NYSE 100 
plot(nyse_xts$RETURN,
     main = "NYSE 100 Index", ylab = "Niveles del índice de precios", 
     lwd = 1)

# Gráfica de los retornos del índice  (Ver gráfica 3.3 pag. 120 del libro del Enders)
plot(nyse_xts$RATE,
     main = "Retornos porcentuales del NYSE 100 Index", ylab = "Variación procentual diaria del índice", 
     lwd = 1)

# 2.2 Pruebas de raíz unitaria ----

# 2.2.1 Pruebas para la variable RETURN ----

# Característica de RETURN
### media muestral (no condicional) de RETURN
mean(nyse_xts$RETURN)
### varianza muestral (no condicional) de RETURN
sd(nyse_xts$RETURN)
### número de observaciones de RETURN
length(nyse_xts$RETURN)

# Pruebas de ADF

# Prueba con trend
adf.trend = ur.df(nyse_xts$RETURN, type="trend", lags = 2); plot(adf.trend)
summary(adf.trend)

# Prueba con drift
adf.drift = ur.df(nyse_xts$RETURN, type="drift", lags = 2); plot(adf.drift)
summary(adf.drift) # No rechazo y parace que el término de deriva es significativo

# Prueba de Phillips Perron
pp = ur.pp(nyse_xts$RETURN) 

# Prueba de KPSS
kpss = ur.kpss(nyse_xts$RETURN)

# 2.2.2 Pruebas para la variable RATE ----

### media muestral (no condicional) de RATE
mean(nyse_xts$RATE)
### varianza muestral (no condicional) de RATE
sd(nyse_xts$RATE)
### número de observaciones de RATE
length(nyse_xts$RATE)

# Pruebas de ADF

# Prueba con none
adf.none = ur.df(nyse_xts$RATE, type="none", lags = 2); plot(adf.none)
summary(adf.none)

# Prueba de Phillips Perron
pp = ur.pp(nyse_xts$RATE)

# Prueba de KPSS
kpss = ur.kpss(nyse_xts$RATE)

# Conclusión de las pruebas de raíz unitaria: 
## La variable RETURN no es estacionaria 
## La variable RATE = 100*log(RETURN_{t} - RETURN_{t-1}) sí es estacionaria 

# Por tanto, todo el análisis subsecuente de la media del modelo se debe hacer
# utilizando la variable RATE(Los retornos porcentuales del NYSE 100 Index)

# 2.3 Gráfica de la función de densidad de RETURN vs. distribución normal y distribución t ----

# Construcción de la base de datos para gráficar las 3 distribuciones
nyse_grafica = nyse %>% 
               mutate(normal = rnorm(nrow(nyse), sd = sd(nyse_xts$RATE)), t = rt(nrow(nyse), df = 2)) %>% 
               pivot_longer(cols = 3:5, names_to = "Distribuciones", 
                            values_to = "Valores_distribuciones")

# Nota: 
## En la base de datos anterior, se simuló: 
### una distribución normal con la misma varianza que la distribución de los retornos del NYSE 100 Index 
### una distribución t con 2 grados de libertad 

# Función que construye la gráfica de densidades
density_comparacion = nyse_grafica %>% 
  ggplot(aes(x = Valores_distribuciones, color = Distribuciones)) +
  geom_density(size = 1) + 
  xlim(-7.5, 7.5) +
  theme_light() +
  ggtitle("Distribución NYSE 100 Index vs.\nDistribución normal vs.\nDistribución t") +
  ylab("Densidades") +
  xlab("Figurar 3.13 Enders: Valores distribuciones"); density_comparacion 

# Como pueden observar es la misma gráfica que presenta Enders en la figura 3.13

# Nota: El warning message ocurre porque se colocaron límites en la escala 
# del eje x de la gráfica para tener una mejor visualización de las 
# distribuciones 

# Nota: La gráfica se ve algo pixeleada cuando se expande porque se usaron
#       muy pocas observaciones para simular las distribuciones
#       Para hacer densidades más precisas se necesitan al menos como 10000 observaciones

# Nota: Sobre la elección de la distribución para los errores del modelo

## La distribución de los retornos tiene un pico más pronunciado a comparación
## de la distribución normal y la distribución t con 2 grados de libertad

## De igual forma, las colas de la distribución de RATE tiene colas
## más pesadas que una distribución normal pero no tan pesadas como una distribución t
## con 2 grados de libertad

## Por tanto, por las funciones de densidad anteriores, y teniendo en cuenta
## que la mayoría de los retornos de los activos financieros no siguen una 
## distribución normal (al presentar colas gruesas como el presente ejemplo)
## se obta por estimar el modelo simulando los errores como si siguieran
## una distribución t 

# Nota: La gráfica de la distribución de RATE claramente tiene todas las 
#       características de una distribución leptocúrtica, dado que tiene
#       un pico más pronunciado alrededor de su media y colas más gruesas
#       que una distribución normal, por lo que se espera que su curtosis
#       sea mayor a 3

# La kurtosis es 8.13 lo que claramente muestra que los retornos porcentuales
# del NYSE 100 Index siguen una distribución leptocúrtica
kurtosis(nyse_xts$RATE) 

# 2.4 ACF y PACF de la variable RETURN y de RETURN^2 ----

lags = 30

## ACF y PACF de la variable RATE: 
## Permite saber intuitivamente si el proceso es débilmente dependiente o no
ggAcf(nyse_xts$RATE,lag.max=lags,plot=T,lwd=2) + ggtitle("ACF de los retornos diarios del NYSE Index 100")
ggPacf(nyse_xts$RATE,lag.max=lags,plot=T,lwd=2) + ggtitle("PACF de los retornos diarios del NYSE Index 100")

# De las ACF y PACF anteriores pareciera que los retornos siguen un proceso débilmente dependiente en media

## ACF y PACF de la variable RATE^2: 
## Permite saber intuitivamente si el proceso es débilmente dependiente o no
ggAcf(as.ts(nyse_xts$RATE2),lag.max=lags,plot=T,lwd=2) + ggtitle("ACF de los retornos diarios al cuadrado del NYSE Index 100")
ggPacf(as.ts(nyse_xts$RATE2),lag.max=lags,plot=T,lwd=2) + ggtitle("PACF de los retornos diarios al cuadrado del NYSE Index 100")

# La ACF de los retornos diarios al cuadrado es una clara muestra de que 
# hay heterocedasticidad condicional en los retornos del NYSE 100 Index

# 2.5 Identificación y estimación del modelo de media ----

# 2.5.1 Selección del modelo ARIMA con el menor criterio de información ----

# Método manual para identificar el ARIMA usando criterios de información 

#Ahora vamos a ver lo que muestran los criterios AIC y BIC
AR.m <- 2 #Supondremos que el rezago autorregresivo máximo es 2 (pmax)
MA.m <- 2 #Supondremos que el rezago de promedio móvil máximo es 2. (qmax)

# función que me permite crear un data frame para distintos modelos ARIMA(p,d,q)
# en donde cada fila del df me da el orden del modelo y los AIC y BIC correspondientes a dicho modelo
# Ojo: utlizar method = "ML" dentro de arima(), de lo contrario les arrojará error
arma_seleccion_df = function(ts_object, AR.m, MA.m, d, bool_trend, metodo){
  index = 1
  df = data.frame(p = double(), d = double(), q = double(), AIC = double(), BIC = double())
  for (p in 0:AR.m) {
    for (q in 0:MA.m)  {
      fitp <- arima(ts_object, order = c(p, d, q), include.mean = bool_trend, 
                    method = metodo)
      df[index,] = c(p, d, q, AIC(fitp), BIC(fitp))
      index = index + 1
    }
  }  
  return(df)
}

# 2. Creo la función arma_min_AIC para seleccionar el mejor modelo según AIC.
arma_min_AIC = function(df){
  df2 = df %>% 
    filter(AIC == min(AIC))
  return(df2)
}

# 3. Creo la función arma_min_BIC para seleccionar el mejor modelo según BIC
arma_min_BIC = function(df){
  df2 = df %>% 
    filter(BIC == min(BIC))
  return(df2)
}

# Llamo la función arma_selection_df para construir un data frame con todos los posibles modelos
# ARIMA(p, d, q). Acá se usa d = 0 dado que no es necesario diferenciar la serie (RATE ya es estacionaria)

mod_d0 = arma_seleccion_df(nyse_xts$RATE, AR.m, MA.m, d = 0, TRUE, "ML")
view(mod_d0)

# Selecciono el mejor modelos según criterios de información cuando d=1
min_aic_0 = arma_min_AIC(mod_d0); min_aic_0 #ARIMA (2,0,0)
min_bic_0 = arma_min_BIC(mod_d0); min_bic_0 #ARIMA (2,0,0)

# En el método manual se seleccionó que el modelo que minimizaba tanto el 
# AIC como el BIC era un ARMA(2,0). Si se lee el Enders se observa que él
# también selecciona un modelo ARMA(2,0) para su modelo de media

#
# Método automático para identificar el ARIMA usando criterios de información 
# Usando la función auto.arima() del paquete forecast
#

# Nota: La misma función auto.arima es capaz de reconocer el orden de diferenciación
# Para que la serie quede estacionaria. 
# En realidad la función auto.arima calcula un modelo SARIMA porque además de 
# modelas la parte ARMA de la serie también modela la componente estacional de 
# la serie
# Para mayor info. sobre los modelos SARIMA: Ver capítulo 2 sección 11: Seasonality del Enders
auto.arima(nyse_xts$RATE, method = "ML")

# En el método manual se seleccionó que el modelo que minimizaba tanto el 
# AIC como el BIC era un ARMA(0,2). Si se lee el Enders se observa que él
# selecciona un modelo ARMA(2,0) (como en el método manual).

# Como pueden observar el método manuel es más confiable que el 
# método automático por lo que el modelo de media que se seleccionará será 
# el de un ARMA(2,0)

# 2.5.2 Estimación del modelo ARIMA ----

arima_2.0.0_d0 = arima(nyse_xts$RATE, order = c(2,0,0), include.mean = T, method = "ML"); summary(arima_2.0.0_d0) # modelamiento ARIMA(2,0,0)

# Se observa que los dos coeficientes autoregresivos son estadísticamente significativos

# 2.5.3 Validación de los supuestos del modelo ARIMA ----

## ACF y PACF para los residuales del modelo de media
ggAcf(residuals(arima_2.0.0_d0), lag.max=lags, plot=T, lwd=2) + ggtitle("ACF de los residuales del modelo de media")
ggPacf(residuals(arima_2.0.0_d0), lag.max=lags, plot=T, lwd=2) + ggtitle("PACF de los residuales del modelo de media")

# la ACF y PACF parece indicar que no hay correlación serial en los residuales del modelo

resid = as.double(residuals(arima_2.0.0_d0))
resid2 = as.ts(resid * resid)

## ACF y PACF para los residuales del modelo de media
ggAcf(resid2, lag.max=lags, plot=T, lwd=2) + ggtitle("ACF de los residuales al cuadrado del modelo de media")
ggPacf(resid2, lag.max=lags, plot=T, lwd=2) + ggtitle("PACF de los residuales del modelo de media")

# Pruebas formales: 

# 1. Pruebas de correlación serial en los residuales

#Generalmente la prueba se hace sobre un 1/4 de la muestra, pero también la haremos para otros rezagos. 
lags.test = length(nyse_xts$RATE)/4;lags.test

# Test Box-Pierce para autocorrelación en los residuales
Box.test(residuals(arima_2.0.0_d0),lag=lags.test, type = c("Box-Pierce")) #No rechazo H0, no se cumple el supuesto. 
# Test Ljung-Box para autocorrelación en los residuales.
Box.test(residuals(arima_2.0.0_d0),lag=lags.test, type = c("Ljung-Box")) #No rechazo H0, no se cumple el supuesto.

# 2. Pruebas de Heterocedasticidad en los residuales

# Ahora vamos a mirar el supuesto de heterocedasticidad con un test de efectos ARCH. La prueba nos dice que si los residuales
# son heterocedasticos, los residuales al cuadrado deben estar correlacionados. Hay dos formas de hacer la prueba: Un test 
# Pormenteau y un Test tipo multiplicadores de Lagrange.

arch <-arch.test(arima_2.0.0_d0, output=TRUE)#Rechazo H0 en ambas pruebas, así que los residuales son heterocedásticos.
# El resultado de la prueba da que la serie no es estacionaria en varianza por lo que la varianza
# cambia a lo largo de la serie como se observa si se gráfica la serie

# En este caso, hay que estimar un modelo GARCH para poder modelar la varianza condicional 

# 3. Pruebas de normalidad en los residuales

#QQ-PLOT 
par(mfrow=c(1,1))
Residuales=arima_2.0.0_d0$residuals
qqPlot(Residuales)

#Prueba Jarque-Bera
jarque.bera.test(residuals(arima_2.0.0_d0)) #Se rechaza H0, no hay normalidad. 

# 3. Modelamiento de la varianza condicional ----

# Modelo para la varianza condicional 

## El objetivo es modelar y pronósticar adecuadamente el comportamiento de la
## varianza condicional 
## Condicional porque captura toda la información disponible hasta el periodo t
## se modela la varianza del activo, porque ésta es una medidad de la volatilidad
## y por ende, del riesgo del activo 
## (Recordar que la volatilidad de un activo es la desviación estándar de sus retornos)
## Modelar adecuadamente la varianza condicional require un gran numéro de observaciones

# Nota: Hay 3 formas de detectar errores ARCH en el modelo
## 1. ACF de los residuales al cuadrado del modelo 
## 2. Estadístico Q sobre los residuales al cuadrado 
## 3. LM-ARCH Test (Test de multiplicadores de Lagrange tipo ARCH sobre los residuales)

# 3.1 Estimaciones de diferentes especificaciones del modelo para la varianza condicional ----

# Como en una metodología de Box Jenkins estándar, se desea estimar modelos
# parsiomoniosos

# Existen diferentes especificaciones para modelar la varianza condicional:
## sGARCH(p, q): Modelo Garch estándar. Lo más usual es estimar un GARCH(1,1)
## ARCH-M: Modelo que se incluye la varianza condicional como regresor en el modelo de media
## Modelos con asimetría frente a la información: gjrGARCH o eGARCH
## iGARCH: Integrated GARCH

# Nota: En el comando ugarchspec, donde dice variance.model aparece todos los posibles
#       modelos GARCH que se pueden estimar con rugarch

# Nota: Dada la tremendo número de posibles especificaciones para un modelo GARCH
#       es posible sobreajustar el modelo a los datos que se tiene, lo cual no siempre es conveniente
#       en particular si se quieren realizar pronósticos
#       Por tanto, lo mejor es empezar con un modelo simple y determinar si dicho modelo
#       es o no es adecuado. 
#       Si no se cumple alguno de los muchos test de diagnóstico, es posible
#       usar especificaciones de GARCH más complicadas

# Nota: Como funciona el paquete rugarch: 
## 1. ugarchspec: Se colocan todas las especificaciones del modelo 
### variance.model: Todas las especificaciones para el modelamiento de la varianza
### mean.model: Todas las especificaciones para el modelamiento de la media
### distribution.model: La distribución teórica que se va a usar para modelar los errores
## 2. ugarchfit: Para estimar el modelo por máxima verosimilitud 
##               Rocordar que un modelo GARCH es un modelo no lineal por lo que
##               no se puede estimar por OLS. El método más usual en la práctica
##               de estimación es máxima verosimilitud donde se utiliza una
##               distribución teórica a la hora de hacer el proceso de maximización
## 3. ugarchforcast: Para hacer pronósticos del modelo 
##                   Ya sean pronósticos sobre la media o sobre la varianza condicional

# Nota: rugarch permite agregarle regresores exógenos tanto al modelo de media
#       como al modelo de varianza

# Nota: Distribuciones que pueden ser empleadas por rugarch para modelar los errores: 
## norm: normal
## snorm: skew-normal
## std: t
## sstd: skew-t 
## ged: generalized error distribution
## sged skew generalized error distribution 

# 3.1.1 Estimación de un sGARCH(1,1): Modelo estándar GARCH(1,1) ----

# Estimación de un sGARCH(1,1) distribuido con errores normales

garchspec_norm = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                            mean.model = list(armaOrder = c(2,0)),
                            distribution.model = "norm")

garchfit_norm = ugarchfit(spec = garchspec_norm, data = nyse_xts$RATE); garchfit_norm
# AIC: 2.8722
# BIC: 2.8836

# Estimación de un sGARCH(1,1) distribuido con errores t

garchspec_t = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                            mean.model = list(armaOrder = c(2,0)),
                            distribution.model = "sstd")

garchfit_t = ugarchfit(spec = garchspec_t, data = nyse_xts$RATE); garchfit_t
# AIC: 2.8298
# BIC: 2.8450
# skew: 0.88 (asimetría izquierda, dado que skew < 1)
# shape: 7.47 (Distribución t con 7 grados de libertad)
# Nota: los parámetros 
## skew: denota el grado de asimetría de la distribución
## shape: denota los grados de libertad de la distribución t
# Nota: Ahora de calculr el AIC y el BIC, el software tiene en cuenta 
#       todos los parámetros incluyendo skew y shape

# Dado que skew y shape resultaron ser variables significativas y además 
# tanto el AIC como el BIC del modelo con la t se escogió, y teniendo en cuenta
# el histograma de las distribuciones, se concluye que el modelo donde se 
# asume que los errores distribuyen como una t es el mejor modelo

# 3.1.2 Estimación de un iGARCH(1,1): GARCH(1,1) integrado con distribución t ----

# Dada que la suma de los coeficientes alpha1 y beta1 es muy cercana a 1
# (Recordar que dichos coeficientes son los coeficientes asociados al modelo
#  GARCH de la varianza condicional)
# Se decide estimar un iGARCH.

igarchspec_t = ugarchspec(variance.model = list(model = "iGARCH", garchOrder = c(1,1)),
                         mean.model = list(armaOrder = c(2,0)),
                         distribution.model = "sstd")

igarchfit_t = ugarchfit(spec = igarchspec_t, data = nyse_xts$RATE); igarchfit_t
# AIC: 2.8294
# BIC: 2.8428

# Nota: Un modelo iGARCH no va a ajustar los datos tan bien como un sGARCH
#       dado que impone la restrición adicional de que alpha1 + beta1 = 1
#       No obstante, un modelo iGARCH es más parsimonioso que un modelo GARCH(1,1)
#       dado que hay un coeficiente de menos que hay que estimar (i.e beta1 = 1 - alpha1)

# Se puede notar que guiandose por criterios de información, 
# el modelo que se debería seleccionar debería ser un iGARCH

# 3.1.3 Estimación de un iGARCH(1,1) con arch-m: igarch arch-M. ----
# En un modelo arch-M: Se incluye la varianza condicional como 
#                      regresor adicional para el modelo de media
# La justificación de incluir la varianza condicional como regresor adicional 
# en el modelo de media es si el activo por ejemplo tiene una prima de riesgo
# variable en el tiempo. Es decir, a mayor riesgo (mayor varianza condicional)
# mayores retornos del activo

igarchspec_m_t = ugarchspec(variance.model = list(model = "iGARCH", garchOrder = c(1,1)),
                          mean.model = list(armaOrder = c(2,0), archm = TRUE),
                          distribution.model = "sstd")

igarchfit_m_t = ugarchfit(spec = igarchspec_m_t, data = nyse_xts$RATE); igarchfit_m_t
# AIC: 2.8300
# BIC: 2.8452

# Se puede observar que el parámetro asociado a la varianza condicional del modelo
# no es significativo 
# Además, por criterios de información se tiene que el AIC y el BIC es mayor
# en el modelo que incluye la varianza condicional como regresor adicional en
# el modelo de media, respecto al modelo que no inlcuye dicha varianza en el
# modelo de media

# Conclusión: Se prefiere el modelo iGARCH(1,1) con distribución de los errores t

# 4. Pruebas de diagnóstico sobre el modelo GARCH seleccionado ----

# Necesitamos saber si el modelo seleccionado iGARCH(1,1) con distribución t
# pasa las diferentes pruebas de diasnóstico
# En un modelo GARCH (cualquiera que sea), dichas pruebas de diagnóstico 
# se deben realizar sobre los residuales estandarizados

# La ventaja del paquete rugarch es que todas las pruebas de diagnóstico
# ya se encuetran incluidas en la salida del objeto del modelo estimado
igarchfit_t

# Se observa: 
## Weighted Ljung-Box Test on Standardized Residuals: 
### Se cumple el supuesto de no correlación serial (H0 no se rechaza)

## Weighted ARCH LM Tests:
### Se cumple el supuesto de no heterocedasticidad (H0 no se rechaza)



