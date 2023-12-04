### Trabajo 3 

library(MASS)
library(readr)
setwd("C:/Users/Usuario/Pictures/DATOSR") 
datos<- read_csv("nacimientos.csv")
datos <- as.data.frame(datos)
head(datos)
names(datos)
datos1 <- datos[, c("Peso","Género","Nivel Educativo del Padre" ,"Factor RH","Grupo Sanguíneo","Talla","Tiempo de Gestación","Número de Consultas Prenatales","Tipo de Parto","Multiplicidad de Embarazo","APGAR1","APGAR2","Edad de la Madre","Número de Hijos Nacidos Vivos","Edad del Padre","Nivel Educativo de la Madre")]

datos1$`Nivel Educativo del Padre`=as.factor(datos1$`Nivel Educativo del Padre`)
datos1$`Grupo Sanguíneo`=as.factor(datos1$`Grupo Sanguíneo`)
datos1$`Factor RH`=as.factor(datos1$`Factor RH`)
datos1$`Género`=as.factor(datos1$`Género`) 
datos1$`Tipo de Parto`=as.factor(datos1$`Tipo de Parto`) 
datos1$`Multiplicidad de Embarazo`=as.factor(datos1$`Multiplicidad de Embarazo`)  


datos1 <- na.omit(datos1)
head(datos1)

names(datos1)


###-----------------------------------------------###

### 5) Modelo
### Metodo de Selección de variables 
### Regresion sin variables explicativas 
RLM.vacio <- lm(Peso ~ 1, data = datos1)
summary(RLM.vacio)
### Regresion con todas variables explicativas 
RLM.completo <- lm(Peso ~ ., data = datos1)
summary(RLM.completo)

### Método de regresión Forward 
### Se selecciona el modelo con menor AIC  
RLM.Forward <- step(RLM.vacio,
                    scope = list(lower=RLM.vacio,upper=RLM.completo),
                    direction = "forward")
summary(RLM.Forward) ###

### Método de regresión Backward
RLM.Backward <- step(RLM.completo,
                     scope = list(lower=RLM.vacio,upper=RLM.completo),
                     direction = "backward")
summary(RLM.Backward) ### 

### Método de regresión Stepwise
RLM.stepwise <- step(RLM.vacio,
                     scope = list(lower=RLM.vacio,upper=RLM.completo),
                     direction = "both")
summary(RLM.stepwise) ### 




### Datos con solo las variables seleccionadas 
datos2 <- datos1[, c("Peso","Talla","Tiempo de Gestación","Multiplicidad de Embarazo","Número de Hijos Nacidos Vivos")]
names(datos2)
head(datos2)
colnames(datos2)<-c("y", "x1", "x2", "x3", "x4")

### y  = Peso,
### x1 = `Talla`
### x2 = `Tiempo de Gestación`
### x3 = `Multiplicidad de Embarazo`_Numero de Fetos
### x4 = `Numero de Hijos Nacidos Vivos`
head(datos2)

attach(datos2)
#detach(datos2)
#rm(x2)
###-----------------------------------------------###


### 1) Ajuste del Modelo
model = lm(y~ ., data = datos2)
resumen = summary(model) 
resumen

library(car)
#Grafica de lo regresores 
#par(mfrow=c(1,2))
plot(y~x1,data=datos2)
plot(y~x2)
plot(y~x3,data=datos2)
plot(y~x4,data=datos2)
influenceIndexPlot(model)



###-----------------------------------------------###




### 2) Realización de Inferencia Estadística 
### Prueba de Hipotesis t.test para los Parametros(Betas)
library(broom)
tidy(modelo)

### Intervalos de Confianza para los Parametros(Betas)
confint(modelo, level = 0.95)
confint(modelo, level = 0.9) ### Significativos al 0.1

### Estadistica F_Anova
library(olsrr)
anova(modelo)
ols_step_both_p(model, details = TRUE) #Stepwise Regression



###-----------------------------------------------###




### 4) Analisis Diagnostico del Modelo  
### Residuales 
library(qpcR)
influencia  = influence.measures(modelo)
p <- length(modelo$coefficients); p  #  número de predictores
n=length(y)


y.est <- predict(modelo)
ei = modelo$residuals;ei #  residiuales crudos
di = ei/resumen$sigma;di #  residiuales estandarizados
ri = stdres(modelo);ri #  residiuales estudentizados
hii = influencia$infmat[,6];hii # leverage
hii.l = hatvalues(modelo)#hii del libro ?
ti = studres(modelo);ti #  residiuales R-Student
e.i = PRESS(modelo)$residuals;e.i # Residual press
PRESS <- (ei/(1-hii.l))^2; sum(PRESS)
Di=ri^2/p*hii.l/(1-hii.l);round(Di,5)#Distancia de Cook
obs.influ <- influence.measures(modelo)
ScaledResiduals = data.frame(round(cbind(y.est,ei,di,ri,hii.l,hii,e.i,ti,PRESS,Di),4));ScaledResiduals
df <- obs.influ$infmat
df.s <- as.data.frame(df); head(df.s) ### Observaciones INFLU



###  Verificar graficamente observaciones influyentes 
par(mfrow=c(2,3))
plot(model,which = 1) # Residuales versus valores ajustados
plot(model,which = 2) # Gráfico QQ
plot(model,which = 3) # Residuales Estadandarizados versus valores ajustados

plot(modelo,which = 4) # Distancia de Cook
plot(model,which = 5) # Residuales Estadandarizados versus Levarege
plot(model,which = 6) # Distancia de Cook vs Leverage



### Puntos de Balanceo e Influencia 
#which(hii.l > (2*p)/n)
which(hii > (2*p)/n)
which(abs(ti) > 2)

#Distancia de Cook
qf(0.05, p, n-p) ## > 0.27
which(Di > 0.27)
plot(modelo,which = 4) # Distancia de Cook


### DFFITS > 2*sqrt(p/n) examinar 
which(abs(df.s$dffit) > 2*sqrt(p/n))
### DFBETAS > 2/n Examinar 
which(abs(df.s$dfb.1_) > 2/sqrt(n))### B0
which(abs(df.s$dfb.x1) > 2/sqrt(n)) ### B1
which(abs(df.s$dfb.x2) > 2/sqrt(n)) ### B2
which(abs(df.s$dfb.x3SI) > 2/sqrt(n)) ### B3
which(abs(df.s$dfb.x4) > 2/sqrt(n)) ### B4
which(abs(df.s$dfb.x5) > 2/sqrt(n)) ###B5


### Cov.r, tiene que estar dentro del rango 1-+(3*p)/n para ser normal
a <- which(df.s$cov.r <= 1-(3*p)/n | df.s$cov.r >= 1+(3*p)/n)
b <- df.s[a,]
### Observacion COV.r >1 mejora la estimacion 
b[c(which(b$cov.r > 1)),]
### Oservacion Cov.r < 1 degrada la estimacion
b[c(which(b$cov.r < 1)),]


### Analicemos loa datos sin observaciones influyentes
datos.sin.infl= datos2[-c(which(abs(ti) > 2)),];datos.sin.infl
dim(datos.sin.infl)
dim(datos2)

### Modelo con Oservaciónes Influyentes 
modelo
### Modelo sin Observaciones Influyentes 
modelo.sin <- lm(y ~ ., data = datos.sin.infl)
resumen.sin <- summary(modelo.sin)

### Modelos
resumen
resumen.sin



###-----------------------------------------------###


### 3) Validacion de Supuestos (Normalidad, Independencia..)

### Normalidad_Prueba shapiro-wilk 
### Los errorres siguen una distribución normal
### Si p-valor es mayor que 0.05 asumimos normalidad 
shapiro_test<-shapiro.test(ei);shapiro_test
shapiro_test<-shapiro.test(ri);shapiro_test
### Metodo Grafico 
qqnorm(ei)#residuales 
qqline(ei)


### Homocedasticidad (Varianza Constante de Residuos)
### Prueba de homocedasticidad (Breusch-Pagan)
library(lmtest)
bptest(modelo)
### Gráfico de dispersión de los residuos contra los valores ajustados
plot(modelo$fitted.values, modelo$residuals)
abline(h = 0, col = "red", lty = 2)


### Aleatoriedad de los Residuos
### Gráfico de residuos contra la variable predictora
plot(x1, modelo$residuals)
abline(h = 0, col = "red", lty = 2)


### Independencia de los Residuos
### Gráfico de autocorrelación parcial de los residuos
library(car)
residualPlots(modelo)

