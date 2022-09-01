
# === LIBRERIAS === (((
sprintf(" --- LIBRERIAS --- ")
suppressPackageStartupMessages(library(lmtest))
suppressPackageStartupMessages(library(gridExtra))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(GGally))
suppressPackageStartupMessages(library(car))
suppressPackageStartupMessages(library(agricolae))
suppressPackageStartupMessages(library(qqplotr))
suppressPackageStartupMessages(library(stats))
suppressPackageStartupMessages(library(DescTools))
# )))

# === DATOS === (((
sprintf(" --- DATOS --- ")
datos = read.table('DATOS_COMPLETOS.txt', header = TRUE, stringsAsFactors = TRUE)
attach(datos)
datos$Concentracion = as.factor(datos$Concentracion)
regresion_lineal = lm(mV ~ Sexo * Compuesto * Concentracion,data = datos)
# regresion_lineal$coefficients
# )))

# === CAJAS SIMULTÁNEAS === (((
sprintf(" --- DIAGRAMA DE CAJAS SIMULTÁNEAS --- ")
ggplot(datos) + aes(x=Sexo,y=mV, fill = Sexo) + geom_boxplot(alpha=0.8) + facet_wrap(~Compuesto) 
ggplot(datos) + aes(x=Sexo,y=mV, fill = Sexo) + geom_boxplot(alpha=0.8) + facet_wrap(~Concentracion) 
ggplot(datos) + aes(x=Compuesto,y=mV, fill = Compuesto) + geom_boxplot(alpha=0.8) + facet_wrap(~Sexo) + theme(axis.text.x = element_blank(), legend.position = "bottom")
ggplot(datos) + aes(x=Compuesto,y=mV, fill = Compuesto) + geom_boxplot(alpha=0.8) + facet_wrap(~Concentracion)  + theme(legend.position =  "bottom",text = element_text(size = 12), axis.text.x = element_blank())
ggplot(datos) + aes(x=Concentracion,y=mV, fill = Concentracion) + geom_boxplot(alpha=0.8) + facet_wrap(~Sexo) 
ggplot(datos) + aes(x=Concentracion,y=mV, fill = Concentracion) + geom_boxplot(alpha=0.8) + facet_wrap(~Compuesto) 

# GRAFICO IMPORTANTE :)
ggplot(datos) + aes(x=Compuesto,y=mV, fill = Concentracion) + geom_boxplot(alpha=0.8) + facet_wrap(~Sexo)  + theme(legend.position = "bottom", text = element_text(size = 12), axis.text.x = element_text(angle = 90, hjust = 1))
# )))

# === SUPUESTOS DEL MODELO === (((
sprintf(" --- SUPUESTOS DEL MODELO --- ")

# === NORMALIDAD === (((
sprintf(" --- 1. NORMALIDAD --- ")
# GRÁFICA
resultados  =  aov(regresion_lineal)
anova       =  anova(resultados)
residuos    =  resid(resultados)
# ggplot(data = NULL, mapping=aes(sample=residuos)) + stat_qq_band() + stat_qq_line() + stat_qq_point() + labs(x="Cuantiles Teóricos", y="Cuantiles")
# SHAPIRO
sprintf("Shapiro Test.")
shapiro.test(residuos)
# KOLMOGOROV-SMIRNOFF
sprintf("KS Test")
ks.test(residuos,"pnorm",0, sqrt(anova$Mean[8])) # es el 8vo elemento de la Tabla ANOVA [columna Mean].
# )))

# === HOMOCEDASTICIDAD === (((
sprintf(" --- 2. HOMOCEDASTICIDAD --- ")
# BARTLETT
leveneTest(mV ~ interaction(Sexo,Concentracion,Compuesto), data = datos)
bartlett.test(mV ~ interaction(Sexo,Concentracion,Compuesto), data = datos)
# GRÁFICO
# y_estimada       =  fitted(regresion)
# residu_estandar  =  rstudent(regresion)
# graf_1 = ggplot(data = NULL, aes(x= F.Control,y=residuos)) + geom_point() + ggtitle("mV vs Resiuales.")      + labs(x="mV",y="Residuales")     + geom_hline(yintercept=0)
# graf_2 = ggplot(data = NULL, aes(x= F.Control,y=residu_estandar))   + geom_point() + ggtitle("mV vs Resid. Estand.")  + labs(x="mV",y="Resid. Estand.") + geom_hline(yintercept=0)
# grid.arrange(graf_1,graf_2,ncol=2,nrow=1) 
# )))

# === INDEPENDENCIA === (((
sprintf(" --- 3. INDEPENDENCIA --- ")
# DURBIM WATSON
dwtest(regresion_lineal)
# GRÁFICO
# n = dim(datos)[1]
# ggplot(data = NULL, aes(x=1:n,y=residu_estandar)) + geom_line() + ggtitle("Orden de Corrida vs Resid. Estand.") + labs(x="Orden de Corrida",y="Resid. Estand.")  + geom_hline(yintercept=0)
# )))

# )))

# === ANOVA === (((
sprintf(" --- ANOVA --- ")
anova
sprintf("Desv. Estand.")
sqrt(anova$Sum[8])
sprintf("SUMA DE CUADRADOS:")
sum(anova$Sum) # se calcula el total de la Suma de Cuadrados
sprintf("GRADOS DE LIBERTAD DE SCT:")
sum(anova$Df) # se calculan los grados de libertad de SCT
# )))

# === NUEVA ANOVA === (((
sprintf(" --- NUEVA ANOVA --- ")
sprintf("Se elimina la variable Sexo, porque no es significativa.")
regresion_lineal = lm(mV ~ Compuesto * Concentracion,data = datos)
# regresion_lineal$coefficients
resultados = aov(regresion_lineal)
anova = anova(resultados)
anova
sprintf("Desv. Estand.")
sqrt(anova$Sum[4])
sprintf("SUMA DE CUADRADOS:")
sum(anova$Sum) # se calcula el total de la Suma de Cuadrados
sprintf("GRADOS DE LIBERTAD DE SCT:")
sum(anova$Df) # se calculan los grados de libertad de SCT
# )))

# === TUKEY (COMPARACIONES) === (((
sprintf(" --- TUKEY COMPARACIONES --- ")
TukeyHSD(resultados)
# )))

# === LSD === (((
sprintf("--- LSD ---")
LSD.test(regresion_lineal , c("Compuesto" , "Concentracion") , alpha=0.05 , console=TRUE)
# )))
