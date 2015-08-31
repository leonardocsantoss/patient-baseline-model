# Bibliotecas
library(car)
library(ggplot2)
library(MASS)
library(boot)
library(plyr)
options(scipen=999, digits=3)

# Carrega os dados
load("data.RData")
load("data.test.RData")

##### Análise inferencial

maiorPvalue = function(fit){
  coefficients = data.frame(summary(fit)$coefficients)
  charttime_time = c('charttime_time01', 'charttime_time02', 'charttime_time03', 'charttime_time04', 'charttime_time05', 'charttime_time06', 'charttime_time07', 'charttime_time08', 'charttime_time09', 'charttime_time10', 'charttime_time11', 'charttime_time12', 'charttime_time13', 'charttime_time14', 'charttime_time15', 'charttime_time16', 'charttime_time17', 'charttime_time18', 'charttime_time19', 'charttime_time20', 'charttime_time21', 'charttime_time22', 'charttime_time23')
  last_drugs = c('last_drugDigoxin', 'last_drugDilantin', 'last_drugFK506', 'last_drugGentamycin/Peak', 'last_drugPhenobarbital', 'last_drugVancomycin/Peak', 'last_drugVancomycin/Random', 'last_drugVancomycin/Trough')
  coefficients = subset(coefficients, !(rownames(coefficients) %in% charttime_time) & !(rownames(coefficients) %in% last_drugs) & rownames(coefficients) != "(Intercept)")
  print(subset(coefficients, Pr...t.. == max(coefficients$Pr...t..)))
}

### Modelo de regressão da Pressão Arterial Sistólica(sbp_value)

# Modelo proposto por Silva (2015) usando MLGs
sbp.fit = glm(sbp_value ~ co2_value + pt_value + renal_failure + congestive_heart_failure + 
                other_neurological + sex + solid_tumor + last_drug + charttime_time + hypothyroidism + 
                cardiac_arrhythmias + rheumatoid_arthritis + valvular_disease + height + hr_value + 
                weight:co2_value + sex:pt_value + sex:hr_value + rr_value:gl_value + hr_value:gl_value +
                hr_value:pt_value + sex:co2_value + height:pt_value + gl_value:co2_value + weight:hr_value +
                admit_age:weight + height:gl_value + height:hr_value + pt_value:co2_value + height:co2_value +
                admit_age:co2_value, data=data, family=Gamma(link="inverse"))
summary(sbp.fit)
maiorPvalue(sbp.fit)
# R² = 0.825
cor(sbp.fit$fitted.values,sbp.fit$y)^2

### Validation
## Checking model assumptions using residuals
sbp.estud = rstudent(sbp.fit)

# Histogram
hist(sbp.estud, main="Histogram studentized residals", breaks="FD", freq=FALSE)
curve(dnorm(x, mean=0, sd=1), col="red", lwd=2, add=TRUE)

# qqplot
qqPlot(sbp.estud, distribution="norm", pch=20, main="QQ-Plot studentized residuals")

# shapiro, p-value = 0.06131 Normal com 95% de certeza
shapiro.test(sbp.estud)

## Predict
# Selecionar amostra para testes
sbp.data.test = data.test
sbp.data.test = arrange(sbp.data.test, sbp_value)

# Calcular os valores baseado na regressão
sbp.predict = data.frame(fit=predict.glm(sbp.fit, sbp.data.test, type="response"))
sbp.predict = arrange(sbp.predict, fit)

# Plotar o gráfico
ggplot(sbp.predict, aes(x=as.integer(rownames(sbp.predict)), y=fit, colour="predict")) +
  geom_line(shape=1) +
  geom_line(data=sbp.data.test, mapping=aes(x=as.integer(rownames(sbp.data.test)), y=sbp_value, colour="sample")) +
  labs(title="Predict sbp_value", x="", y="sbp_value") +
  scale_colour_manual("Values of", breaks=c("predict", "sample"), values=c("predict"="red", "sample"="black"))

# p-value = 0.3837, com 95% de confiança eu não posso rejeita a H0. Então os dados são iguais
t.test(sbp.data.test$sbp_value, sbp.predict$fit, paired=F, var.equal=T, conf.level=0.95)

rm(sbp.fit, sbp.predict, sbp.data.test, sbp.estud, maiorPvalue)
