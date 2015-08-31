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
pt.fit = glm(pt_value ~ sbp_value + deficiency_anemias + paralysis + coagulopathy + fluid_electrolyte +
               co2_value + last_drug + gl_value + charttime_time + hypertension + valvular_disease + 
               renal_failure + chronic_pulmonary + rheumatoid_arthritis + pulmonary_circulation + 
               rr_value:sbp_value + hr_value:gl_value + weight:sbp_value + height:hr_value + sex:hr_value + 
               sex:weight + sex:gl_value + gl_value:co2_value + height:gl_value + weight:hr_value + 
               admit_age:co2_value + height:sbp_value + admit_age:rr_value, data=data, family=Gamma(link="inverse"))
summary(pt.fit)
#maiorPvalue(pt.fit)
# R² = 0.755
cor(pt.fit$fitted.values,pt.fit$y)^2

### Validation
## Checking model assumptions using residuals
pt.estud = rstudent(pt.fit)

# Histogram
hist(pt.estud, main="Histogram studentized residals", breaks="FD", freq=FALSE)
curve(dnorm(x, mean=0, sd=1), col="red", lwd=2, add=TRUE)

# qqplot
qqPlot(pt.estud, distribution="norm", pch=20, main="QQ-Plot studentized residuals")

# shapiro, p-value = 0.4675 Normal com 95% de certeza
shapiro.test(pt.estud)

## Predict
# Selecionar amostra para testes
pt.data.test = data.test
pt.data.test = arrange(pt.data.test, pt_value)

# Calcular os valores baseado na regressão
pt.predict = data.frame(fit=predict.glm(pt.fit, pt.data.test, type="response"))
pt.predict = arrange(pt.predict, fit)

# Plotar o gráfico
ggplot(pt.predict, aes(x=as.integer(rownames(pt.predict)), y=fit, colour="predict")) +
  geom_line(shape=1) +
  geom_line(data=pt.data.test, mapping=aes(x=as.integer(rownames(pt.data.test)), y=pt_value, colour="sample")) +
  labs(title="Predict pt_value", x="", y="pt_value") +
  scale_colour_manual("Values of", breaks=c("predict", "sample"), values=c("predict"="red", "sample"="black"))

# Verificar normalidade
shapiro.test(pt.data.test$pt_value)
shapiro.test(pt.predict$fit)

# p-value = 0.3914, com 95% de confiança eu posso não rejeita a H0. Então os dados são iguais
t.test(pt.data.test$pt_value, pt.predict$fit, paired=F, var.equal=T, conf.level=0.95)

rm(pt.fit, pt.predict, pt.data.test, pt.estud, maiorPvalue)
