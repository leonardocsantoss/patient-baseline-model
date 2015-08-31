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

### Modelo de regressão da Frequência Cardíaca(hr_value)

# Modelo proposto por Silva (2015) usando MLGs
hr.fit = glm(hr_value ~ obesity + 
               chronic_pulmonary + fluid_electrolyte + rheumatoid_arthritis + liver_disease + sex + 
               pulmonary_circulation + gl_value + paralysis + diabetes_uncomplicated + last_drug + 
               valvular_disease + deficiency_anemias + charttime_time + cardiac_arrhythmias + 
               congestive_heart_failure + hypertension + sex:weight + height:co2_value + sbp_value:gl_value +
               admit_age:sbp_value + sex:co2_value + height:weight + rr_value:co2_value + height:sbp_value +
               admit_age:rr_value + height:rr_value + rr_value:sbp_value + weight:rr_value + 
               sex:sbp_value + rr_value:pt_value + weight:pt_value + weight:co2_value +
               sex:admit_age, data=data, family=Gamma(link="inverse"))
#step(hr.fit)
summary(hr.fit)
maiorPvalue(hr.fit)
# R² = 0.822
cor(hr.fit$fitted.values,hr.fit$y)^2


### Validation
## Checking model assumptions using residuals
hr.estud = rstudent(hr.fit)

# Histogram
hist(hr.estud, main="Histogram studentized residals", breaks="FD", freq=FALSE)
curve(dnorm(x, mean=0, sd=1), col="red", lwd=2, add=TRUE)

# qqplot
qqPlot(hr.estud, distribution="norm", pch=20, main="QQ-Plot studentized residuals")

# shapiro, p-value = 0.9918. Normal com 95% de certeza
shapiro.test(hr.estud)

## Predict
# Selecionar amostra para testes
hr.data.test = data.test
hr.data.test = arrange(hr.data.test, hr_value)

# Calcular os valores baseado na regressão
hr.predict = data.frame(fit=predict.glm(hr.fit, hr.data.test, type="response"))
hr.predict = arrange(hr.predict, fit)

# Plotar o gráfico
ggplot(hr.predict, aes(x=as.integer(rownames(hr.predict)), y=fit, colour="predict")) +
  geom_line(shape=1) +
  geom_line(data=hr.data.test, mapping=aes(x=as.integer(rownames(hr.data.test)), y=hr_value, colour="sample")) +
  labs(title="Predict hr_value", x="", y="hr_value") +
  scale_colour_manual("Values of", breaks=c("predict", "sample"), values=c("predict"="red", "sample"="black"))

# Verificar normalidade
shapiro.test(hr.data.test$hr_value)
shapiro.test(hr.predict$fit)

# p-value = 0.7036, com 95% de confiança eu não posso rejeita a H0. Então os dados são iguais
t.test(hr.data.test$hr_value, hr.predict$fit, paired=T, var.equal=T, conf.level=0.95)

rm(hr.fit, hr.predict, hr.data.test, hr.estud, maiorPvalue)
