# Bibliotecas
library(car)
library(ggplot2)
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

### Modelo de regressão da Frequência respiratória(rr_value)
# Modelo proposto por Silva (2015) usando MLGs
rr.fit = glm(rr_value ~ other_neurological + gl_value + chronic_pulmonary + hypothyroidism + 
               rheumatoid_arthritis + last_drug + renal_failure + metastatic_cancer + sex + 
               congestive_heart_failure + fluid_electrolyte + solid_tumor + weight + admit_age + 
               weight_loss + charttime_time + lymphoma + weight:admit_age + sbp_value:admit_age +
               co2_value:hr_value + weight:co2_value + pt_value:sex + sex:co2_value + height:hr_value +
               pt_value:admit_age + pt_value:height + height:co2_value + sex:admit_age +
               pt_value:co2_value, data=data, family=inverse.gaussian(link = "1/mu^2"))
#step(rr.fit)
summary(rr.fit)
maiorPvalue(rr.fit)
# R² = 0.711
cor(rr.fit$fitted.values,rr.fit$y)^2

### Validation
## Checking model assumptions using residuals
rr.estud = rstudent(rr.fit)

# Histogram
hist(rr.estud, main="Histogram studentized residals", breaks="FD", freq=FALSE)
curve(dnorm(x, mean=0, sd=1), col="red", lwd=2, add=TRUE)

# qqplot
qqPlot(rr.estud, distribution="norm", pch=20, main="QQ-Plot studentized residuals")

# shapiro, p-value = 0.995. Normal com 95% de certeza
shapiro.test(rr.estud)

## Predict
# Selecionar amostra para testes
rr.data.test = data.test
rr.data.test = arrange(rr.data.test, rr_value)

# Calcular os valores baseado na regressão
rr.predict = data.frame(fit=predict.glm(rr.fit, rr.data.test, type="response"))
rr.predict = arrange(rr.predict, fit)

# Plotar o gráfico
ggplot(rr.predict, aes(x=as.integer(rownames(rr.predict)), y=fit, colour="predict")) +
  geom_line(shape=1) +
  geom_line(data=rr.data.test, mapping=aes(x=as.integer(rownames(rr.data.test)), y=rr_value, colour="sample")) +
  labs(title="Predict rr_value", x="", y="rr_value") +
  scale_colour_manual("Values of", breaks=c("predict", "sample"), values=c("predict"="red", "sample"="black"))

# Verificar normalidade
shapiro.test(rr.data.test$rr_value)
shapiro.test(rr.predict$fit)

# p-value = 0.5012, com 95% de confiança eu não posso rejeita a H0. Então os dados são iguais
t.test(rr.data.test$rr_value, rr.predict$fit, paired=T, var.equal=T, conf.level=0.95)

rm(rr.fit, rr.predict, rr.data.test, rr.estud, maiorPvalue)
