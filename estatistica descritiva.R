# Bibliotecas
options(scipen=9999, digits=3)

# Carrega os dados
load("data.RData")
load("data.test.RData")

##### Análise descritiva

# Função summary.full adiciona o Desvio padrão no summary
summary.full = function(field){
  print(summary(field))
  print("S.D.")
  print(sd(field))
}

summary.full(data$admit_age)
summary.full(data$height)
summary.full(data$weight)
summary.full(data$rr_value)
summary.full(data$hr_value)
summary.full(data$sbp_value)
summary.full(data$pt_value)
summary.full(data$gl_value)
summary.full(data$co2_value)

# Função para plotar o histograma e boxpot
hist.plot = function(data, field, ylab="", xlab="", main=""){
  par(mfrow=c(1,2), oma=c(0,0,2,0), cex.lab=0.8)
  field.data = data[, field]
  
  hist(field.data, prob=TRUE, right=T, col="gray", ylab=ylab, xlab=paste(c("Shapiro-Wilk: ", shapiro.test(field.data)$p.value)), main="") 
  lines(density(field.data, adjust=2), lwd=2, lty=1, col="red")
  curve(dnorm(x, mean=mean(field.data), sd=sd(field.data, 2)), add=TRUE, lwd=2, lty=2, col="blue")
  
  boxplot(field.data, col="gray", yylab=ylab, xlab=xlab, main="", horizontal=T)
  text(max(field.data), 1.15, round(max(field.data), digits=2), cex=1.0)
  text(median(field.data), 1.35, round(median(field.data), digits=2), cex=1.0)
  text(min(field.data), 1.15, round(min(field.data), digits=2), cex=1.0)
  text(quantile(field.data, 1/4), 1.3, "Q1", cex=1.0)
  text(quantile(field.data, 3/4), 1.3, "Q3", cex=1.0)
  title(main=main, outer=TRUE)
}

hist.plot(data, "admit_age", main="Idade")
hist.plot(data, "height", main="Altura")
hist.plot(data, "weight", main="Peso")
hist.plot(data, "gl_value", main="Glicose")
hist.plot(data, "co2_value", main="CO2")
hist.plot(data, "rr_value", main="Frequência respiratória")
hist.plot(data, "hr_value", main="Frequência cardíaca")
hist.plot(data, "sbp_value", main="Pressão arterial sistólica")
hist.plot(data, "pt_value", main="Temperatura corporal")


# Plot dos dados de testes
hist.plot(data.test, "admit_age", main="Idade")
hist.plot(data.test, "height", main="Altura")
hist.plot(data.test, "weight", main="Peso")
hist.plot(data.test, "gl_value", main="Glicose")
hist.plot(data.test, "co2_value", main="CO2")
hist.plot(data.test, "rr_value", main="Frequência respiratória")
hist.plot(data.test, "hr_value", main="Frequência cardíaca")
hist.plot(data.test, "sbp_value", main="Pressão arterial sistólica")
hist.plot(data.test, "pt_value", main="Temperatura corporal")
