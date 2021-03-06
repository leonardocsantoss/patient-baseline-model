# Bibliotecas
library(plyr)
library(kohonen)

options(scipen=999)

# Carrega os dados
data = read.csv("data.csv", head=T)

# Removendo a data e pegando apenas a hora
data$charttime_date = NULL
tmp.chartime_time = strsplit(array(data$charttime_time), "-")
tmp.chartime_time = matrix(unlist(tmp.chartime_time), ncol=3, byrow=TRUE)
data$charttime_time = tmp.chartime_time[,1]

remove(tmp.chartime_time)

#### Limpando os dados
## Linhas: 2861
## Observações: 2861*5 = 14305
## Pacientes: 1216
## Internações: 1235

# Limpando os campos desnecessários
data$hr = data$bp = data$rr = data$bt = data$gl = data$co2 = NULL

### Removendo as linhas duplicadas
data = data[!duplicated(data), ]

## Após remoção das linhas duplicadas
## Linhas: 1679
## Observações: 1679*5 = 8395
## Pacientes: 1216
## Internações: 1235


### Aplicando limiares

# 15 <= admit_age < 101
data = subset(data, admit_age>=15)
data = subset(data, admit_age<101)

# 100 <= height < 201
data = subset(data, height>=100)
data = subset(data, height<201)

# weight > 0
data = subset(data, weight>0)

# 38 <= hr_value < 151
data = subset(data, hr_value>=38)
data = subset(data, hr_value<151)

# 45 <= sbp_value < 213
data = subset(data, sbp_value>=45)
data = subset(data, sbp_value<213)

# 8 <= rr_value < 39
data = subset(data, rr_value>=8)
data = subset(data, rr_value<39)

# 31 <= bt_value < 43
data = subset(data, bt_value>=31)
data = subset(data, bt_value<43)

# 47 <= gl_value < 189
data = subset(data, gl_value>=47)
data = subset(data, gl_value<189)

## Nº após limiares
## Linhas: 1474
## Observações: 1475*5 = 7370
## Pacientes: 1092
## Internações: 1108

### Internação com no mínimo 15 obs.(3 linhas) e no máximo 50 obs.(10 linhas) com a intenção de reduzir a instabilidade dos dados
tmp.lenght_icustay = ddply(data, .(icustay_id), nrow)
names(tmp.lenght_icustay) = c("icustay_id", "lenght_icustay")
data = merge(data, tmp.lenght_icustay, by="icustay_id")
data = subset(data, lenght_icustay>=3)
data = subset(data, lenght_icustay<=10)
remove(tmp.lenght_icustay)

## Nº após limpeza no número de internações
## Linhas: 221
## Observações: 221*5 = 1105
## Pacientes: 60
## Internações: 60

### Selecionando a 2 observações randomicas para gerar os modelos e 1 para testar eles.
data = arrange(data, icustay_id, charttime_time)

tmp.data = subset(data, lenght_icustay==0)
data.test = subset(data, lenght_icustay==0)

for(tmp.rdata in split(data, data$icustay_id)){
  m = sample(1:nrow(tmp.rdata), 3)

  tmp.data = rbind(tmp.data, tmp.rdata[m[1], ])
  tmp.data = rbind(tmp.data, tmp.rdata[m[2], ])
  data.test = rbind(data.test, tmp.rdata[m[3], ])
}

data = tmp.data
remove(tmp.data, tmp.rdata, m)

### Nº após Selecionando a 1ª observação e a do meio
## Linhas: 120
## Observações: 120*5 = 600
## Pacientes: 60
## Internações: 60
### Dados de testes
## Linhas: 60
## Observações: 60*5 = 300
## Pacientes: 60
## Internações: 60

# Removendo as variáveis que não possuem valores: peptic_ulcer, aids, blood_loss_anemia, drug_abuse, psychoses, depression
data$peptic_ulcer = data$aids = data$blood_loss_anemia = data$drug_abuse = data$psychoses = data$depression = data$lenght_icustay = NULL
data.test$peptic_ulcer = data.test$aids = data.test$blood_loss_anemia = data.test$drug_abuse = data.test$psychoses = data.test$depression = data.test$lenght_icustay = NULL

# Ajustando variáveis
data$charttime_time = as.factor(data$charttime_time)
data$last_drug = as.factor(data$last_drug)

data.test$charttime_time = as.factor(data.test$charttime_time)
data.test$last_drug = as.factor(data.test$last_drug)

# Teste rápido de regressão
summary(lm(rr_value ~ other_neurological + paralysis + hypertension + cardiac_arrhythmias + gl_value + chronic_pulmonary + diabetes_complicated + hypothyroidism + rheumatoid_arthritis + last_drug + obesity + bt_value + pulmonary_circulation + renal_failure + metastatic_cancer + sex + coagulopathy + congestive_heart_failure + fluid_electrolyte + sbp_value + liver_disease + solid_tumor + alcohol_abuse + deficiency_anemias + height + weight + valvular_disease + co2_value + admit_age + weight_loss + diabetes_uncomplicated + charttime_time + peripheral_vascular + hr_value + lymphoma + bt_value:gl_value + hr_value:bt_value + admit_age:weight + height:sbp_value + hr_value:gl_value + admit_age:sbp_value + height:weight + hr_value:co2_value + weight:co2_value + sex:gl_value + weight:bt_value + sex:hr_value + sex:bt_value + sex:co2_value + weight:gl_value + sex:weight + admit_age:co2_value + hr_value:sbp_value + admit_age:height + weight:sbp_value + admit_age:gl_value + height:hr_value + sbp_value:bt_value + admit_age:bt_value + admit_age:hr_value + sbp_value:co2_value + sex:sbp_value + height:gl_value + height:bt_value + height:co2_value + sex:admit_age + gl_value:co2_value + weight:hr_value + bt_value:co2_value + sex:height + sbp_value:gl_value, data=data))$r.squared
summary(lm(hr_value ~ hypothyroidism + weight_loss + renal_failure + lymphoma + obesity + metastatic_cancer + chronic_pulmonary + fluid_electrolyte + rheumatoid_arthritis + co2_value + liver_disease + height + alcohol_abuse + admit_age + peripheral_vascular + sex + pulmonary_circulation + gl_value + paralysis + other_neurological + weight + diabetes_uncomplicated + rr_value + last_drug + coagulopathy + valvular_disease + deficiency_anemias + solid_tumor + bt_value + charttime_time + cardiac_arrhythmias + congestive_heart_failure + diabetes_complicated + hypertension + sbp_value + sex:gl_value + sex:rr_value + weight:sbp_value + height:bt_value + sex:weight + height:co2_value + sbp_value:gl_value + admit_age:bt_value + bt_value:co2_value + admit_age:sbp_value + sex:co2_value + height:weight + gl_value:co2_value + rr_value:co2_value + height:sbp_value + weight:gl_value + admit_age:rr_value + admit_age:co2_value + sex:height + bt_value:gl_value + height:rr_value + admit_age:height + sex:bt_value + height:gl_value + rr_value:sbp_value + sbp_value:bt_value + weight:rr_value + sex:sbp_value + rr_value:bt_value + admit_age:weight + rr_value:gl_value + weight:bt_value + weight:co2_value + admit_age:gl_value + sbp_value:co2_value + sex:admit_age, data=data))$r.squared
summary(lm(sbp_value ~ co2_value + bt_value + renal_failure + diabetes_complicated + lymphoma + pulmonary_circulation + alcohol_abuse + congestive_heart_failure + diabetes_uncomplicated + chronic_pulmonary + paralysis + peripheral_vascular + gl_value + other_neurological + hypertension + metastatic_cancer + deficiency_anemias + sex + weight + coagulopathy + solid_tumor + last_drug + charttime_time + hypothyroidism + admit_age + cardiac_arrhythmias + rr_value + rheumatoid_arthritis + valvular_disease + weight_loss + obesity + liver_disease + fluid_electrolyte + height + hr_value + admit_age:gl_value + weight:co2_value + height:weight + weight:bt_value + rr_value:bt_value + sex:gl_value + sex:bt_value + rr_value:hr_value + sex:hr_value + rr_value:gl_value + admit_age:height + hr_value:co2_value + height:rr_value + admit_age:bt_value + hr_value:gl_value + hr_value:bt_value + sex:weight + sex:co2_value + admit_age:rr_value + sex:height + height:bt_value + rr_value:co2_value + gl_value:co2_value + weight:gl_value + weight:hr_value + admit_age:weight + sex:admit_age + height:gl_value + height:hr_value + bt_value:co2_value + height:co2_value + sex:rr_value + admit_age:co2_value + weight:rr_value + admit_age:hr_value + bt_value:gl_value, data=data))$r.squared
summary(lm(bt_value ~ sbp_value + rr_value + solid_tumor + deficiency_anemias + height + paralysis + coagulopathy + obesity + fluid_electrolyte + other_neurological + peripheral_vascular + admit_age + co2_value + alcohol_abuse + congestive_heart_failure + lymphoma + metastatic_cancer + liver_disease + last_drug + gl_value + charttime_time + weight_loss + hypertension + diabetes_complicated + cardiac_arrhythmias + valvular_disease + hypothyroidism + renal_failure + chronic_pulmonary + weight + diabetes_uncomplicated + sex + hr_value + rheumatoid_arthritis + pulmonary_circulation + rr_value:sbp_value + hr_value:co2_value + hr_value:sbp_value + rr_value:gl_value + hr_value:gl_value + height:weight + rr_value:hr_value + weight:sbp_value + sex:admit_age + sex:co2_value + height:hr_value + sex:hr_value + admit_age:hr_value + height:co2_value + weight:gl_value + sbp_value:co2_value + sbp_value:gl_value + sex:weight + height:rr_value + sex:gl_value + rr_value:co2_value + admit_age:height + weight:rr_value + sex:rr_value + admit_age:weight + admit_age:sbp_value + sex:height + gl_value:co2_value + height:gl_value + admit_age:gl_value + weight:hr_value + admit_age:co2_value + height:sbp_value + weight:co2_value + sex:sbp_value + admit_age:rr_value, data=data))$r.squared


# Salvando o data.RData
save(data, file="data.RData")
save(data.test, file="data.test.RData")