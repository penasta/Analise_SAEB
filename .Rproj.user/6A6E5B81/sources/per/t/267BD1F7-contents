library(readr)
library(tidyverse)
library(RColorBrewer)
library(viridis)  
library(e1071)
library(psych)
library(pastecs)

amostra <- read_csv("Banco/amostra_150167636.csv")

# summary(amostra$NOTA_LP)
# str(amostra)

amostra <- amostra %>%
  mutate_if(sapply(amostra, is.character), as.factor)
amostra$ANO <- factor(amostra$ANO)
amostra$MUNICIPIO <- factor(amostra$MUNICIPIO)

# 2. Liste as variáveis disponibilizadas na amostra que você recebeu (com seu número de matrícula) 
# e classifique-as segundo o tipo.

# rmkd

# 3. Escolha CINCO variáveis categóricas e apresente-as em forma tabular e gráfica. 
# Comente os resultados.

# COMPUTADOR     ESC_MAE        ESC_PAI        AFAZERES_DOM          TRABALHO       

#summary(amostra$COMPUTADOR)
#summary(amostra$ESC_MAE)
#summary(amostra$ESC_PAI)
#summary(amostra$AFAZERES_DOM)
#summary(amostra$TRABALHO)

#--------------------------------------------------

table(amostra$COMPUTADOR)
amostra$COMPUTADOR <- factor(amostra$COMPUTADOR,levels = c("Não tem", "Sim, um", "Sim, dois",
                                                           "Sim, três","quatro ou mais"))
ggplot(amostra,aes(COMPUTADOR)) + 
  geom_bar(na.rm=T,fill=c("red","blue","green","yellow"))+ 
  scale_x_discrete(na.translate = FALSE)+ 
  ggtitle("Título") +
  xlab("Eixo X") +
  ylab("Eixo Y")

#--------------------------------------------------

table(amostra$ESC_MAE)
amostra$ESC_MAE <- factor(amostra$ESC_MAE,levels = c("Nunca estudou",
"Não completou a 4.ª série/5.º ano do Ensino Fundamental ",
"Completou a 4.ª série/5.º ano, mas não completou a 8.ª série/9.º ano do Ensino Fundamental",
"Completou a 8.ª série/9.º ano do Ensino Fundamental, mas não completou o Ensino Médio",
"Completou o Ensino Médio, mas não completou a Faculdade",
"Completou a Faculdade","Não sei"))
ggplot(amostra,aes(ESC_MAE)) +
  geom_bar(na.rm=T,fill=c("red","blue","green","yellow","yellow","yellow","yellow"))+
  scale_x_discrete(na.translate = FALSE)+ 
  ggtitle("Título") +
  xlab("Eixo X") +
  ylab("Eixo Y")

#--------------------------------------------------

table(amostra$ESC_PAI)
amostra$ESC_PAI <- factor(amostra$ESC_PAI,levels = c("Nunca estudou",
"Não completou a 4.ª série/5.º ano do Ensino Fundamental ",
"Completou a 4.ª série/5.º ano, mas não completou a 8.ª série/9.º ano do Ensino Fundamental",
"Completou a 8.ª série/9.º ano do Ensino Fundamental, mas não completou o Ensino Médio",
"Completou o Ensino Médio, mas não completou a Faculdade",
"Completou a Faculdade","Não sei"))
ggplot(amostra,aes(ESC_PAI)) +
  geom_bar(na.rm=T,fill=c("red","blue","green","yellow","yellow","yellow","yellow"))+
  scale_x_discrete(na.translate = FALSE)+ 
  ggtitle("Título") +
  xlab("Eixo X") +
  ylab("Eixo Y")

#--------------------------------------------------

table(amostra$AFAZERES_DOM)
amostra$COMPUTADOR <- factor(amostra$COMPUTADOR,levels = c("Não faço trabalhos domésticos",
"Menos de 1 hora", "Entre 1 e 2 horas","Mais de 2 horas, até 3 horas","Mais de 3 horas"))
ggplot(amostra,aes(AFAZERES_DOM)) +
  geom_bar(na.rm=T,fill=c("red","blue","green","yellow","yellow"))+
  scale_x_discrete(na.translate = FALSE)+ 
  ggtitle("Título") +
  xlab("Eixo X") +
  ylab("Eixo Y")

#--------------------------------------------------

table(amostra$TRABALHO)
ggplot(amostra,aes(TRABALHO)) +
  geom_bar(na.rm=T,fill=c("red","blue"))+
  scale_x_discrete(na.translate = FALSE)+ 
  ggtitle("Título") +
  xlab("Eixo X") +
  ylab("Eixo Y")

#--------------------------------------------------
#--------------------------------------------------
#--------------------------------------------------
#--------------------------------------------------

# 4. Para as variáveis quantitativas NOTA_LP e NOTA_MT apresente:
# Distribuição de frequências, com intervalos de classe


#--------------------------------------------------

# Exemplo:
# < 200 | <200-250 | 250-300 | 300-350 | 350 +
# lp1 <- amostra$NOTA_LP[amostra$NOTA_LP<200]
# lp2 <- amostra$NOTA_LP[amostra$NOTA_LP>200&amostra$NOTA_LP<250]
# lp3 <- amostra$NOTA_LP[amostra$NOTA_LP>250&amostra$NOTA_LP<300]
# lp4 <- amostra$NOTA_LP[amostra$NOTA_LP>300&amostra$NOTA_LP<350]
# lp5 <- amostra$NOTA_LP[amostra$NOTA_LP>350]

# < 200 | <200-250 | 250-300 | 300-350 | 350 +
# mt1 <- amostra$NOTA_MT[amostra$NOTA_MT<200]
# mt2 <- amostra$NOTA_MT[amostra$NOTA_MT>200&amostra$NOTA_MT<250]
# mt3 <- amostra$NOTA_MT[amostra$NOTA_MT>250&amostra$NOTA_MT<300]
# mt4 <- amostra$NOTA_MT[amostra$NOTA_MT>300&amostra$NOTA_MT<350]
# mt5 <- amostra$NOTA_MT[amostra$NOTA_MT>350]

#--------------------------------------------------

# Histograma

hist(amostra$NOTA_LP,
     main = "título",
     xlab = "eixo x", 
     ylab = "eixo y")

hist(amostra$NOTA_MT,
     main = "título",
     xlab = "eixo x", 
     ylab = "eixo y")

# Medidas de posição, variabilidade, assimetria e curtose.

skewness(amostra$NOTA_LP)
skewness(amostra$NOTA_MT)

kurtosis(amostra$NOTA_LP)
kurtosis(amostra$NOTA_MT)

# Box-plot

boxplot(amostra$NOTA_MT,amostra$NOTA_LP, 
        names=c("Notas matemática","Notas português"),
        xlab="Eixo X",ylab="eixo Y", col = c("orange", "yellow"),
        main = "Título")

# Análise dos resultados, incluindo comentários sobre normalidade (não é para fazer testes de hipóteses).

