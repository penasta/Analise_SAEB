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

# < 200 | <200-250 | 250-300 | 300-350 | 350 +
lp1 <- amostra$NOTA_LP[amostra$NOTA_LP<200]
lp2 <- amostra$NOTA_LP[amostra$NOTA_LP>200&amostra$NOTA_LP<250]
lp3 <- amostra$NOTA_LP[amostra$NOTA_LP>250&amostra$NOTA_LP<300]
lp4 <- amostra$NOTA_LP[amostra$NOTA_LP>300&amostra$NOTA_LP<350]
lp5 <- amostra$NOTA_LP[amostra$NOTA_LP>350]

t <- data.frame(length(lp1),length(lp2),length(lp3),length(lp4),length(lp5))

# < 200 | <200-250 | 250-300 | 300-350 | 350 +
mt1 <- amostra$NOTA_MT[amostra$NOTA_MT<200]
mt2 <- amostra$NOTA_MT[amostra$NOTA_MT>200&amostra$NOTA_MT<250]
mt3 <- amostra$NOTA_MT[amostra$NOTA_MT>250&amostra$NOTA_MT<300]
mt4 <- amostra$NOTA_MT[amostra$NOTA_MT>300&amostra$NOTA_MT<350]
mt5 <- amostra$NOTA_MT[amostra$NOTA_MT>350]

t2 <- data.frame(length(mt1),length(mt2),length(mt3),length(mt4),length(mt5))
t2

colnames(t) <- c("Até 200","200 a 250","250 a 300","300 a 350","350 ou mais")
colnames(t2) <- c("Até 200","200 a 250","250 a 300","300 a 350","350 ou mais")

freq <- rbind(t,t2)
rownames(freq) <- c("Notas em língua portuguesa","Notas em matemática")

rm(t,t2,lp1,lp2,lp3,lp4,lp5,mt1,mt2,mt3,mt4,mt5)

#--------------------------------------------------

# Histograma

hist(amostra$NOTA_LP,
     main = "Histograma - Notas em língua portuguesa",
     xlab = "", 
     ylab = "")

hist(amostra$NOTA_MT,
     main = "Histograma - Notas em matemática",
     xlab = "", 
     ylab = "")

#--------------------------------------------------
# Medidas de posição, variabilidade(Média, Mediana, desvio padrão, Range, Mean Deviation,
# Interquartile Range), assimetria e curtose

# NOTA_LP

m1 <- quantile(amostra$NOTA_LP)
m1 <-as.data.frame(m1)
m2 <- mean(amostra$NOTA_LP)
m3 <- median(amostra$NOTA_LP)
m4 <- sd(amostra$NOTA_LP)
m5 <- max(amostra$NOTA_LP)-min(amostra$NOTA_LP)
m6 <- sum(abs(amostra$NOTA_LP-mean(amostra$NOTA_LP)))/length(amostra$NOTA_LP)
m7 <- IQR(amostra$NOTA_LP)
m8 <- skewness(amostra$NOTA_LP)
m9 <- kurtosis(amostra$NOTA_LP)

mlp <- rbind(m1,m2,m3,m4,m5,m6,m7,m8,m9)

colnames(mlp) <- "Valores"
rownames(mlp) <- c("Menor valor","Primeiro quartil","Segundo quartil","Terceiro quartil","Maior valor",
                   "Média","Mediana","Desvio padrão","Amplitude","Deesvio absoluto",
                   "Amplitude interquartílica","Assimetria","Curtose")
mlp <- t(mlp)
mlp <-as.data.frame(mlp)

rm(m1,m2,m3,m4,m5,m6,m7,m8,m9)

# NOTA_MT

m1 <- quantile(amostra$NOTA_MT)
m1 <-as.data.frame(m1)
m2 <- mean(amostra$NOTA_MT)
m3 <- median(amostra$NOTA_MT)
m4 <- sd(amostra$NOTA_MT)
m5 <- max(amostra$NOTA_MT)-min(amostra$NOTA_MT)
m6 <- sum(abs(amostra$NOTA_MT-mean(amostra$NOTA_MT)))/length(amostra$NOTA_MT)
m7 <- IQR(amostra$NOTA_MT)
m8 <- skewness(amostra$NOTA_MT)
m9 <- kurtosis(amostra$NOTA_MT)

mmt <- rbind(m1,m2,m3,m4,m5,m6,m7,m8,m9)

colnames(mmt) <- "Valores"
rownames(mmt) <- c("Menor valor","Primeiro quartil","Segundo quartil","Terceiro quartil","Maior valor",
                   "Média","Mediana","Desvio padrão","Amplitude","Deesvio absoluto",
                   "Amplitude interquartílica","Assimetria","Curtose")
mmt <- t(mmt)
mmt <-as.data.frame(mmt)

rm(m1,m2,m3,m4,m5,m6,m7,m8,m9)

#--------------------------------------------------

# Box-plot

boxplot(amostra$NOTA_MT,amostra$NOTA_LP, 
        names=c("Notas matemática","Notas português"),
        xlab="",ylab="",
        main = "Box-plot das notas em matemática e língua portuguesa")#,
      # col = c("orange", "yellow")

# Análise dos resultados, incluindo comentários sobre normalidade (não é para fazer testes de hipóteses).

pacman::p_load(dplyr,ggpubr)

# Visual methods
# Density plot and Q-Q plot can be used to check normality visually.
# Density plot: the density plot provides a visual judgment about whether the distribution is bell shaped.

ggdensity(amostra$NOTA_MT, 
          main = "Gráfico de densidade das notas em matemática",
          xlab = "Notas",
          ylab = "Densidade")

ggdensity(amostra$NOTA_LP, 
          main = "Gráfico de densidade das notas em língua portuguesa",
          xlab = "Notas",
          ylab = "Densidade")

# Q-Q plot: Q-Q plot (or quantile-quantile plot) draws the correlation between 
# a given sample and the normal distribution. A 45-degree reference line is also plotted.

ggqqplot(amostra$NOTA_MT)
ggqqplot(amostra$NOTA_LP)

# Testes de normalidade:

shapiro.test(amostra$NOTA_MT)
shapiro.test(amostra$NOTA_LP)
