library(readr)
amostra <- read_csv("Banco/amostra_150167636.csv")

# summary(amostra$NOTA_LP)
# str(amostra)

library(tidyverse)

amostra <- amostra %>%
  mutate_if(sapply(amostra, is.character), as.factor)
amostra$ANO <- factor(amostra$ANO)
amostra$MUNICIPIO <- factor(amostra$MUNICIPIO)

# 2. Liste as variáveis disponibilizadas na amostra que você recebeu (com seu número de matrícula) e classifique-as segundo o tipo.
# rmkd

# 3. Escolha CINCO variáveis categóricas e apresente-as em forma tabular e gráfica. Comente os resultados.

# COMPUTADOR     ESC_MAE        ESC_PAI        AFAZERES_DOM          TRABALHO       
summary(amostra$COMPUTADOR)
summary(amostra$ESC_MAE)
summary(amostra$ESC_PAI)
summary(amostra$AFAZERES_DOM)
summary(amostra$TRABALHO)



# 4. Para as variáveis quantitativas NOTA_LP e NOTA_MT apresente:
# Distribuição de frequências, com intervalos de classe

summary(amostra$NOTA_LP)
# < 200 | <200-250 | 250-300 | 300-350 | 350 +

lp1 <- amostra$NOTA_LP[amostra$NOTA_LP<200]
lp2 <- amostra$NOTA_LP[amostra$NOTA_LP>200&amostra$NOTA_LP<250]
lp3 <- amostra$NOTA_LP[amostra$NOTA_LP>250&amostra$NOTA_LP<300]
lp4 <- amostra$NOTA_LP[amostra$NOTA_LP>300&amostra$NOTA_LP<350]
lp5 <- amostra$NOTA_LP[amostra$NOTA_LP>350]


summary(amostra$NOTA_MT)
# < 200 | <200-250 | 250-300 | 300-350 | 350 +
mt1 <- amostra$NOTA_MT[amostra$NOTA_MT<200]
mt2 <- amostra$NOTA_MT[amostra$NOTA_MT>200&amostra$NOTA_MT<250]
mt3 <- amostra$NOTA_MT[amostra$NOTA_MT>250&amostra$NOTA_MT<300]
mt4 <- amostra$NOTA_MT[amostra$NOTA_MT>300&amostra$NOTA_MT<350]
mt5 <- amostra$NOTA_MT[amostra$NOTA_MT>350]


Histograma
Medidas de posição, variabilidade, assimetria e curtose.
Box-plot
Análise dos resultados, incluindo comentários sobre normalidade (não é para fazer testes de hipóteses).
