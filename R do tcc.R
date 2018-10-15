library(dplyr)
library(readxl)
RESULTADO_TCC <- read_excel("C:/Users/debsoliveira/Desktop/tcc- gleyce/RESULTADO TCC.xlsx", 
                            sheet = "certo")
#View(RESULTADO_TCC)

dados= RESULTADO_TCC %>% tbl_df()
summary(dados)

#selecting useful columns
questionario = dados %>%
  select(Phys, Emot, Others)


#add a new columns to sum up all 
 questionario = dados %>%
  mutate(
    questions = Phys + Emot + Others
    )
   
 #29 minutos
summary(questionario$questions)

dados = dados %>%
  rename(Questoes = Phys) %>%
  rename(Questoes = Emot) %>%
  rename(Questoes = Others) 

#na.omit(dados) #remove as linhas com NA

#?complete.cases() # retorna observações completas

#is.na(dados) # identifica os NA

#Correlação
#?cor.test
cor.test(dados$NYHAPACIENTE,dados$NYHAprontu�rio)

t.test(dados$NYHAPACIENTE,dados$NYHAprontu�rio) #maior diferença

#distribui��o do n�mero de tabagistas
hist(dados$Tabagista)

boxplot(dados$Idade)
plot(x=dados$Others,y=dados$Emot)
cor.test(dados$Others,dados$Emot)
t.test(dados$Others,dados$Emot)
?tapply

tapply(dados$Phys,dados$Emot,mean) 

library(Hmisc)
m<- rcorr(as.matrix(dados)) #coeficiente de correlação, n e valor p
m

?rcorr

m$r # matriz de coeficiente de correlação
m$P # matriz de valor p

library(corrplot)

corrplot(m$r,p.mat=m$P,sig.level= 0.005)

corrplot(m$r, method="shade")

#corrplot(m$r,p.mat=m$P,sig.level=0.001,method="color",type="upper")


library("PerformanceAnalytics")

md<- dados[,c(15,16)] # filtrando apenas algumas colunas
chart.Correlation(md, histogram = TRUE)
#chart.Correlation(md)

View(dados)
md<- dados[,c(12,16)]
chart.Correlation(md, histogram = TRUE)


library(corrplot)

?corrplot
corrplot(dados) # padrão com c�???rculos
corrplot(dados, method="color") #cores

library(xda)

#resume as variáveis numéricas
numSummary(dados)
