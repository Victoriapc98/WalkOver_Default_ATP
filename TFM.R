#Cargamos la base de datos y las librerias necesarias para limparla. 
load("C:/Users/Victoria/Desktop/TFM/Datos ATP/231109_ATP.RData")
library(dplyr)
#Creamos una base de datos con las variables de interes. 
ATP <- subset(ATP, select = -sex)
df_ATP<- select(ATP,c(-Columna1, -Columna2, -Retirement, -sex))

#Observamos la base de datos. 


#Creamos la variable Walkover y recodificamos WalkOver y Default. 
df_ATP$WalkOver <- ifelse(df_ATP$Final_Partit == "W/O", TRUE, FALSE)
df_ATP$Default<-factor(df_ATP$Default, levels = c("TRUE", "FALSE"), labels = c("YES", "NO"))
df_ATP$WalkOver<-factor(df_ATP$WalkOver, levels = c("TRUE", "FALSE"), labels = c("YES", "NO"))

#Recodificamos el resto de variables y comprobamos los cambios. 
df_ATP$year <- as.numeric(df_ATP$year)
df_ATP$tourney_level<- as.factor(df_ATP$tourney_level)
df_ATP$winner_hand<- as.factor(df_ATP$winner_hand)
df_ATP$surface<- as.factor(df_ATP$surface)
df_ATP$loser_hand<- as.factor(df_ATP$loser_hand)
df_ATP$round<- as.factor(df_ATP$round)
df_ATP$Final_Partit<- as.factor(df_ATP$Final_Partit)
df_ATP$sets<- as.factor(df_ATP$sets)
df_ATP$winner_rank<-as.numeric(df_ATP$winner_rank)
df_ATP$loser_rank<- as.numeric(df_ATP$loser_rank)
df_ATP$dif_rank<- as.numeric(df_ATP$dif_rank)
str(df_ATP)

##Resumimos las variables.

library(SmartEDA)
library(openxlsx)

#Creamos un informe exploratorio. 
ExpReport(
  df_ATP,
  Template = NULL,
  Target = NULL,
  label = NULL,
  theme = "Default",
  op_file = "report.html",
  op_dir = getwd(),
  sc = NULL,
  sn = NULL,
  Rc = NULL
)

#Variables categÃ³ricas

Summary_Cat<- ExpCTable(df_ATP, Target = NULL, margin = 1, clim = 10, nlim = 10, round = 2, bin = 3, per = FALSE, weight = NULL)
write.xlsx(Summary_Cat, "C:/Users/Victoria/Desktop/TFM/Datos ATP/Summary_Cat.csv", rowNames = FALSE)

#Variables numÃ©ricas. 

Summary_Num<-ExpNumStat(df_ATP,by="A",gp=NULL,Qnt=seq(0,1,0.1),MesofShape=2,Outlier=TRUE,round=2)
selected_columns <- c("Vname", "mean", "median", "SD", "IQR")
Summary_numeric <- Summary_Num[, selected_columns]
write.xlsx(Summary_Num, "C:/Users/Victoria/Desktop/TFM/Datos ATP/Summary_Num.csv", rowNames = FALSE)

# Cálculo de la proporción de incidencia de WalkOver
prop_walkover <- sum(df_ATP$WalkOver == "YES") / nrow(df_ATP)

# Cálculo del intervalo de confianza para la proporción de WalkOver (95%)
ci_walkover <- prop.test(sum(df_ATP$WalkOver == "YES"), nrow(df_ATP))$conf.int

# Cálculo de la proporción de incidencia de Default
prop_default <- sum(df_ATP$Default == "YES") / nrow(df_ATP)

# Cálculo del intervalo de confianza para la proporción de Default (95%)
ci_default <- prop.test(sum(df_ATP$Default == "YES"), nrow(df_ATP))$conf.int

# Imprimir resultados
cat("Proporción de incidencia de WalkOver:", prop_walkover, "\n")
cat("Intervalo de confianza para WalkOver:", ci_walkover[1], "-", ci_walkover[2], "\n\n")

cat("Proporción de incidencia de Default:", prop_default, "\n")
cat("Intervalo de confianza para Default:", ci_default[1], "-", ci_default[2], "\n")

##Análisis bivariante.
library(compareGroups)
#Sintesis covariables según presencia de Walkover.
summary_walkover<-compareGroups(WalkOver~tourney_level+surface+round+sets+winner_hand+loser_hand+winner_age+loser_age+dif_age+winner_rank+loser_rank+dif_rank, df_ATP)
df_summary_walkover<- createTable(summary_walkover)
export2word(df_summary_walkover, file='summary_walkover.docx')

#Síntesis covariables según presencia de Defaults. 
summary_default<-compareGroups(Default~tourney_level+surface+round+sets+games+winner_hand+loser_hand+winner_age+loser_age+dif_age+winner_rank+loser_rank+dif_rank, df_ATP)
df_summary_default<- createTable(summary_default)
export2word(df_summary_default, file='summary_default.docx')
