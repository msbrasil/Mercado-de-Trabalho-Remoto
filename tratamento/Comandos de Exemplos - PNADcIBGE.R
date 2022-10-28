##############################################################################################################################

# Limpando arquivos armazenados na memoria
rm(list = ls(all = TRUE))

# Definindo limite de memoria para compilacao do programa
aviso <- getOption("warn")
options(warn = -1)
memory.limit(size = 20000)
options(warn = aviso)
rm(aviso)

# Definindo opcao de codificacao dos caracteres e linguagem
aviso <- getOption("warn")
options(warn = -1)
options(encoding = "latin1")
options(warn = aviso)
rm(aviso)

# Definindo opcao de exibicao de numeros sem exponencial
aviso <- getOption("warn")
options(warn = -1)
options(scipen = 999)
options(warn = aviso)
rm(aviso)

# Definindo opcao de repositorio para instalacao dos pacotes necessarios
aviso <- getOption("warn")
options(warn = -1)
options(repos = structure(c(CRAN = "https://cran.r-project.org/")))
options(warn = aviso)
rm(aviso)

# Definindo area de trabalho
caminho <- getwd()
setwd(caminho)

# Carregando informacoes do pacote PNADcIBGE
if ("PNADcIBGE" %in% rownames(installed.packages()) == FALSE) {
  install.packages("PNADcIBGE", dependencies = TRUE)
}
library("PNADcIBGE")
packageDescription("PNADcIBGE")
help(package = "PNADcIBGE")

# Descrevendo as funcoes do pacote PNADcIBGE
get_pnadc
read_pnadc
pnadc_labeller
pnadc_deflator
pnadc_design
pnadc_example

# Obtendo informacoes do pacote haven
if ("haven" %in% rownames(installed.packages()) == FALSE) {
  install.packages("haven")
}
library("haven")
write_sas
write_dta
write_sav

# Gerando microdados da PNAD Continua
# variaveis_selecionadas <- c("V2007", "V2009", "V2010", "VD3004", "VD4019", "VD4031")
#variaveis_selecionadas <- c("Ano", "Trimestre", "UF", "UPA","V1014", "V1016", "V1008", "V2007", "V2010", "VD4010", "V4022")
#dadosPNADc2022_2 <- get_pnadc(year = 2022, quarter = 2, design = FALSE)
#write.csv(dadosPNADc2022_2, '/home/matheus/Microdados PNAD/2022_2.csv')
#dadosPNADc2022_1 <- get_pnadc(year = 2022, quarter = 1, design = FALSE)
#write.csv(dadosPNADc2022_1, '/home/matheus/Microdados PNAD/2022_1.csv')
#dadosPNADc2021_4 <- get_pnadc(year = 2021, quarter = 4, design = FALSE)
#write.csv(dadosPNADc2021_4, '/home/matheus/Microdados PNAD/2021_4.csv')
#dadosPNADc2021_3 <- get_pnadc(year = 2021, quarter = 3, design = FALSE)
#write.csv(dadosPNADc2021_3, '/home/matheus/Microdados PNAD/2021_3.csv')
#dadosPNADc2021_2 <- get_pnadc(year = 2021, quarter = 2, design = FALSE)
#write.csv(dadosPNADc2021_2, '/home/matheus/Microdados PNAD/2021_2.csv')
#dadosPNADc2021_1 <- get_pnadc(year = 2021, quarter = 1, design = FALSE)
#write.csv(dadosPNADc2021_1, '/home/matheus/Microdados PNAD/2021_1.csv')
#dadosPNADc2020_4 <- get_pnadc(year = 2020, quarter = 4, design = FALSE)
#write.csv(dadosPNADc2020_4, '/home/matheus/Microdados PNAD/2020_4.csv')
#dadosPNADc2020_3 <- get_pnadc(year = 2020, quarter = 3, design = FALSE)
#write.csv(dadosPNADc2020_3, '/home/matheus/Microdados PNAD/2020_3.csv')
#dadosPNADc2020_2 <- get_pnadc(year = 2020, quarter = 2, design = FALSE)
#write.csv(dadosPNADc2020_2, '/home/matheus/Microdados PNAD/2020_2.csv')
#dadosPNADc2020_1 <- get_pnadc(year = 2020, quarter = 1, design = FALSE)
#write.csv(dadosPNADc2020_1, '/home/matheus/Microdados PNAD/2020_1.csv')
dadosPNADc2019_4 <- get_pnadc(year = 2019, quarter = 4, design = FALSE)
write.csv(dadosPNADc2019_4, '/home/matheus/Microdados PNAD/2019_4.csv')
#dadosPNADc2019_3 <- get_pnadc(year = 2019, quarter = 3, design = FALSE)
#write.csv(dadosPNADc2019_3, '/home/matheus/Microdados PNAD//2019_3.csv')
#dadosPNADc2019_2 <- get_pnadc(year = 2019, quarter = 2, design = FALSE)
#write.csv(dadosPNADc2019_2, '/home/matheus/Microdados PNAD//2019_2.csv')
dadosPNADc2019_1 <- get_pnadc(year = 2019, quarter = 1, design = FALSE)
write.csv(dadosPNADc2019_1, '/home/matheus/Microdados PNAD/2019_1.csv')
dadosPNADc2018_4 <- get_pnadc(year = 2018, quarter = 4, design = FALSE)
dadosPNADc2018_3 <- get_pnadc(year = 2018, quarter = 3, design = FALSE)
dadosPNADc2018_2 <- get_pnadc(year = 2018, quarter = 2, design = FALSE)
dadosPNADc2018_1 <- get_pnadc(year = 2018, quarter = 1, design = FALSE)

#write.csv(dadosPNADc2022_2, '/media/matheus/HD/UFPR/Microdados PNADC/microdados_filtrado/2022_2.csv')


head(dadosPNADc2022_2)

# baixar todos os conteúdos
# concatenar
# exportar e analisar no python

# Carregando informacoes do pacote survey
if ("survey" %in% rownames(installed.packages()) == FALSE) {
  install.packages("survey")
}
library("survey")

# Estimando total da renda mensal habitual
totalrenda <- svytotal(x = ~VD4019, design = dadosPNADc, na.rm = TRUE)
totalrenda

# Exibindo coeficiente de variacao e intervalo de confianca
cv(totalrenda)
confint(totalrenda)

# Estimando total de sexo e cor ou raca
totalsexoraca <- svytotal(x = ~ V2007 + V2010, design = dadosPNADc, na.rm = TRUE)
totalsexoraca

# Estimando media da renda mensal habitual
mediarenda <- svymean(x = ~VD4019, design = dadosPNADc, na.rm = TRUE)
mediarenda

# Estimando proporcao de sexo
propsexo <- svymean(x = ~V2007, design = dadosPNADc, na.rm = TRUE)
propsexo

# Estimando mediana e quantis da renda mensal habitual
medianarenda <- svyquantile(x = ~VD4019, design = dadosPNADc, quantiles = c(0.25, 0.5, 0.75, 0.9, 0.95, 0.98), na.rm = TRUE)
medianarenda

# Estimando a renda media para mulheres acima de 30 anos
rendaM30 <- svymean(x = ~VD4019, design = subset(dadosPNADc, V2007 == "Mulher" & V2009 > 30), na.rm = TRUE)
rendaM30

# Estimando a frequencia relativa de homens e mulheres em cada nivel de instrucao
freqSexoInstr <- svyby(formula = ~V2007, by = ~VD3004, design = dadosPNADc, FUN = svymean, na.rm = TRUE, vartype = NULL)
print(freqSexoInstr, row.names = FALSE)

# Esbocando boxplot do numero de horas trabalhadas por sexo
svyboxplot(formula = VD4031 ~ V2007, design = dadosPNADc, all.outliers = TRUE)

# Esbocando histograma da distribuicao de frequencia do numero de horas trabalhadas
svyhist(formula = ~ as.numeric(VD4031), design = dadosPNADc, main = "Histograma", xlab = "N?mero de Horas Trabalhadas", ylab = "Densidade")

# Esbocando grafico de dispersao entre numero de horas trabalhadas e renda mensal habitual
svyplot(formula = VD4019 ~ VD4031, design = dadosPNADc, style = "transparent", xlab = "Horas habitualmente trabalhadas", ylab = "Rendimento habitual")

# Realizando teste t de student para diferenca de rendimentos entre sexos
svyttest(formula = VD4019 ~ V2007, design = dadosPNADc)

# Estimando a frequencia de diferentes locais de trabalho para cada UF
freqUf <- svyby(formula = ~V4022, by = ~UF, design = dadosPNADc, FUN = svytotal, na.rm = TRUE, vartype = NULL)
print(freqUf, row.names = FALSE)




# Obtendo mais informacoes do pacote survey
help(package = "survey")

# Carregando informacoes do pacote convey
if ("convey" %in% rownames(installed.packages()) == FALSE) {
  install.packages("convey")
}
library("convey")
help(package = "convey")

##############################################################################################################################
