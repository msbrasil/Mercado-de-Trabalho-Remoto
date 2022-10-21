##############################################################################################################################

# Exercicio 00 - Configurando memoria e opcoes do programa
rm(list=ls(all=TRUE))
aviso <- getOption("warn")
options(warn=-1)
memory.limit(size=20000)
options(encoding="latin1")
options(repos=structure(c(CRAN="https://cran.r-project.org/")))
options(warn=aviso)
rm(aviso)
caminho <- getwd()
setwd(caminho)

# Exercicio 01 - Carregando pacotes e gerando microdados
if("PNADcIBGE" %in% rownames(installed.packages())==FALSE)
{
  install.packages("PNADcIBGE", dependencies=TRUE)
}
library("PNADcIBGE")
if("survey" %in% rownames(installed.packages())==FALSE)
{
  install.packages("survey")
}
library("survey")
if("convey" %in% rownames(installed.packages())==FALSE)
{
  install.packages("convey")
}
library("convey")
dadosPNADc_3Tri2017 <- get_pnadc(year=2017, quarter=3, labels=TRUE, deflator=TRUE, design=TRUE)
dadosPNADc_4Tri2017 <- get_pnadc(year=2017, quarter=4, labels=TRUE, deflator=TRUE, design=TRUE)
dadosPNADc_1Tri2018 <- get_pnadc(year=2018, quarter=1, labels=TRUE, deflator=TRUE, design=TRUE)
dadosPNADc_2Tri2018 <- get_pnadc(year=2018, quarter=2, labels=TRUE, deflator=TRUE, design=TRUE)
dadosPNADc_3Tri2018 <- get_pnadc(year=2018, quarter=3, labels=TRUE, deflator=TRUE, design=TRUE)
dadosPNADc_4Tri2018 <- get_pnadc(year=2018, quarter=4, labels=TRUE, deflator=TRUE, design=TRUE)
dadosPNADc_1Tri2019 <- get_pnadc(year=2019, quarter=1, labels=TRUE, deflator=TRUE, design=TRUE)
dadosPNADc_2Tri2019 <- get_pnadc(year=2019, quarter=2, labels=TRUE, deflator=TRUE, design=TRUE)
dadosPNADc_3Tri2019 <- get_pnadc(year=2019, quarter=3, labels=TRUE, deflator=TRUE, design=TRUE)
dadosPNADc_1Ent2018 <- get_pnadc(year=2018, interview=1, labels=TRUE, deflator=TRUE, design=TRUE, defyear=2018)
dadosPNADc_5Ent2018 <- get_pnadc(year=2018, interview=5, labels=TRUE, deflator=TRUE, design=TRUE, defyear=2018)
dadosPNADc_2Tem2017 <- get_pnadc(year=2017, topic=2, labels=TRUE, deflator=TRUE, design=TRUE, defyear=2017, defperiod=2)
dadosPNADc_4Tem2017 <- get_pnadc(year=2017, topic=4, labels=TRUE, deflator=TRUE, design=TRUE, defyear=2017, defperiod=4)

# Exercicio 02 - Obtendo populacao por nivel de instrucao e media dos rendimentos reais
PopInst_1Tri2018 <- svytotal(x=~VD3004, design=dadosPNADc_1Tri2018, na.rm=TRUE)
PopInst_1Tri2018
PopInst_2Tri2018 <- svytotal(x=~VD3004, design=dadosPNADc_2Tri2018, na.rm=TRUE)
PopInst_2Tri2018
PopInst_3Tri2018 <- svytotal(x=~VD3004, design=dadosPNADc_3Tri2018, na.rm=TRUE)
PopInst_3Tri2018
PopInst_4Tri2018 <- svytotal(x=~VD3004, design=dadosPNADc_4Tri2018, na.rm=TRUE)
PopInst_4Tri2018
dadosPNADc_1Tri2018$variables <- transform(dadosPNADc_1Tri2018$variables, RendHabDef=VD4019*Habitual)
dadosPNADc_1Tri2018$variables <- transform(dadosPNADc_1Tri2018$variables, RendEfeDef=VD4020*Efetivo)
RendHabDef_1Tri2018 <- svymean(x=~RendHabDef, design=dadosPNADc_1Tri2018, na.rm=TRUE)
RendHabDef_1Tri2018
RendEfeDef_1Tri2018 <- svymean(x=~RendEfeDef, design=dadosPNADc_1Tri2018, na.rm=TRUE)
RendEfeDef_1Tri2018
dadosPNADc_2Tri2018$variables <- transform(dadosPNADc_2Tri2018$variables, RendHabDef=VD4019*Habitual)
dadosPNADc_2Tri2018$variables <- transform(dadosPNADc_2Tri2018$variables, RendEfeDef=VD4020*Efetivo)
RendHabDef_2Tri2018 <- svymean(x=~RendHabDef, design=dadosPNADc_2Tri2018, na.rm=TRUE)
RendHabDef_2Tri2018
RendEfeDef_2Tri2018 <- svymean(x=~RendEfeDef, design=dadosPNADc_2Tri2018, na.rm=TRUE)
RendEfeDef_2Tri2018
dadosPNADc_3Tri2018$variables <- transform(dadosPNADc_3Tri2018$variables, RendHabDef=VD4019*Habitual)
dadosPNADc_3Tri2018$variables <- transform(dadosPNADc_3Tri2018$variables, RendEfeDef=VD4020*Efetivo)
RendHabDef_3Tri2018 <- svymean(x=~RendHabDef, design=dadosPNADc_3Tri2018, na.rm=TRUE)
RendHabDef_3Tri2018
RendEfeDef_3Tri2018 <- svymean(x=~RendEfeDef, design=dadosPNADc_3Tri2018, na.rm=TRUE)
RendEfeDef_3Tri2018
dadosPNADc_4Tri2018$variables <- transform(dadosPNADc_4Tri2018$variables, RendHabDef=VD4019*Habitual)
dadosPNADc_4Tri2018$variables <- transform(dadosPNADc_4Tri2018$variables, RendEfeDef=VD4020*Efetivo)
RendHabDef_4Tri2018 <- svymean(x=~RendHabDef, design=dadosPNADc_4Tri2018, na.rm=TRUE)
RendHabDef_4Tri2018
RendEfeDef_4Tri2018 <- svymean(x=~RendEfeDef, design=dadosPNADc_4Tri2018, na.rm=TRUE)
RendEfeDef_4Tri2018

# Exercicio 03 - Obtendo percentual de populacao na forca de trabalho e em idade para trabalhar por nivel de instrucao
PropForca_3Tri2017 <- svymean(x=~VD4001, design=dadosPNADc_3Tri2017, na.rm=TRUE)
PropForca_3Tri2017
PropForca_3Tri2018 <- svymean(x=~VD4001, design=dadosPNADc_3Tri2018, na.rm=TRUE)
PropForca_3Tri2018
PropForca_3Tri2019 <- svymean(x=~VD4001, design=dadosPNADc_3Tri2019, na.rm=TRUE)
PropForca_3Tri2019
PropForcaInst_3Tri2017 <- svyby(formula=~VD4001, by=~VD3004, design=subset(dadosPNADc_3Tri2017, V2009 >= 14), FUN=svymean, na.rm=TRUE, vartype=NULL)
print(PropForcaInst_3Tri2017, row.names=FALSE)
PropForcaInst_3Tri2018 <- svyby(formula=~VD4001, by=~VD3004, design=subset(dadosPNADc_3Tri2018, V2009 >= 14), FUN=svymean, na.rm=TRUE, vartype=NULL)
print(PropForcaInst_3Tri2018, row.names=FALSE)
PropForcaInst_3Tri2019 <- svyby(formula=~VD4001, by=~VD3004, design=subset(dadosPNADc_3Tri2019, V2009 >= 14), FUN=svymean, na.rm=TRUE, vartype=NULL)
print(PropForcaInst_3Tri2019, row.names=FALSE)

# Exercicio 04 - Obtendo a taxa de desocupacao e o grafico desta serie
TaxDesocup_3Tri2017 <- svyratio(numerator=~VD4002=="Pessoas desocupadas", denominator=~VD4001=="Pessoas na força de trabalho", design=dadosPNADc_3Tri2017, na.rm=TRUE)
TaxDesocup_3Tri2017
TaxDesocup_4Tri2017 <- svyratio(numerator=~VD4002=="Pessoas desocupadas", denominator=~VD4001=="Pessoas na força de trabalho", design=dadosPNADc_4Tri2017, na.rm=TRUE)
TaxDesocup_4Tri2017
TaxDesocup_1Tri2018 <- svyratio(numerator=~VD4002=="Pessoas desocupadas", denominator=~VD4001=="Pessoas na força de trabalho", design=dadosPNADc_1Tri2018, na.rm=TRUE)
TaxDesocup_1Tri2018
TaxDesocup_2Tri2018 <- svyratio(numerator=~VD4002=="Pessoas desocupadas", denominator=~VD4001=="Pessoas na força de trabalho", design=dadosPNADc_2Tri2018, na.rm=TRUE)
TaxDesocup_2Tri2018
TaxDesocup_3Tri2018 <- svyratio(numerator=~VD4002=="Pessoas desocupadas", denominator=~VD4001=="Pessoas na força de trabalho", design=dadosPNADc_3Tri2018, na.rm=TRUE)
TaxDesocup_3Tri2018
TaxDesocup_4Tri2018 <- svyratio(numerator=~VD4002=="Pessoas desocupadas", denominator=~VD4001=="Pessoas na força de trabalho", design=dadosPNADc_4Tri2018, na.rm=TRUE)
TaxDesocup_4Tri2018
TaxDesocup_1Tri2019 <- svyratio(numerator=~VD4002=="Pessoas desocupadas", denominator=~VD4001=="Pessoas na força de trabalho", design=dadosPNADc_1Tri2019, na.rm=TRUE)
TaxDesocup_1Tri2019
TaxDesocup_2Tri2019 <- svyratio(numerator=~VD4002=="Pessoas desocupadas", denominator=~VD4001=="Pessoas na força de trabalho", design=dadosPNADc_2Tri2019, na.rm=TRUE)
TaxDesocup_2Tri2019
TaxDesocup_3Tri2019 <- svyratio(numerator=~VD4002=="Pessoas desocupadas", denominator=~VD4001=="Pessoas na força de trabalho", design=dadosPNADc_3Tri2019, na.rm=TRUE)
TaxDesocup_3Tri2019
TaxDesocup_Temporario <- round(c(TaxDesocup_3Tri2017$ratio[1],TaxDesocup_4Tri2017$ratio[1],TaxDesocup_1Tri2018$ratio[1],TaxDesocup_2Tri2018$ratio[1],TaxDesocup_3Tri2018$ratio[1],TaxDesocup_4Tri2018$ratio[1],TaxDesocup_1Tri2019$ratio[1],TaxDesocup_2Tri2019$ratio[1],TaxDesocup_3Tri2019$ratio[1])*100, digits=1)
plot(x=c(1:length(TaxDesocup_Temporario)), y=TaxDesocup_Temporario, xlim=c(0,10), ylim=c(0,15), xaxt="n", yaxt="s", xlab="Trimestre/Ano", ylab="Taxa de Desocupação (%)", main="Taxa de Desocupação por Trimestre e Ano")
axis(side=1, at=c(1:length(TaxDesocup_Temporario)), labels=c("3Tri2017","4Tri2017","1Tri2018","2Tri2018","3Tri2018","4Tri2018","1Tri2019","2Tri2019","3Tri2019"))
lines(x=c(1:length(TaxDesocup_Temporario)), y=TaxDesocup_Temporario)
text(x=c(1:length(TaxDesocup_Temporario)), y=TaxDesocup_Temporario+1, labels=TaxDesocup_Temporario)
rm(TaxDesocup_Temporario)

# Exercicio 05 - Obtendo rendimento real domiciliar por UF
dadosPNADc_1Ent2018$variables <- transform(dadosPNADc_1Ent2018$variables, ChaveDom=paste0(UPA,V1008,V1014))
dadosPNADc_1Ent2018$variables <- transform(dadosPNADc_1Ent2018$variables, RendHabDef=VD4019*CO2)
dadosPNADc_1Ent2018$variables <- transform(dadosPNADc_1Ent2018$variables, RendEfeDef=VD4048*CO2e)
dadosPNADc_1Ent2018_Temporario <- subset(dadosPNADc_1Ent2018, V2005 != "17" & V2005 != "18" & V2005 != "19")
dadosPNADc_1Ent2018_Temporario$variables <- transform(dadosPNADc_1Ent2018_Temporario$variables, SomaDef=rowSums(cbind(RendHabDef,RendEfeDef), na.rm=TRUE))
RendDomDef_Temporario <- aggregate(formula=SomaDef~ChaveDom, data=dadosPNADc_1Ent2018_Temporario$variables, FUN=sum)
dadosPNADc_1Ent2018_Temporario$variables$RendDomDef <- as.vector(unlist(merge(x=dadosPNADc_1Ent2018_Temporario$variables, y=RendDomDef_Temporario, by.x="ChaveDom", by.y="ChaveDom", all.x=TRUE, all.y=FALSE)$SomaDef.y))
rm(RendDomDef_Temporario)
RendDomUF_1Ent2018 <- svyby(formula=~RendDomDef, by=~UF, design=dadosPNADc_1Ent2018_Temporario, FUN=svytotal, na.rm=TRUE, vartype=NULL)
print(RendDomUF_1Ent2018, row.names=FALSE)
rm(dadosPNADc_1Ent2018_Temporario)

# Exercicio 06 [microdados anuais acumulados em determinada visita] - Obtendo tempo dedicado a cuidado com pessoas e afazeres domesticos por sexo
SomenteCuidadoSexo_5Ent2018 <- svyby(formula=~V4121B, by=~V2007, design=subset(dadosPNADc_5Ent2018, (V4117A == "1" | V4119 == "1") & (V4120 != "1" & V4121A != "1")), FUN=svymean, na.rm=TRUE, vartype=NULL)
print(SomenteCuidadoSexo_5Ent2018, row.names=FALSE)
confint(object=SomenteCuidadoSexo_5Ent2018, level=0.95)
SomenteAfazeresSexo_5Ent2018 <- svyby(formula=~V4121B, by=~V2007, design=subset(dadosPNADc_5Ent2018, (V4117A != "1" & V4119 != "1") & (V4120 == "1" | V4121A == "1")), FUN=svymean, na.rm=TRUE, vartype=NULL)
print(SomenteAfazeresSexo_5Ent2018, row.names=FALSE)
confint(object=SomenteAfazeresSexo_5Ent2018, level=0.95)
CuidadoAfazeresSexo_5Ent2018 <- svyby(formula=~V4121B, by=~V2007, design=subset(dadosPNADc_5Ent2018, (V4117A == "1" | V4119 == "1") & (V4120 == "1" | V4121A == "1")), FUN=svymean, na.rm=TRUE, vartype=NULL)
print(CuidadoAfazeresSexo_5Ent2018, row.names=FALSE)
confint(object=CuidadoAfazeresSexo_5Ent2018, level=0.95)

# Exercicio 06 [microdados anuais concentrados em determinado trimestre] - Obtendo taxa de analfabetismo para base de educacao e media do rendimento domiciliar per capita por posse de televisao
AnalfEduc_2Tem2017 <- svyby(formula=~V3001, by=~UF, design=subset(dadosPNADc_2Tem2017, substr(x=UPA, start=1, stop=1) == 3 & V2009 >= 15), FUN=svymean, na.rm=TRUE, vartype=NULL)
print(AnalfEduc_2Tem2017, row.names=FALSE)
confint(object=AnalfEduc_2Tem2017, level=0.95)
RendTIC_4tem2017 <- svyby(formula=~VDI5008, by=~substr(x=UPA, start=1, stop=1)+S01025, design=subset(dadosPNADc_4Tem2017, V2005 == "Pessoa responsável pelo domicílio"), FUN=svymean, na.rm=TRUE, vartype=NULL)
print(RendTIC_4tem2017, row.names=FALSE)
confint(object=RendTIC_4tem2017, level=0.95)

##############################################################################################################################