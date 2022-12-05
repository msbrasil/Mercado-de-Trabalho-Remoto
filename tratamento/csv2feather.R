library("PNADcIBGE")
library(feather)
path_data <- '/home/matheus/Microdados PNAD/PNADC_2021_trimestre4.txt'
path_input <- '/home/matheus/Microdados PNAD/PNADC_2021_input_comp.txt'
path_dict <- '/home/matheus/Microdados PNAD/dicionario_PNADC_microdados_trimestre4_20220916.xlsx'

pnad <- read_pnadc(microdata = path_data, input_txt = path_input)
pnad <- pnadc_labeller(pnad, dictionary.file = path_dict)

head(pnad)

write_feather(pnad, '/home/matheus/Microdados PNAD/2021_4_tic.feather')
