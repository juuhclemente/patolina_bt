list_pkgs <- c("quantmod", "rvest", "tidyverse", "PerformanceAnalytics")

for(pkg in list_pkgs){
#  print(pkg)
  if(!(pkg %in% rownames(installed.packages()))) install.packages(pkg)
  suppressWarnings(library(pkg, character.only = TRUE))
}

key_av <- Sys.getenv("QUANTMOD_KEY")
setDefaults(getSymbols.av, api.key= key_av)

env_fred <- new.env() 
nomes <- c("brl_usd", "ethereum_usd", "litcoin_usd", "bra_preco", "bra_desempr", "btc_usd") 
simbolos <- c("DEXBZUS", "CBETHUSD", "CBLTCUSD", "BRACPIALLMINMEI", "LRUNTTTTBRM156S", "CBBTCUSD") 
lista_nomes <- list(nomes, simbolos)  

for(i in 1: length(nomes)){ 
  
  print(lista_nomes[[1]][i])
  
  assign(lista_nomes[[1]][i],                                             
         getSymbols(lista_nomes[[2]][i], src = "FRED", auto.assign = F),           
         envir = env_fred) 
  }

env_av <- new.env()
getSymbols("MSFT", src = "av", auto.assign = T)


chartSeries(env_fred$ethereum_usd, TA='addBBands();addVo();addMACD()',subset='2018') 
chartSeries(env_fred$btc_usd, TA='addBBands();addVo();addMACD()',subset='2018')   
chartSeries(env_fred$litcoin_usd, TA='addBBands();addVo();addMACD()',subset='2018')

print("oi")
