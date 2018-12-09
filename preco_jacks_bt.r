##### Configuracoes Iniciais #####
#setwd("/home/juuhclemente/Documents/R_bt")

#user_renviron <- path.expand(file.path("~", ".Renviron"))
#file.edit(user_renviron)
#Sys.setenv(R_TELEGRAM_BOT_PatolinaBot="senha")

### Pacotes ###
library(telegram) #conex?o com o telegram
library(telegram.bot) #conex?o com o telegram
library(tidyverse) #Facilita a manipula??o de tabelas
library(rvest) # Parsing of HTML/XML files
#library(stringr)# String manipulation
#library(rebus) # Verbose regular expressions
library(lubridate) # Eases DateTime manipulation
#library(readr)
#library(xts)

df_precos <- readRDS("df_precos.rds")
lista_itens <- c("JackDan", "JackHon")

##### Configuracoes Iniciais Bot #####

updater <- Updater(token = bot_token("PatolinaBot"))
bot <- TGBot$new(token = bot_token("PatolinaBot"))
dispatcher <- updater$dispatcher

#bot$getMe()
#msgs <- bot$getUpdates()
#id_bot <- msgs$message$chat$id[1]

bot$set_default_chat_id(647697486) #id_bot

##### Funções validadas iniciais telegram.bot #####

lista_funcoes <- function(bot, update){
  ls_funcoes <- paste(sprintf("Ola %s!", update$message$from$first_name),  
                      "Patolina eh uma beberrona, e ta procurando ofertas de bebidas pra sustentar o vicio! kkk.", 
                      "Para interagir com Patolina, mande os comandos: ",
                      "/oi - p/ receber uma foto dessa princesa e/ou do seu irmaozin",
                      "/preco_hj - p/ ver o melhor preco dos produtos hj",
                      "/preco_mes - p/ ver o melhor preco dos ultimos 30 dias",
                      "/preco_all - p/ ver o melhor preco desde o inicio das consultas",
                      "/lista_item - p/ ver os itens possiveis de pesquisar. Pode colocar o nome do item como argumento na pesquisa.", 
                      sep = "\n")
  bot$sendMessage(chat_id = update$message$chat_id, 
                  text = ls_funcoes)
  
  updater$stop_polling()
}

msg_handler <- MessageHandler(lista_funcoes, MessageFilters$text)
dispatcher$add_handler(msg_handler)

foto_bbs <- function(bot, update){
  
  photos <- list.files("./imgs")
  photo_id <- floor(runif(1, 1, length(photos) + 1))
  
  photo_path <- paste0("./imgs/", photos[photo_id])
  
  bot$sendPhoto(chat_id = update$message$chat_id,
                photo = photo_path,
                caption = sprintf("Oi, %s!", update$message$from$first_name))
  updater$stop_polling()
}

oi_handler <- CommandHandler('oi', foto_bbs)
dispatcher$add_handler(oi_handler)

min_vlr <- function(periodo, bot, update, args){
  
  min_vlr <- df_precos %>%
  {if(periodo == "hj") filter(., substr(hora, 1, 10) == Sys.Date()) 
    else if(periodo == "mes") filter(., hora >= Sys.Date()-30) 
    else filter(., hora == hora)} %>%
    {if (args %in% lista_itens) filter(., item == args) else filter(., item == item)} %>%
    group_by(item) %>%
    slice(which.min(valor))
  
  txt_periodo <- ifelse(periodo == "hj", "hoje", ifelse(periodo == "mes", "nos ultimos 30 dias", ""))
  
  min_vlr_tx <- paste0("Menor valor encontrado por produto ", txt_periodo, ":")
  
  apply(min_vlr, 1, function(x){
    min_vlr_tx <<- paste0(min_vlr_tx, "\n", x$item, ": ", x$valor, " Loja: ", x$empresa, ". \n")
  })
  
  #chat_id <- if(is.na(update), NULL, update$message$chat_id)
    
  if(is.na(args)) bot$sendMessage(chat_id = update$message$chat_id, text = min_vlr_tx)
  else if(args %in% lista_itens) bot$sendMessage(chat_id = update$message$chat_id, text = min_vlr_tx)
  else bot$sendMessage(chat_id = update$message$chat_id, text = "Este item nao faz parte da lista")
  #  return(min_vlr_tx)
}

preco_hj <- function(bot, update, args){
  min_vlr_tx <- min_vlr("hj", bot = bot, update = update, args = args) 
  updater$stop_polling()
}

preco_hj(bot, updater, args = NA)

preco_hj_handler <- CommandHandler('preco_hj', preco_hj, pass_args = TRUE)
dispatcher$add_handler(preco_hj_handler)

preco_mes <- function(bot, update, args){
  min_vlr_tx <- min_vlr("mes", bot = bot, update = update, args = args)  
  updater$stop_polling()
}

preco_mes_handler <- CommandHandler('preco_mes', preco_mes, pass_args = TRUE)
dispatcher$add_handler(preco_mes_handler)

preco_all <- function(bot, update, args){
  min_vlr_tx <- min_vlr("all", bot = bot, update = update, args = args) 
  updater$stop_polling()
}

preco_all_handler <- CommandHandler('preco_all', preco_all, pass_args = TRUE)
dispatcher$add_handler(preco_all_handler)

itens_consulta <- function(bot, update){
  bot$sendMessage(chat_id = update$message$chat_id,
                  text = paste("Os itens disponiveis para consulta sao: ", paste(lista_itens, collapse = ', ')))
  
  updater$stop_polling()
}

itens_handler <- CommandHandler('lista_item', itens_consulta)
dispatcher$add_handler(itens_handler)

unknown <- function(bot, update){
  bot$sendMessage(chat_id = update$message$chat_id,
                  text = "Este nao eh um comando valido.")
  
  updater$stop_polling()
}

unknown_handler <- MessageHandler(unknown, MessageFilters$command)
dispatcher$add_handler(unknown_handler)

##### Funções TO-DO/DOING telegram.bot #####

# sudoUser <- function(message){
#   message$from_user == 647697486
# }
# 
# # Example of a 'kill' command
# kill <- function(bot, update){
#   bot$sendMessage(chat_id = update$message$chat_id,
#                   text = "Ateh a proxima!")
#   # Clean 'kill' update
#   bot$getUpdates(offset = update$update_id + 1)
#   # Stop the updater polling
#   updater$stop_polling()
# }

##### Bota telegram pra rodar #####
#updater$start_polling()
#msgs <- bot$getUpdates(limit = 1)

##### Lista URL links #####
urls <- list("SuperAdega"= c("JackDan"= 'https://www.superadega.com.br/whisky-jack-daniels-1l/p',
                             "JackHon"= 'https://www.superadega.com.br/whiskey-jack-daniel-honey-1l/p'),
             "BullBev"= c("JackDan"= 'http://bullbev.com.br/whiskey-jack-daniels-1000ml/761',
                          "JackHon"= 'http://bullbev.com.br/whiskey-jack-daniels-honey-1000ml/24205'),
             "Imigrantes" = c("JackDan"= 'https://www.imigrantesbebidas.com.br/whiskey-jack-daniels-1-l',
                              "JackHon"= 'https://www.imigrantesbebidas.com.br/jack-daniel-s-honey-1-l'),
             "CasaBebida" = c("JackDan"= 'https://www.casadabebida.com.br/whisky/whisky-jack-daniels/',
                              "JackHon"= 'https://www.casadabebida.com.br/whisky/whisky-jack-daniels-honey/'))

pega_valor_bebida <- function(x, lista_itens){
    print(x)
    preco <- vector()
    
    for(item in lista_itens){
      
      le_url <- read_html(urls[[x]][item])
      
      if(x == "SuperAdega")
        valor <- le_url %>%
          html_nodes('.skuBestPrice') 
      else if(x == "BullBev")
        valor <- le_url %>%
          html_nodes('.preco_produto') %>%
          html_nodes('.preco') 
      else if(x == "Imigrantes"){
        valor <- le_url %>%
          html_nodes('.unit-price') 
      }
      else if(x == "CasaBebida"){
        valor <- le_url %>%
          html_nodes('.selling-info') %>%
          html_nodes('.price')
        
      }
      preco[item] <- valor %>%
        html_text() %>%
        parse_number()/100
      
    }  
#    print(paste0("Preco JackDan:", preco[1], " Preco JackHon:", preco[2]))
    
    return(preco)
}

pega_menor_vlr <- function(item, l_precos, hr_consulta){
  preco_item <- lapply(l_precos, function(x){
    x[item]
  })
  
  df_tmp <- data_frame(hora = hr_consulta, empresa = names(urls), valor = as.numeric(preco_item), 
                       item = item, data = ymd(substr(hr_consulta, 1, 10)))
  
  if(!exists("df_precos")){
    df_precos <- df_tmp
  } else{
    df_precos <<- rbind(df_precos, df_tmp)  
  }
  
  # melhor_preco <- df_precos %>%
  #   mutate(valor = as.numeric(valor)) %>%
  #   filter(hora == hr_consulta) %>%
  #   group_by(item) %>%
  #   slice(which.min(valor))
  # 
  # bot$sendMessage(paste0("Melhor preco de", item, "encontrado:",  melhor_preco[1, "valor"], "loja:", melhor_preco[1, "empresa"]))
}

update_precos <- function(urls, lista_itens){
  
  hr_consulta <<- lubridate::now()
  
  df_precos <- readRDS("df_precos.rds")
  
  precos <- lapply(names(urls), pega_valor_bebida, lista_itens = lista_itens)
  
  for(item in lista_itens){
    pega_menor_vlr(item, precos, hr_consulta)
  }  
  
  saveRDS(df_precos, "df_precos.rds")
}

bot_vlr_bebida <- function(lista_itens, tempo_consulta){
  offset <- 0
  avisa_consulta <- 1
  while(TRUE){

    if(hour(hr_consulta) >= 8 & hour(hr_consulta) <= 9 & avisa_consulta == 1){
      bot$sendMessage("Ju, qual o melhor preco hj?")
      avisa_consulta <- 0
    }
    
    diff_tempo <- as.numeric(difftime(Sys.time(), hr_consulta, units = "secs"))
    
    if(diff_tempo >= tempo_consulta){ #realiza a consulta a cada "tempo_consulta" segundos
      try(update_precos(urls, lista_itens))
      bot$sendMessage(paste("Hora consulta:", hr_consulta))
      avisa_consulta <- 1
      #Sys.sleep(freq)
    }
    
    getUpd <- tryCatch(bot$getUpdates(offset = offset + 1, limit = 1), error = NULL)
    
    if(length(getUpd) > 0) {
      try({updater$start_polling(verbose = T)
            offset <- getUpd$update_id})
    }
    
  }
}

##### Bota pra rodarrr #####
#hr_consulta <- lubridate::now()
#hr_consulta <- ymd_hms("2018-09-11 08:03:02")
bot_vlr_bebida(lista_itens, tempo_consulta = 7200)

#up <- bot$getUpdates(offset = offset + 1, limit = 1)

##### Testes aleat #####
# df_precos <- df_precos %>%
#   mutate(valor = as.numeric(valor),
#          hora = as.POSIXct(hora)) %>%
#   mutate(data = ymd(substr(hora, 1, 10)))

# xts_precos <- df_precos %>%
#   xts(df_precos[, -1], order.by = df_precos$hora)



