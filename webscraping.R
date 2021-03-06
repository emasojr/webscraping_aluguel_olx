# Limpando a memória
rm(list=ls())

# Nome da Cidade que Está pesquisando
cidade = 'São Paulo'

# Instalando bibliotecas necessárias
list.of.packages = c('dplyr', 'rvest','funModeling','lubridate','rstatix','PMCMR',
                      'mice','VIM','stringr','readr','magrittr')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Carregando bibliotecas
lapply(list.of.packages, library, character.only = TRUE)

# Salvando link para Scraping

# O primeiro passo é gerar um link de pesquisa de alugueis, por exemplo, o link na
# OLX de imóveis em SP, na Zona Oeste para os bairros: Butantã, Jaguaré e Pinheiros é:
# https://sp.olx.com.br/sao-paulo-e-regiao/zona-oeste/imoveis/aluguel?o=1&pe=4000&sd=2922&sd=2932&sd=2915
# O termo "?o=" é o divisor de página, então, para realizar o scraping, o url_base 
# fica com o link até o divisor, e complement o restante do link. Não se esqueça 
# de remover o número referente à pagina. Por exemplo:
url_base = 'https://sp.olx.com.br/sao-paulo-e-regiao/zona-oeste/imoveis/aluguel?o='
complement = '&pe=4000&sd=2922&sd=2932&sd=2915'
url_num = read_html(paste0(url_base,1,complement))

# Contando o número de páginas
num_pag = url_num %>%
  html_nodes('span') %>%
  html_text()
num_pag = num_pag[substr(num_pag,1,6)=='1 - 50']

num_pag = ifelse(substr(num_pag,12,12)=='.',as.numeric(paste0(substr(num_pag,11,11),substr(num_pag,13,15))),
                 as.numeric(substr(num_pag,11,13)))
np = num_pag/50
num_pag = ceiling(np)
rf = round((np - floor(np))*50-2+12, digits = 0)

# Scraping
result = data.frame()
for (j in 1:(num_pag)){
  print(paste("Página: ", j))
  
  olx = paste0(url_base, j, complement)
  olx = read_html(olx)
  
  divs = olx %>%
    html_nodes('a') %>% 
    html_attr("href")
  
  for (k in 12:ifelse(j<num_pag,61,rf)) {
    
    tryCatch({
      cas = divs[[k]]
      cas = read_html(cas) 
    }, error = function(e){})
    
    
    Infos = cas %>% html_nodes('dd') %>% html_text()
    if(length(Infos)<3){
      print('Sem informação suficiente no anúncio')
    }
    else{
      if(Infos[length(Infos)-2]==cidade | Infos[length(Infos)-1]==cidade | Infos[length(Infos)]==cidade){
        
        titulo = as.vector(cas %>% html_nodes('h1') %>% html_text())
        
        preco = cas %>% html_nodes('h2') %>% html_text()
        preco = preco[1]
        
        categoria = as.vector(cas %>% html_nodes('a') %>% html_text())
        m = ifelse(is.na(as.numeric(categoria[14])),
                   ifelse(is.na(as.numeric(categoria[13])),1,-1),0)
        
        quartos = categoria[14+m]
        
        if (quartos=='') {
          quartos = '1'
          categoria = 'Não identificado'
          condominio = 'R$ 0'
          IPTU = 'R$ 0'
          m2 = '0m²'
          banheiros = '1'
          vagas = '0'
          Infos = cas %>% html_nodes('dd') %>% html_text()
          bairro = ifelse(Infos[length(Infos)-1]==cidade,Infos[length(Infos)],Infos[length(Infos)-1])
          datap = ifelse(as.vector(cas %>% html_nodes('span') %>% html_text())[11]=='Localização',
                         as.vector(cas %>% html_nodes('span') %>% html_text())[12], 
                         ifelse(as.vector(cas %>% html_nodes('span') %>% html_text())[11]=='-',
                                as.vector(cas %>% html_nodes('span') %>% html_text())[10],
                                as.vector(cas %>% html_nodes('span') %>% html_text())[11]))
        } 
        else {
          if (is.na(as.numeric(substr(quartos,1,1)))) {
            quartos = categoria[16]
            categoria = categoria[14]
          } 
          else{
            categoria = categoria[12+m]
          }
          
          Infos = cas %>% html_nodes('dd') %>% html_text()
          
          ref1 = substr(Infos[1], nchar(Infos[1])-2+1, nchar(Infos[1]))
          ref2 = substr(Infos[2], nchar(Infos[2])-2+1, nchar(Infos[2]))
          ref3 = substr(Infos[1], 1,1)
          ref4 = substr(Infos[2], 1,1)
          
          if (ref1=='m²') {
            condominio = 'R$ 0'
            IPTU = 'R$ 0'
            l = 2
            r = 0
          } 
          else if (ref2=='m²'){
            condominio = Infos[1]
            IPTU = 'R$ 0'
            l = 1
            r = 0
          } 
          else if (ref3!='R'){
            condominio = 'R$ 0'
            IPTU = 'R$ 0'
            r = 1
            l = 2
          }
          else if (ref3=='R' & ref4!='R'){
            condominio = Infos[1]
            IPTU = 'R$ 0'
            r = 1
            l = 1
          }
          else {
            condominio = Infos[1]
            IPTU = Infos[2]
            l = 0
            r = 0
          }
          
          m2 = ifelse(r==1,NA,Infos[3-l-r])
          banheiros = ifelse(is.na(as.numeric(substr(Infos[4-l-r],1,1))),NA,
                             ifelse(nchar(Infos[4-l-r])==8,NA,Infos[4-l-r]))
          vagas = ifelse(nchar(Infos[5-l-r])==8,NA,Infos[5-l-r])
          vagas = ifelse(is.na(as.numeric(vagas)),NA,vagas)
          bairro = ifelse(Infos[length(Infos)-1]==cidade,Infos[length(Infos)],Infos[length(Infos)-1])
          link = divs[[k]]
          datap = ifelse(as.vector(cas %>% html_nodes('span') %>% html_text())[11]=='Localização',
                         as.vector(cas %>% html_nodes('span') %>% html_text())[12], 
                         ifelse(as.vector(cas %>% html_nodes('span') %>% html_text())[11]=='-',
                                as.vector(cas %>% html_nodes('span') %>% html_text())[10],
                                as.vector(cas %>% html_nodes('span') %>% html_text())[11]))
        }
        
        result = rbind(result, data.frame(titulo,datap,categoria,bairro,preco,condominio,IPTU,m2,quartos,banheiros,vagas,link))
        print(paste('Anúncio',(k-11)))
      }
      else{
        print('Não corresponde à mesma cidade')
      }
    }
  }
}
