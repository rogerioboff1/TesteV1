
#' @title Teste V1
#'
#' @description Descricao do teste V1
#'
#' @param
#'
#' @return
#'
#' @examples
#'
#' @export
#'


# https://www.analyticsvidhya.com/blog/2017/03/create-packages-r-cran-github/

#------------------------------------.
#      Carregamento de pacotes   ----
#------------------------------------.

usePackage <- function(Packages, print.check = T) {
  #  Funcao para carregar pacotes, caso o pacote nao esteja instalado,
  # ele automaticamente baixa e carrega

  # Packages: 'nome do pacote' (aceita um vetor de nomes tambem)

  if(class(Packages) != 'character') stop('\nPackage must be a character!')

  for(ipack in seq_along(Packages)){
    Package <- Packages[ipack]
    if(print.check) cat('\n- Checking package ', Package, '...\n')
    if (!is.element(Package, installed.packages()[,1])){
      message('\n\n Instalando pacote: ', Package, '\n\n')
      install.packages(Package, dep = TRUE)
    }
    require(Package, character.only = TRUE)
  }
}

usePackage(c('dplyr', 'tidyverse', 'magrittr'))
filter <- dplyr::filter
select <- dplyr::select
arrange <- dplyr::arrange




# usePackage('sdasd')
# usePackage('ggplot2')



#----------------------------------.
#      Tabela de frequencia    ----
#----------------------------------.

freQ <- function(var_x, order = 'freq', ...){
  # Funcao para fazer FREQUENCIES
  # Se passar um data.frame ou tible... ele fara a freq para cada variavel

  usePackage('summarytools', print.check = F)
  usePackage('dplyr', print.check =F)
  usePackage('tibble', print.check =F)


  var_x <- if(is.null(dim(var_x))) tibble::enframe(var_x, name = NULL) else as_tibble(var_x) # transforma em um tibble
  resultsFreq <- vector("list", ncol(var_x)) # pra salvar os restultados
  names(resultsFreq) <- names(var_x)

  for(ivar_x in seq_along(var_x))  resultsFreq[[ivar_x]] <- freq(var_x[[ivar_x]], order = order,...)
  return(resultsFreq)

}


# a <- data.frame(a = 1:10, b = c(2,6,4,4,4,6,1,9,9,2)) %>% as_tibble()
# dim(a$a)
# is.null(dim(a$a))
# (ra <- freQ(a))
# (ra <- freQ(a$a))
# str(ra)
# ra[[1]]
# ra$a
# ra[[2]]
# freQ(1:5)


#-----------------------------------.
# Coluna com maior/ menor valor ----
#-----------------------------------.

varBiggestValue <- function(df_x){
  # Retorna o coluna com o maior valor da linha do df_x
  # https://stackoverflow.com/questions/17735859/for-each-row-return-the-column-name-of-the-largest-value

  usePackage('data.table', print.check = F)
  df_x <- data.frame(df_x)
  DT <- data.table(value = unlist(df_x, use.names = FALSE),
                   colid = 1:nrow(df_x), rowid = rep(names(df_x), each = nrow(df_x)))
  setkey(DT, colid, value)

  return(DT[J(unique(colid), DT[J(unique(colid)), value, mult='last']), rowid, mult='first'])
}


varSmallestValue <- function(df_x){
  # Retorna o coluna com o maior valor da linha do df_x
  # Que nem o de cima, mas retorna o menor valor

  usePackage('data.table', print.check = F)
  df_x <- data.frame(df_x)
  DT <- data.table(value = unlist(df_x, use.names = FALSE),
                   colid = 1:nrow(df_x), rowid = rep(names(df_x), each = nrow(df_x)))
  setkey(DT, colid, value)

  return(DT[J(unique(colid), DT[J(unique(colid)), value, mult='first']), rowid, mult='first'])
}


# (a <- data.frame(a = 1:10, b = -4:5, c = rnorm(10, 0, 50)))
# varBiggestValue(a)
# varSmallestValue(a)






#-------------------------------------------.
# Converte string/ factor para numeric  ----
#-------------------------------------------.

to.numeric <- function(stringNum){
  if(is.null(stringNum)) stop('\nstringNum nao especificado')
  stringNum <- factor(stringNum)
  return(as.numeric(levels(stringNum))[stringNum])
}






#--------------------------------------------------------------------------------------------------------------------------.
# Padronizacoes do texto
clean.string <- function(Xstring, utf8codif = T, removeStopWords = T){
  #  Quando o vetor vem de algum arquivo externos, tipo excel, ha problemas de codificacao
  # Quando o objeto foi criado no R, usa utf8codif = F

  if(is_tibble(Xstring)) Xstring <- c(Xstring)[[1]]

  ##Remove Acentos
  if(utf8codif == T){
    Xstring <- iconv(Xstring, from='UTF-8', to='ASCII//TRANSLIT')
  } else{
    Xstring <- iconv(Xstring, to='ASCII//TRANSLIT')
  }


  ##Remove stop words cl?ssicas
  if(removeStopWords == T)  Xstring <- tm::removeWords(Xstring, tm::stopwords('pt'))

  ##Demais limpeza
  Xstring <- Xstring %>%
    tm::removeNumbers() %>%                         # Remove os numeros
    stringr::str_to_lower() %>%                     # Deixa minusculo
    stringr::str_replace_all('[[:punct:]]', '') %>% # Remove Caracteres especiais
    tm::removePunctuation() %>%
    stringr::str_squish() %>%                       # Espacamento Simples
    stringr::str_trim()                             # Remove o spaco do inicio ou do final

  return(Xstring)
}


