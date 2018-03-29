#' Busca todos os dados disponíveis de Data de Entrada e Data de Registro de 
#' imigrantes no Brasil e empilha duas tabelas para cada informação

require(dplyr)

# Monta URLs de requisição:
request_url <- c(
  paste0(
    rep(
      'http://obmigra.mte.gov.br/index.php/microdados/sincre?start=', 2
    ),
    seq(0, 10, 10)
  )
)

# Raspa os links das tabelas:
get_page_links <- function(urls) {
  xpath_a <- "//*[@id='k2Container']/div[2]/div/div[1]/div[4]/div/ul/li/a"
  data <- data.frame(stringsAsFactors = F)
    purrr::map_df(urls, function (x) {
      print(x)
      html <- httr::GET(x) %>%
        httr::content('text')
      url <- xml2::read_html(html) %>% 
        rvest::html_nodes(xpath = xpath_a) %>% 
        rvest::html_attr("href") %>% 
        paste0('http://obmigra.mte.gov.br', .)
      name <- xml2::read_html(html) %>% 
        rvest::html_nodes(xpath = xpath_a) %>% 
        rvest::html_text() %>% 
        stringr::str_trim()
      temp <- data.frame(
        name             = name, 
        url              = url,
        stringsAsFactors = F
      )
      data <- bind_rows(data, temp) %>% 
        mutate(clean_name = stringr::str_replace(name, "^SINCRE ", "") %>% 
                 stringr::str_replace("\\(.*", "") %>% 
                 stringr::str_replace("_ DATA DE |_DATA DE |- DATA DE ", "") %>% 
                 stringr::str_trim() %>% 
                 stringr::str_replace("\\s", "_")
      )
  })
}

page_links <- get_page_links(urls = request_url)

# Baixa os arquivos dos links raspados:
download <- function(data) {
  if (!dir.exists("data")) {
    dir.create("data")
  } else {
    cat('directory already exists\n')
  }
  for (i in 1:nrow(data)) {
    file_name <- data[i, 3]
    file_path <- paste0('data/', file_name, '.txt')
    print(paste0("downloading ", file_name, "..."))
    if (!file.exists(file_path)) {
      download.file(data[i, 2], file_path)
    } else {
      print('file has already been downloaded')
    }
  }
}

download(page_links)

files_list_entrada <- list.files('data/', 'ENTRADA.txt$')
files_list_registro <- list.files('data/', 'REGISTRO.txt$')

bind_tables <- function (files_list) {
  data <- data.frame(stringsAsFactors = F)
  purrr::map_df(seq_along(files_list), function (i) {
    file_path <- paste0('data/', files_list[i])
    temp <- readr::read_delim(file      = file_path,
                              delim     = ';', 
                              locale = readr::locale(encoding = 'ISO-8859-1')
                              ) %>% 
      mutate(ANO = files_list[i] %>% 
               stringr::str_extract('^\\d{4}') %>% 
               as.numeric())
    head(temp)
    data <- bind_rows(data, temp)
  })
}

entrada <- bind_tables(files_list = files_list_entrada)
readr::write_delim(x     = entrada, 
                   path  = 'binded_tables/entrada.txt', 
                   delim = ';', 
                   na    = '')

registro <- bind_tables(files_list = files_list_registro)
readr::write_delim(x     = registro, 
                   path  = 'binded_tables/registro.txt', 
                   delim = ';', 
                   na    = '')
