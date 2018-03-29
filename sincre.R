#' Busca todos os dados disponíveis de Data de Entrada e Data de Registro de 
#' imigrantes no Brasil e empilha duas tabelas para cada informação

require(dplyr)

# monta final das URLs ---------------------------------------------------------
url_end <- c(paste(
  seq(52, 66, 1), 
  seq(2000, 2014, 1),
  sep = "-"
  )
)

request_url <- c(
  paste0(
    rep(
      'http://obmigra.mte.gov.br/index.php/microdados/sincre/itemlist/category/',
      length(url_end)
    ),
    url_end
  )
)

get_page_links <- function(urls) {
  xpath_a <- "//*[@id='k2Container']/div[1]/div/div[1]/div[4]/div/ul/li/a"
  xpath_last_update <- "//*[@id='k2Container']/div[1]/div/div[2]/ul/li[1]/text()"
  perm_df <- data.frame(stringsAsFactors = F)
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
      last_update <- xml2::read_html(html) %>% 
        rvest::html_nodes(xpath = xpath_last_update) %>% 
        rvest::html_text() %>% 
        strptime(format = "%d/%m/%y") %>% 
        as.POSIXct()
      temp <- data.frame(
        name             = name, 
        url              = url,
        last_update      = last_update,
        stringsAsFactors = F
      )
      data <- bind_rows(data, temp) %>% 
        mutate(clean_name = stringr::str_replace(name, "^SINCRE ", "") %>% 
                 stringr::str_replace("\\(.*", "") %>% 
                 stringr::str_replace("_ DATA DE |_DATA DE ", "") %>% 
                 stringr::str_trim() %>% 
                 stringr::str_replace("\\s", "_")
      )
  })
}

page_links <- get_page_links(urls = request_url)

download <- function(data) {
  if (!dir.exists("data")) {
    dir.create("data")
  } else {
    cat('directory already exists\n')
  }
  for (i in 1:nrow(data)) {
    print(paste0("downloading ", file_name, "..."))
    file_name <- data[i, 4]
    file_path <- paste0('data/', file_name, '.txt')
    if (!file.exists(file_name)) {
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
                   path  = 'entrada.txt', 
                   delim = ';', 
                   na    = '')

registro <- bind_tables(files_list = files_list_registro)
readr::write_delim(x     = registro, 
                   path  = 'registro.txt', 
                   delim = ';', 
                   na    = '')
