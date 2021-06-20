get_stop_words <- function() {
  require(textreadr)
  
  # get stopwords from .docx
  dir_docx <- '00-input-files/02-stopwords/stopwords-from-docx/'
  f_stopwords <- dir_ls(dir_docx)
  
  cli_alert_info('Fetching stop words from .docx ({length(f_stopwords)} files)')
  sw_read_docx <- function(x) {
    tibble(country = basename(x),
           source = 'docx files',
           word = read_docx(x))
  }
  
  
  df_sw_from_docx <- bind_rows(
    map(f_stopwords, sw_read_docx)
  )
  cli_alert_success('\tGot {nrow(df_sw_from_docx)} stopwords')
  
  cli_alert_info('Fetching stop words from stopwords-iso (json file)')
  
  # get stopwords from json
  # source: https://github.com/stopwords-iso/stopwords-iso
  l_json <- jsonlite::fromJSON('00-input-files/02-stopwords/stopwords-iso.json')
  
  df_sw_from_json <- tibble()
  for (i_name in names(l_json)) {
    df_sw_from_json <- bind_rows(
      df_sw_from_json,
      tibble(country = i_name,
             word = l_json[[i_name]],
             source = 'https://github.com/stopwords-iso/stopwords-iso')
    )
  }
  
  cli_alert_success('\tGot {nrow(df_sw_from_json)} stopwords')
  
  
  df_sw <- bind_rows(
    df_sw_from_json,
    df_sw_from_docx
  )

  return(df_sw)
  
}