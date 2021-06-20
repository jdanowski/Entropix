get_bigrams_to_remove <- function() {
  require(readr)
  require(stringr)
  
  my_bigrams <- read_lines('00-input-files/02-stopwords/bigrams-to-remove.txt')
  
  my_bigrams <- str_to_lower(my_bigrams)
  
  return(my_bigrams)
  
}