process_single_chunk <- function(chunk_in, alpha_words, bigrams_to_remove) {
  
  #cli_alert_info('\tCreating Bigram table')
  df_bigrams <- tibble(text = chunk_in) %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) 
  
  rm(chunk_in)
  
  #n_original <- nrow(df_bigrams)
  #cli_alert_success('\t\t-Got {n_original} rows of bigrams')
  
  #cli_alert_info('\tSeparating words')
  
  splitted_bigrams <- str_split(df_bigrams$bigram, ' ')
  
  df_bigrams <- df_bigrams %>%
    mutate(word1 = map_chr(splitted_bigrams, function(x) x[1]),
           word2 = map_chr(splitted_bigrams, function(x) x[2]))
  
  rm(splitted_bigrams)
  
  #cli_alert_info('\tCleaning bigrams')
  
  unique_stopwords <- unique(df_stop_words$word)
  
  # removing stop words
  idx1 <- is.na(fmatch(df_bigrams$word1, unique_stopwords))
  idx2 <- is.na(fmatch(df_bigrams$word2, unique_stopwords))
  idx3 <- idx1&idx2
  
  df_bigrams <- df_bigrams[idx3, ]
  # df_bigrams <- df_bigrams %>%
  #   filter(!word1 %in% unique_stopwords) %>%
  #   filter(!word2 %in% unique_stopwords)
  
  n_after_rm_sw <- nrow(df_bigrams)
  
  #cli_alert_success('\t\tRemoved stopwords -- {percent(n_after_rm_sw/n_original)} remain')
  
  if (use_word_filtering) {
    
    # only keep words in words-alpha
    idx1 <- !(is.na(fmatch(df_bigrams$word1, alpha_words)))
    idx2 <- !(is.na(fmatch(df_bigrams$word2, alpha_words)))
    idx3 <- idx1&idx2
    
    df_bigrams <- df_bigrams[idx3, ]
    
    # df_bigrams <- df_bigrams %>%
    #   filter(word1 %in% alpha_words) %>%
    #   filter(word2 %in% alpha_words)
    # 
    n_after_rm_alpha <- nrow(df_bigrams)
    #cli_alert_success('\t\tRemoved non alpha words-- {percent(n_after_rm_alpha/n_original)} remain')
    
  }

  # create counts
  df_count <- df_bigrams %>%
    mutate(bigram = str_c(word1, ' ', word2)) %>%
    filter(!bigram %in% bigrams_to_remove) %>%
    group_by(bigram, word1, word2) %>%
    summarise(freq = n())

  rm(df_bigrams)
  
  return(df_count)
}