calculate_bigram_entropy <- function(f_in, df_stop_words,
                                     show_plots = TRUE,
                                     use_word_filtering,
                                     n_pieces,
                                     use_bigrams_to_remove) {

  require(readr)
  require(stringr)
  require(fastmatch)
  require(dplyr)
  require(tictoc)
  require(cli)
  require(scales)
  require(tidytext)
  require(tidyr)
  require(ggplot2)
  require(writexl)
  require(cowplot)
  require(progress)
  
  # set time
  tic()
  
  options(dplyr.summarise.inform = FALSE)
  
  # loging details (not used)
  #my_username <- Sys.getenv('USER')
  #my_time <- format(Sys.time(), '%Y%d%m_%H%M%S')
  
  basename_f_in <- basename(f_in)
  basename_f_noext <- tools::file_path_sans_ext(basename_f_in)
  
  # set up output files
  f_out_1 <- path('01-ouput-files/filtered-by-entropy-threshold/',
                  str_glue('BIGRAMS-filtered-by-entropy-threshold--{basename_f_noext}.csv'))
  
  f_out_2 <- path('01-ouput-files/filtered-by-entropy-crossover/',
                  str_glue('BIGRAMS-filtered-by-entropy-crossover--{basename_f_noext}.csv'))
  
  f_out_3 <- path('01-ouput-files/entropy-calculation/',
                  str_glue('BIGRAMS-entropy-calculation--{basename_f_noext}.xlsx'))
  
  f_out_4 <- path('01-ouput-files/log-files/',
                  str_glue('BIGRAMS-logfile--{basename_f_noext}.xlsx'))
  
  cli_alert_info('Reading and Processing {f_in}')
  
  # read txt file ----
  txt <- read_lines(f_in, progress = FALSE, skip = 1)#[1:250000]
  
  cli_alert_success('\tGot {length(txt)} lines ')
  
  if (length(txt) == 0) {
    cli_alert_danger('\tGot zero lines!!! Exiting')
    return(invisible(FALSE))
  }
  
  # clean up text
  cli_alert_info('\tCleaning raw text')
  
  txt <- str_to_lower(txt)
  cli_alert_success('\t\t-Converted to str_lower')

  # replace non-alphanumeric
  txt <- str_replace_all(txt, "[^[:alnum:]]", ' ')
  cli_alert_success('\t\t-Replaced all non-alphanumeric characters by " "')
  
  # removing numbers
  txt <- str_replace_all(txt, "[0-9]", ' ')
  cli_alert_success('\t\t-Removed all numbers')
  
  # remove double whitespace
  txt <- str_squish(txt)
  cli_alert_success('\t\t-Removed double whitespaces')
  
  if (length(txt) == 0) {
    cli_alert_danger('\tGot zero lines!!! Exiting')
    return(invisible(FALSE))
  }
  
  cut_index <- cut(seq(1, length(txt)), breaks = n_pieces )
  l_split <- split(txt, cut_index )
  
  rm(txt); gc()
  
  pb <- progress_bar$new(
    format = "\t\tProcessing [:bar] :percent eta: :eta",
    total = n_pieces, clear = FALSE, width= 60)
  
  cli_alert_info('\tProcessing single chunks of text')

  alpha_words <- unique(
    read_lines('00-input-files/01-words-alpha/words_alpha.txt')
  )
  
  if (use_bigrams_to_remove) {
    my_bigrams_to_remove <- get_bigrams_to_remove()
  } else {
    my_bigrams_to_remove <- ''
  }
  
  l_processed <- list()
  for (i_chunk in seq(1, length(l_split))) {
    pb$tick()
    #cli_alert_info('\tProcessing chunk {i_chunk}/{n_pieces}')
    
    l_processed[[i_chunk]] <- process_single_chunk(l_split[[i_chunk]], 
                                                   alpha_words,
                                                   bigrams_to_remove = my_bigrams_to_remove)
    
    l_split[[i_chunk]] <- NA
    gc()
  }
  
  rm(l_split); gc()
  
  cli_alert_info('\tComputing entropy')
  
  if (nrow(bind_rows(l_processed)) < 5) {
    cli_alert_danger('\tNot enough bigrams!!! Exiting')
    return(invisible(FALSE))
  }

  df_count <- bind_rows(l_processed)  %>%
    group_by(bigram, word1, word2) %>%
    summarise(freq = sum(freq)) %>%
    arrange(desc(freq)) %>%
    na.omit() %>%
    ungroup() 
  
  if (!any(df_count$freq > threshold_freq_xlsx)) {
    cli_alert_danger('\tNot enough bigrams frequency!!! Exiting')
    return(invisible(FALSE))
  }
  
  
  df_count <-   df_count %>%
    filter(freq >= threshold_freq_xlsx) %>% # filter for freq > than threshold
    # calculate entropy here ----
    mutate(p = freq/sum(freq), # For each bigram, compute the probability by dividing its frequency by the total bigram frequency.
           entropy = p*(log2(1/p)), # Compute the entropy for each bigram:  p(log2(1/p).
           cum_entropy = cumsum(entropy), # For each bigram, compute the cumulative entropy by summing down the list to that point.
           r_number = 1:nrow(.), # row number (number of bigrams, used next)
           max_cum_entropy = log2(r_number), # Compute the maximum cumulative entropy by log2N, where N is the number of bigrams in the cumulative list to that point.
           entropy_diff = max_cum_entropy - cum_entropy # Subtract the cumulative entropy from the maximum cumulative entropy as the entropy difference.
    ) 
  
  rm(l_processed); gc()
  
  cli_alert_info('\tFinding entropy difference threshold')
  
  # find point of change of sign for entropy diff 
  idx_candidate_threshold <- first(which(diff(df_count$entropy_diff) < 0))
  
  # If the threshold occurs amidst a sequence of bigrams with the same count, 
  # backup up to the next highest frequency for the cut point.
  
  candidate_freq <- df_count$freq[idx_candidate_threshold]
  
  idx_threshold <- last(which(df_count$freq > candidate_freq))
  entropy_diff_threshold <- df_count$entropy_diff[idx_threshold]
  freq_at_threshold <- df_count$freq[idx_threshold]  
  
  df_thresh <- tibble(x_change = idx_threshold, 
                      y_change = entropy_diff_threshold,
                      freq = df_count$freq[idx_threshold],
                      label = 'Optimal Entropy') 
  
  cli_alert_success('\t\tThreshold at observation {idx_threshold}, value = {format(entropy_diff_threshold)}, at freq = {freq_at_threshold}')
  
  cli_alert_info('\tFinding entropy crossover')
  
  # Subtract the entropy difference threshold from the cumulative entropy.
  df_count <- df_count %>%
    mutate(optimal_entropy = entropy_diff_threshold,
           diff_from_threshold = cum_entropy - optimal_entropy)
  
  # Find the point at which the sign changes from minus to plus, called the entropy crossover. 
  # If the crossover occurs amidst a sequence of bigrams with the same count, backup 
  # up to the next highest frequency for the cut point.
  idx_candidate_crossover <- first(which(diff(sign(df_count$diff_from_threshold))!=0)) + 1
  candidate_freq <- df_count$freq[idx_candidate_crossover]
  
  idx_crossover <- last(which(df_count$freq > candidate_freq))
  
  df_cross <- tibble(x_change = idx_crossover, 
                     y_change = df_count$diff_from_threshold[idx_crossover],
                     entropy_diff = df_count$entropy_diff[idx_crossover],
                     freq = df_count$freq[idx_crossover]) 
  
  df_count <- df_count %>%
    mutate(entropy_cross_over_point = diff_from_threshold[idx_crossover])
  
  cli_alert_success('\t\tentropy crossover at observation {idx_crossover}')
  
  # plot entropy difference

  # Show on the same graph the cumulative entropy, maximum entropy,
  # and the entropy difference, marking two points, the maximum entropy 
  # difference point, and the crossover point.
  
  df_temp <- df_count %>%
    select(r_number, cum_entropy, max_cum_entropy, entropy_diff) %>%
    pivot_longer(cols = names(.)[2:4]) %>%
    mutate(name = recode_factor(name, 
                                'cum_entropy' = 'Cumulative Entropy',
                                'max_cum_entropy' = 'Max. Cumulative Entropy',
                                'entropy_diff' = 'Entropy Difference'))
  
  p <- ggplot(df_temp, aes(x = r_number, y = value, color = name)) + 
    geom_line(size = 0.75) +  
    geom_point(data = df_thresh, aes(x = x_change, y = y_change), 
               color = 'blue', size = 4) + 
    geom_point(data = df_cross , aes(x = x_change, y = entropy_diff), 
               color = 'red', size = 4) + 
    labs(title = str_glue('Entropies for Bigrams -- {basename_f_in}'),
         subtitle = str_glue('Optimal Entropy (blue) at x={idx_threshold}, freq = {df_thresh$freq}',
                             '\n',
                             'Entropy Crossover Point (red) at x={idx_crossover}, freq = {df_cross$freq}'),
         x = 'Row Number',
         y = 'Entropies',
         color = 'Type of Entropy') + 
    theme_light() 
  
  # p1 <- ggplot(df_count, aes(x = r_number, 
  #                           y = entropy_diff)) + 
  #   geom_line(size = 0.75) + 
  #   geom_point(data = df_thresh, aes(x = x_change, y = y_change), 
  #              color = 'blue', size = 4) + 
  #   labs(title = str_glue('Entropy Difference for Bigrams -- {basename_f_in}'),
  #        subtitle = str_glue('Threshold solution at x={idx_threshold}, freq = {df_thresh$freq}'),
  #        x = 'Row Number',
  #        y = 'Entropy Difference') + 
  #   theme_light()
  # 
  # p2 <- ggplot(df_count, 
  #              aes(x = r_number,
  #                  y = diff_from_threshold)) + 
  #   geom_line(size = 0.75) + 
  #   geom_point(data = df_cross , aes(x = x_change, y = y_change), 
  #              color = 'blue', size = 4) + 
  #   labs(title = str_glue('Entropy Crossover for Bigrams -- {basename_f_in}'),
  #        subtitle = str_glue('Threshold solution at x={idx_crossover}, freq = {df_cross$freq}'),
  #        x = 'Row Number',
  #        y = 'Crossover Difference') + 
  #   theme_light()
  # 
  # p <- plot_grid(p1, p2, nrow = 2, labels = 'AUTO')
  
  if (show_plots) {
    x11(); print(p) 
  }
  
  f_fig <- path('01-ouput-files/figures/',
                str_glue('Bigrams-Entropy-Crossover--{basename_f_noext}.png') )
  ggsave(f_fig, plot = p)
  
  
  cli_alert_info('\tBuilding output files')
  
  # build first output (filtered table -- threshold)
  df_count_filtered_threshold <- df_count %>%
    filter(r_number <= idx_threshold) %>%
    select(word1, word2, freq)
  
  write_csv(x = df_count_filtered_threshold,
            file = f_out_1)
  
  cli_alert_success('\t\tFile {f_out_1} created')
  
  # build second output (filtered table -- crossover)
  df_count_filtered_crossover <- df_count %>%
    filter(r_number <= idx_crossover) %>%
    select(word1, word2, freq)
  
  write_csv(x = df_count_filtered_crossover,
            file = f_out_2)
  
  cli_alert_success('\t\tFile {f_out_2} created')
  
  
  # build second output (table with entropy calculations)
  write_xlsx(x = df_count %>%
               filter(freq >= threshold_freq_xlsx), 
             path = f_out_3)
  
  cli_alert_success('\t\tFile {f_out_3} created')
  
  # Add an additional output log file: 
  # Total number of bigrams, sum of frequencies, optimal entropy row number, 
  # the entropy crossover row number, and the difference in the row numbers.
  df_log <- tibble(filename = basename_f_in,
                   `execution_datetime` = Sys.time(),
                   `Total Number of bigrams` = nrow(df_count),
                   `Sum of Frequencies` = sum(df_count$freq),
                   `Optimal entropy row number` = idx_threshold,
                   `Entropy crossover row number` = idx_crossover,
                   `Difference row number` = abs(idx_threshold - idx_crossover),
                   `exact_optimal_row` = idx_candidate_threshold,
                   `adjusted_optimal_row` = idx_threshold,
                   `exact_adjusted_difference` = idx_candidate_threshold - idx_threshold,
                   `n_rows_same_freq_as_exact` = sum(df_count$freq == df_count$freq[idx_candidate_threshold]),
                   `freq_at_exact_optimal_row` =  df_count$freq[idx_candidate_threshold],
                   # new cols
                   `Percent of Total Bigrams at Adjusted Optimal Row` = `Optimal entropy row number`/`Total Number of bigrams`,
                   `Percent of Total Bigrams at Adjusted Crossover row` = `Entropy crossover row number`/`Total Number of bigrams`,
                   `Percent Difference of Total Bigrams_ Adjusted Crossover - Optimal` = `Percent of Total Bigrams at Adjusted Crossover row` -`Percent of Total Bigrams at Adjusted Optimal Row`,
                   `Base_Percent_of_Total_Optimal` = (`Percent of Total Bigrams at Adjusted Optimal Row` + 
                                                        `Percent of Total Bigrams at Adjusted Crossover row` + 
                                                        `Percent Difference of Total Bigrams_ Adjusted Crossover - Optimal`)/3,
                   
                   
                   
  )
  
  write_xlsx(df_log, 
             f_out_4)
  
  cli_alert_success('\t\tFile {f_out_4} created')    
  
  my_toc <- toc(quiet = TRUE)
  diff_t <- my_toc$toc - my_toc$tic  
  
  cli_alert_info('Finished in {format(diff_t/60, digits = 4)} minutes')
  
  cat('\n\n')
  
  return(invisible(TRUE))
}
