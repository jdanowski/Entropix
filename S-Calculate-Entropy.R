library(cli)
library(fs)
library(readr)
library(purrr)
library(tictoc)
library(dplyr)
 
graphics.off()

# source functions
source('R-fcts/fcts-calculate-entropy.R')
source('R-fcts/fcts-stopwords.R')
source('R-fcts/fcts_process_single_chunk.R')
source('R-fcts/fcts-bigrams-to-remove.R')

# folder with text files to read (any extension is acceptable)
f_to_read <- dir_ls('00-input-files/00-txt-to-process/')

# increase memory size (only for Windows) -- not necessary
#utils::memory.limit(size = 15000) # in MB (10k = 10 GB)

# Number of pieces to break text object for processing
# The higher the value of n_pieces, lower the RAM memory usage, at 
# a overall speed cost
n_pieces <- 5

# For all calculations, this threshold will remove any bigram 
# with freq lower than threshold_freq.
threshold_freq_xlsx <- 10

# Use word filtering? 
# this option uses the words at "00-input-files/01-words-alpha/words_alpha.txt" 
# to filter all bigrams (keeping only matches to words_alpha)
use_word_filtering <- FALSE

# show plots? if TRUE, will show a new plot and save it in /figures
# if FALSE, will only save it and will not show
show_plots <- TRUE

# get stop words
df_stop_words <- get_stop_words()

# you can set custom bigrams to remove from output and calculations 
# such as "native video" and other.
# Add custom workds at file /00-input-files/02-stopwords/bigrams-to-remove.txt
use_bigrams_to_remove <- TRUE


walk(.x = f_to_read, 
     .f = calculate_bigram_entropy, 
     df_stop_words = df_stop_words,
     show_plots = show_plots, 
     use_word_filtering = use_word_filtering,
     n_pieces = n_pieces,
     use_bigrams_to_remove = use_bigrams_to_remove)

# consolidate logs
dir_logs <- '01-ouput-files/log-files/'
f_log <- dir_ls(dir_logs) 

idx <- str_detect(basename(f_log), 'BIGRAMS')
f_log <- f_log[idx]

df_logs <- bind_rows(
        map(f_log, readxl::read_excel)
)

write_xlsx(df_logs, 
           path(dir_logs, 'CONSOLIDATED.xlsx'))

# all output files are here
dir_tree('01-ouput-files/')
