#este código carrega o texto embaralhado, transforma em tipo tidytext e conta as letras
library(readr)
library(dplyr)
library(tidytext)

text <- read_file("scrambled_text.txt")
text_df <- data_frame(txt = text)

tidytxt <- text_df %>%
unnest_tokens(char, txt, token="characters", to_lower=FALSE)

#conta as letras
tidytxt %>%
 count(char, sort = TRUE)

#fazer download de um livro em português (antigo)
library(gutenbergr)
livro <- gutenberg_download(c(31509))
tidylivro <- livro %>%
unnest_tokens(word, text)
