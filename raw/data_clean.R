library(tidyverse)

RedWine <- read.csv(file = "raw/winequality-red.csv",
                     header = T, sep = ";")
RedWine <- RedWine %>%
  dplyr::select(quality, colnames(RedWine)[-12]) %>%
  dplyr::mutate(quality=as.ordered(quality))

head(RedWine)

WhiteWine <- read.csv(file = "raw/winequality-white.csv",
                     header = T, sep = ";")
WhiteWine <- WhiteWine %>%
  dplyr::select(quality, colnames(WhiteWine)[-12]) %>%
  dplyr::mutate(quality=as.ordered(quality))

save(RedWine, file = "data/RedWine.rda", compress='xz')
save(WhiteWine, file = "data/WhiteWine.rda", compress='xz')
