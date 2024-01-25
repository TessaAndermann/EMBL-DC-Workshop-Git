library(tidyverse)

raw_cts <- read_csv("data_rnaseq/counts_raw.csv")
trans_cts <- read_csv("data_rnaseq/counts_transformed.csv")
sample_info <- read_csv("data_rnaseq/sample_info.csv")
test_result <- read_csv("data_rnaseq/test_result.csv")

#format changing from wide to long
#ggplot wants long format

trans_cts_long <- trans_cts %>% 
  pivot_longer(names_to = "sample", values_to = "cts", cols = wt_0_r1:mut_180_r3)

#alternative
#trans_cts_long <- trans_cts %>% 
  #pivot_longer(names_to = "sample", values_to = "cts", cols = -gene)

#first we need to join trans_cts and sample_info
trans_cts_long <- full_join(trans_cts_long, sample_info, join, by ="sample")
#to test if things are equal
#identical(a,b)

#showing distribution of counts across samples
#separate based on type
#first histogram to look at distribution

trans_cts_long %>%
  ggplot(aes(x = cts)) +
  geom_freqpoly()

#or alternatively you want it to be separated
trans_cts_long %>%
  ggplot(aes(x = cts, colour = replicate)) +
  geom_freqpoly(binwidth = 1)

#Separate based on strain and timepoint
trans_cts_long %>%
  ggplot(aes(x = cts, colour = replicate)) +
  geom_freqpoly(binwidth = 1) +
  facet_grid(rows = vars(strain), cols = vars(minute))

#make the same plot with raw counts
raw_cts_long <- raw_cts %>% 
  pivot_longer(names_to = "sample", values_to = "cts", cols = wt_0_r1:mut_180_r3)

raw_cts_long <- full_join(raw_cts_long, sample_info, join, by ="sample")
raw_cts_long %>%
  ggplot(aes(x = cts, colour = replicate)) +
  geom_freqpoly() +
  facet_grid(rows = vars(strain), cols = vars(minute))

#log-transform
raw_cts_long <- full_join(raw_cts_long, sample_info, join, by ="sample")
raw_cts_long %>%
  ggplot(aes(x = cts, colour = replicate)) +
  geom_freqpoly() +
  facet_grid(rows = vars(strain), cols = vars(minute)) +
  scale_x_log10()

#alternative log

raw_cts_long %>%
  ggplot(aes(x = log10(cts), colour = replicate)) +
  geom_freqpoly() +
  facet_grid(rows = vars(strain), cols = vars(minute)) 
