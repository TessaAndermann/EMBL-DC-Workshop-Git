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

#Error message
#Removed 645 rows containing non-finite values (`stat_bin()`). #removed rows that contained non-finite values
#if you want to plot the points even if they have zeros, you can artificially convert zero to 1
#Because the log10 of 1 is zero
#Add 1 to the entire counts table

raw_cts_long %>%
  ggplot(aes(x = log10(cts+1), colour = replicate)) +
  geom_freqpoly() +
  facet_grid(rows = vars(strain), cols = vars(minute)) 

#But there is a small peak at zero on plot
raw_cts_long %>%
  ggplot(aes(x = log10(cts+1), colour = replicate)) +
  geom_freqpoly(binwidth =1) +
  facet_grid(rows = vars(strain), cols = vars(minute))

#try making a boxplot
raw_cts_long %>% 
  ggplot(aes(x = factor(minute),y = log10(cts +1), fill = strain)) +
  geom_boxplot()

#separate by replicates into grid
raw_cts_long %>% 
  ggplot(aes(x = factor(minute),y = log10(cts +1), fill = strain)) +
  geom_boxplot() +
  facet_grid(cols = vars(replicate))

#change what is separated
raw_cts_long %>% 
  ggplot(aes(x = factor(minute),y = log10(cts +1), fill = replicate)) +
  geom_boxplot() +
  facet_grid(cols = vars(strain))

#are replicates correlated with each other T0 and T30
#trans_ctx (wide version is better for correlation)

trans_cts %>%
  ggplot(aes(x= wt_0_r1, y = wt_30_r1)) +
  geom_point()

#to add line for correlation

trans_cts %>%
  ggplot(aes(x= wt_0_r1, y = wt_30_r1)) +
  geom_point()+
  geom_abline(colour = "brown")

#every possible correlation could be done separately or I could show correlations bteween all samples in a single plot 
#Calculate pairwise correltaion between all samples
#remove column that contains gene names

trans_cts_corr <- trans_cts %>%
  select(-gene) %>% 
  cor(method = "spearman")

#make heatmap
install.packages(corrr)
library(corrr)

rplot(trans_cts_corr) +
  theme(axis.text.x = element_text(angle = 45,hjust = 1))

#comparing raw and trans counts
summary(raw_cts_long$cts)
summary(trans_cts_long$cts)
#log transformation done
#make a scatterplot
#first w the raw counts

raw_cts %>% 
  ggplot(aes(x = wt_0_r1, y=wt_0_r2)) +
           geom_point()
#log 2 transformation

raw_cts %>%
  ggplot(aes(x = wt_0_r1 + 1, y=wt_0_r2 +1)) +
  geom_point() +
  scale_x_continuous(trans = "log2") +
  scale_y_continuous(trans = "log2")
  
#work in the long format so you can group by

raw_cts_long %>% 
  group_by(gene) %>% 
  summarize(mean_cts =mean(cts), var_cts =var(cts))
#this combines group and replicate
  
raw_cts_long %>% 
  group_by(gene) %>% 
  summarize(mean_cts =mean(cts), var_cts =var(cts)) %>% 
  ggplot(aes(x=mean_cts, y=var_cts)) +
  geom_point() +
  geom_abline(colour ="brown") +
  scale_x_continuous(trans ="log2") +
  scale_y_continuous(trans = "log2")
  
#positive relationship between mean and variance is expdcted

#poisson -variabnce and mean expected to be the same, not following poisson distribution

#apply methods to transform data so mean and variance are the same
#deseq2 is commonly used to transform

trans_cts_long %>% 
  group_by(gene) %>% 
  summarize(mean_cts =mean(cts), var_cts =var(cts)) %>% 
  mutate(above_four = var_cts >4) %>%
  ggplot(aes(x=mean_cts, y=var_cts)) 
  
#next section PCA
#Create matrix which can only have 1 type of data

pca_matrix <- trans_cts %>% 
  column_to_rownames("gene") %>% 
  as.matrix() %>% 
  t() #to transpose the matrix

sample_pca <- prcomp(pca_matrix) 
class(sample_pca)  
str(sample_pca)  
summary(sample_pca)
pca_matrix[1:5,1:5]
as_tibble(pca_matrix)
as_tibble(pca_matrix, rownames = "sample")
pc_eigenvalues <- sample_pca$sdev^2

# let's adapt our data into a tibble ready to be plotted with ggplot
pc_eigenvalues <- tibble(PC = factor(c(1:length(pc_eigenvalues))),
                         variance = pc_eigenvalues) %>% 
  mutate(pct = variance / sum(variance)*100 ) %>% 
  mutate(pct_cum = cumsum(pct))

# pareto plot / chart
pc_eigenvalues %>% 
  ggplot(aes(x = PC)) +
  geom_col(aes(y = pct)) +
  geom_line(aes(y = pct_cum, group = 1)) +
  geom_point(aes(y = pct_cum)) +
  # geom_hline(yintercept = 90) + # if you want to get an idea of how many PC you need to cover 90% of your variance
  labs(x= "Principal component",
       y = "Fraction of explained variance")

# this is plotting in BARS the variance explained by each PC
# the LINE + POINTS plots the cumulative variance explained

pc_scores <- sample_pca$x %>% 
  as_tibble(rownames = "sample")

pc_scores %>% 
  ggplot(aes(x = PC1, y = PC2)) +
  geom_point()

pca_plot <-pc_scores %>% 
  full_join(sample_info, by = "sample") %>% 
  ggplot(aes(x = PC1, y = PC2, color = factor(minute), shape = strain)) +
  geom_point()

# let's look at the loadings plot as well
pc_loadings <- sample_pca$rotation %>% 
  as_tibble(rownames = "gene")

top_genes <- pc_loadings %>% 
  select(gene, PC1, PC2) %>% 
  pivot_longer(matches("PC"), names_to = "PC", values_to = "loadings") %>% 
  group_by(PC) %>% 
  arrange(desc(abs(loadings))) %>% 
  slice(1:10) %>% 
  pull(gene) %>% 
  unique()

top_loadings <- pc_loadings %>% 
  filter(gene %in% top_genes)

loadings_plot <-ggplot(data = top_loadings) +
  geom_segment(aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.1, "in")),
               color = "brown") +
  geom_text(aes(x = PC1, y = PC2, label = gene),
            nudge_y = 0.005, size = 3) +
  scale_x_continuous(expand = c(0.02, 0.02))

#source at the top will execute everything
#manipulate the plots in a figure and store result of plotting in a variable
library(patchwork)

(pca_plot | loadings_plot)
#the plots are side by side, horizontal alignment
(pca_plot / loadings_plot)
#plots on top of one another
#if you have three components
(pca_plot | pca_plot | pca_plot) / loadings_plot +
  plot_annotation(tag_levels = "A")

library(ggfortify)
#comes with autoplot
autoplot(sample_pca)
#labels have variance in this case

autoplot(sample_pca, data = sample_info, 
         colour ="minute", shape ="strain")

library(broom)

#if you want to do loops in a certain way, there is a forcats package to help with loops
#search for tidyverse packages

tidy(sample_pca, matrix ="eigenvalues")
#the same as pc_eigenvalues <- tibble(PC etc....) above automatically
tidy(sample_pca, matrix = "loadings")
#this is the same as loadings vectors as above

#another hack
pca_plot <-autoplot(sample_pca, 
         data = sample_info %>%
           mutate(minute =as.factor(minute)),
         colour ="minute", shape ="strain")
#using as a factor makes the colors appear nicely


#differential expression analysis
test_result

#log fold change standard error
#baseMean column  is normalized expresion level of the gene
#this is what you get after deseq
#log2fold change is the amount of change between two conditions, time point change in this case
#lfcSE is standard error is associated with log2fold change
#stat value for each each gene computed as log2fold change /lfcSE compared to standard normal distribution
# comparison is the timepoint relative to zero?

#MA plot, plots basemean against the log2fold change, log-transform basemean
test_result %>%
  ggplot(aes(x = log10(baseMean), y= log2FoldChange)) +
  geom_point(alpha = 0.1) +
  facet_wrap(facets = vars(comparison))
 
#alpha is translparency
test_result %>%
  ggplot(aes(x = log10(baseMean), y= log2FoldChange)) +
  geom_point(alpha = 0.1) +
  facet_grid(facets = vars(comparison))

ma_plot <- test_result %>%
  mutate(sig = ifelse(padj <0.01, log2FoldChange, NA)) %>% 
  ggplot(aes(x =log10(baseMean), y = log2FoldChange))+
  geom_point(alpha = 0.1) +
  geom_point(aes(y=sig), colour ="tomato", size =1) +
  geom_hline (yintercept = 0, colour = "dodgerblue") +#another layer of points
  facet_wrap(facets = vars(comparison))

#ifelse is first the test, what to do if true, what to do if false
#sig is a new colummn that will be filled with result of test, and if test is true, use the given column and if not use NA
(ma_plot | pca_plot)

#trends of genes across timepoints
#Visualizing the expression trends
#STep1 get candidate genes that are adjusted significant p<0.01

#pull function is a way of extracting one columna dn turning into a vector from a tibble
candidate_genes <- test_result %>% 
  filter(padj <0.01) %>% 
  pull(gene) %>% #test_results[,"gene"] is equivalent, aka test_results$gene
  unique()

#1. get trans_cts in long format
trans_cts_long <- trans_cts %>% 
  pivot_longer(cols = wt_0_r1:mut_180_r3, names_to = "sample",
               values_to = "cts") %>% 
  full_join(sample_info, by = "sample")

#2. filter trans_cts_long for candidate genes and compute mean expression value for each gene in each timepoint and each genotype
trans_cts_mean <- trans_cts_long %>% 
  filter(gene %in% candidate_genes) %>% 
  group_by(gene, strain, minute) %>% 
  summarize(mean_cts = mean(cts), nrep =n()) %>% 
  ungroup()
#nrep gives you number of elements within a group but only in summarize function

#3. plot the trends
trans_cts_mean %>% 
  ggplot(aes(x=minute, y=mean_cts)) +
  geom_line(aes(group = gene), alpha = 0.3) +
  facet_grid(rows = vars(strain))

#scaling data to improve visualization
trans_cts_mean <-trans_cts_long %>% 
  filter(gene %in% candidate_genes) %>% 
  group_by(gene) %>% 
  mutate(cts_scaled = (cts - mean(cts) /sd(cts))) %>% 
  group_by(gene,strain, minute) %>% 
    summarize(mean_cts_scaled = mean(cts_scaled),
              nrep = n())
  
trans_cts_mean %>% 
  ggplot(aes(x=minute,y=mean_cts_scaled)) +
  geom_line(aes(group= gene), alpha =0.3) +
  geom_hline(yintercept = 0, colour ="brown", linetype ="dashed") +
  facet_grid(rows =vars(strain))

#Clustering 
#create a matrix of counts
hclust_matrix <- trans_cts %>% 
  select(-gene) %>% 
  as.matrix()
rownames(hclust_matrix) <-trans_cts$gene
hclust_matrix <- hclust_matrix [candidate_genes,]

hclust_matrix <- hclust_matrix %>% 
  t() %>% 
  scale() %>% 
  t()
#transpose cols to rows and then scale and then transpose again
#scale function applies to columns only and not rows
