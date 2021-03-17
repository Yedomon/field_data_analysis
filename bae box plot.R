# Installation

remotes::install_github(
  repo = "IndrajeetPatil/ggstatsplot", # package path on GitHub
  dependencies = TRUE, # installs packages which ggstatsplot depends on
  upgrade_dependencies = TRUE,   # updates any out of date dependencies
  force = TRUE
)


# Package loading

library(ggstatsplot)


# Data importation

genes_counts = read.csv("gene_counts_rna.csv", h=T, sep = ",")

fold_change = read.csv("fold_change_rna.csv", h=T, sep = ",")


# plot
p = ggstatsplot::ggbetweenstats(
   data = genes_counts,
   x = Organ,
   y = Count,
   title = "A"
   ) +
  xlab("Organ") + ylab("Abundence count") +
  
p




# plot

q = ggstatsplot::ggbetweenstats(
  data = fold_change,
  x = Organ,
  y = log2_fc_plus_1,
  title = "B"
   ) +
  xlab("Organ") + ylab(expression(log[2]~Fold~change + 1))

q


# Loading library

library(patchwork)

p + q
