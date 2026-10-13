# Installs the R packages used by quality_of_life_index.R (only the ones missing)
pkgs <- c("psych", "VIM", "mice", "factoextra", "ggplot2", "corrplot",
          "cluster", "pheatmap", "mclust")
missing <- setdiff(pkgs, rownames(installed.packages()))
if (length(missing)) install.packages(missing)
