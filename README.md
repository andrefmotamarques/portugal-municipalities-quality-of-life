# Quality of Life Index for Portuguese Municipalities

Building a multidimensional quality-of-life index for all Portuguese municipalities from official statistics, using PCA to find the underlying dimensions and clustering to group municipalities into comparable profiles.

Team project for the *Unsupervised Statistical Learning* course, MSc in Data Science, ISCTE (2025/26), developed with Afonso Claudino.

## Data

Municipal indicators from the Portuguese National Statistics Institute ([INE](https://www.ine.pt)) covering economic, social, demographic, housing, cultural and environmental topics, for every municipality in mainland Portugal, the Azores and Madeira.

## Method

1. **Data preparation:** harmonise the INE CSV exports into one dataset, normalise count variables by resident population, and impute missing values with multiple imputation (`mice`).
2. **Dimension reduction:** check suitability with the KMO test (overall KMO = 0.80 after reducing 40 candidate variables to 30), then PCA with varimax rotation (`psych::principal`), keeping 5 interpretable components:
   - Social inequality
   - Public services
   - Investment in culture
   - Economic engine
   - Human capital
3. **Clustering:** hierarchical clustering (Ward.D2) to explore structure, K-means on the component scores with k chosen by elbow and silhouette, and a model-based `Mclust` check of assignment uncertainty.
4. **Profiling:** clusters are characterised with passive "profile" variables (standardised), not used in the clustering itself.

## Results

Five municipality profiles emerged, including an "average" group close to the national mean, metropolitan municipalities with strong economies but housing and demographic pressure, emerging urban municipalities with the most favourable overall profile, and low-density municipalities facing ageing and weaker economic activity.

The main finding is that no profile scores high on every dimension. Quality of life comes from trade-offs between economic dynamism, housing conditions and demographics.

## Repository structure

```
quality_of_life_index.R   # data preparation, imputation, PCA, clustering, profiling
report.pdf                # full written report (Portuguese)
```

## How to run

The INE source files are not included. Download the indicators from the INE portal into the repository root (the script reads every `.csv` there, plus the `Habitantes - Normalizado/` and `Profile - Normalizado/` folders), then run `quality_of_life_index.R` from the repository root, with the packages listed below installed.

## Tech stack

R · psych · mice · VIM · factoextra · cluster · mclust · ggplot2 · pheatmap

## Team

André Marques and Afonso Claudino. The full report with the team's methodology and discussion is in [`report.pdf`](report.pdf) (Portuguese).
