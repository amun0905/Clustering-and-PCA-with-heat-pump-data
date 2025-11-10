# Heat Pump Data Analysis

This repository contains two R scripts that analyze electricity consumption data from a UK heat pump dataset. The analyses focus on dimensionality reduction and unsupervised learning to uncover patterns in daily energy usage.

---

## Scripts

### 1. PCA.R

#### Title: Principal Component Analysis (PCA) on Heat Pump Data

#### Objective:
Reduce the dimensionality of hourly electricity consumption data and visualize how well principal components represent daily usage patterns.

#### Workflow:
- Load and preprocess data for house code 5132.
- Aggregate electricity demand (Ehp) to hourly values.
- Reshape data to wide format: each row = day, each column = hour.
- Apply PCA using `prcomp()` (no scaling to preserve raw values).
- Visualize:
  - Scree plot (fviz_eig)
  - Variable contributions (fviz_pca_var, fviz_contrib)
  - Reconstructed data using first 2 PCs.
- Compare reconstructed patterns to original data.

#### Libraries Used:
- tidyverse
- lubridate
- factoextra

---

### 2. Clustering.R

#### Title: Clustering Daily Electricity Profiles

#### Objective:
Identify distinct daily heating patterns using clustering techniques on hourly electricity consumption data.

#### Workflow:
- Load and preprocess data for house code `5132`.
- Aggregate `Ehp` to hourly values and reshape to wide format.
- Apply clustering methods:
  - K-Means: Determine optimal clusters using Elbow and Silhouette methods.
  - DBSCAN: Density-based clustering with `kNNdistplot` to find epsilon.
  - Gaussian Mixture Models (GMM): Probabilistic clustering with BIC selection.
- Visualize cluster profiles by hour.
- Analyze cluster distribution across weekdays and months.

#### Libraries Used:
- tidyverse
- lubridate
- cluster
- factoextra
- mclust
- dbscan

---

## Dataset

- Source: UK Data Service RHPP dataset  
  https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=8151&type=Data%20catalogue#!/details

