# DARLViz

This repository contains the R code used to produce visualizations for the World Bank publication **"Recipe for a Livable Planet"**.

## 📘 About the Publication
*"Recipe for a Livable Planet"* is a World Bank report that explores strategies and pathways for achieving sustainable food systems while addressing the challenges of climate change, nutrition, and resilience. The report emphasizes evidence-based policy recommendations and leverages data-driven insights, including the visualizations developed here.

Full report: [World Bank – Recipe for a Livable Planet](https://www.worldbank.org/en/news/report/2025/01/23/recipe-for-a-livable-planet)  
(Note: update link if a permanent report page is available.)

## 📊 About This Repository
The code in this repository was developed to support key figures, charts, and data visualizations featured in the report. The scripts are written in **R** and focus on:
- Cleaning and preparing agricultural, climate, and economic datasets.
- Generating publication-quality static and interactive plots.
- Ensuring reproducibility of the visual outputs.

## 🛠️ Getting Started

### Requirements
- R (≥ 4.0)
- Suggested R packages:
  - `tidyverse`
  - `ggplot2`
  - `dplyr`
  - `readr`
  - `sf`
  - `rnaturalearth`
  - `rnaturalearthdata`
  - `plotly`
  - `here`

Install the required packages in R with:
```r
install.packages(c("tidyverse", "ggplot2", "dplyr", "readr", 
                   "sf", "rnaturalearth", "rnaturalearthdata", 
                   "plotly", "here"))
```

### Usage
1. Clone the repository:
   ```bash
   git clone https://github.com/mtnorton/DARLViz.git
   cd DARLViz
   ```
2. Open the `.R` scripts or `.Rmd` files in RStudio.
3. Run the code to reproduce the figures. Outputs (plots, data summaries) will be generated as specified in the scripts.

## 📜 License
This repository is shared for educational and research purposes. Please attribute the World Bank and the author(s) where appropriate.

## ✨ Acknowledgments
This work was conducted in collaboration with colleagues at the World Bank as part of the **"Recipe for a Livable Planet"** report team.  
Special thanks to contributors who supported data preparation, analysis, and peer review.

---
Maintainer: [Michael Norton](https://github.com/mtnorton)
