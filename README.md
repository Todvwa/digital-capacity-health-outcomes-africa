# Digital Health, Health System Capacity, and Health Outcomes in Africa

## Abstract
This project examines the relationship between digital capacity, health system infrastructure, and health outcomes across African countries between 2000 and 2022. Using panel data methods, the analysis investigates whether digital technologies such as internet access and mobile connectivity improve health outcomes independently or primarily through their interaction with existing health system capacity.

The study applies two-way fixed effects panel regression models, distributed lag specifications, and interaction models to evaluate whether digital technologies function as substitutes or complements to traditional health system inputs.

## Research Question
Do digital technologies improve health outcomes independently, or do they become effective primarily when combined with stronger health system capacity?

## Key Contribution
This project studies **health production complementarities** between digital infrastructure (“bytes”) and traditional health system capacity (“bricks”) using a multi-country African panel dataset (2000–2022). The main contribution is testing whether digital health gains depend on institutional and workforce capacity.

## Data

The analysis uses a **panel dataset of African countries from 2000–2022**, constructed from multiple publicly available sources:

- World Bank World Development Indicators (WDI)
- World Health Organization (WHO) Global Health Observatory
- Our World in Data (OWID)

The dataset was constructed through:
- Harmonizing country-year identifiers across datasets  
- Cleaning missing and inconsistent observations  
- Merging multiple international data sources  
- Constructing lagged digital variables  
- Generating interaction terms between digital and health system capacity variables  

### Key Variables

**Health Outcomes**
- Under-five mortality rate  
- Maternal mortality ratio  
- Neonatal mortality rate  

**Digital Capacity**
- Internet users (% of population)  
- Mobile cellular subscriptions (per 100 people)  

**Health System Capacity**
- Health expenditure  
- Healthcare workforce indicators (e.g., nurses per capita)  
- Access to improved water  
- Access to improved sanitation  

**Controls**
- GDP per capita  
- Fertility rate  
- Demographic and infrastructure controls  

## Methodology

The empirical analysis uses panel econometric techniques designed to isolate within-country variation over time.

### Baseline Model
Two-way fixed effects regression:

- Country fixed effects control for time-invariant heterogeneity  
- Year fixed effects control for global shocks  

### Distributed Lag Models
Digital variables are lagged to capture delayed effects of technology adoption on health outcomes.

### Interaction Model
The main specification tests whether digital capacity and health system capacity are complements:

- Interaction between digital access and healthcare workforce  
- Tests whether digital health gains depend on system readiness  

### Estimation Strategy
- Two-way fixed effects (country + year)  
- Clustered standard errors at the country level  
- Robustness checks using alternative specifications (log, first differences, lag structures)  

## Key Findings

- Digital technologies alone show limited independent effects on mortality outcomes after controlling for structural factors  
- Health system capacity and demographic variables remain strong and consistent predictors of health outcomes  
- The effect of digital technologies is conditional on health system capacity, especially workforce strength  
- Evidence supports a **complementarity framework** between digital infrastructure and traditional health inputs  

## Software & Tools

- R
- RStudio
- LaTeX

### R Packages
- tidyverse  
- fixest  
- plm  
- ggplot2  
- modelsummary  
- broom  
- janitor  
- skimr  
- kableExtra  

## Reproducibility

This project is fully reproducible. The analysis scripts:

- Clean and merge raw datasets  
- Construct variables and lag structures  
- Estimate econometric models  
- Generate tables and figures automatically  
- Export results to `/output/`  

### To replicate:
1. Download data from WDI, WHO, and OWID sources  
2. Run `data_cleaning.R` to construct the final dataset  
3. Run `analysis.R` to estimate models  
4. Run `figures.R` and `tables.R` to generate outputs  
5. Compile `paper.tex` for the final manuscript  

## Repository Structure

```text
digital-health-africa/
│
├── data/
│ └── africa_health_panel_clean.csv
│
├── scripts/
│ ├── data_cleaning.R
│ ├── analysis.R
│
├── output/
│ ├── figures/
│ └── tables/
│
├── paper/
│ ├── paper.tex
│ ├── paper.pdf
│ └── references.bib
│
└── README.md
```

## References

All references are included in:

`paper/references.bib`

Key literature includes:
- Grossman (1972) — Health production function  
- Cutler et al. (2006) — Health and economic development  
- Aker & Mbiti (2010) — Mobile phones in Africa  
- Labrique et al. (2013) — Digital health systems  
- Hjort & Tian (2019) — Digital technology and development  
- WHO Digital Health Strategy (2020–2025)  

## Author

**Todvwa Dlamini**  
University of Oklahoma  
M.A. Economics | B.S. Information Science & Technology
