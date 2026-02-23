# UV Degradation of *Typha domingensis* / *Boehmeria nivea* Biogeosynthetic Composite

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.18748157.svg)](https://doi.org/10.5281/zenodo.18748157)

Research data and statistical analysis scripts supporting the manuscript:

> **Mechanical Stability and Degradation Kinetics of a Natural-Fiber Bio-Geosynthetic Composite from *Typha domingensis* and *Boehmeria nivea* under Accelerated Aging**

## Repository Structure

```
data/
├── tensile/        Raw tensile test data (xlsx, txt) per exposure cycle (0–120)
├── puncture/       Raw puncture test data (Typha 50:50 geocomposite)
└── processed/      Statistical outputs, Weibull parameters, p-values, RData

scripts/            R scripts for Weibull, GLM/GEE, ANOVA, regression, and figure generation

figures/
├── tensile/        Tensile test plots
├── puncture/       Puncture test plots
└── comparison/     Comparative analysis plots
```

## Data Description

| Dataset | Description | Format |
|---------|-------------|--------|
| `data/tensile/Resumo - RESINADOS.xlsx` | Summary of tensile test results across all exposure cycles | Excel |
| `data/tensile/*.txt` | Individual tensile test curves (force × displacement) | TXT |
| `data/puncture/` | Puncture test curves for Typha 50:50 geocomposite | XLS/TXT |
| `data/processed/resultados_tracao.txt` | Consolidated tensile results | TXT |
| `data/processed/resultados_puncao.txt` | Consolidated puncture results | TXT |
| `data/processed/p_values_consolidados.csv` | All p-values from GLM/GEE/ANOVA | CSV |
| `data/processed/PARAMETROS_WEIBULL_GLM_CONSOLIDADOS.txt` | Weibull shape/scale parameters and GLM outputs | TXT |
| `data/processed/resultados_analise_weibull.RData` | Full Weibull analysis workspace | RData |

## Key Scripts

| Script | Purpose |
|--------|---------|
| `scripts/modelo_puncao.r` | GLM (Gamma–log) and Weibull analysis for puncture tests |
| `scripts/VERIFICACAO_MANUSCRITO.r` | Audit script verifying all statistical claims in the manuscript |
| `scripts/VERIFICACAO_V2.r` | Updated verification with bootstrap CI |
| `scripts/anova_check.r` | ANOVA assumption checks |
| `scripts/regression_check.r` | Temporal regression model comparison |
| `scripts/CRIAR_FIGURA_COMPARATIVA.R` | Generate comparative tensile vs. puncture figures |
| `scripts/audit_discussion.r` | Cross-check Discussion section claims against data |

## Reproducibility

Scripts were developed in R 4.x with the following packages: `stats`, `MASS`, `survival`, `boot`, `ggplot2`, `car`, `geepack`.

To reproduce the analysis:

```r
source("scripts/VERIFICACAO_MANUSCRITO.r")
source("scripts/modelo_puncao.r")
```

## License

This dataset is released under [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/).

## Citation

If you use these data, please cite:

Santos, L.D.V.; Holanda, F.S.R.; da Silva, E.G.; Azerêdo, A.J.S.; Rodrigues Júnior, J.J.; de Araújo Filho, R.N.; Sussuchi, E.M.; Pedrotti, A.; da Silva Filho, C.I. (2025). *Mechanical Stability and Degradation Kinetics of a Natural-Fiber Bio-Geosynthetic Composite from Typha domingensis and Boehmeria nivea under Accelerated Aging*. [Dataset]. Zenodo. https://doi.org/10.5281/zenodo.18748157
