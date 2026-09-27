# TMAO-Manuscript-09.2026
Analysis code and figure-generation scripts for the TMAO project, including the original analyses and revised analyses updated in September 2026.

# TMAO Analysis Code

This repository contains the R code used for the statistical analyses and figure generation for our study investigating trimethylamine N-oxide (TMAO) in relation to neurological and cognitive outcomes.

The repository includes both the **original analysis code** and **revised analyses performed in September 2026**.

## Repository structure

Analyses with filenames containing **`09.2026`** represent the revised analyses performed in September 2026.

Files without the `09.2026` designation correspond to the original analyses performed during earlier stages of the project.

The revised analyses were performed to update and refine the statistical models, improve consistency across outcomes and sensitivity analyses, and address methodological and code-related issues identified during manuscript revision.

## Main analyses

The repository includes analyses examining the association of TMAO with:

- cognitive function and longitudinal cognitive change;
- incident cognitive impairment;
- hippocampal volume;
- neurofilament light chain (NFL);
- stroke-related subgroups and sensitivity analyses.

The cognitive outcomes analysed include:

- Montreal Cognitive Assessment (MoCA);
- Cognitive Construct score (CoCo);
- Trail Making Test A (TMT-A);
- Trail Making Test B (TMT-B);
- Semantic Fluency (SF);
- Digit Symbol Substitution Test (DSST).

## Revised analyses: September 2026

Files labelled with **`09.2026`** contain the revised analysis code.

These revisions include, where applicable:

- updated mixed-effects model specifications;
- sensitivity analyses;
- stroke subgroup analyses;
- updated Cox proportional hazards analyses;
- revised Kaplan–Meier analyses.

For longitudinal continuous TMAO analyses, interaction models use:

## Original analyses

Files that do **not** contain `09.2026` represent the original analysis code.

These files are retained for transparency and reproducibility and document the analytical workflow used during earlier stages of the project.

Because the analyses evolved during manuscript preparation and revision, the September 2026 files should be considered the **most recent analysis versions** where corresponding revised files are available.

## Figures

`Figures.R` contains code used to generate figures and supplementary visualisations, including:

- TMAO quintile boxplots;
- forest plots;
- spline-based association plots;
- Kaplan–Meier curves;
- cognitive trajectory plots;
- descriptive and sensitivity figures.

Some figure-generation sections may rely on intermediate result files generated from the statistical models.

## Statistical methods

Depending on the outcome, analyses include:

- linear mixed-effects models using `nlme::lme`;
- Cox proportional hazards regression;
- Kaplan–Meier survival analysis;
- restricted cubic or spline-based visualisation of continuous associations;
- subgroup and sensitivity analyses.

Mixed-effects models account for the hierarchical structure of the data where appropriate.

TMAO was analysed both:

1. categorically using quintiles; and
2. continuously using log-transformed TMAO (`log1p(tmao)`).

## Covariate adjustment

The degree of covariate adjustment varies according to the specific analysis and outcome.

Models generally include combinations of:

- age;
- sex;
- educational level;
- health status;
- depressive symptoms;
- smoking;
- body mass index;
- hypertension;
- physical activity;
- alcohol consumption;
- coronary heart disease;
- previous stroke/TIA;
- diabetes;
- kidney function.

The exact covariates used in each model are documented directly in the corresponding R scripts.

## Reproducibility

Patient-level datasets are **not included in this public repository** because they contain sensitive clinical research data.

The analysis scripts therefore require access to the corresponding study datasets to reproduce the numerical results.

Typical input files include datasets such as:

```text
Baseline.csv
Fu5.csv
FUP3.csv
FUP3.long.csv
WGS.csv
WGS.long.csv
```
These files should be stored locally and must not be committed to a public repository.

## Software

Analyses were conducted in R.

The code primarily uses packages including:

nlme (3.1-167), forestplot (3.1.6), ggfortify (0.4.17), lme4 (1.1-36), ggpubr (0.6.1), ggcorrplot (0.1.4.1), PerformanceAnalytics (2.0.8), tidyverse (2.0.0), survival (3.8-3), survminer (0.5.0), parameters (0.24.2), sjtable2df (0.0.4), finalfit (1.0.8), broom (1.0.8), forcats (1.0.0), gridExtra (2.3), arsenal (3.7.1), knitr (1.52)

Additional packages are used for specific analyses and figures.

Because R packages may change over time, minor differences in formatting or package behaviour may occur when running the code under newer package versions.

## Data availability

The individual-level study data are not publicly distributed through this repository.

Access to the underlying data is subject to the applicable study governance, ethical approvals, and data-sharing requirements.

## Versioning

For clarity:

- **Files containing `09.2026`**: revised analyses performed in September 2026.
- **Files without `09.2026`**: original analyses retained for transparency and documentation of the analytical history.

Where both an original and a September 2026 version exist, the **September 2026 version supersedes the earlier analysis for the corresponding revised analysis**.

## Contact

For questions regarding the analysis code or study methodology, please contact the study authors.
