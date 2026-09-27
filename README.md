# TMAO-Manuscript-09.2026
Analysis code and figure-generation scripts for the TMAO project, including the original analyses and revised analyses updated in September 2026.

Files

1. 01_TMAO_Longitudinal_Cognition.Rmd
   - Longitudinal LME analyses for CoCo, MoCA, TMT-A, TMT-B, semantic fluency, and DSST.
   - Six models: 1, 2, 3, 4a, 4b, 5.
   - One ggfortify::autoplot() companion diagnostic for MoCA.
   - 
2. 02_TMAO_Cognitive_Events_Sensitivity_Subgroups.Rmd
   - Time-to-event cognitive analyses.
   - Long-term/short-term sensitivity analyses.
   - Cognitive stroke-subgroup analyses from the supplied stroke file.
   - Six models wherever the corresponding analysis was present in the supplied source.
     
3. 03_TMAO_Hippocampal_Volume.Rmd
   - Bilateral, left, and right hippocampal-volume analyses.
   - Stroke subgroup and long-term/short-term sensitivity analyses.
   - Six models: 1, 2, 3, 4a, 4b, 5.
   - One ggfortify::autoplot() companion diagnostic for bilateral HV.
     
4. 04_TMAO_NfL.Rmd
   - NfL descriptive summary and mixed-effects models.
   - Six models: 1, 2, 3, 4a, 4b, 5.
   - One ggfortify::autoplot() companion diagnostic for NfL.
     
Model sequence
Cognitive analyses
- Model 1: age + sex + education + quality of life + GDS
- Model 2: Model 1 + smoking + BMI + hypertension + physical activity + alcohol
- Model 3: Model 2 + coronary artery disease + previous stroke/TIA
- Model 4a: Model 3 + eGFR
- Model 4b: Model 3 + diabetes
- Model 5: Model 3 + diabetes + eGFR
Hippocampal volume analyses
- Model 1: age + sex + intracranial volume + education
- Model 2: Model 1 + smoking + BMI + hypertension + physical activity + alcohol
- Model 3: Model 2 + coronary artery disease + previous stroke/TIA
- Model 4a: Model 3 + eGFR
- Model 4b: Model 3 + diabetes
- Model 5: Model 3 + diabetes + eGFR
  
For stroke-defined subgroup analyses, previous stroke/TIA is not included as an adjustment variable, matching the supplied subgroup scripts.

NfL analyses
- Model 1: age + sex
- Model 2: Model 1 + BMI + hypertension
- Model 3: Model 2 + coronary artery disease + previous stroke/TIA
- Model 4a: Model 3 + eGFR
- Model 4b: Model 3 + diabetes
- Model 5: Model 3 + diabetes + eGFR
- 
Analysis N
Each model set prints the complete-case participant N by fixed baseline TMAO quintile. Longitudinal models additionally report the number of contributing observations.

Diagnostics
The requested ggfortify::autoplot() check is included once for MoCA, bilateral hippocampal volume, and NfL. These are companion lm diagnostics. Primary inference remains based on the nlme::lme models.

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

## Software

Analyses were conducted in R.

The code primarily uses packages including:

nlme (3.1-167), forestplot (3.1.6), ggfortify (0.4.17), lme4 (1.1-36), ggpubr (0.6.1), ggcorrplot (0.1.4.1), PerformanceAnalytics (2.0.8), tidyverse (2.0.0), survival (3.8-3), survminer (0.5.0), parameters (0.24.2), sjtable2df (0.0.4), finalfit (1.0.8), broom (1.0.8), forcats (1.0.0), gridExtra (2.3), arsenal (3.7.1), knitr (1.52)

Additional packages are used for specific analyses and figures.

Because R packages may change over time, minor differences in formatting or package behaviour may occur when running the code under newer package versions.

## Data availability

The individual-level study data are not publicly distributed through this repository.

Access to the underlying data is subject to the applicable study governance, ethical approvals, and data-sharing requirements.


## Contact

For questions regarding the analysis code or study methodology, please contact the study authors.
