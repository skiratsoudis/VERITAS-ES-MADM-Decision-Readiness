# VERITAS-ES-MADM Decision-Readiness Framework

This repository contains the computational materials for **VERITAS-ES-MADM**, a fuzzy entropy–synergy multicriteria decision-readiness framework for claim verification under uncertain and conflicting evidence.

The repository accompanies a reproducible computational case study involving monitored disinformation-related claims evaluated under three operational scenarios: balanced monitoring, rapid-spread alerting, and strategic-harm policy support.

## Repository contents

```text
VERITAS-ES-MADM-Decision-Readiness/
├── app/
│   └── app.R
├── scripts/
│   └── VERITAS_Standalone_Figure_Generator_Fig02_to_Fig14.R
├── data/
│   ├── input/
│   │   ├── VERITAS_OpenAccess_CaseStudy_Master.xlsx
│   │   ├── VERITAS_Input_S0_BalancedMonitoring_AppReady.xlsx
│   │   ├── VERITAS_Input_S1_RapidSpreadAlert_AppReady.xlsx
│   │   └── VERITAS_Input_S2_StrategicHarmPolicy_AppReady.xlsx
│   └── output/
│       ├── VERITAS_ES_MADM_results_S0.xlsx
│       ├── VERITAS_ES_MADM_results_S1.xlsx
│       └── VERITAS_ES_MADM_results_S2.xlsx
├── figures/
├── docs/
│   └── COMPUTATIONAL_ANALYSIS.md
├── CITATION.cff
├── .zenodo.json
├── .gitignore
├── DATA_LICENSE.md
└── LICENSE
```

## Software requirements

The Shiny application and figure generator were developed in **R**. The following packages are required:

```r
install.packages(c(
  "shiny", "shinythemes", "readxl", "writexl", "ggplot2", "DT", "dplyr",
  "tidyr", "stringr", "scales", "ggrepel", "forcats"
))
```

## Running the VERITAS-ES-MADM Decision Studio

From the repository root, run:

```r
shiny::runApp("app")
```

The application imports scenario-specific Excel input files, performs the VERITAS-ES-MADM computation, and exports claim-level probabilities, diagnostic indices, decision-readiness measures, and scenario-level summaries.

## Reproducing manuscript figures

The standalone figure generator reads the exported scenario result workbooks from `data/output/` and produces manuscript-ready figures corresponding to Figures 2–14.

From the repository root, run:

```r
source("scripts/VERITAS_Standalone_Figure_Generator_Fig02_to_Fig14.R")
```

The generated figures are exported to the `figures/` directory. The script exports both PNG and PDF files when the corresponding options are enabled.

## Case-study scenarios

The repository includes three scenario configurations:

| Scenario | Name | Purpose |
|---|---|---|
| S0 | Balanced monitoring | Reference configuration for general claim verification |
| S1 | Rapid-spread alert | Early-warning posture under higher uncertainty and reduced readiness |
| S2 | Strategic-harm policy | Policy-support posture under stronger evidence maturity |

## Output interpretation

The VERITAS-ES-MADM workflow produces final truth-state probabilities over three states: supported, refuted, and unresolved. These probabilities are complemented by diagnostic indices, including evidence conflict, source independence, actionability, truth-state distinction, evidence–criteria synergy, and the raw Decision Readiness Index.

The raw DRI controls decision closure. The scenario-relative RDRI is provided only for within-scenario visualization and monitoring prioritization.

## Citation

Please cite the repository using the metadata provided in `CITATION.cff`. Once the repository is archived through Zenodo, please cite the Zenodo DOI corresponding to the archived release.

## License

The software code is released under the MIT License. The demonstration datasets and computational outputs are released under the terms specified in `DATA_LICENSE.md`.
