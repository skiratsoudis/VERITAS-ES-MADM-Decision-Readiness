# VERITAS-ES-MADM Decision-Readiness Framework

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.20098226.svg)](https://doi.org/10.5281/zenodo.20098226)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)

This repository contains the computational materials for **VERITAS-ES-MADM**, a fuzzy entropy–synergy multicriteria decision-readiness framework for claim verification under uncertain and conflicting evidence.

The repository accompanies a reproducible open-access computational case study involving monitored disinformation-related claims evaluated under three operational scenarios: **balanced monitoring**, **rapid-spread alerting**, and **strategic-harm policy support**.

VERITAS-ES-MADM is not designed to replace automated fact-checking, evidence retrieval, or expert verification. It provides a transparent decision-readiness layer that evaluates whether available evidence is sufficiently mature, coherent, independent, non-conflicting, and discriminative to justify operational classification.

---

## Repository DOI

The archived release is available through Zenodo:

**DOI:** [10.5281/zenodo.20098226](https://doi.org/10.5281/zenodo.20098226)

Please cite the Zenodo record and/or the metadata provided in [`CITATION.cff`](CITATION.cff) when using the repository.

---

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
│   └── .gitkeep
├── docs/
│   └── COMPUTATIONAL_ANALYSIS.md
├── README.md
├── CITATION.cff
├── .zenodo.json
├── .gitignore
├── DATA_LICENSE.md
└── LICENSE
```

---

## What is included

| Component | Purpose |
|---|---|
| `app/app.R` | Shiny application implementing the VERITAS-ES-MADM computational workflow. |
| `data/input/` | Scenario-specific Excel input files and the master case-study workbook. |
| `data/output/` | Exported result workbooks for the three case-study scenarios. |
| `scripts/` | Standalone R script for reproducing manuscript Figures 2–14. |
| `figures/` | Output directory for generated manuscript figures. |
| `docs/COMPUTATIONAL_ANALYSIS.md` | Concise computational description of the model workflow and case-study logic. |
| `CITATION.cff` | Citation metadata for the repository. |
| `.zenodo.json` | Zenodo archiving metadata. |

---

## Software requirements

The application and figure-generation workflow were developed in **R**. The following packages are required:

```r
install.packages(c(
  "shiny",
  "shinythemes",
  "readxl",
  "writexl",
  "ggplot2",
  "DT",
  "dplyr",
  "tidyr",
  "stringr",
  "scales",
  "ggrepel",
  "forcats"
))
```

---

## Running the VERITAS-ES-MADM Decision Studio

From the repository root, run:

```r
shiny::runApp("app")
```

The application imports scenario-specific Excel input files, executes the VERITAS-ES-MADM workflow, and exports:

- final truth-state probabilities;
- claim-level diagnostic indices;
- source-independence and actionability measures;
- raw Decision Readiness Index values;
- scenario-relative readiness scores;
- final claim classifications;
- scenario-level summaries.

---

## Reproducing manuscript figures

The standalone figure generator reads the exported scenario result workbooks from `data/output/` and reproduces the manuscript figures corresponding to **Figures 2–14**.

From the repository root, run:

```r
source("scripts/VERITAS_Standalone_Figure_Generator_Fig02_to_Fig14.R")
```

Generated figures are written to the `figures/` directory. The script is intended to support direct reproducibility of the figure set used in the associated manuscript.

---

## Case-study scenarios

The repository includes three scenario configurations:

| Scenario | Name | Analytical role |
|---|---|---|
| S0 | Balanced monitoring | Reference configuration for general claim verification. |
| S1 | Rapid-spread alert | Conservative early-warning posture under higher uncertainty and reduced readiness. |
| S2 | Strategic-harm policy | Policy-support posture under stronger evidence maturity. |

The scenario structure allows the same monitored claim set to be evaluated under different operational priorities while preserving the same core computational model.

---

## Output interpretation

The VERITAS-ES-MADM workflow produces final probability distributions over three truth states:

- **T**: supported;
- **F**: refuted;
- **U**: unresolved.

These probabilities are complemented by diagnostic indices, including:

- Veracity Normalized Information;
- Evidence-Criteria Synergy;
- Truth-State Distinction Index;
- Evidence Conflict Index;
- Source Independence Degree;
- Actionability Factor;
- raw Decision Readiness Index;
- scenario-relative Decision Readiness Index.

The **raw DRI** controls decision closure. The **scenario-relative RDRI** is provided only for within-scenario visualization and monitoring prioritization; it is not used as a classification threshold.

A claim may therefore remain unresolved even when it is visibly supported-leaning or refuted-leaning, if the evidence is not sufficiently mature for operational closure.

---

## Scope and interpretation note

The included case study should be interpreted as a reproducible computational demonstration of the VERITAS-ES-MADM decision-readiness logic. It is not a machine-learning benchmark trained directly on raw textual evidence.

Empirical deployments should connect the framework with explicit evidence-ingestion pipelines, source-dependence estimation procedures, human review protocols, and external validation datasets.

---

## Recommended citation

```text
Kiratsoudis, S., Tsiantos, V., & Spyropoulos, A. Z. (2026).
VERITAS-ES-MADM Decision-Readiness Framework: Computational materials,
case-study inputs/outputs, and figure-generation scripts (v1.0.0)
[Software and dataset]. Zenodo. https://doi.org/10.5281/zenodo.20098226
```

Alternatively, use the citation metadata in [`CITATION.cff`](CITATION.cff).

---

## License

The software code is released under the terms of the [MIT License](LICENSE).

The demonstration datasets and exported computational outputs are released under the terms specified in [`DATA_LICENSE.md`](DATA_LICENSE.md).
