# Computational Analysis of the VERITAS-ES-MADM Case Study

## Purpose

This repository operationalizes the VERITAS-ES-MADM framework as a reproducible computational workflow for claim verification and decision-readiness monitoring under uncertain and conflicting evidence.

The computational objective is not to replace automated fact-checking or evidence retrieval. Instead, the workflow provides a decision-readiness layer that evaluates whether available evidence is sufficiently mature, coherent, independent, non-conflicting, and discriminative to justify operational classification.

## Model workflow

The computational process follows seven main stages:

1. **Evidence-state representation.** Each claim is evaluated over three truth states: supported, refuted, and unresolved. Criterion-level evidence assessments are represented through central scores and uncertainty spreads.
2. **Fuzzy α-cut construction.** Evidence intervals are generated from triangular fuzzy representations at a selected α level.
3. **Criterion orientation.** Benefit and cost criteria are transformed into a common higher-is-better orientation.
4. **Preference conditioning.** PROMETHEE-type preference functions compare truth-state hypotheses under each criterion.
5. **Conditional probability construction.** Preference-enhanced scores are converted into coherent criterion-conditioned truth-state probabilities.
6. **Entropy-sensitive weighting.** Criterion diversification is used to derive objective weights, which are integrated with scenario-specific subjective weights.
7. **Decision-readiness diagnostics.** Final truth-state probabilities are complemented by evidence conflict, source independence, actionability, truth-state distinction, evidence–criteria synergy, and raw DRI.

## Scenario design

The case study includes three scenarios:

- **S0 Balanced monitoring:** reference configuration for general verification.
- **S1 Rapid-spread alert:** conservative early-warning configuration with higher uncertainty.
- **S2 Strategic-harm policy:** policy-support configuration with stronger evidence maturity.

This scenario structure allows the same claim set to be evaluated under different operational priorities without changing the core model.

## Classification logic

A claim is not classified solely because one truth-state probability is largest. Final classification requires the joint satisfaction of probability dominance, confidence margin, evidence conflict, and raw decision-readiness conditions.

This design separates probabilistic tendency from operational decision maturity. Claims may therefore remain unresolved even when they are visibly supported-leaning or refuted-leaning, if the evidence is not sufficiently mature for decision closure.

## Reproducibility

The repository provides:

- scenario-specific input workbooks;
- exported scenario result workbooks;
- a Shiny application for running the VERITAS-ES-MADM workflow;
- a standalone R script for reproducing manuscript figures;
- citation and archiving metadata for GitHub and Zenodo.

The figure-generation script is designed to run from the repository root and export manuscript-ready figures into the `figures/` directory.

## Interpretation note

The included case study should be interpreted as a reproducible computational demonstration. It is not a machine-learning benchmark trained directly on raw textual evidence. Empirical deployments should connect the framework with explicit evidence-ingestion pipelines, source-dependence estimation procedures, and external validation protocols.
