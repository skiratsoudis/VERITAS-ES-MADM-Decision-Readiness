###############################################################################
# VERITAS-ES-MADM Standalone Figure Generator
# Case-study figures for claim verification and decision-readiness monitoring
#
# Author/platform creator: LT COL (ORD) Dr. Sideris Kiratsoudis
# Version: v4 reordered for manuscript integration; case-study figures start at Figure 2 and follow Section 4 order
# Purpose: Read exported VERITAS result workbooks and generate manuscript-ready
#          figures for Section 4 Case Study, Section 5 Discussion, and annexes.
###############################################################################

# -----------------------------------------------------------------------------
# 0. User configuration
# -----------------------------------------------------------------------------

INPUT_FILES <- c(
  "VERITAS_ES_MADM_results_S0.xlsx",
  "VERITAS_ES_MADM_results_S1.xlsx",
  "VERITAS_ES_MADM_results_S2.xlsx"
)

# If the script is saved in the same folder as the Excel files, this works as-is.
# Otherwise, set WORK_DIR manually, e.g. WORK_DIR <- "C:/Users/.../VERITAS"
WORK_DIR <- getwd()

OUTPUT_DIR <- file.path(WORK_DIR, "VERITAS_CaseStudy_Figures")
OUTPUT_PNG_DIR <- file.path(OUTPUT_DIR, "png")
OUTPUT_PDF_DIR <- file.path(OUTPUT_DIR, "pdf")

PRINT_TO_R_STUDIO <- TRUE
EXPORT_PNG <- TRUE
EXPORT_PDF <- TRUE
DPI <- 350

# Decision-rule thresholds used by the VERITAS application
THETA_P <- 0.50
THETA_M <- 0.10
THETA_D <- 0.015
THETA_C <- 0.60

# -----------------------------------------------------------------------------
# 1. Packages
# -----------------------------------------------------------------------------

required_pkgs <- c("readxl", "dplyr", "tidyr", "ggplot2", "stringr", "scales", "ggrepel", "forcats")
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop(
    "Please install the following packages before running this script: ",
    paste(missing_pkgs, collapse = ", "),
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(stringr)
  library(scales)
  library(ggrepel)
  library(forcats)
})

# -----------------------------------------------------------------------------
# 2. Helper functions
# -----------------------------------------------------------------------------

make_dirs <- function() {
  dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)
  dir.create(OUTPUT_PNG_DIR, showWarnings = FALSE, recursive = TRUE)
  dir.create(OUTPUT_PDF_DIR, showWarnings = FALSE, recursive = TRUE)
}

clean_scenario <- function(x) {
  x <- as.character(x)
  dplyr::case_when(
    str_detect(x, "S0") ~ "S0 Balanced monitoring",
    str_detect(x, "S1") ~ "S1 Rapid-spread alert",
    str_detect(x, "S2") ~ "S2 Strategic-harm policy",
    TRUE ~ x
  )
}

truth_label <- function(x) {
  dplyr::case_when(
    x == "T" ~ "T: Supported",
    x == "F" ~ "F: Refuted",
    x == "U" ~ "U: Unresolved",
    TRUE ~ as.character(x)
  )
}

numify <- function(df) {
  likely_num <- c(
    "Alpha", "Beta", "P_T", "P_F", "P_U", "Probability", "VeracityBalance", "Margin",
    "VNI", "ECS", "TDI", "ECI", "SID", "AF", "DRI_Reduced", "DRI", "RDRI",
    "w_OBJ", "w_SBJ", "w_INT", "Diversification", "Entropy", "p_T", "p_F", "p_U",
    "Mu", "Delta", "q", "p", "sigma", "lambda", "SubjectiveWeight"
  )
  df %>% mutate(across(any_of(likely_num), ~ suppressWarnings(as.numeric(.x))))
}

read_sheet_all <- function(files, sheet) {
  bind_rows(lapply(files, function(f) {
    readxl::read_excel(f, sheet = sheet) %>%
      numify() %>%
      mutate(SourceFile = basename(f))
  }))
}

classification_palette <- c(
  "Supported"  = "#00B050",
  "Refuted"    = "#F46D6A",
  "Unresolved" = "#5B95F5",
  "Conflicting" = "#C77CFF"
)

truth_palette <- c(
  "T: Supported" = "#00B050",
  "F: Refuted" = "#F46D6A",
  "U: Unresolved" = "#5B95F5"
)

criterion_type_palette <- c(
  "benefit" = "#1F78B4",
  "cost" = "#B15928"
)

pass_palette <- c(
  "Pass" = "#00B050",
  "Fail" = "#F46D6A"
)

metric_palette <- c(
  "AF" = "#6A4C93",
  "DRI_scaled" = "#1982C4",
  "ECI" = "#FF595E",
  "ECS" = "#8AC926",
  "SID" = "#00A6A6",
  "TDI" = "#FFCA3A",
  "VNI" = "#4267AC"
)

theme_veritas <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = base_size + 3, color = "#0B1F33"),
      plot.subtitle = element_text(size = base_size, color = "#334E68", margin = margin(b = 8)),
      plot.caption = element_text(size = base_size - 2, color = "#627D98", hjust = 0),
      axis.title = element_text(face = "bold", color = "#102A43"),
      axis.text = element_text(color = "#243B53"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#D9E2EC", linewidth = 0.25),
      strip.text = element_text(face = "bold", color = "#102A43"),
      strip.background = element_rect(fill = "#F0F4F8", color = NA),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
}

save_plot <- function(plot, filename, width = 11, height = 7) {
  if (PRINT_TO_R_STUDIO) print(plot)
  if (EXPORT_PNG) {
    ggsave(file.path(OUTPUT_PNG_DIR, paste0(filename, ".png")), plot,
           width = width, height = height, dpi = DPI, bg = "white")
  }
  if (EXPORT_PDF) {
    ggsave(file.path(OUTPUT_PDF_DIR, paste0(filename, ".pdf")), plot,
           width = width, height = height, bg = "white")
  }
}

wrap_claim <- function(x, width = 34) stringr::str_wrap(as.character(x), width = width)

# -----------------------------------------------------------------------------
# 3. Data loading
# -----------------------------------------------------------------------------

make_dirs()

input_paths <- file.path(WORK_DIR, INPUT_FILES)
missing_files <- input_paths[!file.exists(input_paths)]
if (length(missing_files) > 0) {
  stop(
    "Missing input file(s):\n", paste(missing_files, collapse = "\n"),
    "\n\nPlace this script in the folder containing the exported VERITAS result workbooks, or edit WORK_DIR / INPUT_FILES.",
    call. = FALSE
  )
}

summary_df <- read_sheet_all(input_paths, "Summary") %>%
  mutate(
    ScenarioShort = clean_scenario(Scenario),
    ClaimID = factor(ClaimID, levels = paste0("Q", sprintf("%02d", 1:8))),
    Classification = factor(Classification, levels = c("Supported", "Refuted", "Unresolved", "Conflicting")),
    ObservedLabel = factor(ObservedLabel, levels = c("T", "F", "U")),
    PreliminaryState = factor(PreliminaryState, levels = c("T", "F", "U")),
    DominantProbability = pmax(P_T, P_F, P_U, na.rm = TRUE),
    DominantState = case_when(
      P_T >= P_F & P_T >= P_U ~ "T",
      P_F >= P_T & P_F >= P_U ~ "F",
      TRUE ~ "U"
    ),
    ClaimShort = paste0(as.character(ClaimID), " (", as.character(ObservedLabel), ")")
  )

truth_df <- read_sheet_all(input_paths, "TruthStateProbabilities") %>%
  mutate(
    ScenarioShort = clean_scenario(Scenario),
    ClaimID = factor(ClaimID, levels = paste0("Q", sprintf("%02d", 1:8))),
    TruthStateLabel = factor(truth_label(TruthState), levels = c("T: Supported", "F: Refuted", "U: Unresolved"))
  )

weights_df <- read_sheet_all(input_paths, "CriterionWeights") %>%
  mutate(
    ScenarioShort = clean_scenario(Scenario),
    ClaimID = factor(ClaimID, levels = paste0("Q", sprintf("%02d", 1:8))),
    CriterionID = factor(CriterionID, levels = paste0("C", 1:8)),
    CriterionLabel = paste0(as.character(CriterionID), " — ", CriterionName),
    ICI = w_INT * Diversification
  )

criterion_prob_df <- read_sheet_all(input_paths, "CriterionProbabilities") %>%
  mutate(
    ScenarioShort = clean_scenario(Scenario),
    ClaimID = factor(ClaimID, levels = paste0("Q", sprintf("%02d", 1:8))),
    CriterionID = factor(CriterionID, levels = paste0("C", 1:8)),
    CriterionLabel = paste0(as.character(CriterionID), " — ", CriterionName)
  )

criteria_df <- read_sheet_all(input_paths, "Criteria") %>%
  mutate(
    ScenarioShort = clean_scenario(Scenario),
    CriterionID = factor(CriterionID, levels = paste0("C", 1:8)),
    CriterionLabel = paste0(as.character(CriterionID), " — ", CriterionName),
    Type = tolower(Type)
  )

evidence_df <- read_sheet_all(input_paths, "EvidenceScores") %>%
  mutate(
    ScenarioShort = clean_scenario(Scenario),
    ClaimID = factor(ClaimID, levels = paste0("Q", sprintf("%02d", 1:8))),
    TruthStateLabel = factor(truth_label(TruthState), levels = c("T: Supported", "F: Refuted", "U: Unresolved")),
    CriterionID = factor(CriterionID, levels = paste0("C", 1:8))
  )

alpha_df <- read_sheet_all(input_paths, "AlphaSweep") %>%
  mutate(
    ScenarioShort = clean_scenario(Scenario),
    ClaimID = factor(ClaimID, levels = paste0("Q", sprintf("%02d", 1:8))),
    Classification = factor(Classification, levels = c("Supported", "Refuted", "Unresolved", "Conflicting")),
    ObservedLabel = factor(ObservedLabel, levels = c("T", "F", "U"))
  )

# -----------------------------------------------------------------------------
# 4. Derived datasets
# -----------------------------------------------------------------------------

scenario_summary <- summary_df %>%
  group_by(ScenarioShort) %>%
  summarise(
    Supported = sum(Classification == "Supported", na.rm = TRUE),
    Refuted = sum(Classification == "Refuted", na.rm = TRUE),
    Unresolved = sum(Classification == "Unresolved", na.rm = TRUE),
    Conflicting = sum(Classification == "Conflicting", na.rm = TRUE),
    Mean_DRI = mean(DRI, na.rm = TRUE),
    Mean_RDRI = mean(RDRI, na.rm = TRUE),
    Mean_ECI = mean(ECI, na.rm = TRUE),
    Mean_ECS = mean(ECS, na.rm = TRUE),
    Mean_TDI = mean(TDI, na.rm = TRUE),
    Mean_SID = mean(SID, na.rm = TRUE),
    Mean_AF = mean(AF, na.rm = TRUE),
    Mean_VNI = mean(VNI, na.rm = TRUE),
    .groups = "drop"
  )

weights_mean <- weights_df %>%
  group_by(ScenarioShort, CriterionID, CriterionName, CriterionLabel) %>%
  summarise(
    w_OBJ = mean(w_OBJ, na.rm = TRUE),
    w_SBJ = mean(w_SBJ, na.rm = TRUE),
    w_INT = mean(w_INT, na.rm = TRUE),
    Diversification = mean(Diversification, na.rm = TRUE),
    Entropy = mean(Entropy, na.rm = TRUE),
    ICI = mean(ICI, na.rm = TRUE),
    .groups = "drop"
  )

# -----------------------------------------------------------------------------
# 5. Manuscript-ready figures
# -----------------------------------------------------------------------------

# Figure 2 ---------------------------------------------------------------------
fig2 <- criteria_df %>%
  distinct(ScenarioShort, CriterionID, CriterionName, CriterionLabel, Type, SubjectiveWeight) %>%
  ggplot(aes(x = fct_rev(CriterionID), y = SubjectiveWeight, fill = Type)) +
  geom_col(width = 0.72, color = "white", linewidth = 0.25) +
  geom_text(aes(label = sprintf("%.2f", SubjectiveWeight)), hjust = -0.08, size = 3.5, fontface = "bold", color = "#102A43") +
  coord_flip(ylim = c(0, max(criteria_df$SubjectiveWeight, na.rm = TRUE) * 1.20)) +
  facet_wrap(~ScenarioShort, nrow = 1) +
  scale_fill_manual(values = criterion_type_palette, name = "Criterion type") +
  labs(
    title = "Figure 2. Scenario-specific criterion-weight regimes",
    subtitle = "Subjective weights define the operational posture: balanced monitoring, rapid-spread alerting, and strategic-harm policy support.",
    x = NULL, y = "Subjective weight",
    caption = "Criterion IDs: C1 credibility, C2 relevance, C3 primary-source proximity, C4 temporal proximity, C5 source independence, C6 semantic consistency, C7 contradiction intensity, C8 propagation abnormality."
  ) +
  theme_veritas()
save_plot(fig2, "Fig02_scenario_weight_regimes", width = 13, height = 6.2)

# Figure 3 ---------------------------------------------------------------------
evidence_mean <- evidence_df %>%
  group_by(ScenarioShort, ClaimID, TruthStateLabel) %>%
  summarise(MeanMu = mean(Mu, na.rm = TRUE), MeanDelta = mean(Delta, na.rm = TRUE), .groups = "drop")

fig3 <- evidence_mean %>%
  ggplot(aes(x = ClaimID, y = TruthStateLabel, fill = MeanMu)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.2f", MeanMu)), size = 3.1, fontface = "bold", color = "#102A43") +
  facet_wrap(~ScenarioShort, nrow = 1) +
  scale_fill_gradient(low = "#F0F4F8", high = "#0B7285", labels = number_format(accuracy = 0.01), name = "Mean μ") +
  labs(
    title = "Figure 3. Input evidence geometry by claim and truth-state hypothesis",
    subtitle = "Average fuzzy evidence scores show which truth-state hypothesis receives stronger criterion-level support before diagnostic gating.",
    x = "Claim", y = "Truth-state hypothesis",
    caption = "Values are criterion-averaged central evidence scores μ. The full long-format input matrix remains available in the exported EvidenceScores sheets."
  ) +
  theme_veritas()
save_plot(fig3, "Fig03_input_evidence_geometry", width = 13, height = 5.8)

# Figure 4 ---------------------------------------------------------------------
fig4 <- truth_df %>%
  ggplot(aes(x = ClaimID, y = Probability, fill = TruthStateLabel)) +
  geom_col(position = position_dodge(width = 0.82), width = 0.72, color = "white", linewidth = 0.2) +
  geom_hline(yintercept = THETA_P, linetype = "dashed", color = "#243B53", linewidth = 0.45) +
  facet_wrap(~ScenarioShort, nrow = 1) +
  scale_fill_manual(values = truth_palette, name = "Truth-state") +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 0.78)) +
  labs(
    title = "Figure 4. Final truth-state probability profiles",
    subtitle = "Dominant T/F/U probability is necessary but not sufficient for final classification; decision-readiness diagnostics are applied afterwards.",
    x = "Claim", y = "Final probability",
    caption = paste0("Dashed horizontal line marks θP = ", THETA_P, ".")
  ) +
  theme_veritas()
save_plot(fig4, "Fig04_truth_state_probability_profiles", width = 13, height = 6.2)

# Figure 5 ---------------------------------------------------------------------
# Manual ternary/simplex mapping: T=(0,0), F=(1,0), U=(0.5,sqrt(3)/2)
simplex_df <- summary_df %>%
  mutate(
    x_simplex = P_F + 0.5 * P_U,
    y_simplex = sqrt(3) / 2 * P_U
  )
triangle <- data.frame(
  x = c(0, 1, 0.5, 0),
  y = c(0, 0, sqrt(3) / 2, 0),
  label = c("T", "F", "U", "T")
)
vertices <- data.frame(
  x = c(0, 1, 0.5),
  y = c(0, 0, sqrt(3) / 2),
  vertex = c("T: Supported", "F: Refuted", "U: Unresolved"),
  vertex_vjust = c(1.6, 1.6, -1.0)
)

fig5 <- ggplot() +
  geom_path(data = triangle, aes(x = x, y = y), color = "#334E68", linewidth = 0.7) +
  geom_point(data = vertices, aes(x = x, y = y), size = 3, color = "#102A43") +
  geom_text(data = vertices, aes(x = x, y = y, label = vertex, vjust = vertex_vjust), fontface = "bold", color = "#102A43", size = 3.4) +
  geom_point(data = simplex_df, aes(x = x_simplex, y = y_simplex, color = Classification, shape = ObservedLabel), size = 3.6, alpha = 0.95) +
  ggrepel::geom_text_repel(
    data = simplex_df,
    aes(x = x_simplex, y = y_simplex, label = ClaimID),
    size = 3.2, fontface = "bold", color = "#102A43", segment.color = "#9FB3C8",
    max.overlaps = 100, min.segment.length = 0
  ) +
  facet_wrap(~ScenarioShort, nrow = 1) +
  scale_color_manual(values = classification_palette, name = "Classification") +
  coord_equal(xlim = c(-0.06, 1.06), ylim = c(-0.06, 0.93)) +
  labs(
    title = "Figure 5. Truth-state simplex map",
    subtitle = "Each point represents the full probability vector (PT, PF, PU), making supported, refuted, and unresolved tendencies visible in a single diagnostic geometry.",
    x = NULL, y = NULL,
    shape = "Observed label",
    caption = "The simplex is a ternary representation implemented with Cartesian coordinates; it does not require additional R packages such as ggtern."
  ) +
  theme_veritas() +
  theme(axis.text = element_blank(), panel.grid = element_blank())
save_plot(fig5, "Fig05_truth_state_simplex_map", width = 13, height = 6.0)

# Figure 6 ---------------------------------------------------------------------
fig6 <- summary_df %>%
  ggplot(aes(x = Margin, y = DRI, color = Classification, shape = PreliminaryState, size = AF)) +
  geom_vline(xintercept = THETA_M, linetype = "dashed", color = "#52606D", linewidth = 0.45) +
  geom_hline(yintercept = THETA_D, linetype = "dashed", color = "#52606D", linewidth = 0.45) +
  geom_point(alpha = 0.92) +
  ggrepel::geom_text_repel(aes(label = ClaimID), size = 3.2, fontface = "bold", color = "#102A43", max.overlaps = 100) +
  facet_wrap(~ScenarioShort, nrow = 1) +
  scale_color_manual(values = classification_palette, name = "Classification") +
  scale_size_continuous(range = c(3, 8), name = "AF") +
  labs(
    title = "Figure 6. Decision-readiness gating map",
    subtitle = "The map separates probability margin from raw DRI. Claims below the DRI threshold remain unresolved even when their dominant truth-state probability is high.",
    x = "Probability margin", y = "Raw Decision Readiness Index (DRI)",
    shape = "Preliminary state",
    caption = paste0("Dashed lines show θM = ", THETA_M, " and θD = ", THETA_D, ". Point size represents the Abstention Factor (AF).")
  ) +
  theme_veritas()
save_plot(fig6, "Fig06_decision_readiness_gating_map", width = 13, height = 6.4)

# Figure 7 ---------------------------------------------------------------------
fig7 <- summary_df %>%
  ggplot(aes(x = ClaimID, y = DRI, fill = Classification)) +
  geom_col(width = 0.72, color = "white", linewidth = 0.25) +
  geom_hline(yintercept = THETA_D, linetype = "dashed", color = "#102A43", linewidth = 0.5) +
  geom_text(aes(label = sprintf("RDRI=%.2f", RDRI)), angle = 90, vjust = 0.5, hjust = -0.08, size = 3.0, fontface = "bold", color = "#102A43") +
  facet_wrap(~ScenarioShort, nrow = 1) +
  scale_fill_manual(values = classification_palette, name = "Classification") +
  scale_y_continuous(labels = number_format(accuracy = 0.001), limits = c(0, max(summary_df$DRI, na.rm = TRUE) * 1.22)) +
  labs(
    title = "Figure 7. Raw DRI hierarchy and scenario-relative RDRI labels",
    subtitle = "Raw DRI controls classification, whereas RDRI reports within-scenario relative prominence for visualization and monitoring prioritization.",
    x = "Claim", y = "Raw DRI",
    caption = paste0("Dashed horizontal line marks θD = ", THETA_D, ".")
  ) +
  theme_veritas()
save_plot(fig7, "Fig07_raw_dri_hierarchy_rdri_labels", width = 13, height = 6.2)

# Figure 8 ---------------------------------------------------------------------
gates_df <- summary_df %>%
  mutate(
    `Dominant probability gate` = ifelse(DominantProbability >= THETA_P, "Pass", "Fail"),
    `Margin gate` = ifelse(Margin >= THETA_M, "Pass", "Fail"),
    `Raw DRI gate` = ifelse(DRI >= THETA_D, "Pass", "Fail"),
    `Conflict gate` = ifelse(ECI < THETA_C, "Pass", "Fail")
  ) %>%
  select(ScenarioShort, ClaimID, Classification, `Dominant probability gate`, `Margin gate`, `Raw DRI gate`, `Conflict gate`) %>%
  pivot_longer(cols = ends_with("gate"), names_to = "Gate", values_to = "Status") %>%
  mutate(
    Gate = factor(Gate, levels = c("Dominant probability gate", "Margin gate", "Raw DRI gate", "Conflict gate")),
    Status = factor(Status, levels = c("Pass", "Fail"))
  )

fig8 <- gates_df %>%
  ggplot(aes(x = Gate, y = ClaimID, fill = Status)) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(aes(label = ifelse(Status == "Pass", "✓", "×")), size = 5, fontface = "bold", color = "#102A43") +
  facet_wrap(~ScenarioShort, nrow = 1) +
  scale_fill_manual(values = pass_palette, name = "Gate status") +
  labs(
    title = "Figure 8. Decision-rule gate matrix",
    subtitle = "The matrix explains why preliminary truth-state tendencies do or do not become final classifications.",
    x = NULL, y = "Claim",
    caption = paste0("A final Supported/Refuted classification requires probability, margin, and raw DRI gates to pass, while the conflict gate must not be violated. Thresholds: θP=", THETA_P, ", θM=", THETA_M, ", θD=", THETA_D, ", θC=", THETA_C, ".")
  ) +
  theme_veritas() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))
save_plot(fig8, "Fig08_decision_rule_gate_matrix", width = 13, height = 6.2)

# Figure 9 ---------------------------------------------------------------------
scenario_metrics_long <- scenario_summary %>%
  mutate(
    DRI_scaled = Mean_DRI / max(Mean_DRI, na.rm = TRUE)
  ) %>%
  select(ScenarioShort, Mean_ECI, Mean_ECS, Mean_TDI, Mean_SID, Mean_AF, Mean_VNI, DRI_scaled) %>%
  rename(
    ECI = Mean_ECI,
    ECS = Mean_ECS,
    TDI = Mean_TDI,
    SID = Mean_SID,
    AF = Mean_AF,
    VNI = Mean_VNI
  ) %>%
  pivot_longer(cols = -ScenarioShort, names_to = "Metric", values_to = "Value") %>%
  mutate(Metric = factor(Metric, levels = c("VNI", "ECS", "TDI", "ECI", "SID", "AF", "DRI_scaled")))

fig9 <- scenario_metrics_long %>%
  ggplot(aes(x = Metric, y = Value, group = ScenarioShort, color = ScenarioShort)) +
  geom_line(linewidth = 1.0, alpha = 0.85) +
  geom_point(size = 3.0) +
  scale_y_continuous(labels = number_format(accuracy = 0.01), limits = c(0, 1)) +
  labs(
    title = "Figure 9. Scenario-level diagnostic portfolio",
    subtitle = "Mean diagnostics across scenarios; raw DRI is scaled for comparison with bounded diagnostic indices.",
    x = NULL, y = "Scenario-level value",
    color = "Scenario",
    caption = "DRI_scaled = Mean DRI / max(Mean DRI). Raw DRI values should be reported separately in the results table."
  ) +
  theme_veritas()
save_plot(fig9, "Fig09_scenario_level_diagnostic_portfolio", width = 11.5, height = 6.2)


# Figure 10 ---------------------------------------------------------------------
class_counts <- summary_df %>%
  count(ScenarioShort, Classification, name = "Claims")

fig10 <- class_counts %>%
  ggplot(aes(x = ScenarioShort, y = Claims, fill = Classification)) +
  geom_col(width = 0.68, color = "white", linewidth = 0.35) +
  geom_text(aes(label = Claims), position = position_stack(vjust = 0.5), fontface = "bold", color = "white", size = 4.2) +
  scale_fill_manual(values = classification_palette, name = "Final classification") +
  scale_y_continuous(breaks = 0:8, limits = c(0, 8)) +
  labs(
    title = "Figure 10. Scenario-level classification composition",
    subtitle = "Rapid-spread alert suppresses final closure; S0 and S2 classify only decision-ready claims.",
    x = NULL, y = "Number of monitored claims",
    caption = "The figure should be interpreted jointly with Figure 8, which explains the gate-level reason behind classification or unresolved status."
  ) +
  theme_veritas() +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))
save_plot(fig10, "Fig10_scenario_classification_composition", width = 10.5, height = 6.2)


# Figure 11 ---------------------------------------------------------------------
fig11 <- weights_mean %>%
  ggplot(aes(x = Diversification, y = w_INT, size = w_OBJ, color = CriterionID)) +
  geom_point(alpha = 0.90) +
  ggrepel::geom_text_repel(aes(label = CriterionID), size = 3.3, fontface = "bold", color = "#102A43", max.overlaps = 100) +
  facet_wrap(~ScenarioShort, nrow = 1) +
  scale_size_continuous(range = c(3, 8), name = "Mean wOBJ") +
  labs(
    title = "Figure 11. Information-sensitive weighting landscape",
    subtitle = "Integrated weights are shaped by both subjective priorities and criterion-level diversification, rather than by subjective weights alone.",
    x = "Mean diversification", y = "Mean integrated weight",
    color = "Criterion",
    caption = "Each point is criterion-averaged across the eight monitored claims within a scenario."
  ) +
  theme_veritas()
save_plot(fig11, "Fig11_information_sensitive_weighting_landscape", width = 13, height = 6.2)


# Figure 12 ---------------------------------------------------------------------
fig12 <- weights_mean %>%
  ggplot(aes(x = ScenarioShort, y = fct_rev(CriterionID), fill = ICI)) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(aes(label = sprintf("%.3f", ICI)), size = 3.2, fontface = "bold", color = "#102A43") +
  scale_fill_gradient(low = "#F0F4F8", high = "#7B2CBF", name = "Mean ICI") +
  labs(
    title = "Figure 12. Integrated Criteria Importance heatmap",
    subtitle = "ICI = wINT × diversification identifies which criteria combine operational importance with realized discriminatory contribution.",
    x = NULL, y = "Criterion",
    caption = "Higher values indicate stronger effective contribution to claim-level decision diagnostics."
  ) +
  theme_veritas() +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))
save_plot(fig12, "Fig12_integrated_criteria_importance_heatmap", width = 10.5, height = 6.8)


# Figure 13 ---------------------------------------------------------------------
criterion_truth <- criterion_prob_df %>%
  group_by(ScenarioShort, CriterionID, CriterionName) %>%
  summarise(
    `T: Supported` = mean(p_T, na.rm = TRUE),
    `F: Refuted` = mean(p_F, na.rm = TRUE),
    `U: Unresolved` = mean(p_U, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = all_of(c("T: Supported", "F: Refuted", "U: Unresolved")), names_to = "TruthStateLabel", values_to = "MeanProbability") %>%
  mutate(TruthStateLabel = factor(TruthStateLabel, levels = c("T: Supported", "F: Refuted", "U: Unresolved")))

fig13 <- criterion_truth %>%
  ggplot(aes(x = TruthStateLabel, y = fct_rev(CriterionID), fill = MeanProbability)) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(aes(label = sprintf("%.2f", MeanProbability)), size = 3.0, fontface = "bold", color = "#102A43") +
  facet_wrap(~ScenarioShort, nrow = 1) +
  scale_fill_gradient(low = "#F0F4F8", high = "#006D77", name = "Mean probability") +
  labs(
    title = "Figure 13. Criterion-conditioned truth-state probability structure",
    subtitle = "This heatmap shows how each criterion distributes support across T, F, and U after preference-conditioned probability construction.",
    x = NULL, y = "Criterion",
    caption = "Values are averaged across the monitored claims and therefore summarize the scenario-level criterion probability posture."
  ) +
  theme_veritas() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))
save_plot(fig13, "Fig13_criterion_conditioned_truth_state_probabilities", width = 13, height = 6.2)


# Figure 14 ---------------------------------------------------------------------
fig14 <- alpha_df %>%
  ggplot(aes(x = Alpha, y = DRI, group = ClaimID, color = ClaimID)) +
  geom_hline(yintercept = THETA_D, linetype = "dashed", color = "#102A43", linewidth = 0.45) +
  geom_line(linewidth = 0.85, alpha = 0.85) +
  geom_point(size = 2.4, alpha = 0.90) +
  facet_wrap(~ScenarioShort, nrow = 1) +
  scale_x_continuous(breaks = sort(unique(alpha_df$Alpha))) +
  labs(
    title = "Figure 14. α-level robustness of raw decision-readiness",
    subtitle = "DRI trajectories across the α-grid show whether final readiness behaviour is stable under fuzzy interval contraction.",
    x = expression(alpha), y = "Raw DRI",
    color = "Claim",
    caption = paste0("Dashed horizontal line marks θD = ", THETA_D, ". Stable trajectories support the interpretation that scenario effects are structural, not artifacts of the selected α level.")
  ) +
  theme_veritas()
save_plot(fig14, "Fig14_alpha_level_dri_robustness", width = 13, height = 6.4)

# -----------------------------------------------------------------------------
# 6. Export consolidated figure-title index
# -----------------------------------------------------------------------------

figure_titles <- data.frame(
  Figure = paste0("Figure ", 2:14),
  FileStem = c(
    "Fig02_scenario_weight_regimes",
    "Fig03_input_evidence_geometry",
    "Fig04_truth_state_probability_profiles",
    "Fig05_truth_state_simplex_map",
    "Fig06_decision_readiness_gating_map",
    "Fig07_raw_dri_hierarchy_rdri_labels",
    "Fig08_decision_rule_gate_matrix",
    "Fig09_scenario_level_diagnostic_portfolio",
    "Fig10_scenario_classification_composition",
    "Fig11_information_sensitive_weighting_landscape",
    "Fig12_integrated_criteria_importance_heatmap",
    "Fig13_criterion_conditioned_truth_state_probabilities",
    "Fig14_alpha_level_dri_robustness"
  ),
  ManuscriptTitle = c(
    "Scenario-specific criterion-weight regimes",
    "Input evidence geometry by claim and truth-state hypothesis",
    "Final truth-state probability profiles",
    "Truth-state simplex map",
    "Decision-readiness gating map",
    "Raw DRI hierarchy and scenario-relative RDRI labels",
    "Decision-rule gate matrix",
    "Scenario-level diagnostic portfolio",
    "Scenario-level classification composition",
    "Information-sensitive weighting landscape",
    "Integrated Criteria Importance heatmap",
    "Criterion-conditioned truth-state probability structure",
    "α-level robustness of raw decision-readiness"
  ),
  RecommendedSection = c(
    "4.4 Scenario architecture and computational parameterization",
    "4.5 Evidence-score structure",
    "4.6 Consolidated computational results",
    "4.6 Consolidated computational results",
    "4.6 Consolidated computational results / decision-readiness gating",
    "4.6 Consolidated computational results / raw DRI interpretation",
    "4.6 Consolidated computational results / decision-rule gating",
    "4.7 Scenario-level synthesis",
    "4.7 Scenario-level synthesis / Discussion",
    "4.8 Criterion-weight behaviour",
    "4.8 Criterion-level contribution analysis",
    "4.8 Criterion-level probability interpretation",
    "4.9 Alpha-level robustness"
  ),
  stringsAsFactors = FALSE
)

write.csv(figure_titles, file.path(OUTPUT_DIR, "VERITAS_figure_title_index_Fig02_to_Fig14_v4_ordered.csv"), row.names = FALSE)

message("\nVERITAS figure generation completed.")
message("PNG output: ", OUTPUT_PNG_DIR)
message("PDF output: ", OUTPUT_PDF_DIR)
message("Figure-title index: ", file.path(OUTPUT_DIR, "VERITAS_figure_title_index_Fig02_to_Fig14_v4_ordered.csv"))

