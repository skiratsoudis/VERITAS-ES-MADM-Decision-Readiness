###############################################################################
# VERITAS_ES_MADM_DECISION_STUDIO.R
# Corrected build v5: V2 model alignment + RDRI visualization + professional UI/icons
#
# VERITAS-ES-MADM Decision Studio
# Computational Shiny platform for claim-level information verification under
# fuzzy evidence uncertainty, preference-conditioned truth-state probabilities,
# entropy-based criterion informativeness, diagnostic indices, and scenario
# portfolio analysis.
#
# Model creator: LT COL (ORD) Dr. Sideris Kiratsoudis
# Computational template inspired by the ES-MADM III Decision Studio structure.
###############################################################################

suppressPackageStartupMessages({
  library(shiny)
  library(shinythemes)
  library(readxl)
  library(writexl)
  library(ggplot2)
  library(DT)
  library(dplyr)
})

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
EPS <- 1e-12
TRUTH_STATES <- c("T", "F", "U")

# -----------------------------------------------------------------------------
# Numeric utilities
# -----------------------------------------------------------------------------

as_num <- function(x, default = NA_real_) {
  out <- suppressWarnings(as.numeric(x))
  out[is.na(out)] <- default
  out
}

as_chr <- function(x) trimws(as.character(x))

clamp01 <- function(x) {
  out <- suppressWarnings(as.numeric(x))
  out[!is.finite(out)] <- 0
  pmin(1, pmax(0, out))
}

renorm_simplex <- function(x, eps = EPS) {
  nms <- names(x)
  x <- as.numeric(x)
  x[!is.finite(x)] <- 0
  x <- pmax(x, 0)
  s <- sum(x)
  if (!is.finite(s) || s <= eps) {
    out <- rep(1 / length(x), length(x))
    names(out) <- nms
    return(out)
  }
  out <- x / s
  names(out) <- nms
  out
}

entropy_base2 <- function(p, eps = EPS) {
  p <- renorm_simplex(p, eps)
  nz <- p > eps
  -sum(p[nz] * log2(p[nz]))
}

safe_bool <- function(x, default = FALSE) {
  if (is.null(x) || length(x) == 0 || is.na(x[1])) return(default)
  val <- tolower(trimws(as.character(x[1])))
  val %in% c("true", "t", "yes", "y", "1")
}

safe_scalar <- function(x, default = 0) {
  if (is.null(x) || length(x) == 0) return(default)
  out <- suppressWarnings(as.numeric(x[1]))
  if (!is.finite(out)) default else out
}

safe_chr_scalar <- function(x, default = "") {
  if (is.null(x) || length(x) == 0) return(default)
  out <- suppressWarnings(as.character(x[1]))
  if (length(out) == 0 || is.na(out) || !nzchar(trimws(out))) return(default)
  trimws(out)
}

safe_state_name <- function(x, default = "U") {
  out <- safe_chr_scalar(x, default)
  out <- toupper(out)
  if (!(out %in% TRUTH_STATES)) default else out
}

safe_threshold <- function(x, default = 0) {
  clamp01(safe_scalar(x, default))[1]
}

# Repair probability interval bounds so that a feasible box-simplex exists.
repair_interval_bounds <- function(lower, upper, total = 1, eps = EPS) {
  lower <- as.numeric(lower)
  upper <- as.numeric(upper)
  if (length(lower) != length(upper)) stop("Interval bounds must have the same length.")

  lower[!is.finite(lower)] <- 0
  upper[!is.finite(upper)] <- 0
  lower <- pmax(lower, 0)
  upper <- pmax(upper, lower)
  lower <- pmin(lower, total)
  upper <- pmin(upper, total)

  sL <- sum(lower)
  if (sL > total + 1e-10) {
    lower <- lower / sL * total
    upper <- pmax(upper, lower)
    upper <- pmin(upper, total)
  }

  sU <- sum(upper)
  if (sU < total - 1e-10) {
    room <- pmax(total - upper, 0)
    if (sum(room) <= eps) {
      upper <- rep(total / length(upper), length(upper))
    } else {
      upper <- upper + (total - sU) * room / sum(room)
      upper <- pmax(upper, lower)
      upper <- pmin(upper, total)
    }
  }

  list(lower = lower, upper = upper)
}

project_box_simplex <- function(target, lower, upper, total = 1, eps = EPS) {
  target <- as.numeric(target)
  repaired <- repair_interval_bounds(lower, upper, total, eps)
  lower <- repaired$lower
  upper <- repaired$upper

  if (length(target) != length(lower)) stop("Projection target and bounds must have the same length.")
  if (sum(lower) > total + 1e-10 || sum(upper) < total - 1e-10) {
    stop("Projection bounds do not define a nonempty box-constrained simplex.")
  }

  phi <- function(lambda) sum(pmin(pmax(target - lambda, lower), upper)) - total
  lo <- min(target - upper) - total - 1
  hi <- max(target - lower) + total + 1

  for (iter in seq_len(200)) {
    mid <- (lo + hi) / 2
    val <- phi(mid)
    if (abs(val) <= 1e-13) break
    if (val > 0) lo <- mid else hi <- mid
  }

  x <- pmin(pmax(target - (lo + hi) / 2, lower), upper)
  diff <- total - sum(x)
  if (abs(diff) > 1e-11) {
    if (diff > 0) {
      slack <- upper - x
      for (idx in order(slack, decreasing = TRUE)) {
        add <- min(diff, slack[idx])
        x[idx] <- x[idx] + add
        diff <- diff - add
        if (diff <= 1e-12) break
      }
    } else {
      diff <- -diff
      slack <- x - lower
      for (idx in order(slack, decreasing = TRUE)) {
        sub <- min(diff, slack[idx])
        x[idx] <- x[idx] - sub
        diff <- diff - sub
        if (diff <= 1e-12) break
      }
    }
  }
  renorm_simplex(x, eps)
}

# -----------------------------------------------------------------------------
# Preference functions
# -----------------------------------------------------------------------------

preference_value <- function(d, family = "linear", q = 0, p = 0.5, sigma = 0.25) {
  family <- tolower(trimws(family %||% "linear"))
  d <- as.numeric(d)
  q <- safe_scalar(q, 0)
  p <- safe_scalar(p, 0.5)
  sigma <- safe_scalar(sigma, 0.25)
  p <- max(p, q + EPS)
  sigma <- max(sigma, EPS)

  out <- rep(0, length(d))
  if (family %in% c("usual", "type1")) {
    out <- ifelse(d > 0, 1, 0)
  } else if (family %in% c("u-shape", "ushape", "u_shape", "type2")) {
    out <- ifelse(d > q, 1, 0)
  } else if (family %in% c("v-shape", "vshape", "v_shape", "type3")) {
    out <- ifelse(d <= 0, 0, ifelse(d >= p, 1, d / p))
  } else if (family %in% c("level", "type4")) {
    out <- ifelse(d <= q, 0, ifelse(d <= p, 0.5, 1))
  } else if (family %in% c("linear", "type5")) {
    out <- ifelse(d <= q, 0, ifelse(d >= p, 1, (d - q) / (p - q)))
  } else if (family %in% c("gaussian", "type6")) {
    out <- ifelse(d <= 0, 0, 1 - exp(-(d^2) / (2 * sigma^2)))
  } else {
    out <- ifelse(d <= q, 0, ifelse(d >= p, 1, (d - q) / (p - q)))
  }
  clamp01(out)
}

# -----------------------------------------------------------------------------
# Workbook reader
# -----------------------------------------------------------------------------

read_settings <- function(path) {
  sheets <- excel_sheets(path)
  if (!("Settings" %in% sheets)) return(list())
  raw <- read_excel(path, sheet = "Settings")
  if (!all(c("Parameter", "Value") %in% names(raw))) return(list())
  vals <- as.list(raw$Value)
  names(vals) <- raw$Parameter
  vals
}

get_setting_num <- function(settings, key, default) {
  val <- settings[[key]] %||% default
  safe_scalar(val, default)
}

get_setting_chr <- function(settings, key, default) {
  val <- settings[[key]] %||% default
  if (is.na(val) || !nzchar(as.character(val))) default else as.character(val)
}

read_veritas_workbook <- function(path) {
  sheets <- excel_sheets(path)
  required <- c("Settings", "Criteria", "EvidenceScores")
  missing <- setdiff(required, sheets)
  if (length(missing) > 0) stop("Missing required sheet(s): ", paste(missing, collapse = ", "))

  settings <- read_settings(path)
  scenario_name <- get_setting_chr(settings, "ScenarioName", tools::file_path_sans_ext(basename(path)))

  criteria <- read_excel(path, sheet = "Criteria") %>% as.data.frame()
  evidence <- read_excel(path, sheet = "EvidenceScores") %>% as.data.frame()

  if (!("CriterionID" %in% names(criteria))) stop("Criteria sheet must include CriterionID.")
  if (!("CriterionID" %in% names(evidence))) stop("EvidenceScores sheet must include CriterionID.")
  if (!("ClaimID" %in% names(evidence))) stop("EvidenceScores sheet must include ClaimID.")
  if (!("TruthState" %in% names(evidence))) stop("EvidenceScores sheet must include TruthState.")
  if (!all(c("Mu", "Delta") %in% names(evidence))) stop("EvidenceScores sheet must include Mu and Delta.")

  criteria$CriterionID <- as_chr(criteria$CriterionID)
  evidence$CriterionID <- as_chr(evidence$CriterionID)
  evidence$ClaimID <- as_chr(evidence$ClaimID)
  evidence$TruthState <- toupper(as_chr(evidence$TruthState))
  evidence$Mu <- clamp01(as_num(evidence$Mu, 0.5))
  evidence$Delta <- pmax(0, as_num(evidence$Delta, 0))

  if (!("CriterionName" %in% names(criteria))) criteria$CriterionName <- criteria$CriterionID
  if (!("Type" %in% names(criteria))) criteria$Type <- "benefit"
  if (!("PreferenceFunction" %in% names(criteria))) criteria$PreferenceFunction <- "linear"
  if (!("q" %in% names(criteria))) criteria$q <- 0
  if (!("p" %in% names(criteria))) criteria$p <- 0.5
  if (!("sigma" %in% names(criteria))) criteria$sigma <- 0.25
  if (!("lambda" %in% names(criteria))) criteria$lambda <- 0.5
  if (!("SubjectiveWeight" %in% names(criteria))) criteria$SubjectiveWeight <- NA_real_

  criteria$CriterionName <- as_chr(criteria$CriterionName)
  criteria$Type <- tolower(as_chr(criteria$Type))
  criteria$PreferenceFunction <- as_chr(criteria$PreferenceFunction)
  criteria$q <- as_num(criteria$q, 0)
  criteria$p <- as_num(criteria$p, 0.5)
  criteria$sigma <- as_num(criteria$sigma, 0.25)
  criteria$lambda <- clamp01(as_num(criteria$lambda, 0.5))
  criteria$SubjectiveWeight <- as_num(criteria$SubjectiveWeight, NA_real_)

  source_pairs <- NULL
  if ("SourcePairs" %in% sheets) {
    source_pairs <- read_excel(path, sheet = "SourcePairs") %>% as.data.frame()
    if (nrow(source_pairs) > 0) {
      source_pairs$ClaimID <- as_chr(source_pairs$ClaimID)
      source_pairs$Similarity <- clamp01(as_num(source_pairs$Similarity, 0))
    }
  }

  expert_weights <- NULL
  if ("ExpertWeights" %in% sheets) {
    expert_weights <- read_excel(path, sheet = "ExpertWeights") %>% as.data.frame()
    if (nrow(expert_weights) > 0) {
      expert_weights$ExpertID <- as_chr(expert_weights$ExpertID)
      expert_weights$CriterionID <- as_chr(expert_weights$CriterionID)
      expert_weights$Reliability <- pmax(0, as_num(expert_weights$Reliability, 1))
      expert_weights$Weight <- pmax(0, as_num(expert_weights$Weight, 0))
    }
  }

  labels <- NULL
  if ("Labels" %in% sheets) {
    labels <- read_excel(path, sheet = "Labels") %>% as.data.frame()
    if (nrow(labels) > 0) {
      labels$ClaimID <- as_chr(labels$ClaimID)
      labels$ObservedLabel <- toupper(as_chr(labels$ObservedLabel))
    }
  }

  list(
    path = path,
    scenario = scenario_name,
    settings = settings,
    criteria = criteria,
    evidence = evidence,
    source_pairs = source_pairs,
    expert_weights = expert_weights,
    labels = labels
  )
}

subjective_weights <- function(criteria, expert_weights = NULL) {
  crit_ids <- criteria$CriterionID
  m <- length(crit_ids)

  if (!is.null(expert_weights) && nrow(expert_weights) > 0) {
    experts <- unique(expert_weights$ExpertID)
    rho <- sapply(experts, function(g) {
      vals <- expert_weights$Reliability[expert_weights$ExpertID == g]
      mean(vals, na.rm = TRUE)
    })
    rho <- renorm_simplex(rho)
    pooled <- rep(1, m)
    for (idx in seq_along(experts)) {
      g <- experts[idx]
      tmp <- expert_weights[expert_weights$ExpertID == g, c("CriterionID", "Weight")]
      w <- rep(0, m)
      for (j in seq_along(crit_ids)) {
        val <- tmp$Weight[tmp$CriterionID == crit_ids[j]]
        w[j] <- if (length(val) == 0) 0 else val[1]
      }
      w <- renorm_simplex(w)
      pooled <- pooled * pmax(w, EPS)^rho[idx]
    }
    return(renorm_simplex(pooled))
  }

  if ("SubjectiveWeight" %in% names(criteria) && any(is.finite(criteria$SubjectiveWeight))) {
    return(renorm_simplex(ifelse(is.finite(criteria$SubjectiveWeight), criteria$SubjectiveWeight, 0)))
  }

  rep(1 / m, m)
}

source_independence <- function(claim_id, source_pairs, default_sid = 1) {
  default_sid <- safe_threshold(default_sid, 1)
  if (is.null(source_pairs) || nrow(source_pairs) == 0 || !("Similarity" %in% names(source_pairs))) return(default_sid)
  tmp <- source_pairs[source_pairs$ClaimID == claim_id, , drop = FALSE]
  if (nrow(tmp) == 0) return(default_sid)
  sim_mean <- mean(tmp$Similarity, na.rm = TRUE)
  if (!is.finite(sim_mean)) return(default_sid)
  clamp01(1 - sim_mean)[1]
}

# -----------------------------------------------------------------------------
# VERITAS core computation
# -----------------------------------------------------------------------------

compute_veritas_scenario <- function(scenario_obj, alpha = NULL, beta = NULL, thresholds = NULL) {
  settings <- scenario_obj$settings
  alpha <- safe_threshold(alpha %||% get_setting_num(settings, "Alpha", 0.50), 0.50)
  beta <- safe_threshold(beta %||% get_setting_num(settings, "Beta", 0.50), 0.50)
  eps <- max(safe_scalar(get_setting_num(settings, "Epsilon", EPS), EPS), 1e-15)
  default_sid <- safe_threshold(get_setting_num(settings, "SourceIndependenceDefault", 1), 1)

  thresholds <- thresholds %||% list()
  thetaP <- safe_threshold(thresholds$thetaP %||% get_setting_num(settings, "ThetaP", 0.50), 0.50)
  thetaM <- safe_threshold(thresholds$thetaM %||% get_setting_num(settings, "ThetaM", 0.10), 0.10)
  thetaD <- safe_threshold(thresholds$thetaD %||% get_setting_num(settings, "ThetaD", 0.015), 0.015)
  thetaC <- safe_threshold(thresholds$thetaC %||% get_setting_num(settings, "ThetaC", 0.60), 0.60)

  criteria <- scenario_obj$criteria
  evidence <- scenario_obj$evidence
  source_pairs <- scenario_obj$source_pairs
  crit_ids <- criteria$CriterionID
  m <- length(crit_ids)
  K <- length(TRUTH_STATES)
  logK <- log2(K)

  sbj <- subjective_weights(criteria, scenario_obj$expert_weights)

  scenario_name <- safe_chr_scalar(scenario_obj$scenario, "Scenario")
  claim_ids <- unique(evidence$ClaimID)
  claim_ids <- claim_ids[!is.na(claim_ids) & nzchar(as.character(claim_ids))]
  summaries <- list()
  state_probs <- list()
  criterion_rows <- list()
  weights_rows <- list()

  for (claim_id in claim_ids) {
    ev_i <- evidence[evidence$ClaimID == claim_id, , drop = FALSE]
    claim_id <- safe_chr_scalar(claim_id, "Claim")
    claim_text <- if ("ClaimText" %in% names(ev_i)) safe_chr_scalar(unique(ev_i$ClaimText), claim_id) else claim_id
    theme <- if ("Theme" %in% names(ev_i)) safe_chr_scalar(unique(ev_i$Theme), "Unspecified") else "Unspecified"

    P_hat <- matrix(NA_real_, nrow = K, ncol = m, dimnames = list(TRUTH_STATES, crit_ids))
    D <- numeric(m)
    Hcrit <- numeric(m)

    for (j in seq_along(crit_ids)) {
      cid <- crit_ids[j]
      cj <- criteria[j, ]
      ev_ij <- ev_i[ev_i$CriterionID == cid, , drop = FALSE]

      mu <- setNames(rep(0.5, K), TRUTH_STATES)
      delta <- setNames(rep(0, K), TRUTH_STATES)
      for (h in TRUTH_STATES) {
        row <- ev_ij[ev_ij$TruthState == h, , drop = FALSE]
        if (nrow(row) > 0) {
          mu[h] <- row$Mu[1]
          delta[h] <- row$Delta[1]
        }
      }

      xL <- clamp01(mu - (1 - alpha) * delta)
      xU <- clamp01(mu + (1 - alpha) * delta)
      xU <- pmax(xU, xL)

      if (tolower(cj$Type) %in% c("cost", "c", "negative")) {
        rL <- 1 - xU
        rU <- 1 - xL
      } else {
        rL <- xL
        rU <- xU
      }

      piL <- numeric(K)
      piU <- numeric(K)
      names(piL) <- names(piU) <- TRUTH_STATES
      for (h in seq_len(K)) {
        valsL <- c()
        valsU <- c()
        for (ell in seq_len(K)) {
          if (ell == h) next
          dL <- rL[h] - rU[ell]
          dU <- rU[h] - rL[ell]
          valsL <- c(valsL, preference_value(dL, cj$PreferenceFunction, cj$q, cj$p, cj$sigma))
          valsU <- c(valsU, preference_value(dU, cj$PreferenceFunction, cj$q, cj$p, cj$sigma))
        }
        piL[h] <- mean(valsL)
        piU[h] <- mean(valsU)
      }

      lambda <- cj$lambda
      sL <- pmax(0, rL * (1 + lambda * piL))
      sU <- pmax(sL, rU * (1 + lambda * piU))

      if (all(sL <= eps) && all(sU <= eps)) {
        pL <- pU <- rep(1 / K, K)
      } else {
        pL <- numeric(K)
        pU <- numeric(K)
        for (h in seq_len(K)) {
          denL <- sL[h] + sum(sU[-h])
          denU <- sU[h] + sum(sL[-h])
          pL[h] <- ifelse(denL <= eps, 1 / K, sL[h] / denL)
          pU[h] <- ifelse(denU <= eps, 1 / K, sU[h] / denU)
        }
      }
      repaired <- repair_interval_bounds(pL, pU, total = 1, eps = eps)
      pL <- repaired$lower
      pU <- repaired$upper
      mid <- (pL + pU) / 2
      phat <- project_box_simplex(mid, pL, pU, total = 1, eps = eps)
      names(phat) <- TRUTH_STATES

      P_hat[, j] <- phat
      Hcrit[j] <- entropy_base2(phat, eps)
      D[j] <- 1 - Hcrit[j] / logK

      criterion_rows[[length(criterion_rows) + 1]] <- data.frame(
        Scenario = scenario_name,
        ClaimID = safe_chr_scalar(claim_id, "Claim"),
        ClaimText = safe_chr_scalar(claim_text, safe_chr_scalar(claim_id, "Claim")),
        CriterionID = safe_chr_scalar(cid, paste0("C", j)),
        CriterionName = safe_chr_scalar(cj$CriterionName, safe_chr_scalar(cid, paste0("C", j))),
        p_T = safe_threshold(phat["T"], 0), p_F = safe_threshold(phat["F"], 0), p_U = safe_threshold(phat["U"], 0),
        Entropy = safe_scalar(Hcrit[j], 0), Diversification = safe_threshold(D[j], 0),
        stringsAsFactors = FALSE
      )
    }

    w_obj <- if (sum(D) <= eps) rep(1 / m, m) else renorm_simplex(D, eps)
    raw_int <- pmax(w_obj, eps)^beta * pmax(sbj, eps)^(1 - beta)
    w_int <- renorm_simplex(raw_int, eps)

    P_final <- as.numeric(P_hat %*% w_int)
    names(P_final) <- TRUTH_STATES
    P_final <- renorm_simplex(P_final, eps)
    names(P_final) <- TRUTH_STATES

    HY <- safe_scalar(entropy_base2(P_final, eps), 0)
    HYC <- safe_scalar(sum(w_int * Hcrit, na.rm = TRUE), 0)
    Icy <- max(0, safe_scalar(HY - HYC, 0))
    HC <- safe_scalar(entropy_base2(w_int, eps), 0)
    VNI <- safe_threshold((2 * Icy) / (HC + HY + eps), 0)
    ECS <- safe_threshold(sum(w_int * D, na.rm = TRUE), 0)
    TDI <- safe_threshold(1 - HY / logK, 0)
    ECI <- safe_threshold(2 * min(P_final["T"], P_final["F"], na.rm = TRUE), 0)
    SID <- safe_threshold(source_independence(claim_id, source_pairs, default_sid), default_sid)
    AF <- safe_threshold(1 - P_final["U"], 0)
    DRI_Reduced <- safe_threshold(AF * (1 - ECI) * TDI * ((VNI + ECS) / 2), 0)
    DRI <- safe_threshold(DRI_Reduced * SID, 0)

    ord <- order(P_final, decreasing = TRUE)
    yhat <- safe_state_name(names(P_final)[ord[1]], "U")
    margin <- safe_threshold(P_final[ord[1]] - P_final[ord[2]], 0)
    VB <- safe_scalar(P_final["T"] - P_final["F"], 0)

    label <- "Unresolved"
    if (isTRUE(ECI >= thetaC)) {
      label <- "Conflicting"
    } else if (isTRUE(P_final["T"] >= thetaP) && isTRUE(margin >= thetaM) && isTRUE(DRI >= thetaD)) {
      label <- "Supported"
    } else if (isTRUE(P_final["F"] >= thetaP) && isTRUE(margin >= thetaM) && isTRUE(DRI >= thetaD)) {
      label <- "Refuted"
    } else if (isTRUE(P_final["U"] == max(P_final, na.rm = TRUE)) || isTRUE(DRI < thetaD)) {
      label <- "Unresolved"
    }

    summaries[[length(summaries) + 1]] <- data.frame(
      Scenario = scenario_name,
      ClaimID = safe_chr_scalar(claim_id, "Claim"),
      ClaimText = safe_chr_scalar(claim_text, safe_chr_scalar(claim_id, "Claim")),
      Theme = safe_chr_scalar(theme, "Unspecified"),
      Alpha = safe_scalar(alpha, 0.50),
      Beta = safe_scalar(beta, 0.50),
      P_T = safe_threshold(P_final["T"], 0), P_F = safe_threshold(P_final["F"], 0), P_U = safe_threshold(P_final["U"], 0),
      PreliminaryState = safe_state_name(yhat, "U"),
      VeracityBalance = VB,
      Margin = margin,
      VNI = VNI,
      ECS = ECS,
      TDI = TDI,
      ECI = ECI,
      SID = SID,
      AF = AF,
      DRI_Reduced = DRI_Reduced,
      DRI = DRI,
      Classification = label,
      stringsAsFactors = FALSE
    )

    state_probs[[length(state_probs) + 1]] <- data.frame(
      Scenario = rep(scenario_name, K),
      ClaimID = rep(safe_chr_scalar(claim_id, "Claim"), K),
      ClaimText = rep(safe_chr_scalar(claim_text, safe_chr_scalar(claim_id, "Claim")), K),
      TruthState = TRUTH_STATES,
      Probability = as.numeric(P_final),
      stringsAsFactors = FALSE
    )

    weights_rows[[length(weights_rows) + 1]] <- data.frame(
      Scenario = rep(scenario_name, m),
      ClaimID = rep(safe_chr_scalar(claim_id, "Claim"), m),
      CriterionID = crit_ids,
      CriterionName = criteria$CriterionName,
      w_OBJ = w_obj,
      w_SBJ = sbj,
      w_INT = w_int,
      Diversification = D,
      Entropy = Hcrit,
      stringsAsFactors = FALSE
    )
  }

  summary_df <- bind_rows(summaries)
  state_df <- bind_rows(state_probs)
  criterion_df <- bind_rows(criterion_rows)
  weights_df <- bind_rows(weights_rows)

  # V2 alignment: scenario-relative Decision Readiness Index (RDRI).
  # RDRI is reported for visualization/comparative assessment only and is not
  # used in the classification rule, which remains based on raw DRI.
  if (nrow(summary_df) > 0 && "DRI" %in% names(summary_df)) {
    max_dri <- max(summary_df$DRI, na.rm = TRUE)
    if (is.finite(max_dri) && max_dri > eps) {
      summary_df$RDRI <- clamp01(summary_df$DRI / (max_dri + eps))
    } else {
      summary_df$RDRI <- 0
    }
  } else {
    summary_df$RDRI <- numeric(0)
  }

  if (!is.null(scenario_obj$labels) && nrow(scenario_obj$labels) > 0) {
    summary_df <- summary_df %>% left_join(scenario_obj$labels, by = "ClaimID")
  }

  list(
    scenario = scenario_name,
    settings = scenario_obj$settings,
    criteria = criteria,
    evidence = evidence,
    summary = summary_df,
    state_probabilities = state_df,
    criterion_probabilities = criterion_df,
    weights = weights_df
  )
}

alpha_sweep <- function(scenario_obj, alpha_values, beta = NULL, thresholds = NULL) {
  out <- lapply(alpha_values, function(a) compute_veritas_scenario(scenario_obj, alpha = a, beta = beta, thresholds = thresholds)$summary)
  bind_rows(out)
}

# -----------------------------------------------------------------------------
# UI helpers
# -----------------------------------------------------------------------------

metric_card <- function(icon_name, title, value, subtitle, color = "#1f4e79") {
  div(class = "metric-card", style = paste0("border-top: 4px solid ", color, ";"),
      div(class = "metric-icon", icon(icon_name)),
      div(class = "metric-title", title),
      div(class = "metric-value", value),
      div(class = "metric-subtitle", subtitle))
}

app_css <- "
body { background: #f6f8fb; font-family: 'Inter', 'Segoe UI', Arial, sans-serif; }
.hero { background: linear-gradient(135deg, #0f3a5f 0%, #126c7a 55%, #00a896 100%); color: #ffffff; border-radius: 20px; padding: 22px 26px; box-shadow: 0 10px 28px rgba(16,42,67,.18); }
.app-title { font-weight: 800; letter-spacing: .2px; color: #ffffff; margin-top: 0; }
.app-subtitle { color: #dbeafe; font-size: 14px; margin-top: -4px; }
.hero-icon { font-size: 34px; opacity: .95; margin-right: 10px; }
.status-pill { display:inline-block; padding: 6px 10px; border-radius: 999px; background: rgba(255,255,255,.14); color:#ffffff; font-weight:700; font-size: 12px; margin: 4px 4px 0 0; border: 1px solid rgba(255,255,255,.18); }
.well { background: #ffffff; border: 1px solid #d9e2ec; border-radius: 14px; box-shadow: 0 4px 16px rgba(16,42,67,.06); }
.nav-tabs > li > a { font-weight: 700; }
.metric-card { background: white; border-radius: 18px; padding: 17px; margin-bottom: 14px; box-shadow: 0 6px 20px rgba(16,42,67,.09); min-height: 132px; border: 1px solid #e6eef7; }
.metric-icon { float:right; color: #829ab1; font-size: 25px; }
.metric-title { color: #52606d; font-size: 12px; text-transform: uppercase; font-weight: 800; letter-spacing: .8px; }
.metric-value { color: #102a43; font-size: 27px; font-weight: 850; margin-top: 8px; }
.metric-subtitle { color: #627d98; font-size: 12px; margin-top: 5px; }
.viz-card { background: #ffffff; border: 1px solid #d9e2ec; border-radius: 18px; padding: 16px; margin-bottom: 18px; box-shadow: 0 6px 20px rgba(16,42,67,.07); }
.help-box { background:#f0f4f8; border-left:5px solid #1f4e79; padding:13px 14px; border-radius:10px; color:#334e68; }
.small-muted { color:#dbeafe; font-size:12px; }
.top-control { display:inline-block; margin-top:0px; text-align:left; color:#ffffff; }
.control-label { font-weight: 700; color: #243b53; }
body.dark-mode { background: #0b1120; color: #e5e7eb; }
body.dark-mode .hero { background: linear-gradient(135deg, #020617 0%, #0f172a 55%, #075985 100%); box-shadow: 0 10px 28px rgba(0,0,0,.45); }
body.dark-mode .app-title { color: #f8fafc; }
body.dark-mode .app-subtitle, body.dark-mode .small-muted { color: #bae6fd; }
body.dark-mode .well, body.dark-mode .metric-card, body.dark-mode .viz-card { background: #111827; border-color: #334155; box-shadow: 0 6px 22px rgba(0,0,0,.34); }
body.dark-mode .metric-title, body.dark-mode .metric-subtitle, body.dark-mode .metric-icon { color: #94a3b8; }
body.dark-mode .metric-value { color: #f8fafc; }
body.dark-mode .help-box { background: #0f172a; border-left-color: #38bdf8; color: #dbeafe; }
body.dark-mode .nav-tabs { border-bottom-color:#334155; }
body.dark-mode .nav-tabs > li > a { background:#0f172a; color:#cbd5e1; border-color:#334155; }
body.dark-mode .nav-tabs > li.active > a, body.dark-mode .nav-tabs > li.active > a:focus, body.dark-mode .nav-tabs > li.active > a:hover { background:#1e293b; color:#ffffff; border-color:#475569; }
body.dark-mode .form-control, body.dark-mode .selectize-input, body.dark-mode input, body.dark-mode textarea { background:#0f172a !important; color:#e5e7eb !important; border-color:#475569 !important; }
body.dark-mode .selectize-dropdown { background:#111827 !important; color:#e5e7eb !important; border-color:#475569 !important; }
body.dark-mode .control-label { color:#cbd5e1; }
body.dark-mode .dataTables_wrapper, body.dark-mode table.dataTable, body.dark-mode .dataTables_info, body.dark-mode .dataTables_length, body.dark-mode .dataTables_filter, body.dark-mode .paginate_button { color:#e5e7eb !important; }
body.dark-mode table.dataTable tbody tr { background-color:#111827 !important; }
body.dark-mode table.dataTable tbody td, body.dark-mode table.dataTable thead th { border-color:#334155 !important; }
"


veritas_plot_theme <- function(dark = FALSE, rotate_x = FALSE, legend_bottom = FALSE) {
  th <- theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold", size = 14), axis.title = element_text(face = "bold"))
  if (rotate_x) {
    th <- th + theme(axis.text.x = element_text(angle = 20, hjust = 1))
  }
  if (legend_bottom) {
    th <- th + theme(legend.position = "bottom")
  }
  if (isTRUE(dark)) {
    th <- th + theme(
      plot.background = element_rect(fill = "#111827", color = NA),
      panel.background = element_rect(fill = "#111827", color = NA),
      panel.grid.major = element_line(color = "#334155"),
      panel.grid.minor = element_line(color = "#1f2937"),
      text = element_text(color = "#e5e7eb"),
      axis.text = element_text(color = "#cbd5e1"),
      axis.title = element_text(color = "#e5e7eb"),
      plot.title = element_text(face = "bold", color = "#f8fafc"),
      strip.background = element_rect(fill = "#1e293b", color = NA),
      strip.text = element_text(color = "#e5e7eb"),
      legend.background = element_rect(fill = "#111827", color = NA),
      legend.key = element_rect(fill = "#111827", color = NA),
      legend.text = element_text(color = "#e5e7eb"),
      legend.title = element_text(color = "#e5e7eb")
    )
  }
  th
}

ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$head(
    tags$style(HTML(app_css)),
    tags$script(HTML("Shiny.addCustomMessageHandler('toggleDarkMode', function(enabled) { $('body').toggleClass('dark-mode', enabled); });"))
  ),
  br(),
  div(class = "hero",
    fluidRow(
      column(8,
        h2(class = "app-title", tagList(icon("shield"), " VERITAS-ES-MADM Decision Studio")),
        div(class = "app-subtitle", "Fuzzy entropy-synergy platform for claim verification, evidential diagnostics, and decision-readiness assessment"),
        div(
          span(class = "status-pill", tagList(icon("check-circle"), " V2 engine")),
          span(class = "status-pill", tagList(icon("random"), " α-cut uncertainty")),
          span(class = "status-pill", tagList(icon("signal"), " entropy diagnostics")),
          span(class = "status-pill", tagList(icon("tachometer"), " DRI / RDRI"))
        )
      ),
      column(4, align = "right", br(),
        div(class = "top-control", checkboxInput("darkMode", "Dark mode", value = FALSE)),
        tags$br(),
        tags$span(class = "small-muted", tagList(icon("flask"), " Research computational platform"))
      )
    )
  ),
  br(),
  tabsetPanel(id = "mainTabs",
    tabPanel(title = tagList(icon("dashboard"), "Overview"), br(),
      fluidRow(
        column(3, uiOutput("cardStatus")),
        column(3, uiOutput("cardClaims")),
        column(3, uiOutput("cardDRI")),
        column(3, uiOutput("cardConflicts"))
      ),
      fluidRow(
        column(6, div(class="viz-card", plotOutput("plotClassDistribution", height="330px"))),
        column(6, div(class="viz-card", plotOutput("plotScenarioDRI", height="330px")))
      ),
      div(class="viz-card", DTOutput("tableSummaryOverview"))
    ),

    tabPanel(title = tagList(icon("upload"), "Data Import"), br(),
      sidebarLayout(
        sidebarPanel(width = 3,
          fileInput("files", "Upload VERITAS Excel scenario workbooks", multiple = TRUE, accept = c(".xlsx")),
          actionButton("run", "Run computation", icon = icon("play"), class = "btn-primary"),
          hr(),
          numericInput("alpha", "Reference alpha", value = 0.50, min = 0, max = 1, step = 0.05),
          numericInput("beta", "Objective-subjective beta", value = 0.50, min = 0, max = 1, step = 0.05),
          hr(),
          numericInput("thetaP", "thetaP: probability", value = 0.50, min = 0, max = 1, step = 0.05),
          numericInput("thetaM", "thetaM: margin", value = 0.10, min = 0, max = 1, step = 0.05),
          numericInput("thetaD", "thetaD: DRI", value = 0.015, min = 0, max = 1, step = 0.005),
          numericInput("thetaC", "thetaC: conflict", value = 0.60, min = 0, max = 1, step = 0.05)
        ),
        mainPanel(width = 9,
          div(class="help-box",
              strong(tagList(icon("info-circle"), " Input format: ")),
              "Each workbook must contain Settings, Criteria, and EvidenceScores sheets. Optional sheets: ExpertWeights, SourcePairs, Labels."),
          br(),
          tabsetPanel(
            tabPanel("Loaded scenarios", br(), DTOutput("tableLoadedScenarios")),
            tabPanel("Criteria", br(), DTOutput("tableCriteria")),
            tabPanel("Evidence scores", br(), DTOutput("tableEvidence"))
          )
        )
      )
    ),

    tabPanel(title = tagList(icon("table"), "Scenario Results"), br(),
      fluidRow(
        column(3, selectInput("scenarioSelect", "Scenario", choices = character(0))),
        column(3, selectInput("claimSelect", "Claim", choices = character(0))),
        column(6, div(class="help-box", "Use this panel to inspect claim-level probabilities, diagnostics, and criterion weights."))
      ),
      fluidRow(
        column(6, div(class="viz-card", plotOutput("plotTruthProb", height="330px"))),
        column(6, div(class="viz-card", plotOutput("plotDiagnostics", height="330px")))
      ),
      tabsetPanel(
        tabPanel("Summary", br(), DTOutput("tableSummary")),
        tabPanel("Truth probabilities", br(), DTOutput("tableStateProb")),
        tabPanel("Criterion weights", br(), DTOutput("tableWeights")),
        tabPanel("Criterion probabilities", br(), DTOutput("tableCriterionProb"))
      )
    ),

    tabPanel(title = tagList(icon("bar-chart"), "Diagnostics & Visual Analytics"), br(),
      fluidRow(
        column(6, div(class="viz-card", plotOutput("plotDRIClaims", height="380px"))),
        column(6, div(class="viz-card", plotOutput("plotConflictVsDRI", height="380px")))
      ),
      fluidRow(
        column(12, div(class="viz-card", plotOutput("plotDiagnosticHeatmap", height="500px")))
      )
    ),

    tabPanel(title = tagList(icon("line-chart"), "Alpha Robustness"), br(),
      sidebarLayout(
        sidebarPanel(width = 3,
          selectInput("alphaScenario", "Scenario", choices = character(0)),
          textInput("alphaGrid", "Alpha grid", value = "0,0.25,0.5,0.75,1"),
          actionButton("runAlpha", "Run alpha sweep", icon = icon("refresh"), class = "btn-primary")
        ),
        mainPanel(width = 9,
          div(class="viz-card", plotOutput("plotAlphaDRI", height="360px")),
          div(class="viz-card", DTOutput("tableAlphaSweep"))
        )
      )
    ),

    tabPanel(title = tagList(icon("download"), "Export"), br(),
      fluidRow(
        column(4, div(class="viz-card",
          h4("Export results"),
          p("Download all scenario summaries, truth-state probabilities, criterion weights, and criterion-conditioned probabilities."),
          downloadButton("downloadResults", "Download VERITAS results (.xlsx)", class = "btn-success")
        )),
        column(8, div(class="help-box",
          strong(tagList(icon("check-square-o"), " Recommended workflow: ")),
          "1) upload one or more scenario workbooks; 2) run computation; 3) inspect probability and diagnostic plots; 4) export results; 5) verify output workbook against expected case-study behavior."
        ))
      )
    )
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(scenarios = list(), results = list(), alpha = NULL)

  observeEvent(input$darkMode, {
    session$sendCustomMessage("toggleDarkMode", isTRUE(input$darkMode))
  }, ignoreNULL = FALSE)

  thresholds <- reactive(list(
    thetaP = safe_threshold(input$thetaP, 0.50),
    thetaM = safe_threshold(input$thetaM, 0.10),
    thetaD = safe_threshold(input$thetaD, 0.015),
    thetaC = safe_threshold(input$thetaC, 0.60)
  ))

  observeEvent(input$run, {
    req(input$files)
    withProgress(message = "Running VERITAS-ES-MADM scenarios...", value = 0, {
      scen_list <- list()
      res_list <- list()
      n <- nrow(input$files)
      for (idx in seq_len(n)) {
        incProgress(1 / n, detail = basename(input$files$name[idx]))
        obj <- read_veritas_workbook(input$files$datapath[idx])
        obj$scenario <- get_setting_chr(read_settings(input$files$datapath[idx]), "ScenarioName", tools::file_path_sans_ext(input$files$name[idx]))
        scen_list[[obj$scenario]] <- obj
        res_list[[obj$scenario]] <- compute_veritas_scenario(obj, alpha = safe_threshold(input$alpha, 0.50), beta = safe_threshold(input$beta, 0.50), thresholds = thresholds())
      }
      rv$scenarios <- scen_list
      rv$results <- res_list
    })

    scen_names <- names(rv$results)
    updateSelectInput(session, "scenarioSelect", choices = scen_names, selected = scen_names[1])
    updateSelectInput(session, "alphaScenario", choices = scen_names, selected = scen_names[1])
  })

  observeEvent(input$scenarioSelect, {
    req(rv$results[[input$scenarioSelect]])
    claims <- rv$results[[input$scenarioSelect]]$summary$ClaimID
    updateSelectInput(session, "claimSelect", choices = claims, selected = claims[1])
  })

  all_summary <- reactive({
    req(length(rv$results) > 0)
    bind_rows(lapply(rv$results, function(x) x$summary))
  })

  selected_result <- reactive({
    req(input$scenarioSelect, rv$results[[input$scenarioSelect]])
    rv$results[[input$scenarioSelect]]
  })

  selected_claim_summary <- reactive({
    res <- selected_result()
    req(input$claimSelect)
    res$summary %>% filter(ClaimID == input$claimSelect)
  })

  output$cardStatus <- renderUI({
    n <- length(rv$results)
    metric_card("check-circle", "Status", ifelse(n > 0, "Computed", "Awaiting input"), paste(n, "scenario(s) loaded"), "#1f7a4d")
  })
  output$cardClaims <- renderUI({
    val <- if (length(rv$results) == 0) 0 else nrow(all_summary())
    metric_card("file-text", "Claims", val, "Total evaluated claims", "#1f4e79")
  })
  output$cardDRI <- renderUI({
    val <- if (length(rv$results) == 0) "--" else sprintf("%.4f / %.2f", mean(all_summary()$DRI, na.rm = TRUE), mean(all_summary()$RDRI, na.rm = TRUE))
    metric_card("tachometer", "Mean DRI / RDRI", val, "Raw and scenario-relative readiness", "#6f42c1")
  })
  output$cardConflicts <- renderUI({
    val <- if (length(rv$results) == 0) 0 else sum(all_summary()$Classification == "Conflicting", na.rm = TRUE)
    metric_card("exclamation-triangle", "Conflicts", val, "Claims flagged as conflicting", "#b7791f")
  })

  output$tableLoadedScenarios <- renderDT({
    if (length(rv$scenarios) == 0) return(datatable(data.frame(Message = "No scenarios loaded yet.")))
    datatable(data.frame(Scenario = names(rv$scenarios), Claims = sapply(rv$scenarios, function(s) length(unique(s$evidence$ClaimID))), Criteria = sapply(rv$scenarios, function(s) nrow(s$criteria))), options = list(pageLength = 10))
  })

  output$tableCriteria <- renderDT({
    if (length(rv$scenarios) == 0) return(datatable(data.frame(Message = "Load scenarios first.")))
    datatable(bind_rows(lapply(rv$scenarios, function(s) data.frame(Scenario = s$scenario, s$criteria))), options = list(scrollX = TRUE, pageLength = 10))
  })

  output$tableEvidence <- renderDT({
    if (length(rv$scenarios) == 0) return(datatable(data.frame(Message = "Load scenarios first.")))
    datatable(bind_rows(lapply(rv$scenarios, function(s) data.frame(Scenario = s$scenario, s$evidence))), options = list(scrollX = TRUE, pageLength = 15))
  })

  output$tableSummaryOverview <- renderDT({
    req(length(rv$results) > 0)
    datatable(all_summary(), options = list(scrollX = TRUE, pageLength = 10)) %>% formatRound(columns = c("P_T","P_F","P_U","VeracityBalance","Margin","VNI","ECS","TDI","ECI","SID","AF","DRI_Reduced","DRI","RDRI"), digits = 3)
  })

  output$tableSummary <- renderDT({
    req(selected_result())
    datatable(selected_result()$summary, options = list(scrollX = TRUE, pageLength = 10)) %>% formatRound(columns = c("P_T","P_F","P_U","VNI","ECS","TDI","ECI","SID","AF","DRI_Reduced","DRI","RDRI"), digits = 3)
  })

  output$tableStateProb <- renderDT({
    req(selected_result())
    datatable(selected_result()$state_probabilities, options = list(scrollX = TRUE, pageLength = 15)) %>% formatRound("Probability", digits = 3)
  })

  output$tableWeights <- renderDT({
    req(selected_result())
    datatable(selected_result()$weights, options = list(scrollX = TRUE, pageLength = 15)) %>% formatRound(columns = c("w_OBJ", "w_SBJ", "w_INT", "Diversification", "Entropy"), digits = 3)
  })

  output$tableCriterionProb <- renderDT({
    req(selected_result())
    datatable(selected_result()$criterion_probabilities, options = list(scrollX = TRUE, pageLength = 15)) %>% formatRound(columns = c("p_T", "p_F", "p_U", "Entropy", "Diversification"), digits = 3)
  })

  output$plotClassDistribution <- renderPlot({
    req(length(rv$results) > 0)
    df <- all_summary() %>% count(Scenario, Classification)
    ggplot(df, aes(x = Classification, y = n, fill = Classification)) +
      geom_col(width = 0.65, show.legend = FALSE) +
      geom_text(aes(label = n), vjust = -0.35, size = 5, fontface = "bold") +
      facet_wrap(~Scenario) +
      labs(title = "Classification distribution", x = NULL, y = "Claims") +
      veritas_plot_theme(isTRUE(input$darkMode))
  })

  output$plotScenarioDRI <- renderPlot({
    req(length(rv$results) > 0)
    df <- all_summary()
    ggplot(df, aes(x = Scenario, y = RDRI)) +
      geom_boxplot(outlier.shape = NA) +
      geom_jitter(aes(color = Classification), width = 0.12, height = 0, alpha = 0.85, size = 2.8) +
      coord_cartesian(ylim = c(0, 1)) +
      labs(title = "Scenario-relative decision-readiness", x = NULL, y = "RDRI") +
      veritas_plot_theme(isTRUE(input$darkMode), rotate_x = TRUE, legend_bottom = TRUE)
  })

  output$plotTruthProb <- renderPlot({
    req(selected_result(), input$claimSelect)
    df <- selected_result()$state_probabilities %>% filter(ClaimID == input$claimSelect)
    ggplot(df, aes(x = TruthState, y = Probability, fill = TruthState)) +
      geom_col(width = 0.65, show.legend = FALSE) +
      geom_text(aes(label = sprintf("%.3f", Probability)), vjust = -0.35, size = 5, fontface = "bold") +
      coord_cartesian(ylim = c(0, 1)) +
      labs(title = paste("Truth-state probabilities:", input$claimSelect), x = NULL, y = "Probability") +
      veritas_plot_theme(isTRUE(input$darkMode))
  })

  output$plotDiagnostics <- renderPlot({
    row <- selected_claim_summary()
    req(nrow(row) == 1)
    df <- data.frame(Index = c("VNI", "ECS", "TDI", "ECI", "SID", "AF", "DRI", "RDRI"), Value = as.numeric(row[1, c("VNI", "ECS", "TDI", "ECI", "SID", "AF", "DRI", "RDRI")]))
    ggplot(df, aes(x = Index, y = Value)) +
      geom_col(width = 0.65) + geom_text(aes(label = sprintf("%.3f", Value)), vjust = -0.35, size = 4.8, fontface = "bold") +
      coord_cartesian(ylim = c(0, 1)) +
      labs(title = paste("Diagnostic profile:", input$claimSelect), x = NULL, y = "Index value") +
      veritas_plot_theme(isTRUE(input$darkMode))
  })

  output$plotDRIClaims <- renderPlot({
    req(length(rv$results) > 0)
    df <- all_summary() %>%
      mutate(ReadinessLabel = sprintf("DRI=%.4f | RDRI=%.2f", DRI, RDRI))
    ggplot(df, aes(x = reorder(ClaimID, RDRI), y = RDRI, fill = Classification)) +
      geom_col(width = 0.72) +
      geom_text(aes(label = ReadinessLabel), hjust = -0.06, size = 4.3, fontface = "bold", show.legend = FALSE) +
      coord_flip(ylim = c(0, 1.08), clip = "off") +
      facet_wrap(~Scenario, scales = "free_y") +
      labs(title = "Claim-level Decision Readiness Index", subtitle = "Bars show scenario-relative RDRI; labels report raw DRI and RDRI.", x = NULL, y = "RDRI") +
      veritas_plot_theme(isTRUE(input$darkMode), legend_bottom = TRUE)
  })

  output$plotConflictVsDRI <- renderPlot({
    req(length(rv$results) > 0)
    df <- all_summary() %>%
      mutate(PointLabel = sprintf("%s\nECI=%.3f | DRI=%.4f | RDRI=%.2f", ClaimID, ECI, DRI, RDRI))
    ggplot(df, aes(x = ECI, y = RDRI, label = PointLabel, color = Classification)) +
      geom_point(size = 4) +
      geom_text(vjust = -0.7, size = 3.9, fontface = "bold", lineheight = 0.95, show.legend = FALSE) +
      coord_cartesian(xlim = c(0, 1), ylim = c(0, 1.08), clip = "off") +
      facet_wrap(~Scenario) +
      labs(title = "Conflict vs decision-readiness", subtitle = "Y-axis uses RDRI for clear visualization; labels include raw DRI.", x = "Evidence Conflict Index", y = "RDRI") +
      veritas_plot_theme(isTRUE(input$darkMode), legend_bottom = TRUE)
  })

  output$plotDiagnosticHeatmap <- renderPlot({
    req(length(rv$results) > 0)
    df <- all_summary() %>% select(Scenario, ClaimID, AF, DRI, RDRI, ECI, ECS, SID, TDI, VNI)
    idx_cols <- c("AF", "DRI", "RDRI", "ECI", "ECS", "SID", "TDI", "VNI")
    df_long <- do.call(rbind, lapply(idx_cols, function(idx) {
      data.frame(
        Scenario = df$Scenario,
        ClaimID = df$ClaimID,
        Index = idx,
        Value = df[[idx]],
        stringsAsFactors = FALSE
      )
    }))
    df_long <- df_long %>%
      mutate(ValueLabel = sprintf("%.3f", Value),
             LabelColor = ifelse(Value >= 0.55, "white", "black"))
    ggplot(df_long, aes(x = Index, y = ClaimID, fill = Value)) +
      geom_tile(color = "white") +
      geom_text(aes(label = ValueLabel, color = LabelColor), size = 4.6, fontface = "bold", show.legend = FALSE) +
      facet_wrap(~Scenario, scales = "free_y") +
      scale_fill_gradient(limits = c(0, 1), low = "#f7fbff", high = "#1f4e79") +
      scale_color_identity() +
      labs(title = "Diagnostic heatmap", x = NULL, y = NULL, fill = "Value") +
      veritas_plot_theme(isTRUE(input$darkMode))
  })

  observeEvent(input$runAlpha, {
    req(input$alphaScenario, rv$scenarios[[input$alphaScenario]])
    vals <- as.numeric(trimws(unlist(strsplit(input$alphaGrid, ","))))
    vals <- vals[is.finite(vals) & vals >= 0 & vals <= 1]
    if (length(vals) == 0) {
      showNotification("Provide at least one valid alpha value in [0,1].", type = "error")
      return(NULL)
    }
    rv$alpha <- alpha_sweep(rv$scenarios[[input$alphaScenario]], vals, beta = safe_threshold(input$beta, 0.50), thresholds = thresholds())
  })

  output$plotAlphaDRI <- renderPlot({
    req(rv$alpha)
    ggplot(rv$alpha, aes(x = Alpha, y = RDRI, color = ClaimID)) +
      geom_line(size = 1.15) + geom_point(size = 3) +
      coord_cartesian(ylim = c(0, 1.05)) +
      labs(title = "Alpha-level robustness: RDRI trajectories", subtitle = "RDRI is used for visual comparison; raw DRI remains available in the table/export.", x = expression(alpha), y = "RDRI") +
      veritas_plot_theme(isTRUE(input$darkMode), legend_bottom = TRUE)
  })

  output$tableAlphaSweep <- renderDT({
    req(rv$alpha)
    datatable(rv$alpha, options = list(scrollX = TRUE, pageLength = 10)) %>% formatRound(columns = c("P_T", "P_F", "P_U", "VNI", "ECS", "TDI", "ECI", "SID", "AF", "DRI_Reduced", "DRI", "RDRI"), digits = 3)
  })

  output$downloadResults <- downloadHandler(
    filename = function() paste0("VERITAS_ES_MADM_results_", format(Sys.Date(), "%Y%m%d"), ".xlsx"),
    content = function(file) {
      req(length(rv$results) > 0)
      export_list <- list(
        Summary = bind_rows(lapply(rv$results, function(x) x$summary)),
        TruthStateProbabilities = bind_rows(lapply(rv$results, function(x) x$state_probabilities)),
        CriterionWeights = bind_rows(lapply(rv$results, function(x) x$weights)),
        CriterionProbabilities = bind_rows(lapply(rv$results, function(x) x$criterion_probabilities)),
        Criteria = bind_rows(lapply(rv$results, function(x) data.frame(Scenario = x$scenario, x$criteria))),
        EvidenceScores = bind_rows(lapply(rv$results, function(x) data.frame(Scenario = x$scenario, x$evidence)))
      )
      if (!is.null(rv$alpha)) export_list$AlphaSweep <- rv$alpha
      writexl::write_xlsx(export_list, path = file)
    }
  )
}

shinyApp(ui = ui, server = server)
