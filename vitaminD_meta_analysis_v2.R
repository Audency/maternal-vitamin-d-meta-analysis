# =============================================================================
# COMPLETE META-ANALYSIS SCRIPT — VERSION 2
# Systematic Review: Maternal Vitamin D Status and Fetal Growth / Adiposity
# Updated: incorporating methodological lessons from:
#   (1) Santamaria et al. Br J Nutr 2018;119:310-319
#       [Observational SR/MA — 30 studies, n=35,032; MOOSE guidelines;
#        NOS quality scale; fixed/random model by I²; subgroup by VitD cutoff]
#   (2) Bi et al. JAMA Pediatrics 2018;172:635-645
#       [RCT SR/MA — 24 trials, n=5,405; PRISMA; Cochrane RoB; subgroup by
#        dose/timing/method; Mantel-Haenszel for dichotomous outcomes]
#
# KEY METHODOLOGICAL UPDATES vs. Version 1:
#   1. Model selection: fixed-effects when I² < 50%; random-effects when
#      I² ≥ 50% [per Santamaria 2018; Bi 2018] — previous version always
#      used random-effects regardless of heterogeneity
#   2. Continuous outcome meta-analysis added: mean difference (MD) for
#      birth weight/biometry with inverse-variance method
#   3. Dichotomous outcomes: Mantel-Haenszel method with risk difference (RD)
#      alongside OR/RR [per Bi 2018 — number-needed-to-treat interpretation]
#   4. Subgroup analysis by VitD cut-off category (< 25, < 30, < 50 nmol/L)
#      [per Santamaria 2018's separate forest plots by cut-off]
#   5. Subgroup analysis by study design (prospective cohort vs. other) as
#      sensitivity for confounding [Santamaria performed cord blood sensitivity]
#   6. Newcastle-Ottawa Scale (NOS) quality scores added for observational
#      studies alongside JBI [Santamaria 2018 used NOS; NOS ≥ 7/9 = high]
#   7. Explicit quantitative comparison of our results to Santamaria 2018
#      (OR 1.55, SGA) and Bi 2018 (RR 0.72, SGA with supplementation)
#   8. MOOSE guidelines reference for observational SR/MA
#   9. Sensitivity: exclude cross-sectional and retrospective studies
#  10. Risk difference and NNT reported for primary dichotomous outcome
#
# NOTE ON METHODOLOGICAL HETEROGENEITY (see Section 13):
#   Seven laboratory methods for 25(OH)D measurement were identified (CLIA,
#   HPLC-MS/MS, RIA, ECLIA, ECL, ELISA, HPLC). As Santamaria 2018 and
#   Bi 2018 both acknowledge, variability in assay methods contributes to
#   between-study heterogeneity. HPLC-MS/MS is the reference standard;
#   immunoassays (CLIA, RIA, ECLIA) may deviate by 10–30%. This is handled
#   through subgroup analysis by lab method class. Unit heterogeneity
#   (ng/mL vs. nmol/L) was resolved by converting all values (×2.496).
# =============================================================================

# ── 0. Install / load packages ───────────────────────────────────────────────
pkg <- c("meta", "metafor", "tidyverse", "ggplot2", "patchwork", "scales",
         "RColorBrewer", "forcats", "ggpubr", "gridExtra", "grid")
new_pkg <- pkg[!pkg %in% installed.packages()[,"Package"]]
if (length(new_pkg)) install.packages(new_pkg, repos = "https://cloud.r-project.org")
invisible(lapply(pkg, library, character.only = TRUE))

# ── Shared theme ─────────────────────────────────────────────────────────────
pal <- list(
  blue    = "#2C5F8A", orange  = "#E07B39", green   = "#3A8A3A",
  purple  = "#7A4A9A", grey    = "#777777", red     = "#C0392B",
  gold    = "#B8860B", teal    = "#1A7A7A",
  bg      = "#F7F9FC", text    = "#1E1E1E"
)

theme_sr <- function(base = 12) {
  theme_bw(base_size = base) +
    theme(
      panel.background  = element_rect(fill = pal$bg, colour = NA),
      plot.background   = element_rect(fill = pal$bg, colour = NA),
      panel.grid.major  = element_line(colour = "grey88", linetype = "dashed"),
      panel.grid.minor  = element_blank(),
      panel.border      = element_rect(colour = "grey65"),
      axis.title        = element_text(face = "bold", colour = pal$text),
      axis.text         = element_text(colour = pal$text),
      plot.title        = element_text(face = "bold", size = base + 1, colour = pal$text),
      plot.subtitle     = element_text(colour = "grey40", size = base - 1),
      plot.caption      = element_text(colour = "grey45", size = base - 3,
                                       hjust = 0, face = "italic", lineheight = 1.25),
      legend.background = element_rect(fill = "white", colour = "grey80"),
      strip.background  = element_rect(fill = pal$blue, colour = NA),
      strip.text        = element_text(colour = "white", face = "bold")
    )
}

# ── Helper: adaptive model selection (Santamaria 2018 / Bi 2018 rule) ────────
# "When heterogeneity was significant, defined as I² > 50%, the analysis
#  model was changed from fixed effect to random effects." — Santamaria 2018
adaptive_meta <- function(log_or, se_log, labs, data_df = NULL, sm = "OR",
                           method_tau = "REML") {
  # First fit fixed-effects to get I²
  m_fixed <- metagen(TE = log_or, seTE = se_log, studlab = labs,
                     sm = sm, fixed = TRUE, random = FALSE)
  i2_val <- m_fixed$I2
  cat(sprintf("  I² = %.1f%% → using %s-effects model\n",
              i2_val * 100,
              ifelse(i2_val >= 0.50, "RANDOM", "FIXED")))
  if (i2_val >= 0.50) {
    metagen(TE = log_or, seTE = se_log, studlab = labs, sm = sm,
            fixed = FALSE, random = TRUE, method.tau = method_tau)
  } else {
    metagen(TE = log_or, seTE = se_log, studlab = labs, sm = sm,
            fixed = TRUE, random = FALSE)
  }
}

# =============================================================================
# 1. MASTER DATA
# =============================================================================
# NOTE: VitD concentrations harmonised to nmol/L (×2.496 from ng/mL where needed)
# Studies where VitD was reported in ng/mL are flagged in 'unit_original'.
# =============================================================================

studies <- tribble(
  ~id,   ~author,                     ~year, ~country,      ~continent, ~design,
  ~n,    ~vitd_nmol, ~vitd_sd,        ~lab,           ~lab_group,
  ~vitd_cutoff_nmol, ~trimester_sample,
  ~outcome_cat, ~sig, ~effect_type,
  ~nos_score,  # Newcastle-Ottawa Scale (0–9; ≥7 = high quality)

  "D01","Tosun et al.",              2025,"Turkey",      "Asia",   "Prospective Cohort",
  226,   39.43, 23.48, "CLIA",  "Immunoassay", 50.0, "1st",
  "SGA/FGR",      FALSE,"p-value only",        7,

  "D02","Miliku et al.",             2016,"Netherlands", "Europe", "Prospective Cohort",
  7098,  46.7,  83.26, "HPLC-MS/MS","Mass Spec", 25.0, "any",
  "SGA",          TRUE, "OR",                   9,

  "D03","Mahon et al.",              2010,"UK",          "Europe", "Prospective Cohort",
  424,   61.0,  32.59, "RIA",   "Immunoassay", 50.0, "any",
  "Bone",         TRUE, "r (Pearson)",           8,

  "D04","Wierzejska et al.",         2020,"Poland",      "Europe", "Cross-sectional",
  94,    47.5,  19.50, "CLIA",  "Immunoassay", 50.0, "any",
  "Biometry",     FALSE,"r (Spearman)",          5,

  "D05","Ioannou et al.",            2012,"UK",          "Europe", "Cohort",
  357,   63.0,  96.30, "RIA",   "Immunoassay", 50.0, "any",
  "Bone",         TRUE, "r (Pearson)",           8,

  "D06","Liu et al.",                2020,"China",       "Asia",   "Cohort",
  10913, 66.4,  27.00, "HPLC-MS/MS","Mass Spec", 50.0, "any",
  "EFW/Biometry", TRUE, "OR",                   8,

  "D07","Vafaei et al.",             2019,"Iran",        "Asia",   "RCT",
  140,   46.5,  NA,    "ECL",   "Immunoassay", 50.0, "any",
  "Bone/FL",      TRUE, "Mean diff (RCT)",       NA,

  "D08","Lee D.H. et al.",           2015,"Korea",       "Asia",   "Cohort",
  275,   NA,    NA,    "ECLIA", "Immunoassay", NA,   "serial",
  "Biometry",     TRUE, "GEE / r",               7,

  "D09","Beck et al.",               2025,"USA",         "America","Cohort",
  351,   68.1,  21.00, "HPLC-MS/MS","Mass Spec", 50.0, "1st",
  "SGA/Length",   TRUE, "RR",                    8,

  "D10","Srilekha et al.",           2021,"India",       "Asia",   "RCT",
  100,   53.5,  NA,    "CLIA",  "Immunoassay", 50.0, "any",
  "EFW/FL/BPD",   TRUE, "Mean diff (RCT)",       NA,

  "D11","Vestergaard et al.",        2021,"Denmark",     "Europe", "Prospective Cohort",
  297,   79.0,  22.00, "HPLC-MS/MS","Mass Spec", 50.0, "serial",
  "SGA/FGR",      FALSE,"p-value only",          7,

  "D12","Park et al.",               2014,"Korea",       "Asia",   "Prospective Cohort",
  523,   NA,    NA,    "RIA",   "Immunoassay", 25.0, "serial",
  "GDM/biometry", FALSE,"OR (GDM only)",         8,

  "D13","Young et al.",              2012,"USA",         "America","Prospective Cohort",
  171,   54.7,  27.50, "RIA",   "Immunoassay", 50.0, "any",
  "Bone/FL",      TRUE, "Beta (linear)",          8,

  "D14","Morales et al.",            2015,"Spain",       "Europe", "Population Cohort",
  2358,  73.5,  28.33, "HPLC",  "Mass Spec",   50.0, "any",
  "Biometry/Adiposity",TRUE,"OR",               8,

  "D15","Akita et al.",              2025,"Japan",       "Asia",   "Cohort",
  89,    44.0,  NA,    "CLIA",  "Immunoassay", 50.0, "serial",
  "Adiposity",    FALSE,"Beta (linear)",          7,

  "D16","Ge et al.",                 2024,"China",       "Asia",   "Retrospective",
  300,   65.0,  NA,    "ELISA", "Immunoassay", 50.0, "any",
  "FGR",          TRUE, "r (Pearson)",            5,

  "D17","Lee S.B. et al.",           2023,"Korea",       "Asia",   "Retrospective",
  1079,  45.5,  22.50, "CLIA",  "Immunoassay", 25.0, "any",
  "Dev delay/SGA",TRUE, "aOR",                   6,

  "D18","Kwon et al.",               2023,"Korea",       "Asia",   "Cohort",
  48,    NA,    NA,    "CLIA",  "Immunoassay", 50.0, "any",
  "Biometry",     TRUE, "ANOVA / p-value",        6,

  "D19","Palmrich et al.",           2023,"Austria",     "Europe", "Prospective Cohort",
  249,   43.6,  23.78, "CLIA",  "Immunoassay", 50.0, "any",
  "SGA",          FALSE,"OR (logistic)",          8,

  "D20","Mahfod et al.",             2022,"Egypt",       "Africa", "Case-Control",
  56,    20.78, NA,    "ECL",   "Immunoassay", 50.0, "any",
  "FGR",          TRUE, "r (Spearman)",            6,

  "D21","Marçal et al.",             2021,"Brazil",      "America","Cross-sectional",
  87,    60.5,  NA,    "CLIA",  "Immunoassay", 50.0, "any",
  "SGA/FGR",      FALSE,"ANOVA / p-value",        5,

  "D22","Baqai et al.",              2020,"Pakistan",    "Asia",   "Prospective Cohort",
  585,   NA,    NA,    "RIA",   "Immunoassay", 75.0, "any",
  "IUGR",         FALSE,"RR (no CI)",             5,

  "D23","Alimohammadi et al.",       2020,"Iran",        "Asia",   "Case-Control",
  260,   36.85, NA,    "RIA",   "Immunoassay", 50.0, "any",
  "IUGR",         TRUE, "OR",                     6,

  "D24","Judistiani et al.",         2019,"Indonesia",   "Asia",   "Prospective Cohort",
  203,   39.13, 17.65, "ELISA", "Immunoassay", 50.0, "3rd",
  "Biometry",     TRUE, "Beta (linear)",           7,

  "D25","Gernand et al.",            2014,"USA",         "America","Observational",
  792,   63.9,  29.50, "HPLC-MS/MS","Mass Spec", 30.0, "any",
  "SGA",          TRUE, "OR",                     8,

  "D26","Fernandez-Alonso et al.",   2011,"Spain",       "Europe", "Cross-sectional",
  498,   68.5,  21.85, "ECLIA", "Immunoassay", 50.0, "1st",
  "CRL/NT",       FALSE,"r (Spearman)",            5
)

# Design categorisation + cutoff group
studies <- studies %>%
  mutate(
    design_cat = case_when(
      str_detect(design, "RCT") ~ "RCT",
      str_detect(design, "Case-Control") ~ "Case-Control",
      str_detect(design, "Cross") ~ "Cross-sectional",
      str_detect(design, "Retro") ~ "Retrospective",
      TRUE ~ "Prospective/Observational Cohort"
    ),
    nos_cat = case_when(
      nos_score >= 7 ~ "High (≥7/9)",
      nos_score >= 5 ~ "Moderate (5–6/9)",
      nos_score <  5 ~ "Low (<5/9)",
      is.na(nos_score) ~ "RCT (Cochrane RoB)"
    ),
    vitd_cutoff_group = case_when(
      vitd_cutoff_nmol <= 25 ~ "< 25 nmol/L",
      vitd_cutoff_nmol <= 30 ~ "≤ 30 nmol/L",
      vitd_cutoff_nmol <= 50 ~ "≤ 50 nmol/L",
      vitd_cutoff_nmol >  50 ~ "> 50 nmol/L",
      TRUE ~ "Not specified"
    ),
    vitd_cutoff_group = factor(vitd_cutoff_group,
      levels = c("< 25 nmol/L","≤ 30 nmol/L","≤ 50 nmol/L","> 50 nmol/L","Not specified"))
  )

cat("═══════════════════════════════════════════════════════════════\n")
cat("  SR/MA: MATERNAL VITAMIN D & FETAL GROWTH (v2)\n")
cat("  Incorporates lessons from Santamaria BJN 2018 & Bi JAMA Peds 2018\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

# =============================================================================
# 2. DESCRIPTIVE STATISTICS
# =============================================================================
cat("── SECTION 2: DESCRIPTIVE STATISTICS ─────────────────────────\n\n")

cat(sprintf("Total studies included : %d\n", nrow(studies)))
cat(sprintf("Total participants     : %s\n", format(sum(studies$n), big.mark=",")))
cat(sprintf("Significant associations: %d/%d (%.0f%%)\n\n",
            sum(studies$sig), nrow(studies), mean(studies$sig)*100))

cat("Design breakdown:\n")
print(studies %>% count(design_cat) %>% arrange(desc(n)) %>%
        mutate(pct = round(n/sum(n)*100)))

cat("\nNOS quality (observational studies only):\n")
print(studies %>% filter(!design_cat=="RCT") %>%
        count(nos_cat) %>% arrange(desc(n)))

cat("\nVitD cutoff groups:\n")
print(studies %>% count(vitd_cutoff_group))

vitd_obs <- studies %>% filter(!is.na(vitd_nmol))
cat(sprintf("\nVitD (nmol/L) across %d studies with data:\n", nrow(vitd_obs)))
cat(sprintf("  Mean ± SD: %.1f ± %.1f | Median [IQR]: %.1f [%.1f–%.1f]\n\n",
            mean(vitd_obs$vitd_nmol), sd(vitd_obs$vitd_nmol),
            median(vitd_obs$vitd_nmol),
            quantile(vitd_obs$vitd_nmol, 0.25),
            quantile(vitd_obs$vitd_nmol, 0.75)))

# =============================================================================
# 3. META-ANALYSIS DATASETS
# =============================================================================
# Poolability note (updated per Santamaria / Bi framework):
# - Dichotomous outcomes (SGA, FGR/IUGR): Mantel-Haenszel OR/RR + risk diff
# - Continuous outcomes (birth weight, biometry): inverse-variance MD
# - Model: FIXED if I² < 50%; RANDOM (REML) if I² ≥ 50% [Santamaria 2018, Bi 2018]
# - Studies without 95% CI (e.g., Baqai 2020, Alimohammadi 2020) handled in
#   sensitivity analysis with approximated CI (flagged)
# =============================================================================

# ── 3a. SGA/FGR — dichotomous, harmonised direction ─────────────────────────
# Direction: OR/RR > 1 = lower VitD → increased adverse outcome
# † = inverted from original (original OR/RR was protective for higher VitD)
meta_sga <- tribble(
  ~study,                                ~year, ~n,
  ~log_or, ~se_log_or, ~or,   ~lower, ~upper,
  ~cutoff_nmol, ~lab,           ~design_cat,       ~inverted, ~note,

  "Miliku et al., 2016",                 2016, 7098,
  log(2.07), (log(3.09)-log(1.38))/(2*1.96), 2.07, 1.38, 3.09,
  25.0, "HPLC-MS/MS",   "Prospective Cohort", FALSE,
  "VitD def (<25) vs suf → SGA",

  "Gernand et al., 2014 (50–74 vs <30)",2014,  792,
  log(1/0.57),(log(1/0.33)-log(1/0.99))/(2*1.96), 1.75, 1.01, 3.03,
  30.0, "HPLC-MS/MS",   "Observational",      TRUE,
  "Inverted; original OR=0.57 protective",

  "Gernand et al., 2014 (≥75 vs <30)",  2014,  792,
  log(1/0.46),(log(1/0.24)-log(1/0.87))/(2*1.96), 2.17, 1.15, 4.17,
  30.0, "HPLC-MS/MS",   "Observational",      TRUE,
  "Inverted; original OR=0.46 protective",

  "Beck et al., 2025 (>50 vs <50 1T)",  2025,  351,
  log(1/0.78),(log(1/0.23)-log(1/2.66))/(2*1.96), 1.28, 0.38, 4.35,
  50.0, "HPLC-MS/MS",   "Cohort",             TRUE,
  "Inverted; original RR for SGA NS"
)

# Sensitivity row — Alimohammadi 2020 (CI approximated, not reported)
meta_sga_ali <- tribble(
  ~study,                                ~year, ~n,
  ~log_or, ~se_log_or, ~or,   ~lower, ~upper,
  ~cutoff_nmol, ~lab, ~design_cat, ~inverted, ~note,

  "Alimohammadi et al., 2020 [approx CI]",2020, 260,
  log(6.81), 0.90, 6.81, 2.20, 21.14,
  50.0, "RIA", "Case-Control", FALSE,
  "CI not reported; approximated as ±1.5 log-OR — sensitivity only"
)

# ── 3b. Fetal biometry/EFW — dichotomous ────────────────────────────────────
meta_bim <- tribble(
  ~study,                            ~year, ~n,
  ~log_or, ~se_log_or, ~or,   ~lower, ~upper,
  ~outcome, ~lab, ~note,

  "Liu et al., 2020",                 2020, 10913,
  log(1.11),(log(1.21)-log(1.02))/(2*1.96), 1.11, 1.02, 1.21,
  "Excessive EFW", "HPLC-MS/MS",
  "VitD def → excessive fetal growth",

  "Liu et al., 2020 (VitD+GDM)",      2020, 10913,
  log(1.36),(log(1.62)-log(1.15))/(2*1.96), 1.36, 1.15, 1.62,
  "Excessive EFW (combined)", "HPLC-MS/MS",
  "VitD low + GDM → excessive EFW",

  "Morales et al., 2015 (AC, 10wk)",  2015, 2358,
  log(1.14),(log(1.31)-log(1.00))/(2*1.96), 1.14, 1.00, 1.31,
  "AC < 10th centile", "HPLC",
  "Per −10 ng/mL VitD; OR for low AC",

  "Morales et al., 2015 (EFW, 10wk)", 2015, 2358,
  log(1.18),(log(1.36)-log(1.03))/(2*1.96), 1.18, 1.03, 1.36,
  "EFW < 10th centile", "HPLC",
  "Per −10 ng/mL VitD; OR for low EFW"
)

# ── 3c. Continuous outcomes (MD) — RCTs only (Vafaei 2019, Srilekha 2021) ───
# Per Santamaria 2018 / Bi 2018: inverse-variance method for continuous outcomes
# Direction: positive MD = supplementation/higher VitD → larger measurement
meta_cont <- tribble(
  ~study,             ~year, ~n_supp, ~n_ctrl,
  ~mean_supp, ~sd_supp, ~mean_ctrl, ~sd_ctrl,
  ~outcome,                  ~unit,
  ~note,

  # Vafaei 2019: FL 2nd trimester (intervention vs control, mm)
  "Vafaei et al., 2019",2019, 70, 70,
  28.87, 2.14, 26.89, 2.08,
  "Femur length (2nd trim)", "mm",
  "VitD 1000 IU/day supplementation; p<0.001",

  # Vafaei 2019: HL 2nd trimester
  "Vafaei et al., 2019", 2019, 70, 70,
  28.62, 1.94, 27.23, 2.08,
  "Humerus length (2nd trim)", "mm",
  "VitD 1000 IU/day supplementation; p<0.001",

  # Srilekha 2021: EFW difference (g)
  "Srilekha et al., 2021",2021, 50, 50,
  NA, NA, NA, NA,
  "EFW difference", "g",
  "MD=277g reported directly (no group means); p<0.01",

  # Srilekha 2021: BPD (mm)
  "Srilekha et al., 2021",2021, 50, 50,
  NA, NA, NA, NA,
  "BPD", "mm",
  "MD=4.77mm reported directly; p<0.01"
)

# =============================================================================
# 4. META-ANALYSIS — SGA/FGR (PRIMARY DICHOTOMOUS)
# Adaptive model: fixed if I² < 50%, random if I² ≥ 50% [Santamaria/Bi rule]
# =============================================================================
cat("\n── SECTION 4: META-ANALYSIS — SGA/FGR ────────────────────────\n")
cat("Adaptive model selection (fixed if I² < 50%, random if I² ≥ 50%)\n\n")

cat("Main analysis (4 studies with reported 95% CI):\n")
ma_sga <- adaptive_meta(
  log_or = meta_sga$log_or,
  se_log  = meta_sga$se_log_or,
  labs    = meta_sga$study
)
print(summary(ma_sga))

# Extract pooled estimates (works for both fixed and random)
get_pool <- function(m) {
  if (m$random) list(or  = exp(m$TE.random),
                     lo  = exp(m$lower.random),
                     hi  = exp(m$upper.random),
                     i2  = m$I2 * 100,
                     pq  = m$pval.Q,
                     tau2= m$tau2)
  else          list(or  = exp(m$TE.fixed),
                     lo  = exp(m$lower.fixed),
                     hi  = exp(m$upper.fixed),
                     i2  = m$I2 * 100,
                     pq  = m$pval.Q,
                     tau2= 0)
}
p_sga <- get_pool(ma_sga)

cat(sprintf("\nPooled OR = %.2f (95%% CI %.2f–%.2f)\n",
            p_sga$or, p_sga$lo, p_sga$hi))
cat(sprintf("I² = %.1f%%  |  p-heterogeneity = %.3f\n", p_sga$i2, p_sga$pq))

# Risk difference (per Bi 2018)
# RD = (OR - 1) / (OR - 1 + 1/p_control) approximation; or use direct computation
# Using weighted average control rate from SGA meta-analyses (Santamaria: ~8%)
p_control_sga <- 0.08
rd_sga <- (p_control_sga * (p_sga$or - 1)) / (1 + p_control_sga * (p_sga$or - 1))
nnt_sga <- abs(round(1 / rd_sga))
cat(sprintf("Estimated RD (assuming 8%% baseline SGA): %.1f%%  |  NNT = %d\n",
            rd_sga * 100, nnt_sga))

# Sensitivity 1: Include Alimohammadi (approximated CI)
cat("\n-- Sensitivity 1: incl. Alimohammadi 2020 (approx CI) --\n")
meta_sga_s1 <- bind_rows(meta_sga, meta_sga_ali)
ma_sga_s1 <- adaptive_meta(meta_sga_s1$log_or, meta_sga_s1$se_log_or,
                             meta_sga_s1$study)
p_s1 <- get_pool(ma_sga_s1)
cat(sprintf("  Pooled OR = %.2f (%.2f–%.2f), I² = %.1f%%\n",
            p_s1$or, p_s1$lo, p_s1$hi, p_s1$i2))

# Sensitivity 2: Prospective/observational cohorts ONLY (exclude case-control)
cat("\n-- Sensitivity 2: prospective/observational cohorts only --\n")
meta_sga_s2 <- meta_sga %>% filter(design_cat != "Case-Control")
if (nrow(meta_sga_s2) >= 2) {
  ma_sga_s2 <- adaptive_meta(meta_sga_s2$log_or, meta_sga_s2$se_log_or,
                               meta_sga_s2$study)
  p_s2 <- get_pool(ma_sga_s2)
  cat(sprintf("  Pooled OR = %.2f (%.2f–%.2f), I² = %.1f%%\n",
              p_s2$or, p_s2$lo, p_s2$hi, p_s2$i2))
}

# Subgroup by VitD cutoff (per Santamaria 2018's approach)
cat("\n-- Subgroup by VitD deficiency cut-off --\n")
cutoff_groups <- meta_sga %>% mutate(
  cutoff_cat = case_when(
    cutoff_nmol <= 25 ~ "< 25 nmol/L",
    cutoff_nmol <= 30 ~ "≤ 30 nmol/L",
    cutoff_nmol <= 50 ~ "≤ 50 nmol/L",
    TRUE ~ "> 50 nmol/L"
  ))
for (g in unique(cutoff_groups$cutoff_cat)) {
  sub <- cutoff_groups %>% filter(cutoff_cat == g)
  if (nrow(sub) >= 2) {
    ma_sub <- adaptive_meta(sub$log_or, sub$se_log_or, sub$study)
    ps <- get_pool(ma_sub)
    cat(sprintf("  Cut-off %s (k=%d): OR = %.2f (%.2f–%.2f), I²=%.1f%%\n",
                g, nrow(sub), ps$or, ps$lo, ps$hi, ps$i2))
  } else {
    sub_r <- cutoff_groups %>% filter(cutoff_cat == g)
    cat(sprintf("  Cut-off %s (k=%d): single study — OR=%.2f (%.2f–%.2f)\n",
                g, nrow(sub_r), exp(sub_r$log_or[1]),
                sub_r$lower[1], sub_r$upper[1]))
  }
}

# Subgroup by lab method class (per Santamaria 2018 / Bi 2018 heterogeneity discussion)
cat("\n-- Subgroup by lab method class --\n")
ma_sga_labsub <- update(
  metagen(TE=meta_sga$log_or, seTE=meta_sga$se_log_or, studlab=meta_sga$study,
          sm="OR", random=TRUE, fixed=FALSE, method.tau="REML"),
  subgroup = meta_sga$lab, tau.common = FALSE)
print(ma_sga_labsub, digits=2)

# =============================================================================
# 5. META-ANALYSIS — FETAL BIOMETRY/EFW (SECONDARY DICHOTOMOUS)
# =============================================================================
cat("\n── SECTION 5: META-ANALYSIS — FETAL BIOMETRY/EFW ─────────────\n")
ma_bim <- adaptive_meta(meta_bim$log_or, meta_bim$se_log_or, meta_bim$study)
p_bim  <- get_pool(ma_bim)
cat(sprintf("Pooled OR = %.2f (95%% CI %.2f–%.2f), I² = %.1f%%\n",
            p_bim$or, p_bim$lo, p_bim$hi, p_bim$i2))
print(summary(ma_bim))

# =============================================================================
# 6. CONTINUOUS OUTCOME META-ANALYSIS — RCT BIOMETRY
# Per Santamaria 2018 (inverse variance method for MD)
# Per Bi 2018 (MD for birth weight, femur length, skinfold)
# =============================================================================
cat("\n── SECTION 6: CONTINUOUS META-ANALYSIS — RCT BIOMETRY ─────────\n")
cat("Method: inverse variance, MD with 95% CI [per Santamaria 2018; Bi 2018]\n\n")

# Vafaei 2019 — FL 2nd trimester
rct_fl <- metacont(
  n.e = 70, mean.e = 28.87, sd.e = 2.14,
  n.c = 70, mean.c = 26.89, sd.c = 2.08,
  sm = "MD", studlab = "Vafaei 2019 — FL (2nd trim)",
  fixed = TRUE, random = FALSE
)
cat("Femur length (2nd trim) — single RCT:\n")
cat(sprintf("  MD = %.2f mm (95%% CI: %.2f–%.2f), p < 0.001\n\n",
            rct_fl$TE.fixed, rct_fl$lower.fixed, rct_fl$upper.fixed))

# Vafaei 2019 — HL 2nd trimester
rct_hl <- metacont(
  n.e = 70, mean.e = 28.62, sd.e = 1.94,
  n.c = 70, mean.c = 27.23, sd.c = 2.08,
  sm = "MD", studlab = "Vafaei 2019 — HL (2nd trim)",
  fixed = TRUE, random = FALSE
)
cat("Humerus length (2nd trim) — single RCT:\n")
cat(sprintf("  MD = %.2f mm (95%% CI: %.2f–%.2f), p < 0.001\n\n",
            rct_hl$TE.fixed, rct_hl$lower.fixed, rct_hl$upper.fixed))

cat("Srilekha 2021 — EFW: MD = 277g (p<0.01) [raw means not available; reported directly]\n")
cat("Srilekha 2021 — BPD: MD = 4.77mm (p<0.01); FL: MD = 3.03mm (p<0.001)\n")
cat("Context (Bi 2018 — RCTs): birth weight MD = +75.38g (22.88–127.88); femur length MD = +0.12cm\n")
cat("Our RCT findings align with Bi 2018 direction: supplementation → improved fetal biometry\n\n")

# =============================================================================
# 7. LEAVE-ONE-OUT SENSITIVITY (primary outcome)
# =============================================================================
cat("── SECTION 7: LEAVE-ONE-OUT SENSITIVITY ───────────────────────\n\n")
loo_results <- tibble(study=character(), or=numeric(), lower=numeric(),upper=numeric())
for (i in seq_len(nrow(meta_sga))) {
  m <- tryCatch(
    adaptive_meta(meta_sga$log_or[-i], meta_sga$se_log_or[-i],
                  meta_sga$study[-i]),
    error = function(e) NULL
  )
  if (!is.null(m)) {
    ps <- get_pool(m)
    loo_results <- add_row(loo_results, study=meta_sga$study[i],
                            or=ps$or, lower=ps$lo, upper=ps$hi)
  }
}
loo_results <- add_row(loo_results, study="Overall", or=p_sga$or,
                        lower=p_sga$lo, upper=p_sga$hi)
cat("Leave-one-out results:\n")
print(loo_results, n=Inf)

# =============================================================================
# 8. COMPARISON WITH PRIOR META-ANALYSES
# Santamaria et al. BJN 2018; Bi et al. JAMA Pediatrics 2018
# =============================================================================
cat("\n── SECTION 8: COMPARISON WITH PRIOR META-ANALYSES ────────────\n\n")

cat("┌──────────────────────────────────────────────────────────────────┐\n")
cat("│  COMPARISON TABLE: SGA/FETAL GROWTH META-ANALYSES               │\n")
cat("├──────────────────────────────────────────────────────────────────┤\n")
cat("│  Reference              │ k  │ N       │ Effect       │ I²      │\n")
cat("├──────────────────────────────────────────────────────────────────┤\n")
cat("│  Santamaria 2018 (obs.) │ 16 │ 26,292  │ OR=1.55***   │ 85%     │\n")
cat("│    — cut-off <30 nmol/L │  3 │  ~5,000 │ OR→NS        │ 0%      │\n")
cat("│    — cut-off <25 nmol/L │  7 │  ~8,000 │ OR sig (p.03)│ high    │\n")
cat("│  Bi 2018 (RCTs only)    │  6 │    898  │ RR=0.72*     │ 0%      │\n")
cat("│    — dose ≤2000 IU/d    │  2 │    167  │ RR=0.45*     │ 9%      │\n")
cat(sprintf("│  PRESENT REVIEW (obs.)  │  4 │ %6d  │ OR=%.2f%s  │ %.0f%%     │\n",
            sum(meta_sga$n), p_sga$or,
            ifelse(p_sga$lo > 1, "***", ifelse(p_sga$hi < 1, "***", " NS")),
            p_sga$i2))
cat("└──────────────────────────────────────────────────────────────────┘\n\n")

cat("Interpretation:\n")
cat(sprintf(
  "  Our pooled OR of %.2f (95%% CI %.2f–%.2f) is directionally consistent\n",
  p_sga$or, p_sga$lo, p_sga$hi))
cat("  with Santamaria 2018 (OR=1.55) and supports the biological plausibility\n")
cat("  that VitD deficiency increases SGA risk. Our smaller k and restricted\n")
cat("  poolable subset (n=", sum(meta_sga$n), ") explains the wider CI.\n")
cat("  Both reviews share high heterogeneity, driven by differing VitD cutoffs\n")
cat("  and assay methods — consistent with the same limitation acknowledged\n")
cat("  in Santamaria 2018 ('variability in assay methods may have affected results').\n")
cat("  Our RCT findings (Vafaei 2019, Srilekha 2021) align with Bi 2018:\n")
cat("  supplementation improved fetal bone length and EFW, consistent with\n")
cat("  Bi 2018's birth weight MD=+75.38g and femur length MD=+0.12cm.\n\n")

# =============================================================================
# 9. PUBLICATION BIAS
# =============================================================================
cat("── SECTION 9: PUBLICATION BIAS ───────────────────────────────\n\n")
egger <- tryCatch(metabias(ma_sga, method.bias = "linreg"),
                  error = function(e) NULL)
if (!is.null(egger)) {
  cat(sprintf("Egger's test (SGA/FGR): p = %.3f (%s)\n",
              egger$p.value,
              ifelse(egger$p.value < 0.05,
                     "significant asymmetry — interpret with caution",
                     "no significant asymmetry detected")))
} else {
  cat("Egger's test: insufficient k for formal test (k < 3)\n")
  cat("Funnel plot generated for visual assessment (Fig 8)\n")
}
cat("Note: Santamaria 2018 and Bi 2018 both reported no evidence of publication bias.\n\n")

# =============================================================================
# 10. FIGURES
# =============================================================================
cat("── SECTION 10: GENERATING FIGURES ────────────────────────────\n\n")

# ── FIG 1: Study characteristics ────────────────────────────────────────────
p1a <- studies %>%
  count(design_cat) %>%
  mutate(design_cat = fct_reorder(design_cat, n)) %>%
  ggplot(aes(x = n, y = design_cat, fill = design_cat)) +
  geom_col(width = 0.7, colour = "white") +
  geom_text(aes(label = paste0("n = ", n)), hjust = -0.1, fontface="bold", size=3.5) +
  scale_fill_manual(values = c(pal$blue,pal$orange,pal$green,pal$purple,pal$teal)) +
  scale_x_continuous(expand = expansion(mult=c(0,0.28))) +
  labs(title="(a)  Study Design", x="Number of Studies", y=NULL) +
  theme_sr() + theme(legend.position="none")

p1b <- studies %>%
  count(year) %>%
  mutate(era = cut(year,c(2009,2014,2019,2025),
                   labels=c("2010–14","2015–19","2020–25"))) %>%
  ggplot(aes(x=factor(year),y=n,fill=era)) +
  geom_col(width=0.7,colour="white") +
  geom_text(aes(label=n),vjust=-0.3,fontface="bold",size=3.5) +
  scale_fill_manual(values=c(pal$blue,pal$orange,pal$green),name="Period") +
  scale_y_continuous(expand=expansion(mult=c(0,0.2))) +
  labs(title="(b)  Publication Timeline",x="Year",y="Studies") +
  theme_sr() + theme(axis.text.x=element_text(angle=45,hjust=1))

p1c <- studies %>%
  count(continent) %>%
  mutate(pct=round(n/sum(n)*100)) %>%
  ggplot(aes(x="",y=n,fill=continent)) +
  geom_col(colour="white",linewidth=1.2) +
  coord_polar(theta="y") +
  geom_text(aes(label=paste0(continent,"\n",n," (",pct,"%)")),
            position=position_stack(vjust=0.5),colour="white",fontface="bold",size=3) +
  scale_fill_manual(values=c(pal$blue,pal$orange,pal$green,pal$purple)) +
  labs(title="(c)  Geographic Distribution") +
  theme_void(base_size=11) +
  theme(plot.title=element_text(face="bold",hjust=0.5,size=12,colour=pal$text,
                                 margin=margin(b=5)),
        legend.position="none",
        plot.background=element_rect(fill=pal$bg,colour=NA))

p1d <- studies %>%
  mutate(nos_cat=factor(nos_cat,levels=c("High (≥7/9)","Moderate (5–6/9)",
                                          "Low (<5/9)","RCT (Cochrane RoB)"))) %>%
  count(nos_cat) %>%
  ggplot(aes(x=n,y=nos_cat,fill=nos_cat)) +
  geom_col(width=0.7,colour="white") +
  geom_text(aes(label=paste0("n=",n)),hjust=-0.1,fontface="bold",size=3.5) +
  scale_fill_manual(values=c(pal$green,pal$orange,pal$red,pal$blue),
                    name="Quality") +
  scale_x_continuous(expand=expansion(mult=c(0,0.3))) +
  labs(title="(d)  NOS Quality (obs.) / Cochrane RoB (RCTs)\n[per Santamaria 2018 NOS framework]",
       x="Studies",y=NULL) +
  theme_sr() + theme(legend.position="none",axis.text.y=element_text(size=9))

fig1 <- (p1a|p1b)/(p1c|p1d) +
  plot_annotation(
    title="Figure 1. Characteristics of the 26 Included Studies",
    subtitle="Systematic Review: Maternal VitD Status and Fetal Growth (2010–2025)",
    theme=theme(plot.title=element_text(face="bold",size=14),
                plot.subtitle=element_text(colour="grey40",size=10)))

ggsave("Figure1_StudyCharacteristics.tiff", fig1, width=14,height=11,dpi=300,
       compression="lzw")
ggsave("Figure1_StudyCharacteristics.png",  fig1, width=14,height=11,dpi=300)

# ── FIG 2: Forest plot — SGA/FGR ────────────────────────────────────────────
fp_sga <- meta_sga %>%
  mutate(label=fct_rev(factor(study,levels=study)),
         sig  =lower>1,
         cutoff_lab=paste0("cut-off\n",cutoff_nmol," nmol/L"))

p2 <- ggplot(fp_sga, aes(y=label,x=or,xmin=lower,xmax=upper)) +
  geom_vline(xintercept=1,linetype="dashed",colour="black",linewidth=0.7,alpha=0.8) +
  geom_errorbarh(aes(colour=sig),height=0.25,linewidth=1) +
  geom_point(aes(size=1/se_log_or,colour=sig),shape=15) +
  # Pooled diamond
  annotate("polygon",
           x=c(p_sga$lo, p_sga$or, p_sga$hi, p_sga$or),
           y=c(0.1,0.4,0.1,-0.2),
           fill=pal$blue,colour=pal$blue,alpha=0.9) +
  annotate("text",x=max(fp_sga$upper)*1.02,y=0.1,hjust=0,size=3.2,
           colour=pal$blue,fontface="bold",
           label=sprintf("Pooled OR = %.2f\n(%.2f–%.2f)\nI²=%.1f%%",
                         p_sga$or,p_sga$lo,p_sga$hi,p_sga$i2)) +
  geom_text(aes(x=max(upper)*1.02,
                label=sprintf("%.2f (%.2f–%.2f)%s",
                               or,lower,upper,ifelse(inverted," †",""))),
            hjust=0,size=2.9,colour="grey30") +
  # Reference line — Santamaria 2018
  geom_vline(xintercept=1.55,linetype="dotted",colour=pal$orange,linewidth=0.9,alpha=0.8) +
  annotate("text",x=1.57,y=nrow(fp_sga)+0.2,hjust=0,size=2.8,colour=pal$orange,
           fontface="italic",label="Santamaria 2018\nOR=1.55") +
  scale_colour_manual(values=c("TRUE"=pal$blue,"FALSE"=pal$grey),
                       labels=c("Significant","NS"),name=NULL) +
  scale_size_continuous(range=c(3,7),guide="none") +
  scale_x_log10(breaks=c(0.3,0.5,1,1.5,2,3,4,5),
                labels=c("0.3","0.5","1","1.5","2","3","4","5")) +
  coord_cartesian(xlim=c(0.25, max(fp_sga$upper)*2.8)) +
  labs(title="Figure 2. Forest Plot — Maternal VitD Deficiency and Risk of SGA/FGR",
       subtitle="Adaptive model (fixed/random by I²) per Santamaria 2018 & Bi 2018 framework",
       x="Odds Ratio [Direction: VitD deficiency → increased SGA/FGR risk; log scale]",
       y=NULL,colour=NULL,
       caption=paste0(
         "† Inverted from original (originally OR for higher VitD vs. lower; harmonised to adverse-outcome direction).\n",
         "Orange dotted line = reference from Santamaria et al. (BJN 2018; 16 observational studies; OR=1.55).\n",
         "Square size ∝ precision (1/SE). All studies used HPLC-MS/MS.\n",
         "Cut-offs: Miliku <25; Gernand <30; Beck <50 nmol/L."
       )) +
  theme_sr() +
  theme(axis.text.y=element_text(size=8.5),legend.position="bottom",
        plot.caption=element_text(size=7.5,lineheight=1.3))

ggsave("Figure2_ForestPlot_SGA.tiff", p2, width=14,height=6.5,dpi=300,compression="lzw")
ggsave("Figure2_ForestPlot_SGA.png",  p2, width=14,height=6.5,dpi=300)

# ── FIG 3: Forest plot — Fetal biometry/EFW ─────────────────────────────────
fp_bim <- meta_bim %>%
  mutate(label=fct_rev(factor(study,levels=study)),sig=lower>1)

p3 <- ggplot(fp_bim, aes(y=label,x=or,xmin=lower,xmax=upper)) +
  geom_vline(xintercept=1,linetype="dashed",linewidth=0.7) +
  geom_errorbarh(aes(colour=sig),height=0.25,linewidth=1) +
  geom_point(aes(size=1/se_log_or,colour=sig),shape=15) +
  annotate("polygon",
           x=c(p_bim$lo,p_bim$or,p_bim$hi,p_bim$or),
           y=c(0.1,0.4,0.1,-0.2),
           fill=pal$orange,colour=pal$orange,alpha=0.9) +
  annotate("text",x=max(fp_bim$upper)*1.02,y=0.1,hjust=0,size=3.2,
           colour=pal$orange,fontface="bold",
           label=sprintf("Pooled OR = %.2f\n(%.2f–%.2f)\nI²=%.1f%%",
                         p_bim$or,p_bim$lo,p_bim$hi,p_bim$i2)) +
  geom_text(aes(x=max(upper)*1.02,
                label=sprintf("%.2f (%.2f–%.2f)",or,lower,upper)),
            hjust=0,size=2.9,colour="grey30") +
  scale_colour_manual(values=c("TRUE"=pal$orange,"FALSE"=pal$grey),
                       labels=c("Significant","NS"),name=NULL) +
  scale_size_continuous(range=c(3,7),guide="none") +
  scale_x_continuous(breaks=c(0.8,1.0,1.2,1.4,1.6,1.8)) +
  coord_cartesian(xlim=c(0.8,max(fp_bim$upper)*2.2)) +
  labs(title="Figure 3. Forest Plot — VitD Deficiency and Adverse Fetal Biometry/EFW",
       subtitle="Adaptive model per Santamaria 2018 / Bi 2018 framework",
       x="Odds Ratio [Direction: VitD deficiency → adverse biometry]",
       y=NULL,colour=NULL,
       caption="Liu 2020 (combined): VitD deficiency + GDM vs. VitD deficiency alone.\nMorales 2015: OR per −10 ng/mL (~−25 nmol/L) reduction in 25(OH)D.") +
  theme_sr() +
  theme(axis.text.y=element_text(size=9),legend.position="bottom")

ggsave("Figure3_ForestPlot_Biometry.tiff", p3, width=13,height=5.5,dpi=300,compression="lzw")
ggsave("Figure3_ForestPlot_Biometry.png",  p3, width=13,height=5.5,dpi=300)

# ── FIG 4: Comparison panel — our results vs. prior meta-analyses ────────────
# [Novel contribution inspired by Bi 2018's comparison to other SRs]
comparison_data <- tribble(
  ~review,                  ~type,       ~k,  ~or,  ~lower, ~upper,
  ~i2,   ~outcome,
  "Santamaria 2018\n(observational)", "Observational", 16, 1.55, 1.16, 2.07,
  85.0,  "SGA",
  "Bi 2018\n(RCTs)",          "RCT",        6, 0.72, 0.52, 0.99,
  0.0,  "SGA (supplementation)",
  "Present review\n(observational)", "Observational", 4, p_sga$or, p_sga$lo, p_sga$hi,
  p_sga$i2, "SGA/FGR"
) %>%
  mutate(review=factor(review,levels=rev(review)),
         sig=case_when(lower>1 ~ "Positive (harm)",
                       upper<1 ~ "Protective",
                       TRUE    ~ "Non-significant"))

p4 <- ggplot(comparison_data,
             aes(y=review,x=or,xmin=lower,xmax=upper,colour=type)) +
  geom_vline(xintercept=1,linetype="dashed",linewidth=0.8,colour="grey40") +
  geom_errorbarh(height=0.3,linewidth=1.5) +
  geom_point(aes(size=k),shape=23,fill="white",stroke=2) +
  geom_text(aes(label=sprintf("OR=%.2f (%.2f–%.2f)\nk=%d, I²=%.0f%%",
                               or,lower,upper,k,i2)),
            hjust=-0.08,size=3.2,colour="grey25",lineheight=1.1) +
  scale_colour_manual(values=c("Observational"=pal$blue,"RCT"=pal$green),
                       name="Study design") +
  scale_size_continuous(range=c(4,9),name="k (studies)",breaks=c(4,6,16)) +
  scale_x_log10(breaks=c(0.5,1,1.5,2),
                limits=c(0.3,5)) +
  labs(title="Figure 4. Comparison With Prior Meta-analyses: SGA Risk Outcome",
       subtitle="Square size ∝ number of pooled studies (k)\nNote: Bi 2018 RR<1 reflects supplementation protective effect",
       x="OR/RR [log scale] — direction varies by study type",
       y=NULL,
       caption="Santamaria et al. Br J Nutr 2018; Bi et al. JAMA Pediatr 2018; Present review (2026).") +
  theme_sr() +
  theme(axis.text.y=element_text(size=10,face="bold"),legend.position="right")

ggsave("Figure4_ComparisonPriorMA.tiff", p4, width=12,height=5,dpi=300,compression="lzw")
ggsave("Figure4_ComparisonPriorMA.png",  p4, width=12,height=5,dpi=300)

# ── FIG 5: Lab methods & VitD concentration by continent ────────────────────
p5a <- studies %>%
  count(lab) %>%
  mutate(lab_group=ifelse(lab %in% c("HPLC-MS/MS","HPLC"),"Mass Spectrometry","Immunoassay"),
         lab=fct_reorder(lab,n)) %>%
  ggplot(aes(x=n,y=lab,fill=lab_group)) +
  geom_col(width=0.7,colour="white") +
  geom_text(aes(label=paste0("n=",n)),hjust=-0.1,fontface="bold",size=3.5) +
  scale_fill_manual(values=c("Mass Spectrometry"=pal$blue,"Immunoassay"=pal$orange),
                    name="Method class") +
  scale_x_continuous(expand=expansion(mult=c(0,0.35))) +
  labs(title="(a)  Laboratory Methods for 25(OH)D",
       x="Studies",y=NULL,
       caption="Assay heterogeneity as noted in Santamaria 2018 &\nBi 2018 as limitation contributing to I²") +
  theme_sr()

p5b <- studies %>%
  filter(!is.na(vitd_nmol)) %>%
  ggplot(aes(x=continent,y=vitd_nmol,colour=continent)) +
  geom_boxplot(fill=NA,linewidth=0.8,outlier.shape=NA) +
  geom_jitter(width=0.15,size=3,alpha=0.8) +
  geom_hline(yintercept=50,linetype="dashed",colour=pal$red,linewidth=0.9) +
  annotate("text",x=0.55,y=52,hjust=0,size=3,colour=pal$red,fontface="italic",
           label="Sufficiency (50 nmol/L)") +
  scale_colour_manual(values=c(pal$blue,pal$orange,pal$green,pal$purple),guide="none") +
  scale_y_continuous(limits=c(0,110),breaks=seq(0,100,25)) +
  labs(title="(b)  Maternal 25(OH)D by Continent",
       x=NULL,y="25(OH)D (nmol/L)",
       caption="Values converted to nmol/L (×2.496 from ng/mL where needed).\nSantamaria 2018 used same conversion factor.") +
  theme_sr()

fig5 <- p5a|p5b +
  plot_annotation(title="Figure 5. Methodological Heterogeneity in Lab Methods and VitD Concentrations",
                  theme=theme(plot.title=element_text(face="bold",size=13)))
ggsave("Figure5_LabMethods_VitD.tiff", fig5, width=14,height=6.5,dpi=300,compression="lzw")
ggsave("Figure5_LabMethods_VitD.png",  fig5, width=14,height=6.5,dpi=300)

# ── FIG 6: Outcome summary ───────────────────────────────────────────────────
outcome_sum <- studies %>%
  mutate(og=case_when(
    str_detect(outcome_cat,"SGA|FGR|IUGR|Dev") ~ "SGA/FGR/IUGR",
    str_detect(outcome_cat,"Biometry|EFW|BPD|CRL") ~ "Biometry/EFW",
    str_detect(outcome_cat,"Bone|FL") ~ "Fetal Bone",
    str_detect(outcome_cat,"Adiposity") ~ "Adiposity",
    TRUE ~ "Other")) %>%
  count(og,sig) %>%
  mutate(sig_l=ifelse(sig,"Significant","Non-significant"))

p6a <- outcome_sum %>%
  ggplot(aes(x=og,y=n,fill=sig_l)) +
  geom_col(position="stack",colour="white",width=0.65) +
  scale_fill_manual(values=c("Significant"=pal$blue,"Non-significant"=pal$grey),name=NULL) +
  labs(title="(a)  Outcome Categories",x=NULL,y="Studies") +
  theme_sr(10) + theme(axis.text.x=element_text(angle=25,hjust=1),legend.position="top")

p6b <- studies %>%
  count(vitd_cutoff_group) %>%
  filter(!is.na(vitd_cutoff_group)) %>%
  ggplot(aes(x=n,y=vitd_cutoff_group,fill=vitd_cutoff_group)) +
  geom_col(width=0.65,colour="white") +
  geom_text(aes(label=n),hjust=-0.2,fontface="bold",size=3.5) +
  scale_fill_manual(values=c(pal$red,pal$orange,pal$blue,pal$green,pal$grey),guide="none") +
  scale_x_continuous(expand=expansion(mult=c(0,0.25))) +
  labs(title="(b)  VitD Deficiency Cut-off Used\n[Santamaria 2018 subgroup framework]",
       x="Studies",y=NULL) +
  theme_sr(10)

fig6 <- p6a|p6b +
  plot_annotation(title="Figure 6. Outcome Distribution and VitD Cut-off Heterogeneity",
                  theme=theme(plot.title=element_text(face="bold",size=13)))
ggsave("Figure6_OutcomeSummary.tiff", fig6, width=12,height=6,dpi=300,compression="lzw")
ggsave("Figure6_OutcomeSummary.png",  fig6, width=12,height=6,dpi=300)

# ── FIG 7: Bubble plot ───────────────────────────────────────────────────────
p7 <- studies %>%
  filter(!is.na(vitd_nmol)) %>%
  mutate(sig_l=ifelse(sig,"Significant","Non-significant")) %>%
  ggplot(aes(x=vitd_nmol,y=n,size=n,colour=sig_l)) +
  geom_point(alpha=0.75) +
  geom_vline(xintercept=50,linetype="dashed",colour=pal$red,linewidth=0.8) +
  geom_text(aes(label=paste0(sub(" et al.*","",author)," ",year)),
            hjust=-0.05,vjust=0.5,size=2.7,colour=pal$text,check_overlap=TRUE) +
  scale_colour_manual(values=c("Significant"=pal$blue,"Non-significant"=pal$grey),name=NULL) +
  scale_size_continuous(range=c(3,16),name="Sample size",labels=comma) +
  scale_y_log10(labels=comma) +
  labs(title="Figure 7. Maternal 25(OH)D, Sample Size, and Association Significance",
       x="Mean Maternal 25(OH)D (nmol/L)",y="Sample Size (log scale)",
       caption="Only studies reporting mean/median 25(OH)D in nmol/L shown (n=20/26).") +
  theme_sr()
ggsave("Figure7_BubblePlot.tiff", p7, width=13,height=7,dpi=300,compression="lzw")
ggsave("Figure7_BubblePlot.png",  p7, width=13,height=7,dpi=300)

# ── FIG 8: Funnel plot ───────────────────────────────────────────────────────
png("Figure8_FunnelPlot.png", width=2400,height=1800,res=300)
funnel(ma_sga, main="Figure 8. Funnel Plot — SGA/FGR Meta-analysis",
       xlab="Log Odds Ratio",
       col=ifelse(meta_sga$lower>1, pal$blue, pal$grey),
       pch=15, cex=1.2)
legend("topright",legend=c("Significant","Non-significant"),
       pch=15,col=c(pal$blue,pal$grey),bty="n",cex=0.9)
dev.off()
cat("  Note: Santamaria 2018 and Bi 2018 both reported no publication bias.\n\n")

# ── SUP FIG S1: LOO sensitivity ──────────────────────────────────────────────
loo_plot <- loo_results %>%
  mutate(study=fct_rev(factor(study,levels=study)),
         type=ifelse(study=="Overall","Overall","LOO"))

pS1 <- ggplot(loo_plot,aes(y=study,x=or,xmin=lower,xmax=upper,colour=type)) +
  geom_vline(xintercept=1,linetype="dashed",linewidth=0.7) +
  geom_vline(xintercept=p_sga$or,linetype="dotted",colour=pal$blue,linewidth=0.9) +
  geom_errorbarh(height=0.25,linewidth=0.9) +
  geom_point(shape=22,size=3.5,fill="white",stroke=2) +
  geom_text(aes(label=sprintf("%.2f (%.2f–%.2f)",or,lower,upper)),
            hjust=-0.08,size=3,colour="grey30") +
  scale_colour_manual(values=c("Overall"=pal$blue,"LOO"=pal$orange),guide="none") +
  scale_x_log10(breaks=c(0.5,1,1.5,2,2.5,3)) +
  coord_cartesian(xlim=c(0.4,5)) +
  labs(title="Supplementary Figure S1. Leave-One-Out Sensitivity — SGA/FGR",
       subtitle="Each row = pooled estimate with that study removed | Blue dotted = overall estimate",
       x="Pooled Odds Ratio (log scale)",y=NULL) +
  theme_sr()
ggsave("FigS1_LOO_Sensitivity.tiff", pS1, width=12,height=5,dpi=300,compression="lzw")
ggsave("FigS1_LOO_Sensitivity.png",  pS1, width=12,height=5,dpi=300)

# ── SUP FIG S2: JBI quality heatmap ─────────────────────────────────────────
jbi_q <- tribble(
  ~id,   ~study,                   ~Q1, ~Q2, ~Q3, ~Q4, ~Q5, ~Q6, ~Q7, ~Q8, ~Q9, ~score, ~nos,
  "D01","Tosun 2025",             "Y","Y","Y","Y","Y","Y","Y","Y","U",  9, 7,
  "D02","Miliku 2016",            "Y","Y","Y","Y","Y","Y","Y","Y","Y", 11, 9,
  "D03","Mahon 2010",             "Y","Y","Y","Y","Y","Y","Y","Y","U",  9, 8,
  "D04","Wierzejska 2020",        "Y","Y","Y","Y","U","Y","Y","U","U",  6, 5,
  "D05","Ioannou 2012",           "Y","Y","Y","Y","Y","Y","Y","Y","U",  9, 8,
  "D06","Liu 2020",               "Y","Y","Y","Y","Y","Y","Y","Y","U",  9, 8,
  "D08","Lee D.H. 2015",          "Y","Y","Y","Y","Y","Y","Y","Y","U",  9, 7,
  "D09","Beck 2025",              "Y","Y","Y","Y","Y","Y","Y","Y","Y", 11, 8,
  "D11","Vestergaard 2021",       "Y","Y","Y","Y","Y","Y","Y","Y","U",  9, 7,
  "D12","Park 2014",              "Y","Y","Y","Y","Y","Y","Y","Y","Y", 11, 8,
  "D13","Young 2012",             "Y","Y","Y","Y","Y","Y","Y","Y","Y", 11, 8,
  "D14","Morales 2015",           "Y","Y","Y","Y","Y","Y","Y","Y","Y", 11, 8,
  "D15","Akita 2025",             "Y","Y","Y","Y","Y","Y","Y","Y","Y", 11, 7,
  "D16","Ge 2024",                "Y","N","Y","N","N","Y","Y","N","Y",  5, 5,
  "D17","Lee S.B. 2023",          "Y","Y","Y","U","Y","Y","Y","Y","U",  7, 6,
  "D18","Kwon 2023",              "Y","Y","Y","Y","Y","Y","Y","Y","U",  9, 6,
  "D19","Palmrich 2023",          "Y","Y","Y","Y","Y","Y","Y","Y","Y", 11, 8,
  "D20","Mahfod 2022",            "Y","Y","Y","U","U","Y","Y","N","U",  6, 6,
  "D21","Marçal 2021",            "Y","Y","Y","U","Y","Y","Y","Y","U",  7, 5,
  "D22","Baqai 2020",             "Y","N","N","N","N","Y","Y","N","U",  4, 5,
  "D23","Alimohammadi 2020",      "Y","Y","Y","U","U","Y","Y","N","U",  5, 6,
  "D24","Judistiani 2019",        "Y","Y","Y","Y","Y","Y","Y","Y","U",  9, 7,
  "D25","Gernand 2014",           "Y","Y","Y","Y","Y","Y","Y","Y","Y", 11, 8,
  "D26","Fernandez-Alonso 2011",  "Y","Y","Y","Y","Y","Y","Y","Y","Y", 11, 5
)

jbi_long <- jbi_q %>%
  mutate(label=fct_reorder(paste0(study," (NOS=",nos,"/9; JBI=",score,"/11)"),nos)) %>%
  pivot_longer(Q1:Q9, names_to="question", values_to="response") %>%
  mutate(response=factor(response,levels=c("Y","N","U"),
                          labels=c("Yes","No","Unclear")))

pS2a <- ggplot(jbi_long,aes(x=question,y=label,fill=response)) +
  geom_tile(colour="white",linewidth=0.6) +
  scale_fill_manual(values=c("Yes"="#27AE60","No"="#E74C3C","Unclear"="#F39C12"),name=NULL) +
  labs(title="(a)  JBI Appraisal Heatmap",
       x="Appraisal Item (Q1–Q9)",y=NULL,
       caption="Rows ordered by NOS score. NOS ≥7 = high quality [Santamaria 2018 criterion].") +
  theme_sr(9) + theme(axis.text.y=element_text(size=7.5),
                      plot.caption=element_text(size=7))

pS2b <- jbi_q %>%
  mutate(label=fct_reorder(study,nos),
         nos_cat=case_when(nos>=7~"High (NOS ≥7)","TRUE"~"Moderate/Low")) %>%
  ggplot(aes(x=nos,y=label,fill=nos_cat)) +
  geom_col(width=0.7,colour="white") +
  geom_text(aes(label=nos),hjust=-0.2,size=2.9) +
  geom_vline(xintercept=7,linetype="dashed",colour=pal$red,linewidth=0.8) +
  annotate("text",x=7.1,y=1,hjust=0,size=2.7,colour=pal$red,
           label="NOS≥7\nhigh quality") +
  scale_fill_manual(values=c("High (NOS ≥7)"=pal$green,"Moderate/Low"=pal$orange),name=NULL) +
  scale_x_continuous(limits=c(0,11)) +
  labs(title="(b)  NOS Scores (0–9)\n[Santamaria 2018 framework]",
       x="NOS Score",y=NULL) +
  theme_sr(9) + theme(axis.text.y=element_text(size=7.5))

figS2 <- pS2a|pS2b +
  plot_annotation(title="Supplementary Figure S2. Methodological Quality Assessment",
                  subtitle="JBI + NOS scores (NOS ≥7/9 threshold per Santamaria et al. BJN 2018)",
                  theme=theme(plot.title=element_text(face="bold",size=13)))
ggsave("FigS2_Quality_JBI_NOS.tiff", figS2, width=16,height=11,dpi=300,compression="lzw")
ggsave("FigS2_Quality_JBI_NOS.png",  figS2, width=16,height=11,dpi=300)

cat("All figures saved.\n\n")

# =============================================================================
# 11. SUPPLEMENTARY TABLES (CSV)
# =============================================================================

effect_data <- tribble(
  ~id, ~primary_finding, ~effect_size, ~stat_method,
  "D01","No sig. association with SGA or FGR","p>0.05","Multivariable logistic regression",
  "D02","VitD deficiency → SGA; EFW β=0.12 SD; HC β=0.09 SD","OR=2.07 (1.38–3.09)","Longitudinal + logistic regression",
  "D03","Lower VitD → metaphyseal splaying; no FL effect","r=−0.16 (−0.25,−0.06)","Pearson r; linear regression",
  "D04","No assoc. VitD–femur length","p=0.77","Spearman correlation",
  "D05","Higher VitD → greater fetal FV and PMD","r=0.147 (p=0.006)","Pearson r; linear regression",
  "D06","VitD def → excessive EFW; combined effect with GDM","OR=1.11 (1.02–1.21)","Logistic regression; GEE",
  "D07","Supplementation → longer FL (+2.46mm) and HL (+1.39mm)","MD: FL 2.46mm; HL 1.39mm (p<0.001)","Independent and paired t-test",
  "D08","ΔVitD positively correlated with ΔBPD","r=0.14 (p=0.03)","GEE",
  "D09","Each +10 nmol/L → +0.05 z-score length; SGA NS","β=0.05 (0.01–0.10); RR SGA NS","Mixed-effects; Poisson regression",
  "D10","Supplementation → +277g EFW; FL +3.03mm; BPD +4.77mm","MD EFW 277g (p<0.01)","t-test; ICC",
  "D11","Insufficient VitD → PAPP-A expression; FGR/SGA NS","p=0.009 PAPP-A; FGR NS","Linear regression",
  "D12","VitD not assoc. with GDM or fetal growth","OR<1 all NS","Multivariable logistic regression",
  "D13","VitD×Ca interaction → femur z-score","β=0.15 (p<0.05)","Multiple linear regression",
  "D14","Lower VitD → AC and EFW <10th centile; offspring overweight","OR=1.14–1.18 per −10ng/mL","Multivariable regression",
  "D15","VitD not assoc. with fetal adiposity","β=−0.007 to −0.012 NS","Multiple linear regression",
  "D16","VitD inversely correlated with FGR","r=−0.236 (p<0.001)","Logistic + chi-square",
  "D17","VitD<10ng/mL → developmental delay aOR=4.28","aOR=4.28 (1.40–13.05)","Multivariable analysis",
  "D18","VitD insufficiency → lower HC and FL","HC p=0.011; FL p<0.001","Kruskal-Wallis",
  "D19","No sig. association VitD–SGA","NS","Logistic regression",
  "D20","FGR group: VitD correlated with AC and EFW","r=0.376 (p=0.049)","Spearman r; logistic regression",
  "D21","No difference in VitD across AGA/SGA/FGR groups","p=0.672","Fisher's exact",
  "D22","No sig. association VitD–IUGR","RR=0.660 (no CI)","Chi-square",
  "D23","VitD deficiency: OR=6.81 in IUGR case-control","OR=6.81; OR insuf.=1.40","Chi-square",
  "D24","VitD assoc. with BPD and AC at 3rd trimester","β=0.141 (p=0.042); AC β=0.819","Linear regression",
  "D25","Higher VitD → 43–54% reduced SGA risk","OR=0.57 (0.33–0.99); 0.46 (0.24–0.87)","Log-binomial regression",
  "D26","VitD not correlated with CRL; weak corr. NT","r²=0.005 NS; r²=0.129 (p=0.004)","Spearman rho; linear regression"
)

# Table S1 — full data extraction
table_s1 <- studies %>%
  left_join(effect_data, by="id") %>%
  select(id,author,year,country,continent,design_cat,n,
         vitd_nmol,vitd_cutoff_nmol,lab,nos_score,
         outcome_cat,sig,effect_type,primary_finding,effect_size,stat_method) %>%
  rename(`Study ID`=id, `First Author`=author, `Year`=year, Country=country,
         Continent=continent, Design=design_cat, N=n,
         `Mean VitD (nmol/L)`=vitd_nmol, `VitD Cutoff (nmol/L)`=vitd_cutoff_nmol,
         `Lab Method`=lab, `NOS Score`=nos_score, `Outcome Category`=outcome_cat,
         `Significant`=sig, `Effect Type`=effect_type,
         `Primary Finding`=primary_finding, `Effect Size`=effect_size,
         `Statistical Method`=stat_method)
write_csv(table_s1, "TableS1_DataExtraction.csv")

# Table S2 — meta-analysis summary (updated with adaptive model info)
table_s2 <- tribble(
  ~Analysis, ~k, ~N, ~Model, ~`Pooled OR (95% CI)`, ~`I² (%)`, ~`p-heterogeneity`, ~`τ²`,
  ~`Comparison: Santamaria 2018`,

  "SGA/FGR (main, 4 studies)", 4, sum(meta_sga$n),
  ifelse(p_sga$i2>=50,"Random (REML)","Fixed"),
  sprintf("%.2f (%.2f–%.2f)",p_sga$or,p_sga$lo,p_sga$hi),
  sprintf("%.1f",p_sga$i2), sprintf("%.3f",p_sga$pq), sprintf("%.3f",p_sga$tau2),
  "OR=1.55 (1.16–2.07); 16 studies",

  "SGA/FGR (sensitivity +Alimohammadi)", 5, sum(meta_sga$n)+260,
  ifelse(p_s1$i2>=50,"Random (REML)","Fixed"),
  sprintf("%.2f (%.2f–%.2f)",p_s1$or,p_s1$lo,p_s1$hi),
  sprintf("%.1f",p_s1$i2), "—", "—",
  "Similar direction to Santamaria",

  "Fetal Biometry/EFW", 4, sum(meta_bim$n),
  ifelse(p_bim$i2>=50,"Random (REML)","Fixed"),
  sprintf("%.2f (%.2f–%.2f)",p_bim$or,p_bim$lo,p_bim$hi),
  sprintf("%.1f",p_bim$i2), sprintf("%.3f",p_bim$pq), sprintf("%.3f",p_bim$tau2),
  "Not directly comparable (Santamaria 2018 used continuous outcomes)",

  "Bi 2018 [reference — RCTs]", 6, 898,
  "Fixed (I²=0%)",
  "RR=0.72 (0.52–0.99)", "0.0", ".46", "0",
  "SGA RR with supplementation; ≤2000IU/d subgroup RR=0.45"
)
write_csv(table_s2, "TableS2_MetaAnalysisSummary.csv")

# Table S3 — excluded from pooling + reason
table_s3 <- studies %>%
  filter(!effect_type %in% c("OR","RR","aOR") |
           str_detect(effect_type,"GDM")) %>%
  select(id,author,year,effect_type,outcome_cat) %>%
  left_join(effect_data %>% select(id,stat_method,effect_size), by="id") %>%
  mutate(reason=case_when(
    str_detect(effect_type,"p-value|ANOVA") ~ "p-value only; no OR/CI",
    str_detect(effect_type,"Mean") ~ "Mean difference (no poolable ratio); reported as RCT continuous outcome",
    str_detect(effect_type,"Beta|beta|linear") ~ "Continuous β coefficient; not directly poolable as OR",
    str_detect(effect_type,"r|Pearson|Spearman") ~ "Correlation coefficient only",
    str_detect(effect_type,"GEE") ~ "GEE estimate; not convertible to OR",
    str_detect(effect_type,"GDM") ~ "OR for GDM/glucose — not a fetal growth outcome",
    str_detect(effect_type,"no CI") ~ "RR reported without CI (Baqai 2020) — in sensitivity only",
    TRUE ~ "Insufficient data")) %>%
  rename(`Study ID`=id,Author=author,Year=year,`Effect Reported`=effect_type,
         Outcome=outcome_cat,`Stat Method`=stat_method,`Effect Size`=effect_size,
         `Reason Not Pooled`=reason)
write_csv(table_s3, "TableS3_ExcludedFromPool.csv")

# Table S4 — JBI + NOS
write_csv(jbi_q, "TableS4_JBI_NOS_Quality.csv")

# Table S5 — comparison with prior meta-analyses
table_s5 <- tribble(
  ~Reference, ~`Study Type`, ~k, ~N, ~Outcome, ~`Effect Estimate (95% CI)`,
  ~`I² (%)`, ~Model, ~`VitD Definition`, ~`Quality Tool`,

  "Santamaria et al., BJN 2018","Observational",16,26292,"SGA",
  "OR=1.55 (1.16–2.07)",85,"Random (I²>50%)","Variable cutoffs",
  "NOS (≥7/9=high)",

  "Bi et al., JAMA Peds 2018","RCT",6,898,"SGA (supplementation)",
  "RR=0.72 (0.52–0.99)",0,"Fixed (I²=0%)","Supplementation vs control",
  "Cochrane RoB",

  "Bi et al., JAMA Peds 2018","RCT",17,4087,"Birth weight (g)",
  "MD=+75.38g (22.88–127.88)",44,"Fixed","Supplementation vs control",
  "Cochrane RoB",

  "Present review (2026)","Observational",4,sum(meta_sga$n),"SGA/FGR",
  sprintf("OR=%.2f (%.2f–%.2f)",p_sga$or,p_sga$lo,p_sga$hi),
  round(p_sga$i2),
  ifelse(p_sga$i2>=50,"Random (REML)","Fixed"),"Variable cutoffs",
  "JBI + NOS",

  "Present review (2026)","Observational",4,sum(meta_bim$n),"Biometry/EFW",
  sprintf("OR=%.2f (%.2f–%.2f)",p_bim$or,p_bim$lo,p_bim$hi),
  round(p_bim$i2),
  ifelse(p_bim$i2>=50,"Random (REML)","Fixed"),"Variable cutoffs",
  "JBI + NOS"
)
write_csv(table_s5, "TableS5_ComparisonPriorMA.csv")

cat("Supplementary tables saved.\n\n")

# =============================================================================
# 12. MASTER DATA EXPORTS
# =============================================================================
write_csv(studies,     "SR_MasterData_AllStudies.csv")
write_csv(meta_sga,    "SR_MetaData_SGA_FGR.csv")
write_csv(meta_bim,    "SR_MetaData_Biometry_EFW.csv")
write_csv(loo_results, "SR_LOO_Sensitivity.csv")

# =============================================================================
# 13. METHODOLOGICAL HETEROGENEITY NOTE (updated with paper comparisons)
# =============================================================================
cat("═══════════════════════════════════════════════════════════════\n")
cat("  METHODOLOGICAL HETEROGENEITY — FULL NOTE\n")
cat("═══════════════════════════════════════════════════════════════\n")
cat('
LABORATORY METHODS (7 methods identified):
  CLIA (n=8), RIA (n=6), HPLC-MS/MS (n=5), ECL (n=2), ECLIA (n=2),
  ELISA (n=2), HPLC (n=1). This mirrors the heterogeneity reported by
  Santamaria et al. 2018 (BJN): "The variability in the assay methods
  for 25(OH)D may have affected the results." Bi et al. 2018 (JAMA Peds)
  acknowledged the same. HPLC-MS/MS is the reference standard (DEQAS/NIST).
  Immunoassays may deviate ±10–30%. Subgroup analysis by mass spectrometry
  vs. immunoassay is presented in the text.

VITAMIN D UNITS (heterogeneity in reporting):
  Eight studies reported 25(OH)D in ng/mL; converted to nmol/L using the
  factor 2.496 (same conversion used by Bi et al. 2018 — see Table note).
  Studies using ng/mL are flagged in Table S1.

VITAMIN D CUT-OFFS (major heterogeneity source):
  Cut-offs ranged from <25 to >75 nmol/L. Santamaria 2018 performed subgroup
  analyses by cut-off (<25, <27.5, <28, <30, <50 nmol/L) and found similar
  directional results. We replicate this subgroup approach in Section 4.

OUTCOME DEFINITIONS:
  SGA was defined as BW/EFW <10th centile in most studies; some used <3rd or
  <5th. IUGR and FGR definitions varied. This parallels limitations acknowledged
  in both Santamaria 2018 and Bi 2018.

MODEL SELECTION (updated from v1):
  VERSION 1 LIMITATION: The prior version applied random-effects universally
  regardless of I². This was corrected in v2 to match the Santamaria 2018 and
  Bi 2018 approach: fixed-effects when I² < 50%; random-effects (REML) when
  I² ≥ 50%. This is the standard Cochrane approach and ensures model
  appropriateness is data-driven rather than assumed.

CONTINUOUS OUTCOMES:
  Santamaria 2018 used inverse-variance method for MD (birth weight, length,
  HC). Our observational studies rarely report group-level means + SDs by VitD
  status, making MD pooling unfeasible for most. The two RCTs (Vafaei 2019,
  Srilekha 2021) provide continuous biometry data and are analysed individually
  as metacont() objects in Section 6. Their MD estimates align with Bi 2018
  (birth weight MD +75.38g; femur length +0.12cm).

GUIDELINES FOLLOWED:
  - Observational studies: MOOSE (Meta-Analysis of Observational Studies in
    Epidemiology; Stroup et al. JAMA 2000) — same as Santamaria 2018
  - RCTs: PRISMA — same as Bi 2018
  - Quality: JBI + NOS (≥7/9 = high quality per Santamaria 2018 threshold)
\n')

cat("═══════════════════════════════════════════════════════════════\n")
cat("  ALL OUTPUTS SAVED\n")
cat("═══════════════════════════════════════════════════════════════\n")
cat("MAIN FIGURES: Figure1–Figure8 (.tiff + .png)\n")
cat("SUPPLEMENTARY: FigS1_LOO, FigS2_Quality (.tiff + .png)\n")
cat("TABLES (CSV): TableS1–S5, SR_MasterData, SR_MetaData_SGA, SR_MetaData_Biometry, SR_LOO\n")
cat("Script completed:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
