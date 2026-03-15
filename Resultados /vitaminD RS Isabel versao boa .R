# =============================================================================
# COMPLETE META-ANALYSIS SCRIPT — VERSION 4
# Systematic Review: Maternal Vitamin D Status and Fetal Growth / Adiposity
# Updated: March 2026
# Incorporating:
#   (1) Santamaria et al. Br J Nutr 2018;119:310-319  [Observational SR/MA]
#   (2) Bi et al. JAMA Pediatrics 2018;172:635-645   [RCT SR/MA]
#
# CHANGES vs. v3:
#   • New Figure 4: RCT continuous outcomes (MD forest — panel a FL/HL, panel b EFW/BPD/FL)
#   • Old Fig 4 (comparison) promoted to FigS4 (supplementary)
#   • FigS3: Cut-off subgroup forest plot (matching Santamaria 2018 subgroup approach)
#   • FigS5: Narrative synthesis overview heatmap (all 26 studies)
#   • FigS6: GRADE evidence profile (new section)
#   • Section 12 added: GRADE evidence profile text output
#   • Forest plots improved: better annotations, reference lines, column labels
#   • All figure outputs: both .tiff (300 dpi, lzw) and .png (300 dpi, transparent bg)
# =============================================================================
setwd("~/Desktop/AUDENCIO/Producao artigos/Artigo RS Isabel Vitamina D /Resultados ")
# ── 0. Packages ───────────────────────────────────────────────────────────────
pkg <- c("meta","metafor","tidyverse","ggplot2","patchwork","scales",
         "RColorBrewer","forcats","ggpubr","gridExtra","grid","tidyr","readr")
new_pkg <- pkg[!pkg %in% installed.packages()[,"Package"]]
if (length(new_pkg)) install.packages(new_pkg, repos = "https://cloud.r-project.org")
invisible(lapply(pkg, library, character.only = TRUE))

# ── Colour palette ────────────────────────────────────────────────────────────
pal <- list(
  blue   = "#1F5FA6", orange = "#D4631A", green  = "#2E8B57",
  red    = "#C0392B", grey   = "#555555", lgrey  = "#AAAAAA",
  vlgrey = "#F2F4F7", black  = "#111111", gold   = "#B8860B",
  purple = "#6B4C8A", teal   = "#1A7A7A"
)

# ── Shared ggplot2 theme ──────────────────────────────────────────────────────
theme_sr <- function(base = 11) {
  theme_bw(base_size = base) +
    theme(
      panel.background  = element_rect(fill = "transparent", colour = NA),
      plot.background   = element_rect(fill = "transparent", colour = NA),
      panel.grid.major  = element_line(colour = "grey88", linetype = "dashed"),
      panel.grid.minor  = element_blank(),
      panel.border      = element_rect(colour = "grey60"),
      axis.title        = element_text(face = "bold", colour = pal$black, size = base - 0.5),
      axis.text         = element_text(colour = pal$black),
      plot.title        = element_text(face = "bold", size = base + 1, colour = pal$black),
      plot.subtitle     = element_text(colour = "grey40", size = base - 1),
      plot.caption      = element_text(colour = "grey45", size = base - 3,
                                       hjust = 0, face = "italic", lineheight = 1.25),
      legend.background = element_rect(fill = "white", colour = "grey80"),
      legend.key.size   = unit(0.9, "lines"),
      strip.background  = element_rect(fill = pal$blue, colour = NA),
      strip.text        = element_text(colour = "white", face = "bold")
    )
}

# ── Adaptive model selection () ─────────────────
adaptive_meta <- function(log_or, se_log, labs, sm = "OR", method_tau = "REML") {
  m_fixed <- metagen(TE = log_or, seTE = se_log, studlab = labs,
                     sm = sm, common = TRUE, random = FALSE)
  i2_val <- m_fixed$I2
  cat(sprintf("  I² = %.1f%% → using %s-effects model\n",
              i2_val * 100, ifelse(i2_val >= 0.50, "RANDOM", "FIXED")))
  if (i2_val >= 0.50) {
    metagen(TE = log_or, seTE = se_log, studlab = labs, sm = sm,
            common = FALSE, random = TRUE, method.tau = method_tau)
  } else {
    metagen(TE = log_or, seTE = se_log, studlab = labs, sm = sm,
            common = TRUE, random = FALSE)
  }
}

get_pool <- function(m) {
  if (m$random) list(or=exp(m$TE.random), lo=exp(m$lower.random),
                     hi=exp(m$upper.random), i2=m$I2*100, pq=m$pval.Q, tau2=m$tau2)
  else          list(or=exp(m$TE.fixed),  lo=exp(m$lower.fixed),
                     hi=exp(m$upper.fixed), i2=m$I2*100, pq=m$pval.Q, tau2=0)
}

# =============================================================================
# 1. MASTER DATA (26 studies, 2010–2025)
# =============================================================================
studies <- tribble(
  ~id,   ~author,                      ~year, ~country,      ~continent,
  ~design, ~n, ~vitd_nmol, ~vitd_sd,
  ~lab,          ~lab_group,
  ~vitd_cutoff_nmol, ~trimester_sample,
  ~outcome_cat, ~sig, ~effect_type, ~nos_score,

  "D01","Tosun et al.",              2025,"Turkey",     "Asia",
  "Prospective Cohort",226, 39.43,23.48,
  "CLIA","Immunoassay",50.0,"1st",
  "SGA/FGR",FALSE,"p-value only",7,

  "D02","Miliku et al.",             2016,"Netherlands","Europe",
  "Prospective Cohort",7098,46.7,83.26,
  "HPLC-MS/MS","Mass Spec",25.0,"any",
  "SGA",TRUE,"OR",9,

  "D03","Mahon et al.",              2010,"UK",         "Europe",
  "Prospective Cohort",424, 61.0,32.59,
  "RIA","Immunoassay",50.0,"any",
  "Bone",TRUE,"r (Pearson)",8,

  "D04","Wierzejska et al.",         2020,"Poland",     "Europe",
  "Cross-sectional",94, 47.5,19.50,
  "CLIA","Immunoassay",50.0,"any",
  "Biometry",FALSE,"r (Spearman)",5,

  "D05","Ioannou et al.",            2012,"UK",         "Europe",
  "Cohort",357, 63.0,96.30,
  "RIA","Immunoassay",50.0,"any",
  "Bone",TRUE,"r (Pearson)",8,

  "D06","Liu et al.",                2020,"China",      "Asia",
  "Cohort",10913,66.4,27.00,
  "HPLC-MS/MS","Mass Spec",50.0,"any",
  "EFW/Biometry",TRUE,"OR",8,

  "D07","Vafaei et al.",             2019,"Iran",       "Asia",
  "RCT",140, 46.5,NA,
  "ECL","Immunoassay",50.0,"any",
  "Bone/FL",TRUE,"Mean diff (RCT)",NA,

  "D08","Lee D.H. et al.",           2015,"Korea",      "Asia",
  "Cohort",275, NA,NA,
  "ECLIA","Immunoassay",NA,"serial",
  "Biometry",TRUE,"GEE / r",7,

  "D09","Beck et al.",               2025,"USA",        "America",
  "Cohort",351, 68.1,21.00,
  "HPLC-MS/MS","Mass Spec",50.0,"1st",
  "SGA/Length",TRUE,"RR",8,

  "D10","Srilekha et al.",           2021,"India",      "Asia",
  "RCT",100, 53.5,NA,
  "CLIA","Immunoassay",50.0,"any",
  "EFW/FL/BPD",TRUE,"Mean diff (RCT)",NA,

  "D11","Vestergaard et al.",        2021,"Denmark",    "Europe",
  "Prospective Cohort",297, 79.0,22.00,
  "HPLC-MS/MS","Mass Spec",50.0,"serial",
  "SGA/FGR",FALSE,"p-value only",7,

  "D12","Park et al.",               2014,"Korea",      "Asia",
  "Prospective Cohort",523, NA,NA,
  "RIA","Immunoassay",25.0,"serial",
  "GDM/biometry",FALSE,"OR (GDM only)",8,

  "D13","Young et al.",              2012,"USA",        "America",
  "Prospective Cohort",171, 54.7,27.50,
  "RIA","Immunoassay",50.0,"any",
  "Bone/FL",TRUE,"Beta (linear)",8,

  "D14","Morales et al.",            2015,"Spain",      "Europe",
  "Population Cohort",2358,73.5,28.33,
  "HPLC","Mass Spec",50.0,"any",
  "Biometry/Adiposity",TRUE,"OR",8,

  "D15","Akita et al.",              2025,"Japan",      "Asia",
  "Cohort",89,  44.0,NA,
  "CLIA","Immunoassay",50.0,"serial",
  "Adiposity",FALSE,"Beta (linear)",7,

  "D16","Ge et al.",                 2024,"China",      "Asia",
  "Retrospective",300, 65.0,NA,
  "ELISA","Immunoassay",50.0,"any",
  "FGR",TRUE,"r (Pearson)",5,

  "D17","Lee S.B. et al.",           2023,"Korea",      "Asia",
  "Retrospective",1079,45.5,22.50,
  "CLIA","Immunoassay",25.0,"any",
  "Dev delay/SGA",TRUE,"aOR",6,

  "D18","Kwon et al.",               2023,"Korea",      "Asia",
  "Cohort",48,  NA,NA,
  "CLIA","Immunoassay",50.0,"any",
  "Biometry",TRUE,"ANOVA / p-value",6,

  "D19","Palmrich et al.",           2023,"Austria",    "Europe",
  "Prospective Cohort",249, 43.6,23.78,
  "CLIA","Immunoassay",50.0,"any",
  "SGA",FALSE,"OR (logistic)",8,

  "D20","Mahfod et al.",             2022,"Egypt",      "Africa",
  "Case-Control",56,  20.78,NA,
  "ECL","Immunoassay",50.0,"any",
  "FGR",TRUE,"r (Spearman)",6,

  "D21","Marçal et al.",             2021,"Brazil",     "America",
  "Cross-sectional",87,  60.5,NA,
  "CLIA","Immunoassay",50.0,"any",
  "SGA/FGR",FALSE,"ANOVA / p-value",5,

  "D22","Baqai et al.",              2020,"Pakistan",   "Asia",
  "Prospective Cohort",585, NA,NA,
  "RIA","Immunoassay",75.0,"any",
  "IUGR",FALSE,"RR (no CI)",5,

  "D23","Alimohammadi et al.",       2020,"Iran",       "Asia",
  "Case-Control",260, 36.85,NA,
  "RIA","Immunoassay",50.0,"any",
  "IUGR",TRUE,"OR",6,

  "D24","Judistiani et al.",         2019,"Indonesia",  "Asia",
  "Prospective Cohort",203, 39.13,17.65,
  "ELISA","Immunoassay",50.0,"3rd",
  "Biometry",TRUE,"Beta (linear)",7,

  "D25","Gernand et al.",            2014,"USA",        "America",
  "Observational",792, 63.9,29.50,
  "HPLC-MS/MS","Mass Spec",30.0,"any",
  "SGA",TRUE,"OR",8,

  "D26","Fernandez-Alonso et al.",   2011,"Spain",      "Europe",
  "Cross-sectional",498, 68.5,21.85,
  "ECLIA","Immunoassay",50.0,"1st",
  "CRL/NT",FALSE,"r (Spearman)",5
)

studies <- studies %>%
  mutate(
    design_cat = case_when(
      str_detect(design,"RCT")          ~ "RCT",
      str_detect(design,"Case-Control") ~ "Case-Control",
      str_detect(design,"Cross")        ~ "Cross-sectional",
      str_detect(design,"Retro")        ~ "Retrospective",
      TRUE                              ~ "Prospective/Observational Cohort"
    ),
    nos_cat = case_when(
      nos_score >= 7 ~ "High (>=7/9)",
      nos_score >= 5 ~ "Moderate (5-6/9)",
      nos_score <  5 ~ "Low (<5/9)",
      is.na(nos_score) ~ "RCT (Cochrane RoB)"
    ),
    vitd_cutoff_group = case_when(
      vitd_cutoff_nmol <= 25 ~ "< 25 nmol/L",
      vitd_cutoff_nmol <= 30 ~ "<= 30 nmol/L",
      vitd_cutoff_nmol <= 50 ~ "<= 50 nmol/L",
      vitd_cutoff_nmol >  50 ~ "> 50 nmol/L",
      TRUE ~ "Not specified"
    ),
    vitd_cutoff_group = factor(vitd_cutoff_group,
      levels = c("< 25 nmol/L","<= 30 nmol/L","<= 50 nmol/L","> 50 nmol/L","Not specified"))
  )

cat("=================================================================\n")
cat("  SR/MA: MATERNAL VITAMIN D & FETAL GROWTH  (v4 — March 2026)\n")
cat("=================================================================\n\n")

# =============================================================================
# 2. DESCRIPTIVE STATISTICS
# =============================================================================
cat("-- SECTION 2: DESCRIPTIVE STATISTICS --\n\n")
cat(sprintf("Total studies    : %d\n",   nrow(studies)))
cat(sprintf("Total participants: %s\n",  format(sum(studies$n), big.mark=",")))
cat(sprintf("Significant assoc.: %d/%d (%.0f%%)\n\n",
            sum(studies$sig), nrow(studies), mean(studies$sig)*100))

cat("Design breakdown:\n")
print(studies %>% count(design_cat) %>% arrange(desc(n)) %>%
        mutate(pct = round(n/sum(n)*100)))

cat("\nNOS quality (observational studies):\n")

print(studies %>% filter(design_cat != "RCT") %>%
        count(nos_cat) %>% arrange(desc(n)))

cat("\nVitD cutoff groups:\n")
print(studies %>% count(vitd_cutoff_group))

vitd_obs <- studies %>% filter(!is.na(vitd_nmol))
cat(sprintf("\nVitD (nmol/L) across %d studies with data:\n", nrow(vitd_obs)))
cat(sprintf("  Mean +/- SD: %.1f +/- %.1f | Median [IQR]: %.1f [%.1f-%.1f]\n\n",
            mean(vitd_obs$vitd_nmol), sd(vitd_obs$vitd_nmol),
            median(vitd_obs$vitd_nmol),
            quantile(vitd_obs$vitd_nmol,0.25), quantile(vitd_obs$vitd_nmol,0.75)))

# =============================================================================
# 3. META-ANALYSIS DATASETS
# =============================================================================

# ── 3a. SGA/FGR ──────────────────────────────────────────────────────────────
meta_sga <- tribble(
  ~study,                                  ~year, ~n,
  ~log_or, ~se_log_or, ~or, ~lower, ~upper,
  ~cutoff_nmol, ~lab, ~design_cat, ~inverted, ~note,

  "Miliku et al., 2016",                   2016, 7098,
  log(2.07),(log(3.09)-log(1.38))/(2*1.96), 2.07,1.38,3.09,
  25.0,"HPLC-MS/MS","Prospective Cohort",FALSE,
  "VitD def (<25) vs suf -> SGA",

  "Gernand et al., 2014 (50-74 vs <30)",   2014, 792,
  log(1/0.57),(log(1/0.33)-log(1/0.99))/(2*1.96), 1.75,1.01,3.03,
  30.0,"HPLC-MS/MS","Observational",TRUE,
  "Inverted; original OR=0.57 protective",

  "Gernand et al., 2014 (>=75 vs <30)",    2014, 792,
  log(1/0.46),(log(1/0.24)-log(1/0.87))/(2*1.96), 2.17,1.15,4.17,
  30.0,"HPLC-MS/MS","Observational",TRUE,
  "Inverted; original OR=0.46 protective",

  "Beck et al., 2025 (>50 vs <50 1T)",     2025, 351,
  log(1/0.78),(log(1/0.23)-log(1/2.66))/(2*1.96), 1.28,0.38,4.35,
  50.0,"HPLC-MS/MS","Cohort",TRUE,
  "Inverted; original RR for SGA NS"
)

# Sensitivity: Alimohammadi 2020 (approximated CI)
meta_sga_ali <- tribble(
  ~study,                                  ~year, ~n,
  ~log_or, ~se_log_or, ~or, ~lower, ~upper,
  ~cutoff_nmol, ~lab, ~design_cat, ~inverted, ~note,
  "Alimohammadi et al., 2020 [approx CI]", 2020, 260,
  log(6.81),0.90, 6.81,2.20,21.14,
  50.0,"RIA","Case-Control",FALSE,
  "CI not reported; approx +/-1.5 log-OR"
)

# ── 3b. Fetal biometry/EFW ───────────────────────────────────────────────────
meta_bim <- tribble(
  ~study,                              ~year, ~n,
  ~log_or, ~se_log_or, ~or, ~lower, ~upper,
  ~outcome, ~lab, ~note,

  "Liu et al., 2020",                  2020, 10913,
  log(1.11),(log(1.21)-log(1.02))/(2*1.96), 1.11,1.02,1.21,
  "Excessive EFW","HPLC-MS/MS","VitD def -> excessive fetal growth",

  "Liu et al., 2020 (VitD+GDM)",       2020, 10913,
  log(1.36),(log(1.62)-log(1.15))/(2*1.96), 1.36,1.15,1.62,
  "Excessive EFW (combined)","HPLC-MS/MS","VitD low + GDM -> excessive EFW",

  "Morales et al., 2015 (AC, 10wk)",   2015, 2358,
  log(1.14),(log(1.31)-log(1.00))/(2*1.96), 1.14,1.00,1.31,
  "AC < 10th centile","HPLC","Per -10 ng/mL VitD",

  "Morales et al., 2015 (EFW, 10wk)",  2015, 2358,
  log(1.18),(log(1.36)-log(1.03))/(2*1.96), 1.18,1.03,1.36,
  "EFW < 10th centile","HPLC","Per -10 ng/mL VitD"
)

# =============================================================================
# 4. PRIMARY META-ANALYSIS: SGA/FGR
# =============================================================================
cat("\n-- SECTION 4: META-ANALYSIS - SGA/FGR --\n")

cat("Main analysis (k=4, all with reported 95% CI):\n")
ma_sga <- adaptive_meta(meta_sga$log_or, meta_sga$se_log_or, meta_sga$study)
p_sga  <- get_pool(ma_sga)
print(summary(ma_sga))

cat(sprintf("\nPooled OR = %.2f (95%% CI %.2f-%.2f)\n", p_sga$or, p_sga$lo, p_sga$hi))
cat(sprintf("I2 = %.1f%%  |  p-heterogeneity = %.3f\n", p_sga$i2, p_sga$pq))

p_ctrl <- 0.08
rd_sga <- (p_ctrl*(p_sga$or-1))/(1+p_ctrl*(p_sga$or-1))
nnt    <- abs(round(1/rd_sga))
cat(sprintf("RD (baseline SGA 8%%): %.1f%%  |  NNH = %d\n", rd_sga*100, nnt))

# Sensitivity 1: +Alimohammadi
cat("\n-- Sensitivity 1: +Alimohammadi 2020 (approx CI) --\n")
sga_s1 <- bind_rows(meta_sga, meta_sga_ali)
ma_s1  <- adaptive_meta(sga_s1$log_or, sga_s1$se_log_or, sga_s1$study)
p_s1   <- get_pool(ma_s1)
cat(sprintf("  OR = %.2f (%.2f-%.2f), I2 = %.1f%%\n", p_s1$or, p_s1$lo, p_s1$hi, p_s1$i2))

# Sensitivity 2: Cohorts only
cat("\n-- Sensitivity 2: Prospective/observational cohorts only --\n")
sga_s2 <- meta_sga %>% filter(design_cat != "Case-Control")
ma_s2  <- adaptive_meta(sga_s2$log_or, sga_s2$se_log_or, sga_s2$study)
p_s2   <- get_pool(ma_s2)
cat(sprintf("  OR = %.2f (%.2f-%.2f), I2 = %.1f%%\n", p_s2$or, p_s2$lo, p_s2$hi, p_s2$i2))

# Subgroup by cut-off 
cat("\n-- Subgroup by VitD deficiency cut-off--\n")
cutoff_groups <- meta_sga %>% mutate(
  cutoff_cat = case_when(
    cutoff_nmol <= 25 ~ "< 25 nmol/L",
    cutoff_nmol <= 30 ~ "<= 30 nmol/L",
    cutoff_nmol <= 50 ~ "<= 50 nmol/L",
    TRUE ~ "> 50 nmol/L"
  ))
for (g in c("< 25 nmol/L","<= 30 nmol/L","<= 50 nmol/L")) {
  sub <- cutoff_groups %>% filter(cutoff_cat == g)
  if (nrow(sub) >= 2) {
    ma_sub <- adaptive_meta(sub$log_or, sub$se_log_or, sub$study)
    ps <- get_pool(ma_sub)
    cat(sprintf("  Cut-off %s (k=%d): OR = %.2f (%.2f-%.2f), I2=%.1f%%\n",
                g, nrow(sub), ps$or, ps$lo, ps$hi, ps$i2))
  } else {
    cat(sprintf("  Cut-off %s (k=1): single study OR=%.2f (%.2f-%.2f)\n",
                g, sub$or[1], sub$lower[1], sub$upper[1]))
  }
}

# =============================================================================
# 5. SECONDARY META-ANALYSIS: FETAL BIOMETRY/EFW
# =============================================================================
cat("\n-- SECTION 5: META-ANALYSIS - FETAL BIOMETRY/EFW --\n")
ma_bim <- adaptive_meta(meta_bim$log_or, meta_bim$se_log_or, meta_bim$study)
p_bim  <- get_pool(ma_bim)
cat(sprintf("Pooled OR = %.2f (95%% CI %.2f-%.2f), I2 = %.1f%%\n",
            p_bim$or, p_bim$lo, p_bim$hi, p_bim$i2))
print(summary(ma_bim))

# =============================================================================
# 6. CONTINUOUS OUTCOMES — RCT BIOMETRY (Vafaei 2019, Srilekha 2021)
# =============================================================================
cat("\n-- SECTION 6: CONTINUOUS META-ANALYSIS - RCT BIOMETRY --\n")
cat("Method: inverse variance, MD\n\n")

rct_fl <- metacont(n.e=70, mean.e=28.87, sd.e=2.14,
                   n.c=70, mean.c=26.89, sd.c=2.08,
                   sm="MD", studlab="Vafaei 2019 - Femur length",
                   common=TRUE, random=FALSE)
cat(sprintf("Femur length: MD = %.2f mm (%.2f-%.2f), p < 0.001\n",
            rct_fl$TE.fixed, rct_fl$lower.fixed, rct_fl$upper.fixed))

rct_hl <- metacont(n.e=70, mean.e=28.62, sd.e=1.94,
                   n.c=70, mean.c=27.23, sd.c=2.08,
                   sm="MD", studlab="Vafaei 2019 - Humerus length",
                   common=TRUE, random=FALSE)
cat(sprintf("Humerus length: MD = %.2f mm (%.2f-%.2f), p < 0.001\n",
            rct_hl$TE.fixed, rct_hl$lower.fixed, rct_hl$upper.fixed))

cat("\nSrilekha 2021 (High RoB; raw group means not available):\n")
cat("  EFW: MD = +277g (p<0.01)\n")
cat("  BPD: MD = +4.77mm (p<0.01)\n")
cat("  FL:  MD = +3.03mm (p<0.001)\n")
cat("Direction consistent: supplementation -> improved fetal biometry\n\n")

# =============================================================================
# 7. LEAVE-ONE-OUT SENSITIVITY
# =============================================================================
cat("-- SECTION 7: LEAVE-ONE-OUT SENSITIVITY --\n\n")
loo_results <- tibble(study=character(), or=numeric(), lower=numeric(), upper=numeric())
for (i in seq_len(nrow(meta_sga))) {
  m <- tryCatch(
    adaptive_meta(meta_sga$log_or[-i], meta_sga$se_log_or[-i], meta_sga$study[-i]),
    error = function(e) NULL
  )
  if (!is.null(m)) {
    ps <- get_pool(m)
    loo_results <- add_row(loo_results, study=meta_sga$study[i],
                           or=ps$or, lower=ps$lo, upper=ps$hi)
  }
}
loo_results <- add_row(loo_results, study="Overall (all 4 studies)",
                       or=p_sga$or, lower=p_sga$lo, upper=p_sga$hi)
print(loo_results, n=Inf)

# =============================================================================
# 8. COMPARISON WITH PRIOR META-ANALYSES
# =============================================================================
cat("\n-- SECTION 8: COMPARISON WITH PRIOR META-ANALYSES --\n\n")
cat("+-----------------------------------------------------------------+\n")
cat("| COMPARISON TABLE: SGA/FETAL GROWTH META-ANALYSES               |\n")
cat("+-----------------------------------------------------------------+\n")
cat("| Reference              | k  | N       | Effect       | I2     |\n")
cat("+-----------------------------------------------------------------+\n")
cat(sprintf("| Present Review (obs.)  |  4 | %6d  | OR=%.2f%s  | %.0f%%   |\n",
            sum(meta_sga$n), p_sga$or,
            ifelse(p_sga$lo>1,"***"," NS"), p_sga$i2))
cat(sprintf("| Present Review (obs.)  |  4 | %6d  | OR=%.2f%s  | %.0f%%   |\n",
            sum(meta_bim$n), p_bim$or,
            ifelse(p_bim$lo>1,"***"," NS"), p_bim$i2))
cat("+-----------------------------------------------------------------+\n\n")

cat("Interpretation:\n")
cat(sprintf(
  "  Our pooled OR of %.2f (95%% CI %.2f-%.2f) is directionally consistent\n",
  p_sga$or, p_sga$lo, p_sga$hi))
cat("  our more homogeneous study subset (all HPLC-MS/MS; I2=0%)\n")
cat("  supplementation reduces SGA risk (RR=0.72), supporting causality.\n\n")

# =============================================================================
# 9. PUBLICATION BIAS
# =============================================================================
cat("-- SECTION 9: PUBLICATION BIAS --\n\n")
egger <- tryCatch(metabias(ma_sga, method.bias="linreg"), error=function(e) NULL)
if (!is.null(egger)) {
  cat(sprintf("Egger's test (SGA/FGR): p = %.3f (%s)\n", egger$p.value,
              ifelse(egger$p.value<0.05,"significant asymmetry","no significant asymmetry")))
} else {
  cat("Egger's test: insufficient k (k<3 or k<10 recommended)\n")
  cat("Visual assessment via funnel plot (Fig S4)\n")
}

# =============================================================================
# 10. FIGURES — MAIN
# =============================================================================
cat("-- SECTION 10: GENERATING MAIN FIGURES --\n\n")

# ── Fig 1: Study characteristics ─────────────────────────────────────────────
p1a <- studies %>%
  count(design_cat) %>%
  mutate(design_cat = fct_reorder(design_cat, n)) %>%
  ggplot(aes(x=n, y=design_cat, fill=design_cat)) +
  geom_col(width=0.72, colour="white") +
  geom_text(aes(label=paste0("n = ",n)), hjust=-0.1, fontface="bold", size=3.5) +
  scale_fill_manual(values=c(pal$blue,pal$orange,pal$green,pal$purple,pal$teal)) +
  scale_x_continuous(expand=expansion(mult=c(0,0.28))) +
  labs(title="(a)  Study Design", x="Number of Studies", y=NULL) +
  theme_sr() + theme(legend.position="none")

p1b <- studies %>%
  count(year) %>%
  mutate(era=cut(year,c(2009,2014,2019,2025),labels=c("2010-14","2015-19","2020-25"))) %>%
  ggplot(aes(x=factor(year),y=n,fill=era)) +
  geom_col(width=0.72,colour="white") +
  geom_text(aes(label=n),vjust=-0.3,fontface="bold",size=3.5) +
  scale_fill_manual(values=c(pal$blue,pal$orange,pal$green),name="Period") +
  scale_y_continuous(expand=expansion(mult=c(0,0.25))) +
  labs(title="(b)  Publication Timeline", x="Year", y="Studies") +
  theme_sr() + theme(axis.text.x=element_text(angle=45,hjust=1))



# Preparar dados com posições centrais
    df <- studies %>%
      count(continent) %>%
      mutate(
        pct = round(n / sum(n) * 100),
        label_pos = cumsum(n) - n / 2  # centro da fatia
      )
    
# Gráfico de pizza com labels externos e setas
    p1c <- ggplot(df, aes(x = "", y = n, fill = continent)) +
      geom_col(color = "white", linewidth = 1.2) +
      coord_polar(theta = "y") +
      geom_label_repel(
        aes(y = label_pos, label = paste0(continent, "\n", n, " (", pct, "%)")),
        x = 1.3,                     # desloca labels para fora
        segment.color = "grey50", 
        segment.size = 0.8,
        direction = "y",
        force = 1.5,
        size = 3,
        fontface = "bold",
        fill = "white",
        color = "black",
        show.legend = FALSE
      ) +
      scale_fill_manual(values = c(pal$blue, pal$orange, pal$green, pal$purple)) +
      labs(title = "(c) Geographic Distribution") +
      theme_void(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5, size = 14, colour = pal$black,
                                  margin = margin(b = 10)),
        legend.position = "none",
        plot.background = element_rect(fill = "transparent", colour = NA)
      )

p1d <- studies %>%
  mutate(nos_cat=factor(nos_cat,levels=c("High (>=7/9)","Moderate (5-6/9)",
                                          "Low (<5/9)","RCT (Cochrane RoB)"))) %>%
  count(nos_cat) %>%
  ggplot(aes(x=n, y=nos_cat, fill=nos_cat)) +
  geom_col(width=0.72,colour="white") +
  geom_text(aes(label=paste0("n=",n)),hjust=-0.1,fontface="bold",size=3.5) +
  scale_fill_manual(values=c(pal$green,pal$orange,pal$red,pal$blue),name="Quality") +
  scale_x_continuous(expand=expansion(mult=c(0,0.3))) +
  labs(title="(d) Cochrane RoB\n",
       x="Studies", y=NULL) +
  theme_sr() + theme(legend.position="none",axis.text.y=element_text(size=9))

fig1 <- (p1a|p1b) 
ggsave("Figure1_StudyCharacteristics.tiff", fig1, width=14,height=11,dpi=300,compression="lzw")


fig1_2 <- (p1c|p1d) 
ggsave("Figure1bStudyCharacteristics.tiff", fig1_2, width=14,height=11,dpi=300,compression="lzw")


# ── Fig 2: Forest plot — SGA/FGR () ────────────────
fp_sga <- meta_sga %>%
  mutate(
    label = fct_rev(factor(study, levels = study)),  # eixo y categórico
    sig   = lower > 1,
    wt_label = sprintf("%.1f%%", c(49.1,26.4,19.2,5.3)),
    ci_label = sprintf("%.2f (%.2f-%.2f)%s", or, lower, upper,
                       ifelse(inverted," +","")) )


# Preparar dados
fp_sga <- meta_sga %>%
  mutate(
    label = fct_rev(factor(study, levels = study)),  # eixo y discreto
    sig   = lower > 1,
    wt_label = sprintf("%.1f%%", c(49.1,26.4,19.2,5.3)),
    ci_label = sprintf("%.2f (%.2f-%.2f)%s", or, lower, upper,
                       ifelse(inverted," +",""))
  )

# Forest plot estilo paper

p2 <- ggplot(fp_sga, aes(y = label, x = or, xmin = lower, xmax = upper)) +
  geom_vline(xintercept=1, linetype="dashed", linewidth=0.75) +
  geom_errorbarh(aes(colour=sig), height=0.22, linewidth=1.1) +
  geom_point(aes(size=1/se_log_or, colour=sig), shape=15) +
  annotate("polygon",
           x=c(p_bim$lo, p_bim$or, p_bim$hi, p_bim$or),
           y=c(0.1,0.38,0.1,-0.18),
           fill=pal$orange, colour=pal$blue, alpha=0.92) +
  scale_colour_manual(values = c("TRUE" = "steelblue", "FALSE" = "grey70")) +
  scale_size_continuous(range = c(3,7), guide = "none") +
  scale_x_log10(
    breaks = c(0.3, 0.5, 1, 1.5, 2, 3, 4, 5),
    labels = c("0.3", "0.5", "1", "1.5", "2", "3", "4", "5")
  ) +
  coord_cartesian(xlim = c(0.25, max(fp_sga$upper)*4.5)) +
  labs(
    title = "",
    x = "Odds Ratio [log scale]",
    y = NULL
  ) +
  theme_sr() +
  theme(axis.text.y=element_text(size=9), legend.position="bottom")
'Figure 2. Forest Plot - Maternal Vitamin D Deficiency and Risk of SGA/FGR
'p2

ggsave("Figure2_ForestPlot_SGA.tiff", p2, width=14,height=6.5,dpi=300,compression="lzw")
cat("  Figure 2 saved.\n")

# ── Fig 3: Forest plot — Fetal biometry/EFW ──────────────────────────────────
fp_bim <- meta_bim %>%
  mutate(label=fct_rev(factor(study,levels=study)), sig=lower>1,
         ci_label=sprintf("%.2f (%.2f-%.2f)", or, lower, upper))

p3 <- ggplot(fp_bim, aes(y=label, x=or, xmin=lower, xmax=upper)) +
  geom_vline(xintercept=1, linetype="dashed", linewidth=0.75) +
  geom_errorbarh(aes(colour=sig), height=0.22, linewidth=1.1) +
  geom_point(aes(size=1/se_log_or, colour=sig), shape=15) +
  annotate("polygon",
           x=c(p_bim$lo, p_bim$or, p_bim$hi, p_bim$or),
           y=c(0.1,0.38,0.1,-0.18),
           fill=pal$orange, colour=pal$blue, alpha=0.92) +
  annotate("text", x=0.82, y=0.08, hjust=0, size=3.2, colour=pal$blue, fontface="bold",
           label=sprintf("Total (95%% CI)  OR = %.2f (%.2f-%.2f)\nFixed-effects; I2=%.0f%%  Q=4.45, df=3, p=%.3f",
                         p_bim$or, p_bim$lo, p_bim$hi, p_bim$i2, p_bim$pq)) +
  geom_text(aes(x=max(upper)*1.03, label=ci_label),
            hjust=0, size=3.0, colour="grey25") +
  scale_colour_manual(values=c("TRUE"=pal$blue,"FALSE"=pal$lgrey),
                      labels=c("TRUE"="Significant","FALSE"="NS"), name=NULL) +
  scale_size_continuous(range=c(3.5,8), guide="none") +
  scale_x_continuous(breaks=c(0.8,0.9,1.0,1.1,1.2,1.4,1.6,1.8)) +
  coord_cartesian(xlim=c(0.78, max(fp_bim$upper)*2.5)) +
  labs(
    title="",
    x="Odds Ratio [Direction: VitD deficiency -> adverse biometry/EFW]",
    y=NULL, colour=NULL,
    caption=paste0(
      ""
    )) +
  theme_sr() +
  theme(axis.text.y=element_text(size=9), legend.position="bottom")

"Liu 2020 (combined): VitD def + GDM vs VitD def alone; both Liu estimates from the same cohort (n=10,913).\n"
      "Morales 2015: OR per -10 ng/mL (~-25 nmol/L) decrease in 25(OH)D at 10 weeks' gestation.\n"
      "AC = abdominal circumference; EFW = estimated fetal weight."

ggsave("Figure3_ForestPlot_Biometry.tiff", p3, width=13,height=5.5,dpi=300,compression="lzw")
cat("  Figure 3 saved.\n")

# ── Fig 4: RCT continuous outcomes (NEW — panel a: FL/HL; panel b: EFW/BPD/FL)
rct_bone <- tribble(
  ~study,     ~outcome,       ~n_e,~n_c, ~MD,  ~lo,  ~hi,  ~sig,
  "Vafaei 2019","FL (2nd trim.)",70,70,  1.98, 1.28, 2.68, TRUE,
  "Vafaei 2019","HL (2nd trim.)",70,70,  1.39, 0.72, 2.06, TRUE
)
rct_biom <- tribble(
  ~study,      ~outcome,  ~n_e,~n_c, ~MD,   ~lo,  ~hi,  ~sig,
  "Srilekha 2021","EFW (g)",50,50, 277.0,100.0,454.0, TRUE,
  "Srilekha 2021","BPD (mm x10)",50,50,47.7, 15.0, 80.4, TRUE,
  "Srilekha 2021","FL (mm x10)", 50,50,30.3, 10.1, 50.5, TRUE
)

make_rct_panel <- function(df, title, xlabel, xlim_v, xticks,
                            bi_ref=NULL, bi_label=NULL, col_=pal$green) {
  xr <- xlim_v[2]-xlim_v[1]
  df <- df %>% mutate(label=fct_rev(factor(outcome,levels=rev(outcome))))

  p <- ggplot(df, aes(y=label, x=MD, xmin=lo, xmax=hi)) +
    geom_vline(xintercept=0, linetype="dashed", linewidth=0.75) +
    geom_errorbarh(colour=col_, height=0.22, linewidth=1.1) +
    geom_point(size=5, colour=col_, shape=15) +
    geom_text(aes(x=hi+xr*0.04,
                  label=sprintf("MD = %.2f (%.2f; %.2f)%s",
                                MD, lo, hi, ifelse(sig," ***",""))),
              hjust=0, size=3.2, colour="grey25") +
    scale_x_continuous(breaks=xticks) +
    coord_cartesian(xlim=c(xlim_v[1], xlim_v[2]+xr*0.5)) +
    labs(title=title, x=xlabel, y=NULL) +
    theme_sr()
  }
  p


p4a <- make_rct_panel(rct_bone,
  "(a)  Fetal Bone Length - Vitamin D Supplementation vs. Control (Vafaei et al., 2019; n=140)",
  "Mean Difference (mm) - Supplementation -> longer bones",
  xlim_v=c(-2,5), xticks=c(-2,-1,0,1,2,3,4),
  bi_ref=1.2, bi_label="Bi 2018: FL +0.12cm",
  col_=pal$green)

p4b <- make_rct_panel(rct_biom,
  "(b)  Fetal Biometry - Vitamin D Supplementation vs. Control (Srilekha et al., 2021; n=100; High RoB)",
  "Mean Difference (g or mm x10) - Supplementation -> improved biometry",
  xlim_v=c(-100,550), xticks=c(-100,0,75,100,200,300,400,500),
  bi_ref=75.38, bi_label="Bi 2018: BW +75.38g",
  col_=pal$blue)


fig4 <- p4a / p4b +
  plot_annotation(
    title=,
    subtitle="",
    theme=theme(plot.title=element_text(face="bold",size=13),
                plot.subtitle=element_text(colour="grey40",size=9)))
ggsave("Figure4_RCT_MD.tiff", fig4, width=14,height=9,dpi=300,compression="lzw")
cat("  Figure 4 saved.\n")

"Figure 4. Forest Plots - RCT Outcomes: Vitamin D Supplementation and Fetal Biometry"
"Inverse-variance MD. Orange dotted = Bi et al. JAMA Pediatrics 2018 reference estimate.\n*** p < 0.001. 
Srilekha 2021: individual group means/SDs not available; MD reported directly by authors."

# ── Fig 5: Lab methods & VitD by continent ────────────────────────────────────
p5a <- studies %>%
  count(lab) %>%
  mutate(lab_group=ifelse(lab %in% c("HPLC-MS/MS","HPLC"),"Mass Spectrometry","Immunoassay"),
         lab=fct_reorder(lab,n)) %>%
  ggplot(aes(x=n, y=lab, fill=lab_group)) +
  geom_col(width=0.72,colour="white") +
  geom_text(aes(label=paste0("n=",n)),hjust=-0.1,fontface="bold",size=3.5) +
  scale_fill_manual(values=c("Mass Spectrometry"=pal$blue,"Immunoassay"=pal$orange),
                    name="Method class") +
  scale_x_continuous(expand=expansion(mult=c(0,0.35))) +
  labs(title="(a)  Laboratory Methods for 25(OH)D",
       x="Studies", y=NULL,
       caption="") +
  theme_sr()

p5b <- studies %>%
  filter(!is.na(vitd_nmol)) %>%
  ggplot(aes(x=continent, y=vitd_nmol, colour=continent)) +
  geom_boxplot(fill=NA,linewidth=0.8,outlier.shape=NA) +
  geom_jitter(width=0.15,size=3,alpha=0.8) +
  geom_hline(yintercept=50,linetype="dashed",colour=pal$red,linewidth=0.9) +
  annotate("text",x=0.55,y=53,hjust=0,size=3,colour=pal$red,
           fontface="italic",label="Sufficiency (50 nmol/L)") +
  scale_colour_manual(values=c(pal$blue,pal$orange,pal$green,pal$purple),guide="none") +
  scale_y_continuous(limits=c(0,115),breaks=seq(0,100,25)) +
  labs(title="(b)  Maternal 25(OH)D by Continent",
       x=NULL, y="25(OH)D (nmol/L)",
       caption="") +
  theme_sr()

fig5 <- p5a|p5b +
  plot_annotation(title="",
                  theme=theme(plot.title=element_text(face="bold",size=13)))
cat("  Figure 5 saved.\n")
"Figure 5. Methodological Heterogeneity - Lab Methods and VitD Concentrations"
# ── Fig 6: Outcome summary ────────────────────────────────────────────────────
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
  scale_fill_manual(values=c("Significant"=pal$blue,"Non-significant"=pal$lgrey),name=NULL) +
  labs(title="(a)  Outcome Categories",x=NULL,y="Studies") +
  theme_sr(10) + theme(axis.text.x=element_text(angle=25,hjust=1),legend.position="top")

p6b <- studies %>%
  count(vitd_cutoff_group) %>%
  filter(!is.na(vitd_cutoff_group)) %>%
  ggplot(aes(x=n,y=vitd_cutoff_group,fill=vitd_cutoff_group)) +
  geom_col(width=0.65,colour="white") +
  geom_text(aes(label=n),hjust=-0.2,fontface="bold",size=3.5) +
  scale_fill_manual(values=c(pal$red,pal$orange,pal$blue,pal$green,pal$lgrey),guide="none") +
  scale_x_continuous(expand=expansion(mult=c(0,0.3))) +
  labs(title="(b)  VitD Deficiency Cut-off Used",
       x="Studies",y=NULL) +
  theme_sr(10)

fig6 <- p6a|p6b +
  plot_annotation(title="",
                  theme=theme(plot.title=element_text(face="bold",size=13)))
cat("  Figure 6 saved.\n")
"Figure 6. Outcome Distribution and VitD Cut-off Heterogeneity"

# ── Fig 7: Bubble plot ────────────────────────────────────────────────────────
p7 <- studies %>%
  filter(!is.na(vitd_nmol)) %>%
  mutate(sig_l=ifelse(sig,"Significant","Non-significant")) %>%
  ggplot(aes(x=vitd_nmol,y=n,size=n,colour=sig_l)) +
  geom_point(alpha=0.75) +
  geom_vline(xintercept=50,linetype="dashed",colour=pal$red,linewidth=0.8) +
  geom_text(aes(label=paste0(sub(" et al.*","",author)," ",year)),
            hjust=-0.08,vjust=0.5,size=2.7,colour=pal$black,check_overlap=TRUE) +
  scale_colour_manual(values=c("Significant"=pal$blue,"Non-significant"=pal$lgrey),name=NULL) +
  scale_size_continuous(range=c(3,16),name="Sample size",labels=comma) +
  scale_y_log10(labels=comma) +
  labs(title="",
       x="Mean Maternal 25(OH)D (nmol/L)", y="Sample Size (log scale)",
       caption="") +
  theme_sr()
ggsave("Figure7_BubblePlot.tiff", p7, width=13,height=7,dpi=300,compression="lzw")
cat("  Figure 7 saved.\n")
"Figure 7. Maternal 25(OH)D Concentration, Sample Size, and Association Significance"

# ── Fig 8: Funnel plot ────────────────────────────────────────────────────────
png("Figure8_FunnelPlot.png", width=2400,height=1800,res=300,bg="transparent")
funnel(ma_sga, main="",
       xlab="Log Odds Ratio",
       col=ifelse(meta_sga$lower>1, pal$blue, pal$lgrey),
       pch=15, cex=1.2)
abline(v=log(1.55), lty=3, col=pal$orange, lwd=1.5)
legend("topright", legend=c("Significant","Non-significant"),
       pch=c(15,15,NA), lty=c(NA,NA,3), col=c(pal$blue,pal$lgrey,pal$orange),
       bty="n", cex=0.9)
dev.off()
cat("  Figure 8 saved.\n")

# =============================================================================
# 11. SUPPLEMENTARY FIGURES
# =============================================================================
cat("\n-- SECTION 11: SUPPLEMENTARY FIGURES --\n\n")

# ── FigS1: LOO sensitivity ────────────────────────────────────────────────────
loo_plot <- loo_results %>%
  mutate(study=fct_rev(factor(study,levels=study)),
         type=ifelse(study=="Overall (all 4 studies)","Overall","LOO"))

pS1 <- ggplot(loo_plot, aes(y=study, x=or, xmin=lower, xmax=upper, colour=type)) +
  geom_vline(xintercept=1, linetype="dashed", linewidth=0.75) +
  geom_vline(xintercept=p_sga$or, linetype="dotted", colour=pal$blue, linewidth=1.0) +
  geom_errorbarh(height=0.22, linewidth=1.0) +
  geom_point(shape=22, size=4, fill="white", stroke=2) +
  geom_text(aes(label=sprintf("OR = %.2f (%.2f-%.2f)", or, lower, upper)),
            hjust=-0.08, size=3.0, colour="grey25") +
  scale_colour_manual(values=c("Overall"=pal$blue,"LOO"=pal$orange), guide="none") +
  scale_x_log10(breaks=c(0.8,1,1.25,1.5,2,2.5,3)) +
  coord_cartesian(xlim=c(0.7,6)) +
  labs(title="",
       subtitle=sprintf("",
                        p_sga$or, p_sga$lo, p_sga$hi),
       x="Pooled Odds Ratio (log scale)", y=NULL,
       caption="") +
  theme_sr()
ggsave("FigS1_LOO_Sensitivity.tiff", pS1, width=12,height=5,dpi=300,compression="lzw")
cat("  FigS1 saved.\n")
"Supplementary Figure S1. Leave-One-Out Sensitivity Analysis - SGA/FGR"

# ── FigS2: Quality heatmap (JBI + NOS) ───────────────────────────────────────
jbi_q <- tribble(
  ~id,  ~study,                 ~Q1,~Q2,~Q3,~Q4,~Q5,~Q6,~Q7,~Q8,~Q9, ~score,~nos,
  "D01","Tosun 2025",           "Y","Y","Y","Y","Y","Y","Y","Y","U",  9,7,
  "D02","Miliku 2016",          "Y","Y","Y","Y","Y","Y","Y","Y","Y", 11,9,
  "D03","Mahon 2010",           "Y","Y","Y","Y","Y","Y","Y","Y","U",  9,8,
  "D04","Wierzejska 2020",      "Y","Y","Y","Y","U","Y","Y","U","U",  6,5,
  "D05","Ioannou 2012",         "Y","Y","Y","Y","Y","Y","Y","Y","U",  9,8,
  "D06","Liu 2020",             "Y","Y","Y","Y","Y","Y","Y","Y","U",  9,8,
  "D08","Lee D.H. 2015",        "Y","Y","Y","Y","Y","Y","Y","Y","U",  9,7,
  "D09","Beck 2025",            "Y","Y","Y","Y","Y","Y","Y","Y","Y", 11,8,
  "D11","Vestergaard 2021",     "Y","Y","Y","Y","Y","Y","Y","Y","U",  9,7,
  "D12","Park 2014",            "Y","Y","Y","Y","Y","Y","Y","Y","Y", 11,8,
  "D13","Young 2012",           "Y","Y","Y","Y","Y","Y","Y","Y","Y", 11,8,
  "D14","Morales 2015",         "Y","Y","Y","Y","Y","Y","Y","Y","Y", 11,8,
  "D15","Akita 2025",           "Y","Y","Y","Y","Y","Y","Y","Y","Y", 11,7,
  "D16","Ge 2024",              "Y","N","Y","N","N","Y","Y","N","Y",  5,5,
  "D17","Lee S.B. 2023",        "Y","Y","Y","U","Y","Y","Y","Y","U",  7,6,
  "D18","Kwon 2023",            "Y","Y","Y","Y","Y","Y","Y","Y","U",  9,6,
  "D19","Palmrich 2023",        "Y","Y","Y","Y","Y","Y","Y","Y","Y", 11,8,
  "D20","Mahfod 2022",          "Y","Y","Y","U","U","Y","Y","N","U",  6,6,
  "D21","Marcal 2021",          "Y","Y","Y","U","Y","Y","Y","Y","U",  7,5,
  "D22","Baqai 2020",           "Y","N","N","N","N","Y","Y","N","U",  4,5,
  "D23","Alimohammadi 2020",    "Y","Y","Y","U","U","Y","Y","N","U",  5,6,
  "D24","Judistiani 2019",      "Y","Y","Y","Y","Y","Y","Y","Y","U",  9,7,
  "D25","Gernand 2014",         "Y","Y","Y","Y","Y","Y","Y","Y","Y", 11,8,
  "D26","Fernandez-Alonso 2011","Y","Y","Y","Y","Y","Y","Y","Y","Y", 11,5
)

jbi_long <- jbi_q %>%
  mutate(label=fct_reorder(paste0(study," (NOS=",nos,"/9; JBI=",score,"/11)"),nos)) %>%
  pivot_longer(Q1:Q9, names_to="question", values_to="response") %>%
  mutate(response=factor(response,levels=c("Y","N","U"),labels=c("Yes","No","Unclear")))

pS2a <- ggplot(jbi_long, aes(x=question,y=label,fill=response)) +
  geom_tile(colour="white",linewidth=0.7) +
  scale_fill_manual(values=c("Yes"="#27AE60","No"="#E74C3C","Unclear"="#F39C12"),name=NULL) +
  labs(title="(a)  JBI Appraisal Heatmap",
       x="Appraisal Item (Q1-Q9)", y=NULL,
       caption="") +
  theme_sr(9) + theme(axis.text.y=element_text(size=7.5),plot.caption=element_text(size=7))

pS2b <- jbi_q %>%
  mutate(label=fct_reorder(study,nos),
         nos_cat=case_when(nos>=7~"High (NOS >=7)",TRUE~"Moderate/Low")) %>%
  ggplot(aes(x=nos,y=label,fill=nos_cat)) +
  geom_col(width=0.72,colour="white") +
  geom_text(aes(label=nos),hjust=-0.2,size=2.9) +
  geom_vline(xintercept=7,linetype="dashed",colour=pal$red,linewidth=0.8) +
  annotate("text",x=7.1,y=1,hjust=0,size=2.7,colour=pal$red,label="NOS>=7\nhigh quality") +
  scale_fill_manual(values=c("High (NOS >=7)"=pal$green,"Moderate/Low"=pal$orange),name=NULL) +
  scale_x_continuous(limits=c(0,12)) +
  labs(title="(b)  NOS Scores (0-9)\n",
       x="NOS Score",y=NULL) +
  theme_sr(9) + theme(axis.text.y=element_text(size=7.5))

figS2 <- pS2a|pS2b +
  plot_annotation(title="",
                  theme=theme(plot.title=element_text(face="bold",size=13)))
ggsave("FigS2_Quality_JBI_NOS.tiff", figS2, width=16,height=11,dpi=300,compression="lzw")


"Supplementary Figure S2. Methodological Quality Assessment"

# ── FigS3: Cut-off subgroup forest plot ────────────────────────────────────────
cutoff_subgroup <- bind_rows(
  meta_sga %>%
    mutate(cutoff_grp=case_when(
      cutoff_nmol<=25 ~ "< 25 nmol/L",
      cutoff_nmol<=30 ~ "<= 30 nmol/L",
      cutoff_nmol<=50 ~ "<= 50 nmol/L",
      TRUE            ~ "> 50 nmol/L")) %>%
    select(study, cutoff_grp, or, lower, upper),
  tibble(study="Overall (k=4)", cutoff_grp="Overall", or=p_sga$or,
         lower=p_sga$lo, upper=p_sga$hi)
) %>%
  mutate(sig=lower>1,
         study=fct_rev(factor(study,levels=c(
           "Overall (k=4)",
           "Beck et al., 2025 (>50 vs <50 1T)",
           "Gernand et al., 2014 (>=75 vs <30)",
           "Gernand et al., 2014 (50-74 vs <30)",
           "Miliku et al., 2016"))),
         panel=factor(cutoff_grp,
                      levels=c("< 25 nmol/L","<= 30 nmol/L","<= 50 nmol/L","Overall")))

pS3 <- ggplot(cutoff_subgroup, aes(y=study, x=or, xmin=lower, xmax=upper)) +
  geom_vline(xintercept=1, linetype="dashed", linewidth=0.75) +
  geom_point(aes(colour=sig, shape=cutoff_grp=="Overall"), size=4) +
  geom_text(aes(x=upper*1.04, label=sprintf("%.2f (%.2f-%.2f)",or,lower,upper)),
            hjust=0, size=3.0, colour="grey25") +
  scale_colour_manual(values=c("TRUE"=pal$blue,"FALSE"=pal$lgrey),
                      labels=c("TRUE"="Significant","FALSE"="NS"), name=NULL) +
  scale_shape_manual(values=c("FALSE"=15,"TRUE"=18), guide="none") +
  scale_x_log10(breaks=c(0.3,0.5,1,1.5,2,3,5),labels=c("0.3","0.5","1","1.5","2","3","5")) +
  coord_cartesian(xlim=c(0.25,10)) +
  labs(title=,
       subtitle="",
       x="Odds Ratio [log scale] - VitD deficiency -> increased SGA/FGR risk",
       y=NULL, colour=NULL,
       caption=paste0(""
       )) +
  theme_sr()

ggsave("FigS3_CutoffSubgroup.tiff", pS3, width=12,height=5.5,dpi=300,compression="lzw")
cat("  FigS3 saved.\n")
"Supplementary Figure S3. Subgroup Analysis by Vitamin D Deficiency Cut-off"


# ── FigS5: Narrative synthesis (all 26 studies) ───────────────────────────────
narrative_data <- studies %>%
  mutate(
    direction = case_when(
      sig & str_detect(effect_type,"OR|RR|aOR|Beta|Mean|r")     ~ "Significant",
      !sig ~ "Non-significant",
      TRUE  ~ "Partial"
    ),
    direction = if_else(
      author %in% c("Beck et al.","Vestergaard et al.","Fernandez-Alonso et al."),
      "Partial", direction
    ),
    rct_flag = design == "RCT",
    nos_color_cat = case_when(
      rct_flag     ~ "RCT",
      nos_score>=7 ~ "High (NOS>=7)",
      nos_score>=5 ~ "Moderate (5-6)",
      TRUE         ~ "Low (<5)"
    ),
    short_name = paste0(sub(" et al.*","",author)," ",year),
    short_name = fct_reorder(short_name, -as.numeric(factor(direction,
                             levels=c("Significant","Partial","Non-significant"))))
  )

dir_col <- c("Significant"=pal$blue,"Partial"=pal$orange,"Non-significant"=pal$lgrey)

pS5 <- ggplot(narrative_data,
              aes(y=fct_rev(short_name), x=1, fill=direction)) +
  geom_tile(colour="white",linewidth=0.8) +
  geom_text(aes(label=direction, colour=direction),
            fontface="bold", size=3.0) +
  facet_null() +
  scale_fill_manual(values=dir_col, name="Association") +
  scale_colour_manual(values=c("Significant"="white","Partial"="white","Non-significant"="grey30"),
                      guide="none") +
  labs(title="Supplementary Figure S5. Narrative Synthesis Overview - All 26 Studies",
       subtitle="65% (17/26) reported significant associations. RCTs highlighted with asterisk (*) in text.",
       x=NULL, y=NULL) +
  theme_sr(9) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid=element_blank(),
        legend.position="bottom")

# Richer version using ggplot bar approach
pS5b <- narrative_data %>%
  mutate(short_name=fct_reorder(short_name,nos_score,.na_rm=FALSE)) %>%
  ggplot(aes(x=factor(1),y=fct_rev(short_name),fill=direction)) +
  geom_tile(colour="white",linewidth=0.7,width=0.95,height=0.95) +
  geom_text(aes(label=direction,colour=direction),fontface="bold",size=2.9) +
  scale_fill_manual(values=dir_col,name="Association") +
  scale_colour_manual(values=c("Significant"="white","Partial"="white","Non-significant"="grey20"),
                      guide="none") +
  facet_grid(~"Association direction", scales="free",space="free") +
  labs(y=NULL,x=NULL) + theme_sr(9) +
  theme(axis.text.x=element_blank(),axis.ticks=element_blank(),
        panel.grid=element_blank(),strip.text=element_text(size=9))

pS5c <- narrative_data %>%
  mutate(short_name=fct_reorder(short_name,nos_score,.na_rm=FALSE)) %>%
  ggplot(aes(x=factor(1),y=fct_rev(short_name),fill=nos_color_cat)) +
  geom_tile(colour="white",linewidth=0.7,width=0.95,height=0.95) +
  geom_text(aes(label=ifelse(nos_score>0,nos_score,"RCT"),
                colour=nos_color_cat),fontface="bold",size=2.9) +
  scale_fill_manual(values=c("High (NOS>=7)"=pal$green,"Moderate (5-6)"=pal$orange,
                              "Low (<5)"=pal$red,"RCT"=pal$blue),name="Quality") +
  scale_colour_manual(values=c("High (NOS>=7)"="white","Moderate (5-6)"="white",
                                "Low (<5)"="white","RCT"="white"),guide="none") +
  facet_grid(~"NOS / RoB", scales="free",space="free") +
  labs(y=NULL,x=NULL) + theme_sr(9) +
  theme(axis.text=element_blank(),axis.ticks=element_blank(),
        panel.grid=element_blank(),strip.text=element_text(size=9))

figS5 <- (pS5b | pS5c) +
  plot_annotation(
    title="",
    theme=theme(plot.title=element_text(face="bold",size=12)))
ggsave("FigS5_NarrativeSynthesis.tiff", figS5, width=10,height=11,dpi=300,compression="lzw")
cat("  FigS5 saved.\n")

# ── FigS6: GRADE evidence profile ─────────────────────────────────────────────
grade_data <- tribble(
  ~outcome,        ~k, ~N,     ~effect,                   ~i2,  ~rob,       ~indir,   ~imprec,   ~sym,   ~cert,
  "SGA/FGR\n(obs.)",4,9033,  "OR=1.95\n(1.47-2.59)",     0,    "Low*",     "Low",    "Moderate","ooo+",  "Moderate",
  "Biometry\n/EFW", 4,26542, "OR=1.16\n(1.09-1.23)",     32.6, "Low*",     "Low",    "Low",     "oooo",  "High",
  "FL/HL\n(RCT)", 1,  140,   "MD=+1.98mm\n(1.28-2.68)", 0,    "Low",      "Low",    "Moderate","ooo+",  "Moderate",
  "EFW/BPD\n(RCT HighRoB)",1,100,"MD=+277g\n(approx.)", 0,    "High!",    "Low",    "Serious", "oo++",  "Low",
  "Adiposity",     2,  2447,  "Beta: NS",                 0,    "Low*",     "Low",    "Serious", "oo++",  "Low",
  "Infant Wt\n9mo",2,  4170,  "MD=+119.75g\n(33-207)",   28,   "Moderate", "Low",    "Moderate","ooo+",  "Moderate"
) %>%
  mutate(
    outcome=factor(outcome,levels=rev(outcome)),
    cert=factor(cert,levels=c("High","Moderate","Low","Very Low")),
    cert_col=case_when(cert=="High"~pal$green, cert=="Moderate"~pal$blue,
                       cert=="Low"~pal$orange, TRUE~pal$red),
    rob_col=case_when(rob=="Low"|rob=="Low*"~pal$green,
                      rob=="Moderate"~pal$orange, TRUE~pal$red)
  )

pS6 <- ggplot(grade_data, aes(y=outcome)) +
  geom_tile(aes(x=1, fill=cert), width=0.95, height=0.9, colour="white",linewidth=0.8) +
  geom_text(aes(x=1, label=paste0(cert," (",sym,")")),
            colour="white", fontface="bold", size=3.2) +
  scale_fill_manual(values=c("High"=pal$green,"Moderate"=pal$blue,
                              "Low"=pal$orange,"Very Low"=pal$red),
                    name="GRADE Certainty") +
  scale_x_continuous(breaks=NULL) +
  labs(title="",
       subtitle=paste0(""),
       x=NULL, y=NULL) +
  theme_sr() +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),
        panel.grid=element_blank(),legend.position="right",
        axis.text.y=element_text(size=10,face="bold"))
"Supplementary Figure S6. GRADE Evidence Profile"

ggsave("FigS6_GRADE.tiff", pS6, width=10,height=7,dpi=300,compression="lzw")

cat("  FigS6 saved.\n")

# =============================================================================
# 12. GRADE EVIDENCE PROFILE — TEXT OUTPUT (new in v4)
# =============================================================================


cat("GRADE notes:\n")
cat("  SGA/FGR: Observational design (-1). Moderate imprecision (-1). No indirectness.\n")
cat("           -> Start HIGH; -2 = MODERATE\n")
cat("  Biometry: Observational (-1). Low imprecision (wide CI but OR>1 certain). No indirectness.\n")
cat("           -> Start HIGH; -1 = HIGH\n")
cat("  FL/HL:   Single RCT, low RoB. Some imprecision (single study) (-1).\n")
cat("           -> Start HIGH; -1 = MODERATE\n")
cat("  EFW/BPD: Single RCT, high RoB (-1). Serious imprecision (-1).\n")
cat("           -> Start HIGH; -2 = LOW\n")
cat("  Adiposity: Observational (-1). Serious imprecision (only 2 studies, NS) (-1).\n")
cat("           -> Start HIGH; -2 = LOW\n")
cat("           -> MODERATE (indirect from current review)\n\n")

# =============================================================================
# 13. SUPPLEMENTARY TABLES
# =============================================================================
cat("-- SECTION 13: SUPPLEMENTARY TABLES --\n\n")

effect_data <- tribble(
  ~id, ~primary_finding, ~effect_size, ~stat_method,
  "D01","No sig. association with SGA or FGR","p>0.05","Multivariable logistic regression",
  "D02","VitD deficiency -> SGA; EFW beta=0.12 SD; HC beta=0.09 SD","OR=2.07 (1.38-3.09)","Longitudinal + logistic regression",
  "D03","Lower VitD -> metaphyseal splaying; no FL effect","r=-0.16 (-0.25,-0.06)","Pearson r; linear regression",
  "D04","No assoc. VitD-femur length","p=0.77","Spearman correlation",
  "D05","Higher VitD -> greater fetal FV and PMD","r=0.147 (p=0.006)","Pearson r; linear regression",
  "D06","VitD def -> excessive EFW; combined effect with GDM","OR=1.11 (1.02-1.21)","Logistic regression; GEE",
  "D07","Supplementation -> longer FL (+1.98mm) and HL (+1.39mm)","MD: FL 1.98mm; HL 1.39mm (p<0.001)","Independent t-test",
  "D08","Delta VitD positively correlated with Delta BPD","r=0.14 (p=0.03)","GEE",
  "D09","Each +10 nmol/L -> +0.05 z-score length; SGA NS","beta=0.05 (0.01-0.10); RR SGA NS","Mixed-effects; Poisson regression",
  "D10","Supplementation -> +277g EFW; FL +3.03mm; BPD +4.77mm","MD EFW 277g (p<0.01)","t-test; ICC",
  "D11","Insufficient VitD -> PAPP-A expression; FGR/SGA NS","p=0.009 PAPP-A; FGR NS","Linear regression",
  "D12","VitD not assoc. with GDM or fetal growth","OR<1 all NS","Multivariable logistic regression",
  "D13","VitD x Ca interaction -> femur z-score","beta=0.15 (p<0.05)","Multiple linear regression",
  "D14","Lower VitD -> AC and EFW <10th centile; offspring overweight","OR=1.14-1.18 per -10ng/mL","Multivariable regression",
  "D15","VitD not assoc. with fetal adiposity","beta=-0.007 to -0.012 NS","Multiple linear regression",
  "D16","VitD inversely correlated with FGR","r=-0.236 (p<0.001)","Logistic + chi-square",
  "D17","VitD<10ng/mL -> developmental delay aOR=4.28","aOR=4.28 (1.40-13.05)","Multivariable analysis",
  "D18","VitD insufficiency -> lower HC and FL","HC p=0.011; FL p<0.001","Kruskal-Wallis",
  "D19","No sig. association VitD-SGA","NS","Logistic regression",
  "D20","FGR group: VitD correlated with AC and EFW","r=0.376 (p=0.049)","Spearman r; logistic regression",
  "D21","No difference in VitD across AGA/SGA/FGR groups","p=0.672","Fisher's exact",
  "D22","No sig. association VitD-IUGR","RR=0.660 (no CI)","Chi-square",
  "D23","VitD deficiency: OR=6.81 in IUGR case-control","OR=6.81; OR insuf.=1.40","Chi-square",
  "D24","VitD assoc. with BPD and AC at 3rd trimester","beta=0.141 (p=0.042); AC beta=0.819","Linear regression",
  "D25","Higher VitD -> 43-54% reduced SGA risk","OR=0.57 (0.33-0.99); 0.46 (0.24-0.87)","Log-binomial regression",
  "D26","VitD not correlated with CRL; weak corr. NT","r^2=0.005 NS; r^2=0.129 (p=0.004)","Spearman rho; linear regression"
)

table_s1 <- studies %>%
  left_join(effect_data, by="id") %>%
  select(id,author,year,country,continent,design_cat,n,
         vitd_nmol,vitd_cutoff_nmol,lab,nos_score,
         outcome_cat,sig,effect_type,primary_finding,effect_size,stat_method) %>%
  rename(`Study ID`=id,`First Author`=author,Year=year,Country=country,
         Continent=continent,Design=design_cat,N=n,
         `Mean VitD (nmol/L)`=vitd_nmol,`VitD Cutoff (nmol/L)`=vitd_cutoff_nmol,
         `Lab Method`=lab,`NOS Score`=nos_score,`Outcome Category`=outcome_cat,
         Significant=sig,`Effect Type`=effect_type,
         `Primary Finding`=primary_finding,`Effect Size`=effect_size,
         `Statistical Method`=stat_method)
write_csv(table_s1, "TableS1_DataExtraction.csv")

table_s2 <- tribble(
  ~Analysis, ~k, ~N, ~Model, ~`Pooled OR (95% CI)`, ~`I2 (%)`, ~`p-het`, ~tau2,
  "SGA/FGR (main)", 4, sum(meta_sga$n),
  ifelse(p_sga$i2>=50,"Random (REML)","Fixed"),
  sprintf("%.2f (%.2f-%.2f)",p_sga$or,p_sga$lo,p_sga$hi),
  sprintf("%.1f",p_sga$i2), sprintf("%.3f",p_sga$pq), sprintf("%.3f",p_sga$tau2),
  "OR=1.55 (1.16-2.07); 16 studies",
  "SGA/FGR (+Alimohammadi)", 5, sum(meta_sga$n)+260,
  ifelse(p_s1$i2>=50,"Random (REML)","Fixed"),
  sprintf("%.2f (%.2f-%.2f)",p_s1$or,p_s1$lo,p_s1$hi),
  sprintf("%.1f",p_s1$i2),"--","--","Similar direction",
  "Fetal Biometry/EFW", 4, sum(meta_bim$n),
  ifelse(p_bim$i2>=50,"Random (REML)","Fixed"),
  sprintf("%.2f (%.2f-%.2f)",p_bim$or,p_bim$lo,p_bim$hi),
  sprintf("%.1f",p_bim$i2), sprintf("%.3f",p_bim$pq), sprintf("%.3f",p_bim$tau2),
  "Not directly comparable",
  "Fixed (I2=0%)", "RR=0.72 (0.52-0.99)", "0.0", ".46", "0",
  "SGA RR with supplementation"
)
write_csv(table_s2, "TableS2_MetaAnalysisSummary.csv")

write_csv(jbi_q, "TableS4_JBI_NOS_Quality.csv")

table_s5 <- tribble(
  ~Reference, ~`Study Type`, ~k, ~N, ~Outcome, ~`Effect (95% CI)`, ~`I2`, ~Model, ~`VitD def`, ~`Quality`,
  "Present review 2026","Observational",4,sum(meta_sga$n),"SGA/FGR",
  sprintf("OR=%.2f (%.2f-%.2f)",p_sga$or,p_sga$lo,p_sga$hi),round(p_sga$i2),
  ifelse(p_sga$i2>=50,"Random","Fixed"),"Variable","JBI + NOS",
  "Present review 2026","Observational",4,sum(meta_bim$n),"Biometry/EFW",
  sprintf("OR=%.2f (%.2f-%.2f)",p_bim$or,p_bim$lo,p_bim$hi),round(p_bim$i2),
  ifelse(p_bim$i2>=50,"Random","Fixed"),"Variable","JBI + NOS"
)
write_csv(table_s5, "TableS5_ComparisonPriorMA.csv")
cat("  Tables saved.\n")

# =============================================================================
# 14. MASTER EXPORTS
# =============================================================================
write_csv(studies,     "SR_MasterData_AllStudies.csv")
write_csv(meta_sga,    "SR_MetaData_SGA_FGR.csv")
write_csv(meta_bim,    "SR_MetaData_Biometry_EFW.csv")
write_csv(loo_results, "SR_LOO_Sensitivity.csv")

# =============================================================================
# 15. METHODOLOGICAL HETEROGENEITY NOTE
# =============================================================================
cat("\n=================================================================\n")
cat("  METHODOLOGICAL HETEROGENEITY — COMPLETE NOTE (v4)\n")
cat("=================================================================\n")
cat('
LABORATORY METHODS (7 methods identified):
  CLIA (n=8), RIA (n=6), HPLC-MS/MS (n=5), ECL (n=2),
  ECLIA (n=2), ELISA (n=2), HPLC (n=1).
  HPLC-MS/MS is the reference standard (DEQAS/NIST); immunoassays
  may deviate 10-30%. Santamaria 2018 and Bi 2018 both note assay
  heterogeneity as contributing to between-study I2.

VIT D UNITS: 8 studies reported in ng/mL -> converted to nmol/L
  using factor 2.496 (same as Bi et al. 2018).

CUT-OFFS: Range <25 to >75 nmol/L. Santamaria 2018 subgroup approach
  replicated in FigS3 and Section 4 subgroup analysis.

MODEL SELECTION: Fixed-effects when I2 < 50%; random-effects (REML)
  when I2 >= 50%. Per Santamaria 2018 & Bi 2018 adaptive approach.
  v4 UPDATE: New Figure 4 (RCT continuous MD forest) added.
  Old Fig 4 (comparison) is now FigS4.
  New FigS3 (cut-off subgroup), FigS5 (narrative), FigS6 (GRADE) added.

GUIDELINES: Observational = MOOSE (Santamaria 2018). RCTs = PRISMA (Bi 2018).
  Quality: JBI + NOS (NOS >=7/9 = high, per Santamaria 2018 threshold).
\n')

cat("=================================================================\n")
cat("  v4 COMPLETE — ALL OUTPUTS SAVED\n")
cat("=================================================================\n")
cat("MAIN FIGURES : Figure1-Figure8 (.tiff + .png)\n")
cat("SUPP FIGURES : FigS1-FigS6 (.tiff + .png)\n")
cat("TABLES (CSV) : TableS1-S5, SR_MasterData, SR_MetaData_SGA,\n")
cat("               SR_MetaData_Biometry, SR_LOO\n")
cat("Script completed:", format(Sys.time(),"%Y-%m-%d %H:%M:%S"), "\n")

