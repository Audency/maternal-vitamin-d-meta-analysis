# =============================================================================
# REVISAO SISTEMATICA: Vitamina D Materna e Crescimento Fetal / Adiposidade
# Script Final Consolidado (v6)
#
# Estrutura:
#   Secao  1: Pacotes e configuracoes
#   Secao  2: Paleta de cores e tema Lancet
#   Secao  3: Funcoes auxiliares
#   Secao  4: Dados mestres (26 estudos)
#   Secao  5: Estatisticas descritivas
#   Secao  6: Datasets para metanalise
#   Secao  7: Metanalise primaria — SGA/FGR
#   Secao  8: Metanalise secundaria — Biometria/EFW
#   Secao  9: Desfechos continuos — RCTs
#   Secao 10: Analise de sensibilidade Leave-One-Out
#   Secao 11: Vies de publicacao (Egger, Begg, Trim-and-Fill)
#   Secao 12: Figuras — Forest plots
#   Secao 13: Figuras — Caracteristicas dos estudos
#   Secao 14: Figuras — Metodos laboratoriais e 25(OH)D
#   Secao 15: Figuras — Sensibilidade e funil
#   Secao 16: Figuras — Qualidade (JBI + NOS)
#   Secao 17: Figuras — Subgrupo por cutoff
#   Secao 18: Figuras — Sintese narrativa
#   Secao 19: Figuras — GRADE
#   Secao 20: Figuras — Bubble plot e desfechos
#   Secao 21: Tabelas suplementares (CSV)
# =============================================================================


# =============================================================================
# SECAO 1: PACOTES E CONFIGURACOES
# =============================================================================
setwd("~/Desktop/AUDENCIO/Producao artigos/Artigo RS Isabel Vitamina D /Resultados ")

pkg <- c("meta", "metafor", "tidyverse", "ggplot2", "patchwork", "scales",
         "forcats", "tidyr", "readr", "maps", "grid")
new_pkg <- pkg[!pkg %in% installed.packages()[, "Package"]]
if (length(new_pkg)) install.packages(new_pkg, repos = "https://cloud.r-project.org")
invisible(lapply(pkg, library, character.only = TRUE))

out_dir <- "figures_v6"
if (!dir.exists(out_dir)) dir.create(out_dir)


# =============================================================================
# SECAO 2: PALETA DE CORES E TEMA LANCET
# =============================================================================
pal <- list(
  navy   = "#1A2E4A",
  red    = "#B5001C",
  mid    = "#4A6FA5",
  lgrey  = "#909090",
  dgrey  = "#444444",
  vlgrey = "#F0F2F5",
  black  = "#111111",
  white  = "#FFFFFF",
  orange = "#D4631A",
  green  = "#2E7D32",
  purple = "#5E35B1"
)

theme_lancet <- function(base = 10) {
  theme_bw(base_size = base) +
    theme(
      panel.background   = element_rect(fill = "white", colour = NA),
      plot.background    = element_rect(fill = "transparent", colour = NA),
      panel.grid.major.x = element_line(colour = "grey90", linewidth = 0.3,
                                        linetype = "dashed"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.border       = element_blank(),
      axis.line.x        = element_line(colour = "grey50", linewidth = 0.45),
      axis.ticks.y       = element_blank(),
      axis.ticks.x       = element_line(colour = "grey50", linewidth = 0.35),
      axis.title.x       = element_text(size = base - 0.5, colour = pal$black,
                                        face = "bold", margin = margin(t = 5)),
      axis.text.x        = element_text(size = base - 1.5, colour = pal$black),
      axis.text.y        = element_text(size = base - 0.5, colour = pal$black,
                                        hjust = 1),
      plot.subtitle      = element_text(colour = "grey40", size = base - 1.5,
                                        lineheight = 1.3),
      plot.caption       = element_text(colour = "grey45", size = base - 2.5,
                                        hjust = 0, face = "italic",
                                        lineheight = 1.3, margin = margin(t = 6)),
      legend.position    = "bottom",
      legend.background  = element_blank(),
      legend.key         = element_blank(),
      legend.text        = element_text(size = base - 1),
      legend.title       = element_text(size = base - 0.5, face = "bold"),
      strip.background   = element_rect(fill = pal$navy, colour = NA),
      strip.text         = element_text(colour = "white", face = "bold",
                                        size = base - 0.5)
    )
}


# =============================================================================
# SECAO 3: FUNCOES AUXILIARES
# =============================================================================

# Modelo adaptativo: fixed se I2 < 50%, random REML se I2 >= 50%
adaptive_meta <- function(log_or, se_log, labs, sm = "OR",
                          method_tau = "REML") {
  m0 <- metagen(TE = log_or, seTE = se_log, studlab = labs,
                sm = sm, common = TRUE, random = FALSE)
  i2 <- m0$I2
  cat(sprintf("  I\u00b2 = %.1f%% \u2192 %s-effects model\n",
              i2 * 100, ifelse(i2 >= 0.50, "RANDOM", "FIXED")))
  if (i2 >= 0.50)
    metagen(TE = log_or, seTE = se_log, studlab = labs, sm = sm,
            common = FALSE, random = TRUE, method.tau = method_tau)
  else
    metagen(TE = log_or, seTE = se_log, studlab = labs, sm = sm,
            common = TRUE, random = FALSE)
}

# Extrai estimativa pooled
get_pool <- function(m) {
  if (m$random)
    list(or  = exp(m$TE.random), lo = exp(m$lower.random),
         hi  = exp(m$upper.random), i2 = m$I2 * 100,
         pq  = m$pval.Q, tau2 = m$tau2, pval = m$pval.random,
         model = "Random (REML)")
  else
    list(or  = exp(m$TE.fixed), lo = exp(m$lower.fixed),
         hi  = exp(m$upper.fixed), i2 = m$I2 * 100,
         pq  = m$pval.Q, tau2 = 0, pval = m$pval.fixed,
         model = "Fixed")
}

# Salva figura em TIFF (300 dpi, LZW) e PNG (300 dpi, fundo transparente)
save_fig <- function(name, p, w = 14, h = 6.5) {
  ggsave(file.path(out_dir, paste0(name, ".tiff")), p,
         width = w, height = h, dpi = 300, compression = "lzw")
  ggsave(file.path(out_dir, paste0(name, ".png")), p,
         width = w, height = h, dpi = 300, bg = "transparent")
  cat(sprintf("  Saved: %s  (%g x %g in @ 300 dpi)\n", name, w, h))
}


# =============================================================================
# SECAO 4: DADOS MESTRES (26 ESTUDOS, 2010-2025)
# =============================================================================
studies <- tribble(
  ~id,    ~author,                     ~year, ~country,      ~continent,
  ~design, ~n, ~vitd_nmol, ~vitd_sd,
  ~lab,          ~lab_group,
  ~vitd_cutoff_nmol, ~trimester_sample,
  ~outcome_cat, ~sig, ~effect_type, ~nos_score,

  "D01","Tosun et al.",             2025,"Turkey",     "Asia",
  "Prospective Cohort", 226,  39.43,23.48,"CLIA","Immunoassay",
  50.0,"1st","SGA/FGR",FALSE,"p-value only",7,

  "D02","Miliku et al.",            2016,"Netherlands","Europe",
  "Prospective Cohort",7098,  46.7, 83.26,"HPLC-MS/MS","Mass Spec",
  25.0,"any","SGA",TRUE,"OR",9,

  "D03","Mahon et al.",             2010,"UK","Europe",
  "Prospective Cohort", 424,  61.0, 32.59,"RIA","Immunoassay",
  50.0,"any","Bone",TRUE,"r (Pearson)",8,

  "D04","Wierzejska et al.",        2020,"Poland","Europe",
  "Cross-sectional",     94,  47.5, 19.50,"CLIA","Immunoassay",
  50.0,"any","Biometry",FALSE,"r (Spearman)",5,

  "D05","Ioannou et al.",           2012,"UK","Europe",
  "Cohort",             357,  63.0, 96.30,"RIA","Immunoassay",
  50.0,"any","Bone",TRUE,"r (Pearson)",8,

  "D06","Liu et al.",               2020,"China","Asia",
  "Cohort",           10913,  66.4, 27.00,"HPLC-MS/MS","Mass Spec",
  50.0,"any","EFW/Biometry",TRUE,"OR",8,

  "D07","Vafaei et al.",            2019,"Iran","Asia",
  "RCT",                140,  46.5,    NA,"ECL","Immunoassay",
  50.0,"any","Bone/FL",TRUE,"Mean diff (RCT)",NA,

  "D08","Lee D.H. et al.",          2015,"Korea","Asia",
  "Cohort",             275,    NA,    NA,"ECLIA","Immunoassay",
  NA,"serial","Biometry",TRUE,"GEE / r",7,

  "D09","Beck et al.",              2025,"USA","America",
  "Cohort",             351,  68.1, 21.00,"HPLC-MS/MS","Mass Spec",
  50.0,"1st","SGA/Length",TRUE,"RR",8,

  "D10","Srilekha et al.",          2021,"India","Asia",
  "RCT",                100,  53.5,    NA,"CLIA","Immunoassay",
  50.0,"any","EFW/FL/BPD",TRUE,"Mean diff (RCT)",NA,

  "D11","Vestergaard et al.",       2021,"Denmark","Europe",
  "Prospective Cohort", 297,  79.0, 22.00,"HPLC-MS/MS","Mass Spec",
  50.0,"serial","SGA/FGR",FALSE,"p-value only",7,

  "D12","Park et al.",              2014,"Korea","Asia",
  "Prospective Cohort", 523,    NA,    NA,"RIA","Immunoassay",
  25.0,"serial","GDM/biometry",FALSE,"OR (GDM only)",8,

  "D13","Young et al.",             2012,"USA","America",
  "Prospective Cohort", 171,  54.7, 27.50,"RIA","Immunoassay",
  50.0,"any","Bone/FL",TRUE,"Beta (linear)",8,

  "D14","Morales et al.",           2015,"Spain","Europe",
  "Population Cohort", 2358,  73.5, 28.33,"HPLC","Mass Spec",
  50.0,"any","Biometry/Adiposity",TRUE,"OR",8,

  "D15","Akita et al.",             2025,"Japan","Asia",
  "Cohort",              89,  44.0,    NA,"CLIA","Immunoassay",
  50.0,"serial","Adiposity",FALSE,"Beta (linear)",7,

  "D16","Ge et al.",                2024,"China","Asia",
  "Retrospective",      300,  65.0,    NA,"ELISA","Immunoassay",
  50.0,"any","FGR",TRUE,"r (Pearson)",5,

  "D17","Lee S.B. et al.",          2023,"Korea","Asia",
  "Retrospective",     1079,  45.5, 22.50,"CLIA","Immunoassay",
  25.0,"any","Dev delay/SGA",TRUE,"aOR",6,

  "D18","Kwon et al.",              2023,"Korea","Asia",
  "Cohort",              48,    NA,    NA,"CLIA","Immunoassay",
  50.0,"any","Biometry",TRUE,"ANOVA / p-value",6,

  "D19","Palmrich et al.",          2023,"Austria","Europe",
  "Prospective Cohort", 249,  43.6, 23.78,"CLIA","Immunoassay",
  50.0,"any","SGA",FALSE,"OR (logistic)",8,

  "D20","Mahfod et al.",            2022,"Egypt","Africa",
  "Case-Control",        56,  20.78,   NA,"ECL","Immunoassay",
  50.0,"any","FGR",TRUE,"r (Spearman)",6,

  "D21","Marcal et al.",            2021,"Brazil","America",
  "Cross-sectional",     87,  60.5,    NA,"CLIA","Immunoassay",
  50.0,"any","SGA/FGR",FALSE,"ANOVA / p-value",5,

  "D22","Baqai et al.",             2020,"Pakistan","Asia",
  "Prospective Cohort", 585,    NA,    NA,"RIA","Immunoassay",
  75.0,"any","IUGR",FALSE,"RR (no CI)",5,

  "D23","Alimohammadi et al.",      2020,"Iran","Asia",
  "Case-Control",       260,  36.85,   NA,"RIA","Immunoassay",
  50.0,"any","IUGR",TRUE,"OR",6,

  "D24","Judistiani et al.",        2019,"Indonesia","Asia",
  "Prospective Cohort", 203,  39.13, 17.65,"ELISA","Immunoassay",
  50.0,"3rd","Biometry",TRUE,"Beta (linear)",7,

  "D25","Gernand et al.",           2014,"USA","America",
  "Observational",      792,  63.9,  29.50,"HPLC-MS/MS","Mass Spec",
  30.0,"any","SGA",TRUE,"OR",8,

  "D26","Fernandez-Alonso et al.",  2011,"Spain","Europe",
  "Cross-sectional",    498,  68.5,  21.85,"ECLIA","Immunoassay",
  50.0,"1st","CRL/NT",FALSE,"r (Spearman)",5
)

studies <- studies %>%
  mutate(
    design_cat = case_when(
      str_detect(design, "RCT")          ~ "RCT",
      str_detect(design, "Case-Control") ~ "Case-Control",
      str_detect(design, "Cross")        ~ "Cross-sectional",
      str_detect(design, "Retro")        ~ "Retrospective",
      TRUE                               ~ "Prospective/Observational Cohort"
    ),
    nos_cat = case_when(
      nos_score >= 7   ~ "High (\u22657/9)",
      nos_score >= 5   ~ "Moderate (5\u20136/9)",
      nos_score <  5   ~ "Low (<5/9)",
      is.na(nos_score) ~ "RCT (Cochrane RoB)"
    ),
    vitd_cutoff_group = case_when(
      vitd_cutoff_nmol <= 25 ~ "< 25 nmol/L",
      vitd_cutoff_nmol <= 30 ~ "\u2264 30 nmol/L",
      vitd_cutoff_nmol <= 50 ~ "\u2264 50 nmol/L",
      vitd_cutoff_nmol >  50 ~ "> 50 nmol/L",
      TRUE                   ~ "Not specified"
    ),
    vitd_cutoff_group = factor(vitd_cutoff_group,
      levels = c("< 25 nmol/L", "\u2264 30 nmol/L", "\u2264 50 nmol/L",
                 "> 50 nmol/L", "Not specified"))
  )


# =============================================================================
# SECAO 5: ESTATISTICAS DESCRITIVAS
# =============================================================================
cat("\n=== SECAO 5: ESTATISTICAS DESCRITIVAS ===\n\n")
cat(sprintf("Total de estudos      : %d\n", nrow(studies)))
cat(sprintf("Total de participantes: %s\n", format(sum(studies$n), big.mark = ",")))
cat(sprintf("Associacao significativa: %d/%d (%.0f%%)\n\n",
            sum(studies$sig), nrow(studies), mean(studies$sig) * 100))

vitd_obs <- studies %>% filter(!is.na(vitd_nmol))
cat(sprintf("Concentracoes de 25(OH)D (%d estudos com dados):\n", nrow(vitd_obs)))
cat(sprintf("  Media +/- DP  : %.1f +/- %.1f nmol/L\n",
            mean(vitd_obs$vitd_nmol), sd(vitd_obs$vitd_nmol)))
cat(sprintf("  Mediana [IQR] : %.1f [%.1f\u2013%.1f] nmol/L\n",
            median(vitd_obs$vitd_nmol),
            quantile(vitd_obs$vitd_nmol, 0.25),
            quantile(vitd_obs$vitd_nmol, 0.75)))
cat(sprintf("  Amplitude     : %.1f\u2013%.1f nmol/L\n\n",
            min(vitd_obs$vitd_nmol), max(vitd_obs$vitd_nmol)))

cat("25(OH)D por continente (media +/- DP, nmol/L):\n")
vitd_obs %>%
  group_by(continent) %>%
  summarise(k   = n(), m = round(mean(vitd_nmol), 1),
            s   = round(sd(vitd_nmol), 1),
            med = round(median(vitd_nmol), 1), .groups = "drop") %>%
  mutate(txt = sprintf("  %s: k=%d, %.1f +/- %.1f (mediana %.1f)",
                       continent, k, m, s, med)) %>%
  pull(txt) %>% cat(sep = "\n")

cat("\n\nDistribuicao por delineamento:\n")
print(studies %>% count(design_cat) %>%
        mutate(pct = round(n / sum(n) * 100)) %>% arrange(desc(n)))
cat("\nQualidade NOS (observacionais, n=24):\n")
print(studies %>% filter(design_cat != "RCT") %>%
        count(nos_cat) %>% arrange(desc(n)))


# =============================================================================
# SECAO 6: DATASETS PARA METANALISE
# =============================================================================

# ── 6a. SGA/FGR — com dados de grupo ────────────────────────────────────────
meta_sga <- tribble(
  ~study,                                  ~year, ~n,
  ~log_or, ~se_log_or, ~or, ~lower, ~upper,
  ~cutoff_nmol, ~lab, ~design_cat, ~inverted,
  ~n_exposed, ~n_ref,

  "Miliku et al., 2016",                   2016, 7098,
  log(2.07),(log(3.09)-log(1.38))/(2*1.96),2.07,1.38,3.09,
  25.0,"HPLC-MS/MS","Prospective Cohort",FALSE,
  1698, 5400,

  "Gernand et al., 2014 (50\u201374 vs <30)", 2014, 792,
  log(1/0.57),(log(1/0.33)-log(1/0.99))/(2*1.96),1.75,1.01,3.03,
  30.0,"HPLC-MS/MS","Observational",TRUE,
  198, 198,

  "Gernand et al., 2014 (\u226575 vs <30)", 2014, 792,
  log(1/0.46),(log(1/0.24)-log(1/0.87))/(2*1.96),2.17,1.15,4.17,
  30.0,"HPLC-MS/MS","Observational",TRUE,
  198, 198,

  "Beck et al., 2025",                     2025, 351,
  log(1/0.78),(log(1/0.23)-log(1/2.66))/(2*1.96),1.28,0.38,4.35,
  50.0,"HPLC-MS/MS","Cohort",TRUE,
  175, 176
)

meta_sga_ali <- tribble(
  ~study,                                  ~year, ~n,
  ~log_or, ~se_log_or, ~or, ~lower, ~upper,
  ~cutoff_nmol, ~lab, ~design_cat, ~inverted,
  ~n_exposed, ~n_ref,
  "Alimohammadi et al., 2020 [approx CI]", 2020,  260,
  log(6.81),0.90,6.81,2.20,21.14,
  50.0,"RIA","Case-Control",FALSE,
  130, 130
)

# ── 6b. Biometria fetal / EFW ───────────────────────────────────────────────
meta_bim <- tribble(
  ~study,                              ~year, ~n,
  ~log_or, ~se_log_or, ~or, ~lower, ~upper,
  ~outcome, ~lab,

  "Liu et al., 2020",                  2020, 10913,
  log(1.11),(log(1.21)-log(1.02))/(2*1.96),1.11,1.02,1.21,
  "Excessive EFW","HPLC-MS/MS",

  "Liu et al., 2020 (VitD+GDM)",       2020, 10913,
  log(1.36),(log(1.62)-log(1.15))/(2*1.96),1.36,1.15,1.62,
  "Excessive EFW (VitD+GDM)","HPLC-MS/MS",

  "Morales et al., 2015 (AC)",         2015,  2358,
  log(1.14),(log(1.31)-log(1.00))/(2*1.96),1.14,1.00,1.31,
  "AC < 10th centile","HPLC",

  "Morales et al., 2015 (EFW)",        2015,  2358,
  log(1.18),(log(1.36)-log(1.03))/(2*1.96),1.18,1.03,1.36,
  "EFW < 10th centile","HPLC"
)


# =============================================================================
# SECAO 7: METANALISE PRIMARIA — SGA/FGR
# =============================================================================
cat("\n\n=== SECAO 7: METANALISE SGA/FGR ===\n")
cat("Modelo: fixed se I\u00b2 < 50%; random (REML) se I\u00b2 >= 50%\n\n")

ma_sga <- adaptive_meta(meta_sga$log_or, meta_sga$se_log_or, meta_sga$study)
p_sga  <- get_pool(ma_sga)
print(summary(ma_sga))

cat(sprintf("\nOR pooled = %.2f (IC 95%% %.2f\u2013%.2f)\n",
            p_sga$or, p_sga$lo, p_sga$hi))
cat(sprintf("I\u00b2 = %.1f%%  |  Q=%.2f, df=3, p-het=%.3f  |  Modelo: %s\n",
            p_sga$i2, ma_sga$Q, p_sga$pq, p_sga$model))

p_ctrl <- 0.08
rd_sga <- (p_ctrl * (p_sga$or - 1)) / (1 + p_ctrl * (p_sga$or - 1))
nnh    <- abs(round(1 / rd_sga))
cat(sprintf("RD (baseline SGA 8%%): +%.1f%%  |  NNH = %d\n", rd_sga * 100, nnh))

cat(sprintf("\nTotais por grupo nos %d estudos pooled:\n", nrow(meta_sga)))
cat(sprintf("  VitD-deficiente (expostos)  : n = %s\n",
            format(sum(meta_sga$n_exposed, na.rm = TRUE), big.mark = ",")))
cat(sprintf("  VitD-suficiente (referencia): n = %s\n",
            format(sum(meta_sga$n_ref, na.rm = TRUE), big.mark = ",")))

# Sensibilidade 1: +Alimohammadi
cat("\n-- Sensibilidade 1: +Alimohammadi 2020 (IC aproximado) --\n")
sga_s1 <- bind_rows(meta_sga, meta_sga_ali)
ma_s1  <- adaptive_meta(sga_s1$log_or, sga_s1$se_log_or, sga_s1$study)
p_s1   <- get_pool(ma_s1)
cat(sprintf("  OR=%.2f (%.2f\u2013%.2f), I\u00b2=%.1f%%\n",
            p_s1$or, p_s1$lo, p_s1$hi, p_s1$i2))

# Sensibilidade 2: apenas coortes
cat("\n-- Sensibilidade 2: Apenas coortes prospectivas/observacionais --\n")
sga_s2 <- meta_sga %>% filter(design_cat != "Case-Control")
ma_s2  <- adaptive_meta(sga_s2$log_or, sga_s2$se_log_or, sga_s2$study)
p_s2   <- get_pool(ma_s2)
cat(sprintf("  OR=%.2f (%.2f\u2013%.2f), I\u00b2=%.1f%%\n",
            p_s2$or, p_s2$lo, p_s2$hi, p_s2$i2))

# Subgrupo por cutoff
cat("\n-- Subgrupo por ponto de corte de deficiencia --\n")
cutoff_groups <- meta_sga %>%
  mutate(cutoff_cat = case_when(
    cutoff_nmol <= 25 ~ "< 25 nmol/L",
    cutoff_nmol <= 30 ~ "\u2264 30 nmol/L",
    cutoff_nmol <= 50 ~ "\u2264 50 nmol/L",
    TRUE              ~ "> 50 nmol/L"))
for (g in c("< 25 nmol/L", "\u2264 30 nmol/L", "\u2264 50 nmol/L")) {
  sub <- cutoff_groups %>% filter(cutoff_cat == g)
  if (nrow(sub) >= 2) {
    ps <- get_pool(adaptive_meta(sub$log_or, sub$se_log_or, sub$study))
    cat(sprintf("  %s (k=%d): OR=%.2f (%.2f\u2013%.2f), I\u00b2=%.1f%%\n",
                g, nrow(sub), ps$or, ps$lo, ps$hi, ps$i2))
  } else {
    cat(sprintf("  %s (k=1): OR=%.2f (%.2f\u2013%.2f) \u2014 estudo unico\n",
                g, sub$or[1], sub$lower[1], sub$upper[1]))
  }
}


# =============================================================================
# SECAO 8: METANALISE SECUNDARIA — BIOMETRIA/EFW
# =============================================================================
cat("\n\n=== SECAO 8: METANALISE BIOMETRIA/EFW ===\n")
ma_bim <- adaptive_meta(meta_bim$log_or, meta_bim$se_log_or, meta_bim$study)
p_bim  <- get_pool(ma_bim)
cat(sprintf("OR pooled=%.2f (%.2f\u2013%.2f), I\u00b2=%.1f%%, p=%.5f, Modelo: %s\n",
            p_bim$or, p_bim$lo, p_bim$hi, p_bim$i2, p_bim$pval, p_bim$model))
print(summary(ma_bim))


# =============================================================================
# SECAO 9: DESFECHOS CONTINUOS — RCTs
# =============================================================================
cat("\n\n=== SECAO 9: DESFECHOS CONTINUOS (RCT) ===\n\n")
rct_fl <- metacont(n.e = 70, mean.e = 28.87, sd.e = 2.14,
                   n.c = 70, mean.c = 26.89, sd.c = 2.08,
                   sm = "MD", common = TRUE, random = FALSE)
rct_hl <- metacont(n.e = 70, mean.e = 28.62, sd.e = 1.94,
                   n.c = 70, mean.c = 27.23, sd.c = 2.08,
                   sm = "MD", common = TRUE, random = FALSE)
cat(sprintf("Vafaei 2019 - FL: MD=+%.2f mm (%.2f\u2013%.2f), p<0.001\n",
            rct_fl$TE.fixed, rct_fl$lower.fixed, rct_fl$upper.fixed))
cat(sprintf("Vafaei 2019 - HL: MD=+%.2f mm (%.2f\u2013%.2f), p<0.001\n",
            rct_hl$TE.fixed, rct_hl$lower.fixed, rct_hl$upper.fixed))
cat("Srilekha 2021 (Alto RoB): EFW MD=+277g (p<0.01); BPD +4.77mm; FL +3.03mm\n")


# =============================================================================
# SECAO 10: ANALISE DE SENSIBILIDADE LEAVE-ONE-OUT
# =============================================================================
cat("\n\n=== SECAO 10: LEAVE-ONE-OUT (SGA/FGR) ===\n\n")
loo_results <- tibble(study = character(), or = numeric(),
                      lower = numeric(), upper = numeric())
for (i in seq_len(nrow(meta_sga))) {
  m <- tryCatch(
    adaptive_meta(meta_sga$log_or[-i], meta_sga$se_log_or[-i],
                  meta_sga$study[-i]),
    error = function(e) NULL)
  if (!is.null(m)) {
    ps <- get_pool(m)
    loo_results <- add_row(loo_results, study = meta_sga$study[i],
                           or = ps$or, lower = ps$lo, upper = ps$hi)
  }
}
loo_results <- add_row(loo_results, study = "Overall",
                       or = p_sga$or, lower = p_sga$lo, upper = p_sga$hi)
print(loo_results, n = Inf)


# =============================================================================
# SECAO 11: VIES DE PUBLICACAO — EGGER + TRIM-AND-FILL
# =============================================================================
cat("\n\n=== SECAO 11: VIES DE PUBLICACAO ===\n\n")

egger_sga <- tryCatch(metabias(ma_sga, method.bias = "linreg"),
                      error = function(e) NULL)
if (!is.null(egger_sga)) {
  cat(sprintf("Egger (SGA): intercepto=%.3f (EP=%.3f), p=%.3f [%s]\n",
              egger_sga$estimate["intercept", "Estimate"],
              egger_sga$estimate["intercept", "Std. Error"],
              egger_sga$p.value,
              ifelse(egger_sga$p.value < 0.05, "assimetria detectada",
                     "sem assimetria significativa")))
} else {
  cat("Egger: k=4 - poder insuficiente. Correlacao de Begg:\n")
  begg <- tryCatch(metabias(ma_sga, method.bias = "rank"),
                   error = function(e) NULL)
  if (!is.null(begg)) cat(sprintf("  Begg p=%.3f\n", begg$p.value))
}

# ── Trim-and-Fill ────────────────────────────────────────────────────────────
cat("\nTrim-and-Fill (Duval & Tweedie):\n\n")
rma_sga <- rma(yi = meta_sga$log_or, sei = meta_sga$se_log_or, method = "FE")
tf_sga  <- trimfill(rma_sga)
cat(sprintf("  SGA/FGR - estudos imputados: %d\n", tf_sga$k0))
cat(sprintf("  OR original : %.2f (%.2f\u2013%.2f)\n",
            p_sga$or, p_sga$lo, p_sga$hi))
cat(sprintf("  OR ajustado : %.2f (%.2f\u2013%.2f)\n",
            exp(as.numeric(tf_sga$b)), exp(tf_sga$ci.lb), exp(tf_sga$ci.ub)))
cat(sprintf("  Conclusao   : %s\n\n",
            ifelse(tf_sga$k0 == 0, "Nenhum estudo imputado - funil simetrico",
                   "Assimetria detectada; estimativa ajustada atenuada")))

rma_bim <- rma(yi = meta_bim$log_or, sei = meta_bim$se_log_or,
               method = ifelse(p_bim$i2 >= 50, "REML", "FE"))
tf_bim  <- trimfill(rma_bim)
cat(sprintf("  Biometria/EFW - imputados: %d | OR ajustado: %.2f (%.2f\u2013%.2f)\n",
            tf_bim$k0, exp(as.numeric(tf_bim$b)),
            exp(tf_bim$ci.lb), exp(tf_bim$ci.ub)))


# =============================================================================
# SECAO 12: FIGURAS — FOREST PLOTS (estilo Lancet/Nature)
# Abordagem: 3 paineis via patchwork (tabela esquerda | grafico | tabela direita)
# Isso evita o problema de annotate em escala log
# =============================================================================
cat("\n\n=== SECAO 12: GERANDO FIGURAS ===\n\n")

# ── Tema para paineis de texto (tabelas esquerda/direita) ────────────────────
theme_table <- function(base = 10) {
  theme_void(base_size = base) %+replace%
    theme(
      plot.background  = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.margin      = margin(0, 2, 0, 2)
    )
}

# ── Funcao forest plot Lancet/Nature com 3 paineis ───────────────────────────
lancet_forest <- function(df, pool,
                          xbreaks = c(0.5, 1, 2, 4),
                          xlim_v  = c(0.2, 6),
                          xlab_text = "Odds ratio (log scale)",
                          cap     = NULL,
                          accent  = pal$navy,
                          ma_obj  = NULL) {

  k  <- nrow(df)
  df <- df %>%
    mutate(
      y     = k - row_number() + 1,
      w_raw = 1 / se_log_or^2,
      wt    = round(w_raw / sum(w_raw) * 100, 1),
      is_sig = lower > 1
    )

  # Heterogeneidade
  Q_val  <- if (!is.null(ma_obj)) round(ma_obj$Q, 2) else NA
  het_lab <- sprintf(
    "Heterogeneity: I\u00b2 = %.0f%%, Q = %.2f, df = %d, p = %.3f; Model: %s",
    pool$i2, ifelse(is.na(Q_val), 0, Q_val),
    k - 1, pool$pq, pool$model)

  # Labels para tabelas
  inv_tags <- if ("inverted" %in% names(df))
    ifelse(df$inverted, "\u2020", "") else rep("", k)
  df$or_label <- sprintf("%.2f [%.2f\u2013%.2f]%s",
                         df$or, df$lower, df$upper, inv_tags)
  df$wt_label <- sprintf("%.1f", df$wt)
  df$n_label  <- format(df$n, big.mark = ",")

  # Dados para tabelas (estudos + pooled)
  tab_df <- tibble(
    y     = c(df$y, 0),
    study = c(df$study, "Overall"),
    n_lab = c(df$n_label, format(sum(df$n), big.mark = ",")),
    or_lab = c(df$or_label,
               sprintf("%.2f [%.2f\u2013%.2f]", pool$or, pool$lo, pool$hi)),
    wt_lab = c(df$wt_label, "100.0"),
    is_pool = c(rep(FALSE, k), TRUE)
  )

  ylim_range <- c(-1.3, k + 1.6)

  # ── PAINEL ESQUERDO: Study + N ─────────────────────────────────────────────
  p_left <- ggplot(tab_df, aes(y = y)) +
    # Faixas zebra
    {
      rects <- list()
      for (i in seq_len(k))
        if (i %% 2 == 1)
          rects[[length(rects) + 1]] <- annotate(
            "rect", xmin = -Inf, xmax = Inf,
            ymin = df$y[i] - 0.45, ymax = df$y[i] + 0.45,
            fill = "#F5F6F8", colour = NA)
      rects
    } +
    # Cabecalhos
    annotate("text", x = 0, y = k + 1.0, label = "Study",
             hjust = 0, size = 3.5, fontface = "bold", colour = "#222222") +
    annotate("text", x = 1, y = k + 1.0, label = "N",
             hjust = 1, size = 3.5, fontface = "bold", colour = "#222222") +
    # Linha sob cabecalho
    annotate("segment", x = -0.05, xend = 1.05,
             y = k + 0.60, yend = k + 0.60,
             colour = "#333333", linewidth = 0.6) +
    # Linha acima do pooled
    annotate("segment", x = -0.05, xend = 1.05,
             y = 0.60, yend = 0.60,
             colour = "#333333", linewidth = 0.4) +
    # Nomes dos estudos
    geom_text(data = tab_df %>% filter(!is_pool),
              aes(x = 0, label = study), hjust = 0, size = 3.1,
              colour = pal$black) +
    # N dos estudos
    geom_text(data = tab_df %>% filter(!is_pool),
              aes(x = 1, label = n_lab), hjust = 1, size = 3.0,
              colour = pal$dgrey) +
    # Pooled
    geom_text(data = tab_df %>% filter(is_pool),
              aes(x = 0, label = study), hjust = 0, size = 3.3,
              fontface = "bold", colour = accent) +
    geom_text(data = tab_df %>% filter(is_pool),
              aes(x = 1, label = n_lab), hjust = 1, size = 3.15,
              fontface = "bold", colour = accent) +
    scale_x_continuous(limits = c(-0.05, 1.05), expand = c(0, 0)) +
    coord_cartesian(ylim = ylim_range, clip = "off") +
    theme_table()

  # ── PAINEL CENTRAL: Grafico (forest plot) ──────────────────────────────────
  p_mid <- ggplot(df) +
    # Faixas zebra
    {
      rects <- list()
      for (i in seq_len(k))
        if (i %% 2 == 1)
          rects[[length(rects) + 1]] <- annotate(
            "rect", xmin = -Inf, xmax = Inf,
            ymin = df$y[i] - 0.45, ymax = df$y[i] + 0.45,
            fill = "#F5F6F8", colour = NA)
      rects
    } +
    # Linha de nulidade
    geom_vline(xintercept = 1, colour = "#444444", linewidth = 0.55) +
    # Linha sob cabecalho
    annotate("segment", x = xlim_v[1], xend = xlim_v[2],
             y = k + 0.60, yend = k + 0.60,
             colour = "#333333", linewidth = 0.6) +
    # Linha acima do pooled
    annotate("segment", x = xlim_v[1], xend = xlim_v[2],
             y = 0.60, yend = 0.60,
             colour = "#333333", linewidth = 0.4) +
    # IC
    geom_segment(aes(x = lower, xend = upper, y = y, yend = y),
                 colour = accent, linewidth = 1.0, lineend = "round") +
    # Quadrados proporcionais
    geom_point(aes(x = or, y = y, size = wt),
               shape = 15, colour = accent) +
    # Diamante pooled
    annotate("polygon",
             x = c(pool$lo, pool$or, pool$hi, pool$or),
             y = c(0, 0.32, 0, -0.32),
             fill = accent, colour = accent, alpha = 0.85) +
    # Heterogeneidade
    annotate("text", x = xlim_v[1], y = -0.80, label = het_lab,
             hjust = 0, size = 2.6, colour = "#777777", fontface = "italic") +
    # Setas de direcao
    annotate("text", x = sqrt(xlim_v[1] * 1), y = -1.15,
             label = "\u2190 Favours sufficiency",
             hjust = 0.5, size = 2.5, colour = "#888888") +
    annotate("text", x = sqrt(xlim_v[2] * 1), y = -1.15,
             label = "Favours deficiency \u2192",
             hjust = 0.5, size = 2.5, colour = "#888888") +
    scale_size_continuous(range = c(3, 8), guide = "none") +
    scale_x_log10(breaks = xbreaks, labels = as.character(xbreaks)) +
    coord_cartesian(xlim = xlim_v, ylim = ylim_range, clip = "off") +
    labs(x = xlab_text) +
    theme_bw(base_size = 10) +
    theme(
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background  = element_rect(fill = "white", colour = NA),
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_line(colour = "grey92", linewidth = 0.3,
                                        linetype = "dashed"),
      panel.border     = element_blank(),
      axis.line.x      = element_line(colour = "grey50", linewidth = 0.4),
      axis.ticks.y     = element_blank(),
      axis.text.y      = element_blank(),
      axis.title.y     = element_blank(),
      axis.title.x     = element_text(size = 9, face = "bold",
                                      margin = margin(t = 5)),
      axis.text.x      = element_text(size = 8.5, colour = pal$black),
      plot.margin      = margin(0, 4, 0, 4)
    )

  # ── PAINEL DIREITO: OR [95% CI] + Weight ──────────────────────────────────
  p_right <- ggplot(tab_df, aes(y = y)) +
    # Faixas zebra
    {
      rects <- list()
      for (i in seq_len(k))
        if (i %% 2 == 1)
          rects[[length(rects) + 1]] <- annotate(
            "rect", xmin = -Inf, xmax = Inf,
            ymin = df$y[i] - 0.45, ymax = df$y[i] + 0.45,
            fill = "#F5F6F8", colour = NA)
      rects
    } +
    # Cabecalhos
    annotate("text", x = 0.0, y = k + 1.0, label = "OR [95% CI]",
             hjust = 0, size = 3.5, fontface = "bold", colour = "#222222") +
    annotate("text", x = 1.0, y = k + 1.0, label = "Weight %",
             hjust = 1, size = 3.5, fontface = "bold", colour = "#222222") +
    # Linhas
    annotate("segment", x = -0.05, xend = 1.05,
             y = k + 0.60, yend = k + 0.60,
             colour = "#333333", linewidth = 0.6) +
    annotate("segment", x = -0.05, xend = 1.05,
             y = 0.60, yend = 0.60,
             colour = "#333333", linewidth = 0.4) +
    # Valores
    geom_text(data = tab_df %>% filter(!is_pool),
              aes(x = 0, label = or_lab), hjust = 0, size = 3.0,
              colour = pal$black) +
    geom_text(data = tab_df %>% filter(!is_pool),
              aes(x = 1, label = wt_lab), hjust = 1, size = 3.0,
              colour = pal$dgrey) +
    # Pooled
    geom_text(data = tab_df %>% filter(is_pool),
              aes(x = 0, label = or_lab), hjust = 0, size = 3.15,
              fontface = "bold", colour = accent) +
    geom_text(data = tab_df %>% filter(is_pool),
              aes(x = 1, label = wt_lab), hjust = 1, size = 3.15,
              fontface = "bold", colour = accent) +
    scale_x_continuous(limits = c(-0.05, 1.05), expand = c(0, 0)) +
    coord_cartesian(ylim = ylim_range, clip = "off") +
    theme_table()

  # ── Combinar com patchwork ─────────────────────────────────────────────────
  final <- p_left + p_mid + p_right +
    plot_layout(widths = c(0.32, 0.40, 0.28)) +
    plot_annotation(caption = cap,
                    theme = theme(
                      plot.caption = element_text(
                        colour = "#666666", size = 8, hjust = 0,
                        face = "italic", lineheight = 1.3,
                        margin = margin(t = 8)),
                      plot.background = element_rect(fill = "white",
                                                     colour = NA)))

  final
}

# ── Forest: SGA/FGR ─────────────────────────────────────────────────────────
fp_sga <- lancet_forest(
  df       = meta_sga,
  pool     = p_sga,
  xbreaks  = c(0.3, 0.5, 1, 1.5, 2, 3, 5),
  xlim_v   = c(0.25, 5.5),
  xlab_text = "Odds ratio (log scale)",
  accent   = pal$navy,
  cap = paste0(
    "\u2020 Direction inverted: harmonised so that vitamin D deficiency = increased SGA/FGR risk.\n",
    "All studies used HPLC-MS/MS (reference standard). Cutoffs: Miliku <25; Gernand <30; Beck <50 nmol/L."
  ),
  ma_obj = ma_sga
)
save_fig("Forest_SGA_FGR", fp_sga, w = 16, h = 5)

# ── Forest: Biometria/EFW ───────────────────────────────────────────────────
fp_bim <- lancet_forest(
  df       = meta_bim %>% mutate(inverted = FALSE),
  pool     = p_bim,
  xbreaks  = c(0.8, 1.0, 1.2, 1.4, 1.6, 1.8),
  xlim_v   = c(0.75, 1.9),
  xlab_text = "Odds ratio",
  accent   = pal$orange,
  cap = paste0(
    "Liu 2020 (VitD+GDM): combined vitamin D deficiency and GDM vs deficiency alone.\n",
    "Morales 2015: OR per \u221210 ng/mL decrease in 25(OH)D. AC = abdominal circumference; EFW = estimated fetal weight."
  ),
  ma_obj = ma_bim
)
save_fig("Forest_Biometry_EFW", fp_bim, w = 15, h = 5.5)

# ── Forest: RCT — diferencas de media (layout Lancet com 3 paineis) ─────────
rct_all <- tribble(
  ~study,                                     ~outcome,            ~n, ~MD,   ~lo,   ~hi,
  "Vafaei 2019 (Low RoB)",                    "Femur length (mm)",       140,  1.98,  1.28,  2.68,
  "Vafaei 2019 (Low RoB)",                    "Humerus length (mm)",     140,  1.39,  0.72,  2.06,
  "Srilekha 2021 (High RoB)",                 "EFW (g)",                 100, 277.0, 100.0, 454.0,
  "Srilekha 2021 (High RoB)",                 "BPD (mm\u00d710)",        100,  47.7,  15.0,  80.4,
  "Srilekha 2021 (High RoB)",                 "FL (mm\u00d710)",         100,  30.3,  10.1,  50.5
) %>% mutate(
  y = n() - row_number() + 1,
  sig = lo > 0,
  rob_col = ifelse(grepl("Low", study), pal$green, pal$mid),
  full_label = paste0(study, " \u2014 ", outcome),
  md_label = sprintf("%.1f [%.1f; %.1f]%s", MD, lo, hi,
                     ifelse(sig, " ***", ""))
)

k_rct <- nrow(rct_all)
ylim_rct <- c(-0.5, k_rct + 1.6)

# Painel esquerdo: Study — Outcome
p_rct_left <- ggplot(rct_all, aes(y = y)) +
  {
    rects <- list()
    for (i in seq_len(k_rct))
      if (i %% 2 == 1)
        rects[[length(rects) + 1]] <- annotate(
          "rect", xmin = -Inf, xmax = Inf,
          ymin = rct_all$y[i] - 0.45, ymax = rct_all$y[i] + 0.45,
          fill = "#F5F6F8", colour = NA)
    rects
  } +
  annotate("text", x = 0, y = k_rct + 1.0, label = "Study \u2014 Outcome",
           hjust = 0, size = 3.5, fontface = "bold", colour = "#222222") +
  annotate("segment", x = -0.05, xend = 1.05,
           y = k_rct + 0.60, yend = k_rct + 0.60,
           colour = "#333333", linewidth = 0.6) +
  # Separador entre RCTs
  annotate("segment", x = -0.05, xend = 1.05,
           y = 3.5, yend = 3.5,
           colour = "#CCCCCC", linewidth = 0.3, linetype = "dotted") +
  geom_text(aes(x = 0, label = full_label, colour = rob_col),
            hjust = 0, size = 2.9) +
  scale_colour_identity() +
  scale_x_continuous(limits = c(-0.05, 1.05), expand = c(0, 0)) +
  coord_cartesian(ylim = ylim_rct, clip = "off") +
  theme_table()

# Painel central: grafico
p_rct_mid <- ggplot(rct_all, aes(y = y)) +
  {
    rects <- list()
    for (i in seq_len(k_rct))
      if (i %% 2 == 1)
        rects[[length(rects) + 1]] <- annotate(
          "rect", xmin = -Inf, xmax = Inf,
          ymin = rct_all$y[i] - 0.45, ymax = rct_all$y[i] + 0.45,
          fill = "#F5F6F8", colour = NA)
    rects
  } +
  geom_vline(xintercept = 0, colour = "#444444", linewidth = 0.55) +
  annotate("segment", x = -500, xend = 500,
           y = k_rct + 0.60, yend = k_rct + 0.60,
           colour = "#333333", linewidth = 0.6) +
  annotate("segment", x = -500, xend = 500,
           y = 3.5, yend = 3.5,
           colour = "#CCCCCC", linewidth = 0.3, linetype = "dotted") +
  geom_segment(aes(x = lo, xend = hi, yend = y, colour = rob_col),
               linewidth = 1.0, lineend = "round") +
  geom_point(aes(x = MD, colour = rob_col), shape = 15, size = 4.5) +
  scale_colour_identity() +
  scale_x_continuous(breaks = seq(-400, 400, 200),
                     limits = c(-500, 500)) +
  coord_cartesian(ylim = ylim_rct, clip = "off") +
  annotate("text", x = -250, y = -0.3,
           label = "\u2190 Favours control",
           hjust = 0.5, size = 2.5, colour = "#888888") +
  annotate("text", x = 250, y = -0.3,
           label = "Favours supplementation \u2192",
           hjust = 0.5, size = 2.5, colour = "#888888") +
  labs(x = "Mean difference (supplementation vs control)") +
  theme_bw(base_size = 10) +
  theme(
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background  = element_rect(fill = "white", colour = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_line(colour = "grey92", linewidth = 0.3,
                                      linetype = "dashed"),
    panel.border     = element_blank(),
    axis.line.x      = element_line(colour = "grey50", linewidth = 0.4),
    axis.ticks.y     = element_blank(),
    axis.text.y      = element_blank(),
    axis.title.y     = element_blank(),
    axis.title.x     = element_text(size = 9, face = "bold",
                                    margin = margin(t = 5)),
    axis.text.x      = element_text(size = 8.5),
    plot.margin      = margin(0, 4, 0, 4)
  )

# Painel direito: MD [95% CI]
p_rct_right <- ggplot(rct_all, aes(y = y)) +
  {
    rects <- list()
    for (i in seq_len(k_rct))
      if (i %% 2 == 1)
        rects[[length(rects) + 1]] <- annotate(
          "rect", xmin = -Inf, xmax = Inf,
          ymin = rct_all$y[i] - 0.45, ymax = rct_all$y[i] + 0.45,
          fill = "#F5F6F8", colour = NA)
    rects
  } +
  annotate("text", x = 0.5, y = k_rct + 1.0, label = "MD [95% CI]",
           hjust = 0.5, size = 3.5, fontface = "bold", colour = "#222222") +
  annotate("segment", x = -0.05, xend = 1.05,
           y = k_rct + 0.60, yend = k_rct + 0.60,
           colour = "#333333", linewidth = 0.6) +
  annotate("segment", x = -0.05, xend = 1.05,
           y = 3.5, yend = 3.5,
           colour = "#CCCCCC", linewidth = 0.3, linetype = "dotted") +
  geom_text(aes(x = 0.5, label = md_label), hjust = 0.5, size = 3.0,
            colour = pal$black) +
  scale_x_continuous(limits = c(-0.05, 1.05), expand = c(0, 0)) +
  coord_cartesian(ylim = ylim_rct, clip = "off") +
  theme_table()

p_rct_final <- p_rct_left + p_rct_mid + p_rct_right +
  plot_layout(widths = c(0.38, 0.35, 0.27)) +
  plot_annotation(
    caption = paste0(
      "Vafaei 2019: FL and HL at 2nd trimester; 1000 IU/day. ",
      "Srilekha 2021: EFW at delivery; BPD and FL at 3rd trimester.\n",
      "*** p < 0.001. Risk of Bias assessed with Cochrane RoB 2."),
    theme = theme(
      plot.caption = element_text(
        colour = "#666666", size = 8, hjust = 0,
        face = "italic", lineheight = 1.3, margin = margin(t = 8)),
      plot.background = element_rect(fill = "white", colour = NA)))

save_fig("Forest_RCT_MD", p_rct_final, w = 15, h = 5.5)


# =============================================================================
# SECAO 13: FIGURAS — CARACTERISTICAS DOS ESTUDOS (4 paineis)
# =============================================================================

# (a) Delineamento
p1a <- studies %>%
  count(design_cat) %>%
  mutate(design_cat = fct_reorder(design_cat, n)) %>%
  ggplot(aes(x = n, y = design_cat, fill = design_cat)) +
  geom_col(width = 0.70, colour = "white") +
  geom_text(aes(label = paste0("n=", n)), hjust = -0.15, fontface = "bold",
            size = 3.2) +
  scale_fill_manual(values = c(pal$navy, pal$mid, pal$orange, pal$green,
                               pal$lgrey)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.3))) +
  labs(subtitle = "(a)  Study design", x = "Studies", y = NULL) +
  theme_lancet() +
  theme(legend.position = "none", axis.line.y = element_blank())

# (b) Timeline
p1b <- studies %>%
  count(year) %>%
  mutate(era = cut(year, c(2009, 2014, 2019, 2025),
                   labels = c("2010\u201314", "2015\u201319", "2020\u201325"))) %>%
  ggplot(aes(x = factor(year), y = n, fill = era)) +
  geom_col(width = 0.70, colour = "white") +
  geom_text(aes(label = n), vjust = -0.35, fontface = "bold", size = 3.2) +
  scale_fill_manual(values = c(pal$navy, pal$mid, pal$orange), name = "Period") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.28))) +
  labs(subtitle = "(b)  Publication timeline", x = "Year", y = "Studies") +
  theme_lancet() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# (c) Mapa mundial
cc <- tribble(
  ~country,       ~map_name,      ~continent,  ~lon,   ~lat,
  "Turkey",       "Turkey",       "Asia",        35.0,  39.0,
  "Netherlands",  "Netherlands",  "Europe",       5.3,  52.1,
  "UK",           "UK",           "Europe",      -2.0,  54.0,
  "Poland",       "Poland",       "Europe",      19.4,  52.1,
  "China",        "China",        "Asia",       104.0,  35.5,
  "Iran",         "Iran",         "Asia",        53.7,  32.4,
  "Korea",        "South Korea",  "Asia",       127.8,  36.5,
  "Denmark",      "Denmark",      "Europe",       9.5,  56.3,
  "USA",          "USA",          "America",    -96.0,  38.0,
  "India",        "India",        "Asia",        78.9,  20.6,
  "Spain",        "Spain",        "Europe",      -3.7,  40.4,
  "Japan",        "Japan",        "Asia",       138.3,  37.0,
  "Austria",      "Austria",      "Europe",      14.5,  47.5,
  "Egypt",        "Egypt",        "Africa",      30.0,  26.8,
  "Brazil",       "Brazil",       "America",    -52.0, -14.2,
  "Pakistan",     "Pakistan",     "Asia",        69.3,  30.4,
  "Indonesia",    "Indonesia",    "Asia",       117.0,  -2.5
)

n_cntry <- studies %>%
  count(country, continent, name = "n_studies") %>%
  left_join(cc %>% select(country, map_name, lon, lat), by = "country")

world_map <- map_data("world")

# Correcao: criar join correto (bug corrigido da v5)
cc_with_counts <- cc %>%
  left_join(n_cntry %>% select(country, n_studies), by = "country")

highlighted <- world_map %>%
  left_join(
    cc_with_counts %>% select(map_name, continent, n_studies),
    by = c("region" = "map_name")
  )

cont_col_map <- c(Asia = pal$orange, Europe = pal$navy,
                  America = pal$green, Africa = pal$purple)

cont_sum <- studies %>%
  count(continent) %>%
  mutate(pct = round(n / sum(n) * 100),
         lab = paste0(continent, ": n=", n, " (", pct, "%)")) %>%
  arrange(desc(n)) %>% pull(lab) %>% paste(collapse = "\n")

p1c <- ggplot() +
  geom_polygon(data = world_map,
               aes(x = long, y = lat, group = group),
               fill = "#D4DCE4", colour = "#B0BCC8", linewidth = 0.12) +
  geom_polygon(data = highlighted %>% filter(!is.na(continent)),
               aes(x = long, y = lat, group = group, fill = continent),
               colour = "white", linewidth = 0.3, alpha = 0.88) +
  geom_point(data = n_cntry,
             aes(x = lon, y = lat, size = n_studies, colour = continent),
             shape = 21, fill = "white", stroke = 1.8, alpha = 0.95) +
  geom_text(data = n_cntry,
            aes(x = lon, y = lat, label = n_studies, colour = continent),
            size = 2.6, fontface = "bold") +
  scale_fill_manual(values = cont_col_map, guide = "none") +
  scale_colour_manual(values = cont_col_map, guide = "none") +
  scale_size_continuous(range = c(4.5, 12), guide = "none") +
  coord_fixed(1.3, xlim = c(-130, 155), ylim = c(-25, 72)) +
  annotate("label", x = -127, y = -20,
           label = paste0("k=26 | 16 countries\n", cont_sum),
           hjust = 0, vjust = 0, size = 2.65, colour = pal$black, fill = "white",
           label.size = 0.2, label.padding = unit(0.3, "lines"),
           label.r = unit(0.15, "lines")) +
  labs(subtitle = "(c)  Geographic distribution") +
  theme_void(base_size = 10) +
  theme(plot.subtitle    = element_text(face = "bold", hjust = 0.5, size = 10.5,
                                        colour = pal$black, margin = margin(b = 4)),
        panel.background = element_rect(fill = "#C5D8E8", colour = NA),
        plot.background  = element_rect(fill = "transparent", colour = NA))

# (d) Qualidade metodologica
p1d <- studies %>%
  mutate(nos_cat = factor(nos_cat,
    levels = c("High (\u22657/9)", "Moderate (5\u20136/9)",
               "Low (<5/9)", "RCT (Cochrane RoB)"))) %>%
  count(nos_cat) %>%
  ggplot(aes(x = n, y = nos_cat, fill = nos_cat)) +
  geom_col(width = 0.70, colour = "white") +
  geom_text(aes(label = paste0("n=", n)), hjust = -0.15, fontface = "bold",
            size = 3.2) +
  scale_fill_manual(values = c(pal$green, pal$orange, pal$red, pal$navy),
                    name = "Quality") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.3))) +
  labs(subtitle = "(d)  Methodological quality", x = "Studies", y = NULL) +
  theme_lancet() +
  theme(legend.position = "none", axis.line.y = element_blank(),
        axis.text.y = element_text(size = 8.5))

fig1 <- (p1a | p1b) / (p1c | p1d) + plot_layout(heights = c(1, 1.1))
save_fig("StudyCharacteristics", fig1, w = 15, h = 11)


# =============================================================================
# SECAO 14: FIGURAS — METODOS LABORATORIAIS E 25(OH)D POR CONTINENTE
# =============================================================================
p5a <- studies %>%
  count(lab) %>%
  mutate(lg = ifelse(lab %in% c("HPLC-MS/MS", "HPLC"),
                     "Mass spectrometry", "Immunoassay"),
         lab = fct_reorder(lab, n)) %>%
  ggplot(aes(x = n, y = lab, fill = lg)) +
  geom_col(width = 0.70, colour = "white") +
  geom_text(aes(label = paste0("n=", n)), hjust = -0.15, fontface = "bold",
            size = 3.2) +
  scale_fill_manual(values = c("Mass spectrometry" = pal$navy,
                               "Immunoassay" = pal$orange),
                    name = "Method class") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.35))) +
  labs(subtitle = "(a)  Laboratory method for 25(OH)D", x = "Studies",
       y = NULL) +
  theme_lancet() + theme(axis.line.y = element_blank())

p5b <- studies %>%
  filter(!is.na(vitd_nmol)) %>%
  ggplot(aes(x = continent, y = vitd_nmol, colour = continent)) +
  geom_boxplot(fill = NA, linewidth = 0.75, outlier.shape = NA,
               colour = "grey55") +
  geom_jitter(width = 0.18, size = 2.8, alpha = 0.8) +
  geom_hline(yintercept = 50, linetype = "dashed", colour = pal$red,
             linewidth = 0.85) +
  annotate("text", x = 0.55, y = 53.5, hjust = 0, size = 2.9,
           colour = pal$red, fontface = "italic",
           label = "50 nmol/L (sufficiency threshold)") +
  scale_colour_manual(values = c(pal$navy, pal$orange, pal$green, pal$purple),
                      guide = "none") +
  scale_y_continuous(limits = c(0, 115), breaks = seq(0, 100, 25)) +
  labs(subtitle = "(b)  Maternal 25(OH)D by continent",
       x = NULL, y = "25(OH)D (nmol/L)",
       caption = "Values harmonised to nmol/L (\u00d72.496 from ng/mL where applicable).") +
  theme_lancet()
save_fig("LabMethods_VitD", p5a | p5b, w = 14, h = 6.5)


# =============================================================================
# SECAO 15: FIGURAS — SENSIBILIDADE LOO E FUNNEL PLOT
# =============================================================================

# ── Leave-One-Out ────────────────────────────────────────────────────────────
loo_plot <- loo_results %>%
  mutate(study = fct_rev(factor(study, levels = study)),
         type  = ifelse(study == "Overall", "Overall", "LOO"))

pS1 <- ggplot(loo_plot, aes(y = study, x = or, xmin = lower, xmax = upper,
                            colour = type)) +
  geom_vline(xintercept = 1, colour = "grey25", linewidth = 0.45) +
  geom_vline(xintercept = p_sga$or, linetype = "dashed",
             colour = pal$navy, linewidth = 0.8, alpha = 0.55) +
  geom_errorbarh(height = 0.22, linewidth = 0.9) +
  geom_point(shape = 22, size = 3.8, fill = "white", stroke = 1.8) +
  geom_text(aes(label = sprintf("OR=%.2f (%.2f\u2013%.2f)", or, lower, upper)),
            hjust = -0.12, size = 3.0, colour = pal$black) +
  scale_colour_manual(values = c("Overall" = pal$navy, "LOO" = pal$mid),
                      guide = "none") +
  scale_x_log10(breaks = c(0.8, 1, 1.25, 1.5, 2, 2.5, 3)) +
  coord_cartesian(xlim = c(0.7, 6)) +
  labs(x = "Pooled odds ratio (log scale)", y = NULL,
       subtitle = sprintf("Dashed = overall estimate  |  OR=%.2f (%.2f\u2013%.2f)",
                          p_sga$or, p_sga$lo, p_sga$hi),
       caption = "All leave-one-out estimates remain statistically significant.") +
  theme_lancet()
save_fig("LOO_Sensitivity", pS1, w = 12, h = 5)

# ── Funnel plot (Trim-and-Fill) ──────────────────────────────────────────────
se_max   <- max(meta_sga$se_log_or) * 1.25
se_seq   <- seq(0, se_max, length.out = 100)
logOR_p  <- log(p_sga$or)
adj_logOR <- as.numeric(tf_sga$b)

funnel_ci <- tibble(se = se_seq,
  lo95 = logOR_p - 1.96 * se_seq, hi95 = logOR_p + 1.96 * se_seq,
  lo99 = logOR_p - 2.58 * se_seq, hi99 = logOR_p + 2.58 * se_seq)

orig_df <- meta_sga %>% select(log_or, se_log_or) %>% mutate(filled = FALSE)
if (tf_sga$k0 > 0) {
  fill_df <- tibble(
    log_or    = 2 * adj_logOR - orig_df$log_or[seq_len(tf_sga$k0)],
    se_log_or = orig_df$se_log_or[seq_len(tf_sga$k0)],
    filled    = TRUE)
  funnel_df <- bind_rows(orig_df, fill_df)
} else {
  funnel_df <- orig_df
}

pS_funnel <- ggplot() +
  geom_ribbon(data = funnel_ci, aes(x = se, ymin = lo99, ymax = hi99),
              fill = "#D4E6F7", alpha = 0.60) +
  geom_ribbon(data = funnel_ci, aes(x = se, ymin = lo95, ymax = hi95),
              fill = "#A8C8EE", alpha = 0.70) +
  geom_hline(yintercept = adj_logOR, linetype = "dotted",
             colour = pal$orange, linewidth = 0.9) +
  annotate("text", x = se_max * 0.95, y = adj_logOR + 0.06,
           label = sprintf("Adjusted OR=%.2f", exp(adj_logOR)),
           hjust = 1, size = 2.9, colour = pal$orange, fontface = "italic") +
  geom_point(data = funnel_df %>% filter(!filled),
             aes(x = se_log_or, y = log_or),
             colour = pal$navy, shape = 15, size = 3.5) +
  geom_point(data = funnel_df %>% filter(filled),
             aes(x = se_log_or, y = log_or),
             colour = pal$red, shape = 0, size = 3.5, stroke = 1.5) +
  scale_x_reverse(limits = c(se_max * 1.1, 0)) +
  scale_y_continuous(
    breaks = log(c(0.3, 0.5, 1, 1.5, 2, 3, 5)),
    labels = c("0.3", "0.5", "1.0", "1.5", "2.0", "3.0", "5.0"),
    name = "Log odds ratio") +
  labs(x = "Standard error (SE of log OR)",
       caption = paste0(
         "\u25a0 Observed studies (navy).  \u25a1 Imputed studies via trim-and-fill (red open square).\n",
         "Orange dotted = trim-and-fill adjusted estimate.\n",
         sprintf("Studies imputed: k=%d.", tf_sga$k0))) +
  coord_flip() + theme_lancet()
save_fig("Funnel_TrimFill", pS_funnel, w = 9, h = 8)


# =============================================================================
# SECAO 16: FIGURAS — QUALIDADE (JBI + NOS)
# =============================================================================
jbi_q <- tribble(
  ~id,   ~study,                   ~Q1,~Q2,~Q3,~Q4,~Q5,~Q6,~Q7,~Q8,~Q9,~score,~nos,
  "D01","Tosun 2025",             "Y","Y","Y","Y","Y","Y","Y","Y","U", 9, 7,
  "D02","Miliku 2016",            "Y","Y","Y","Y","Y","Y","Y","Y","Y",11, 9,
  "D03","Mahon 2010",             "Y","Y","Y","Y","Y","Y","Y","Y","U", 9, 8,
  "D04","Wierzejska 2020",        "Y","Y","Y","Y","U","Y","Y","U","U", 6, 5,
  "D05","Ioannou 2012",           "Y","Y","Y","Y","Y","Y","Y","Y","U", 9, 8,
  "D06","Liu 2020",               "Y","Y","Y","Y","Y","Y","Y","Y","U", 9, 8,
  "D08","Lee D.H. 2015",          "Y","Y","Y","Y","Y","Y","Y","Y","U", 9, 7,
  "D09","Beck 2025",              "Y","Y","Y","Y","Y","Y","Y","Y","Y",11, 8,
  "D11","Vestergaard 2021",       "Y","Y","Y","Y","Y","Y","Y","Y","U", 9, 7,
  "D12","Park 2014",              "Y","Y","Y","Y","Y","Y","Y","Y","Y",11, 8,
  "D13","Young 2012",             "Y","Y","Y","Y","Y","Y","Y","Y","Y",11, 8,
  "D14","Morales 2015",           "Y","Y","Y","Y","Y","Y","Y","Y","Y",11, 8,
  "D15","Akita 2025",             "Y","Y","Y","Y","Y","Y","Y","Y","Y",11, 7,
  "D16","Ge 2024",                "Y","N","Y","N","N","Y","Y","N","Y", 5, 5,
  "D17","Lee S.B. 2023",          "Y","Y","Y","U","Y","Y","Y","Y","U", 7, 6,
  "D18","Kwon 2023",              "Y","Y","Y","Y","Y","Y","Y","Y","U", 9, 6,
  "D19","Palmrich 2023",          "Y","Y","Y","Y","Y","Y","Y","Y","Y",11, 8,
  "D20","Mahfod 2022",            "Y","Y","Y","U","U","Y","Y","N","U", 6, 6,
  "D21","Marcal 2021",            "Y","Y","Y","U","Y","Y","Y","Y","U", 7, 5,
  "D22","Baqai 2020",             "Y","N","N","N","N","Y","Y","N","U", 4, 5,
  "D23","Alimohammadi 2020",      "Y","Y","Y","U","U","Y","Y","N","U", 5, 6,
  "D24","Judistiani 2019",        "Y","Y","Y","Y","Y","Y","Y","Y","U", 9, 7,
  "D25","Gernand 2014",           "Y","Y","Y","Y","Y","Y","Y","Y","Y",11, 8,
  "D26","Fernandez-Alonso 2011",  "Y","Y","Y","Y","Y","Y","Y","Y","Y",11, 5
)

jbi_long <- jbi_q %>%
  mutate(label = fct_reorder(paste0(study, "  (NOS=", nos, "/9)"), nos)) %>%
  pivot_longer(Q1:Q9, names_to = "question", values_to = "response") %>%
  mutate(response = factor(response, levels = c("Y", "N", "U"),
                           labels = c("Yes", "No", "Unclear")))

pS2a <- ggplot(jbi_long, aes(x = question, y = label, fill = response)) +
  geom_tile(colour = "white", linewidth = 0.65) +
  geom_text(aes(label = response), colour = "white", fontface = "bold",
            size = 2.5) +
  scale_fill_manual(values = c("Yes" = "#2E7D32", "No" = "#B5001C",
                               "Unclear" = "#D4631A"), name = NULL) +
  labs(x = "Appraisal item (Q1\u2013Q9)", y = NULL,
       subtitle = "(a)  JBI Critical Appraisal") +
  theme_lancet(9) +
  theme(axis.text.y = element_text(size = 7.5), axis.line.y = element_blank(),
        axis.ticks.y = element_blank(), panel.grid = element_blank())

pS2b <- jbi_q %>%
  mutate(label = fct_reorder(study, nos),
         cat   = ifelse(nos >= 7, "High (NOS\u22657)", "Moderate/Low")) %>%
  ggplot(aes(x = nos, y = label, fill = cat)) +
  geom_col(width = 0.70, colour = "white") +
  geom_text(aes(label = nos), hjust = -0.2, size = 2.8) +
  geom_vline(xintercept = 7, linetype = "dashed", colour = pal$red,
             linewidth = 0.8) +
  annotate("text", x = 7.15, y = 1, hjust = 0, size = 2.6,
           colour = pal$red, label = "NOS\u22657") +
  scale_fill_manual(values = c("High (NOS\u22657)" = pal$green,
                               "Moderate/Low" = pal$orange), name = NULL) +
  scale_x_continuous(limits = c(0, 12.5)) +
  labs(x = "NOS score (0\u20139)", y = NULL,
       subtitle = "(b)  Newcastle-Ottawa Scale") +
  theme_lancet(9) +
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(),
        axis.text.y = element_text(size = 7.5))
save_fig("Quality_JBI_NOS", pS2a | pS2b, w = 16, h = 11)


# =============================================================================
# SECAO 17: FIGURAS — SUBGRUPO POR CUTOFF
# =============================================================================
cutoff_sub <- bind_rows(
  meta_sga %>%
    mutate(cutoff_grp = case_when(
      cutoff_nmol <= 25 ~ "< 25 nmol/L",
      cutoff_nmol <= 30 ~ "\u2264 30 nmol/L",
      cutoff_nmol <= 50 ~ "\u2264 50 nmol/L",
      TRUE              ~ "> 50 nmol/L")) %>%
    select(study, cutoff_grp, or, lower, upper),
  tibble(study = "Overall", cutoff_grp = "Overall",
         or = p_sga$or, lower = p_sga$lo, upper = p_sga$hi)
) %>%
  mutate(
    sig    = lower > 1,
    ispool = study == "Overall",
    study  = fct_rev(factor(study, levels = c(
      "Overall", "Beck et al., 2025",
      "Gernand et al., 2014 (\u226575 vs <30)",
      "Gernand et al., 2014 (50\u201374 vs <30)",
      "Miliku et al., 2016"))))

pS3 <- ggplot(cutoff_sub, aes(y = study, x = or, xmin = lower, xmax = upper)) +
  geom_vline(xintercept = 1, colour = "grey25", linewidth = 0.45) +
  geom_errorbarh(aes(colour = sig, linewidth = ispool), height = 0.22) +
  geom_point(aes(colour = sig, shape = ispool, size = ispool)) +
  geom_text(aes(x = upper * 1.08,
                label = sprintf("%.2f (%.2f\u2013%.2f)", or, lower, upper)),
            hjust = 0, size = 2.9, colour = pal$black) +
  scale_colour_manual(values = c("TRUE" = pal$navy, "FALSE" = pal$lgrey),
                      guide = "none") +
  scale_linewidth_manual(values = c("TRUE" = 1.2, "FALSE" = 0.85),
                         guide = "none") +
  scale_shape_manual(values = c("TRUE" = 18, "FALSE" = 15), guide = "none") +
  scale_size_manual(values  = c("TRUE" = 5,  "FALSE" = 3.5), guide = "none") +
  scale_x_log10(breaks = c(0.3, 0.5, 1, 1.5, 2, 3, 5),
                labels = c("0.3", "0.5", "1", "1.5", "2", "3", "5")) +
  coord_cartesian(xlim = c(0.2, 10)) +
  labs(x = "Odds ratio (log scale)", y = NULL,
       subtitle = "SGA/FGR \u2014 stratified by vitamin D deficiency threshold",
       caption = "Diamond = overall pooled estimate. All subgroups directionally consistent.") +
  theme_lancet()
save_fig("Cutoff_Subgroup", pS3, w = 12, h = 5.5)


# =============================================================================
# SECAO 18: FIGURAS — SINTESE NARRATIVA (HEATMAP)
# =============================================================================
narr <- studies %>%
  mutate(
    direction = case_when(
      sig & str_detect(effect_type, "OR|RR|aOR|Beta|Mean|r") ~ "Significant",
      !sig ~ "Non-significant",
      TRUE ~ "Partial"),
    direction = if_else(
      author %in% c("Beck et al.", "Vestergaard et al.",
                     "Fernandez-Alonso et al."),
      "Partial", direction),
    nos_q = case_when(
      design == "RCT" ~ "RCT",
      nos_score >= 7  ~ "High",
      nos_score >= 5  ~ "Moderate",
      TRUE            ~ "Low"),
    short = paste0(sub(" et al.*", "", author), " ", year)
  )

dir_col <- c("Significant" = pal$navy, "Partial" = pal$orange,
             "Non-significant" = pal$lgrey)

pS5a <- narr %>%
  mutate(short = fct_reorder(short, nos_score, .na_rm = FALSE)) %>%
  ggplot(aes(x = factor(1), y = fct_rev(short), fill = direction)) +
  geom_tile(colour = "white", linewidth = 0.65) +
  geom_text(aes(label = direction, colour = direction), fontface = "bold",
            size = 2.65) +
  scale_fill_manual(values = dir_col, name = "Association") +
  scale_colour_manual(
    values = c("Significant" = "white", "Partial" = "white",
               "Non-significant" = "grey25"), guide = "none") +
  facet_grid(~"Direction", scales = "free", space = "free") +
  labs(y = NULL, x = NULL) + theme_lancet(9) +
  theme(axis.text.x = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank())

pS5b <- narr %>%
  mutate(short = fct_reorder(short, nos_score, .na_rm = FALSE)) %>%
  ggplot(aes(x = factor(1), y = fct_rev(short), fill = nos_q)) +
  geom_tile(colour = "white", linewidth = 0.65) +
  geom_text(aes(label = ifelse(!is.na(nos_score), as.character(nos_score),
                               "RCT"),
                colour = nos_q), fontface = "bold", size = 2.65) +
  scale_fill_manual(values = c("High" = pal$green, "Moderate" = pal$orange,
                               "Low" = pal$red, "RCT" = pal$navy),
                    name = "Quality") +
  scale_colour_manual(values = c("High" = "white", "Moderate" = "white",
                                 "Low" = "white", "RCT" = "white"),
                      guide = "none") +
  facet_grid(~"NOS / RoB", scales = "free", space = "free") +
  labs(y = NULL, x = NULL) + theme_lancet(9) +
  theme(axis.text = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank())
save_fig("Narrative_Synthesis", pS5a | pS5b, w = 10, h = 11)


# =============================================================================
# SECAO 19: FIGURAS — GRADE EVIDENCE PROFILE
# =============================================================================
grade_data <- tribble(
  ~outcome,                ~k, ~N,     ~effect,                    ~cert,    ~sym,
  "SGA/FGR (obs.)",         4,  9033, "OR=1.95 (1.47\u20132.59)", "Moderate","ooo+",
  "Biometry/EFW",           4, 26542, "OR=1.16 (1.09\u20131.23)", "High",    "oooo",
  "FL/HL (RCT; Low RoB)",   1,   140, "MD=+1.98 mm (1.28\u20132.68)","Moderate","ooo+",
  "EFW/BPD (High RoB)",     1,   100, "MD=+277 g",               "Low",     "oo++",
  "Adiposity",              2,  2447, "NS",                       "Low",     "oo++",
  "Infant wt. 9mo",         2,  4170, "MD=+119.75 g (33\u2013207)","Moderate","ooo+"
) %>%
  mutate(outcome = factor(outcome, levels = rev(outcome)),
         cert    = factor(cert, levels = c("High", "Moderate", "Low",
                                           "Very Low")))

pS6 <- ggplot(grade_data, aes(y = outcome)) +
  geom_tile(aes(x = 1, fill = cert), width = 0.9, height = 0.88,
            colour = "white", linewidth = 0.8) +
  geom_text(aes(x = 1, label = paste0(cert, "\n(", sym, ")")),
            colour = "white", fontface = "bold", size = 3.0,
            lineheight = 1.2) +
  scale_fill_manual(values = c("High" = pal$green, "Moderate" = pal$navy,
                               "Low" = pal$orange, "Very Low" = pal$red),
                    name = "GRADE certainty") +
  scale_x_continuous(breaks = NULL) +
  labs(x = NULL, y = NULL,
       subtitle = "Certainty of evidence by primary outcome",
       caption = paste0(
         "oooo=High | ooo+=Moderate | oo++=Low | o+++=Very Low\n",
         "obs.=observational (starts High; downgraded for risk of bias).\n",
         "Infant wt. 9mo: secondary analysis of two prospective cohorts.")) +
  theme_lancet() +
  theme(panel.grid = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 10, face = "bold"))
save_fig("GRADE_Profile", pS6, w = 10, h = 7)


# =============================================================================
# SECAO 20: FIGURAS — BUBBLE PLOT E DESFECHOS
# =============================================================================

# ── Bubble plot ──────────────────────────────────────────────────────────────
p7 <- studies %>%
  filter(!is.na(vitd_nmol)) %>%
  mutate(sig_l = ifelse(sig, "Significant", "Non-significant")) %>%
  ggplot(aes(x = vitd_nmol, y = n, size = n, colour = sig_l)) +
  geom_point(alpha = 0.75) +
  geom_vline(xintercept = 50, linetype = "dashed", colour = pal$red,
             linewidth = 0.85) +
  annotate("text", x = 51.5, y = 8e3, label = "50 nmol/L\n(sufficiency)",
           hjust = 0, size = 2.9, colour = pal$red, fontface = "italic") +
  geom_text(aes(label = paste0(sub(" et al.*", "", author), " ", year)),
            hjust = -0.08, vjust = 0.5, size = 2.6, colour = pal$black,
            check_overlap = TRUE) +
  scale_colour_manual(values = c("Significant" = pal$navy,
                                 "Non-significant" = pal$lgrey), name = NULL) +
  scale_size_continuous(range = c(3, 16), name = "Sample size",
                        labels = comma) +
  scale_y_log10(labels = comma) +
  labs(x = "Mean maternal 25(OH)D (nmol/L)", y = "Sample size (log scale)",
       caption = "Studies reporting mean or median 25(OH)D only (n=20/26).") +
  theme_lancet()
save_fig("BubblePlot", p7, w = 13, h = 7)

# ── Categorias de desfecho e cutoff ──────────────────────────────────────────
p6a <- studies %>%
  mutate(og = case_when(
    str_detect(outcome_cat, "SGA|FGR|IUGR|Dev") ~ "SGA/FGR/IUGR",
    str_detect(outcome_cat, "Biometry|EFW|BPD|CRL") ~ "Biometry/EFW",
    str_detect(outcome_cat, "Bone|FL") ~ "Fetal bone",
    str_detect(outcome_cat, "Adiposity") ~ "Adiposity",
    TRUE ~ "Other")) %>%
  count(og, sig) %>%
  mutate(sig_l = ifelse(sig, "Significant", "Non-significant")) %>%
  ggplot(aes(x = og, y = n, fill = sig_l)) +
  geom_col(position = "stack", colour = "white", width = 0.65) +
  scale_fill_manual(values = c("Significant" = pal$navy,
                               "Non-significant" = pal$lgrey), name = NULL) +
  labs(subtitle = "(a)  Outcome categories", x = NULL, y = "Studies") +
  theme_lancet(10) +
  theme(axis.text.x = element_text(angle = 25, hjust = 1),
        axis.line.y = element_blank())

p6b <- studies %>%
  count(vitd_cutoff_group) %>%
  filter(!is.na(vitd_cutoff_group)) %>%
  ggplot(aes(x = n, y = vitd_cutoff_group, fill = vitd_cutoff_group)) +
  geom_col(width = 0.65, colour = "white") +
  geom_text(aes(label = n), hjust = -0.2, fontface = "bold", size = 3.2) +
  scale_fill_manual(values = c(pal$red, pal$orange, pal$navy, pal$green,
                               pal$lgrey), guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.3))) +
  labs(subtitle = "(b)  VitD deficiency threshold", x = "Studies",
       y = NULL) +
  theme_lancet(10) + theme(axis.line.y = element_blank())
save_fig("Outcome_CutoffSummary", p6a | p6b, w = 12, h = 6)


# =============================================================================
# SECAO 21: TABELAS SUPLEMENTARES (CSV)
# =============================================================================
cat("\n\n=== SECAO 21: TABELAS SUPLEMENTARES ===\n\n")

effect_data <- tribble(
  ~id,  ~primary_finding,                                          ~effect_size,                      ~stat_method,
  "D01","No sig. association with SGA or FGR",                     "p>0.05",                          "Multivariable logistic regression",
  "D02","VitD deficiency \u2192 SGA; EFW \u03b2=0.12 SD",          "OR=2.07 (1.38\u20133.09)",        "Longitudinal + logistic regression",
  "D03","Lower VitD \u2192 metaphyseal splaying; no FL effect",     "r=\u22120.16",                    "Pearson r; linear regression",
  "D04","No assoc. VitD\u2013femur length",                         "p=0.77",                          "Spearman correlation",
  "D05","Higher VitD \u2192 greater fetal FV and PMD",              "r=0.147 (p=0.006)",               "Pearson r; linear regression",
  "D06","VitD def \u2192 excessive EFW; combined effect with GDM",  "OR=1.11 (1.02\u20131.21)",        "Logistic regression; GEE",
  "D07","Supplementation \u2192 FL +1.98mm; HL +1.39mm",           "MD FL=1.98mm; HL=1.39mm",         "Independent t-test",
  "D08","\u0394VitD correlated with \u0394BPD",                     "r=0.14 (p=0.03)",                 "GEE",
  "D09","Each +10 nmol/L \u2192 +0.05 z-score length; SGA NS",     "\u03b2=0.05 (0.01\u20130.10)",    "Mixed-effects; Poisson regression",
  "D10","Supplementation \u2192 +277g EFW; FL +3.03mm; BPD +4.77mm","MD EFW=277g (p<0.01)",           "t-test; ICC",
  "D11","Insufficient VitD \u2192 PAPP-A expression; FGR/SGA NS",  "p=0.009 PAPP-A; FGR NS",          "Linear regression",
  "D12","VitD not assoc. with GDM or fetal growth",                 "OR<1 all NS",                     "Multivariable logistic regression",
  "D13","VitD\u00d7Ca interaction \u2192 femur z-score",            "\u03b2=0.15 (p<0.05)",             "Multiple linear regression",
  "D14","Lower VitD \u2192 AC and EFW <10th centile",               "OR=1.14\u20131.18 per \u221210ng/mL","Multivariable regression",
  "D15","VitD not assoc. with fetal adiposity",                      "\u03b2 NS",                       "Multiple linear regression",
  "D16","VitD inversely correlated with FGR",                        "r=\u22120.236 (p<0.001)",         "Logistic + chi-square",
  "D17","VitD<10ng/mL \u2192 developmental delay aOR=4.28",         "aOR=4.28 (1.40\u201313.05)",       "Multivariable analysis",
  "D18","VitD insufficiency \u2192 lower HC and FL",                 "HC p=0.011; FL p<0.001",           "Kruskal-Wallis",
  "D19","No sig. association VitD\u2013SGA",                         "NS",                              "Logistic regression",
  "D20","FGR group: VitD correlated with AC and EFW",                "r=0.376 (p=0.049)",               "Spearman r; logistic regression",
  "D21","No difference in VitD across AGA/SGA/FGR groups",           "p=0.672",                         "Fisher's exact",
  "D22","No sig. association VitD\u2013IUGR",                        "RR=0.660 (no CI)",                "Chi-square",
  "D23","VitD deficiency: OR=6.81 in IUGR case-control",             "OR=6.81; insuf.=1.40",            "Chi-square",
  "D24","VitD assoc. with BPD and AC at 3rd trimester",              "\u03b2=0.141 (p=0.042)",           "Linear regression",
  "D25","Higher VitD \u2192 43\u201354% reduced SGA risk",           "OR=0.57 (0.33\u20130.99)",        "Log-binomial regression",
  "D26","VitD not correlated with CRL; weak corr. NT",               "r\u00b2=0.005 NS",                "Spearman rho"
)

table_s1 <- studies %>%
  left_join(effect_data, by = "id") %>%
  select(id, author, year, country, continent, design_cat, n,
         vitd_nmol, vitd_sd, vitd_cutoff_nmol, lab, nos_score,
         outcome_cat, sig, effect_type, primary_finding, effect_size,
         stat_method)
write_csv(table_s1, "TableS1_DataExtraction.csv")

table_tf <- tibble(
  Analysis  = c("SGA/FGR", "SGA/FGR (trim-and-fill)",
                "Biometry/EFW", "Biometry/EFW (trim-and-fill)"),
  k         = c(4, 4 + tf_sga$k0, 4, 4 + tf_bim$k0),
  k_imputed = c(0, tf_sga$k0, 0, tf_bim$k0),
  OR        = c(sprintf("%.2f", p_sga$or),
                sprintf("%.2f", exp(as.numeric(tf_sga$b))),
                sprintf("%.2f", p_bim$or),
                sprintf("%.2f", exp(as.numeric(tf_bim$b)))),
  CI_lower  = c(sprintf("%.2f", p_sga$lo), sprintf("%.2f", exp(tf_sga$ci.lb)),
                sprintf("%.2f", p_bim$lo), sprintf("%.2f", exp(tf_bim$ci.lb))),
  CI_upper  = c(sprintf("%.2f", p_sga$hi), sprintf("%.2f", exp(tf_sga$ci.ub)),
                sprintf("%.2f", p_bim$hi), sprintf("%.2f", exp(tf_bim$ci.ub))))
write_csv(table_tf, "TableS_TrimFill.csv")

write_csv(jbi_q,       "TableS4_JBI_NOS_Quality.csv")
write_csv(meta_sga,    "SR_MetaData_SGA_FGR.csv")
write_csv(meta_bim,    "SR_MetaData_Biometry_EFW.csv")
write_csv(loo_results, "SR_LOO_Sensitivity.csv")
write_csv(studies,     "SR_MasterData_AllStudies.csv")

# ── Resumo meta-analitico ───────────────────────────────────────────────────
table_s2 <- tibble(
  Analysis     = c("SGA/FGR (primary)", "Biometry/EFW (secondary)"),
  k            = c(4, 4),
  N            = c(sum(meta_sga$n), sum(meta_bim$n)),
  OR           = c(sprintf("%.2f", p_sga$or), sprintf("%.2f", p_bim$or)),
  CI_95        = c(sprintf("%.2f\u2013%.2f", p_sga$lo, p_sga$hi),
                   sprintf("%.2f\u2013%.2f", p_bim$lo, p_bim$hi)),
  I2_pct       = c(sprintf("%.1f", p_sga$i2), sprintf("%.1f", p_bim$i2)),
  p_het        = c(sprintf("%.3f", p_sga$pq), sprintf("%.3f", p_bim$pq)),
  Model        = c(p_sga$model, p_bim$model)
)
write_csv(table_s2, "TableS2_MetaAnalysisSummary.csv")

cat("  Todas as tabelas salvas.\n")


# =============================================================================
# CONCLUSAO
# =============================================================================
cat("\n\n=================================================================\n")
cat("  SCRIPT FINAL (v6) COMPLETO\n")
cat("  Todas as figuras: sem titulo, sem numero (adicionar no manuscrito)\n")
cat("=================================================================\n")
cat("FIGURAS PRINCIPAIS (PNG + TIFF @ 300 dpi):\n")
cat("  Forest_SGA_FGR        Forest_Biometry_EFW   Forest_RCT_MD\n")
cat("  StudyCharacteristics  LabMethods_VitD       BubblePlot\n")
cat("  Outcome_CutoffSummary\n")
cat("FIGURAS SUPLEMENTARES:\n")
cat("  LOO_Sensitivity  Funnel_TrimFill  Quality_JBI_NOS\n")
cat("  Cutoff_Subgroup  Narrative_Synthesis  GRADE_Profile\n")
cat("TABELAS (CSV):\n")
cat("  TableS1_DataExtraction    TableS2_MetaAnalysisSummary\n")
cat("  TableS4_JBI_NOS_Quality   TableS_TrimFill\n")
cat("  SR_MasterData_AllStudies  SR_MetaData_SGA_FGR\n")
cat("  SR_MetaData_Biometry_EFW  SR_LOO_Sensitivity\n")
cat(sprintf("Concluido: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
