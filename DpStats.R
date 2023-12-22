library(tidyverse)
library(haven)
library(gt)
library(fixest)
library(modelsummary)
library(glue)
library(multiwayvcov)
library(readxl)
library(naniar)
library(sandwich)
library(zoo)
library(panelView)
library(broom)
library(glue)
library(flextable)
library(scales)
library(patchwork)


rm(list = ls())

dat1 <- read_dta("data/figuredata.dta")
dat2 <- read_dta("data/amnesty.dta")
dat3 <- read_dta("data/Final_Main.dta")
dat4 <- read_dta("data/Final_Supplemental2.dta")
dat5 <- read_dta("data/master2.dta")
dat6 <- read_csv("Data/wmn_emp.csv")

datfinal <- dat3 |>
  filter(year >= 1987) # only filtering >= 1987

freedom <- read_excel("data/FH_final.xlsx")
fh_avg <- read_excel("data/FH_avgranks.xlsx")

# changing freedom average to long
fhavg_long <-
  fh_avg |>
  pivot_longer(
    cols = c(starts_with("19"), starts_with("20")),
    names_to = "year",
    values_to = "rank_avg"
  )

fhavg_long$year <- as.double(fhavg_long$year)

# changing freedom from wide to long -------
freedom_long <-
  freedom |>
  pivot_longer(
    cols = c(starts_with("19"), starts_with("20")),
    names_to = "year",
    values_to = "rank"
  ) |>
  filter(year >= 1987)

freedom_long$year <- as.double(freedom_long$year)

# Women empowerment and Freedom House data arrangement -----------
dat6 <- dat6 |>
  mutate(wmn_emp = v2x_gender, wmn_emp2 = v2x_gencl + v2x_gencs + v2x_genpp) |>
  filter(year >= 1987)

coef_names <- str_subset(colnames(datfinal), "cov")

dat_emp <- datfinal |>
  left_join(dat6, by = c("year", "ccode")) |>
  left_join(freedom_long, by = c("ccode", "year")) |>
  left_join(fhavg_long, by = c("ccode", "year")) |>
  mutate(rank_b = case_when(rank == "F" ~ 1,
                            rank == "NF" ~ 0,
                            rank == "PF" ~ 0.5)) |>
  mutate(
    rank_abl1 = lead(rank_b, 1),
    rank_abl2 = lead(rank_b, 2),
    rank_abl3 = lead(rank_b, 3),
    rank_abl4 = lead(rank_b, 4),
    rank_abl5 = lead(rank_b, 5),
    rank_abl6 = lead(rank_b, 6)
  ) |>
  mutate(rank_bi =
           case_when(rank_avg == "NF" ~ 0,
                     rank_avg == "PF" ~ 0.5,
                     rank_avg == "F"  ~ 1)) |>
  mutate(
    rank_bl1 = lead(rank_b, 1),
    rank_bl2 = lead(rank_b, 2),
    rank_bl3 = lead(rank_b, 3),
    rank_bl4 = lead(rank_b, 4),
    rank_bl5 = lead(rank_b, 5)
  )

# attempt to take wmn_empavg --------------
dat_emp <- dat_emp |>
  arrange(ccode, year)

# Calculate rolling averages for each country separately
dat_empavg <- dat_emp |>
  group_by(ccode) |>
  mutate(rolling_empavg = (rollmean(wmn_emp, k = 4, fill = NA))) |>
  mutate(wmn_empavg = lead(rolling_empavg, 1)) |> # we lead this because the rolling mean takes it forward by 1 year
  ungroup()


dat_empavg <- dat_empavg |>
  mutate(wmn_empl1 = lead(wmn_emp, 1),
         wmn_empl2 = lead(wmn_emp, 2),
         wmn_empl3 = lead(wmn_emp, 3),
         wmn_empl4 = lead(wmn_emp, 4),
         wmn_empl5 = lead(wmn_emp, 5),
         wmn_empavgl1 = lead(wmn_empavg, 1),
         wmn_empavgl2 = lead(wmn_empavg, 2),
         wmn_empavgl3 = lead(wmn_empavg, 3),
         wmn_empavgl4 = lead(wmn_empavg, 4),
         wmn_empavgl5 = lead(wmn_empavg, 5)) 

# missingness in original data set ----

datmissing <- datfinal |> 
  rename(CIRI_indexavg = new_empinxavg, CIRI_index = new_empinx) |>
  select(CIRI_indexavg, CIRI_index, polity2, polity2avg)

miss_plot <- gg_miss_var(datmissing, show_pct = TRUE) +
  labs(x = "C & M Outcomes")

miss_plot |>
  ggsave(filename = "figures/Missingstats.png", width = 6, height = 5)

# missingness in freedom --------

gg_miss_var(freedom_long, show_pct = TRUE) +
  labs(title = "Percentage of data missing in covariates and outcome variables",
       x = "Covariates and Outcome")

fhmiss_plot <-
  dat_emp |>
  select(rank_avg) 

gg_miss_var(fhmiss_plot, show_pct = TRUE)

  

# DEscriptive stats attempt 2 -------

sum_stats <- dat_empavg |>
  mutate_all(~ ifelse(. == -99, NA, .)) |>
  select(rank_b, wmn_empavg, new_empinxavg, polity2avg, EV, l2CPcol2, 
         covwdi_exp, covwdi_imp, covwdi_fdi, covwvs_rel, 
         coviNY_GDP_PETR_RT_ZS, covdemregion, covloggdpC, covloggdp) |>
  pivot_longer(cols = everything()) |>
  group_by(name) |>
  summarize(
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    n = sum(!is.na(value))
  ) |>
  mutate(cat = recode(
    name,
    rank_b = "Human Rights and Democracy Measures",
    wmn_empavg = "Human Rights and Democracy Measures",
    new_empinxavg = "Human Rights and Democracy Measures", 
    polity2avg = "Human Rights and Democracy Measures", 
    EV = "Treatment",
    l2CPcol2 = "Instrumental Variable",
    .default = "Covariates"
    )) |>
  mutate(name = recode_factor(
    name,
    rank_b = "Freedom House Rankings", 
    wmn_empavg = "Women Political Empowerment Index",
    new_empinxavg = "CIRI Human Empowerment Index score averaged over 4 years", 
    polity2avg = "Polity IV Score averaged over 4 years",
    EV = "Net EU Aid", 
    l2CPcol2 = "Colony Status in the second half of the year", 
    covwdi_exp = "Log Exports", 
    covwdi_imp = "Log Imports", 
    covwdi_fdi = "FDI", 
    covwvs_rel = "Religiosity", 
    coviNY_GDP_PETR_RT_ZS = "Petroleum Revenue", 
    covdemregion = "Democracies in the Region", 
    loggdpC = "log GDP per Capita", 
    loggdp = "log GDP")) |>
  arrange(name) |>
  group_by(cat) |>
  gt() |>
  cols_align("left", 1) |>
  cols_label(mean = "Mean", sd = "Std. Dev", name = "") |>
  fmt_number(columns = c(mean, sd), decimals = 2) |>
  fmt_integer(columns = n) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  )



# IV regression column 1 -------------------------------------------------


fit1 <- feols(polity2avg ~ 1 | year + ccode | EV ~ l2CPcol2, cluster = ~ year + ccode, ssc = ssc(fixef.K = "nested", cluster.adj = FALSE), datfinal)


fit2 <- feols(new_empinxavg ~ 1 | year + ccode | EV ~ l2CPcol2, cluster = ~ year + ccode, ssc = ssc(fixef.K = "nested", cluster.adj = FALSE), datfinal) 

# IV Regression column 2 ------------------------------------------------
spec_form <- glue("polity2avg ~ {str_c(coef_names, collapse = ' + ')} | year + ccode | EV ~ l2CPcol2")

fit3 <- feols(as.formula(spec_form), cluster = ~ year + ccode, ssc = ssc(fixef.K = "nested", cluster.adj = FALSE), datfinal)


# attempt for CIRI------------------------------
spec2_form <- glue("new_empinxavg ~ {str_c(coef_names, collapse = ' + ')} | year + ccode | EV ~ l2CPcol2")

fit4 <- feols(as.formula(spec2_form), cluster = ~ year + ccode, ssc = ssc(fixef.K = "nested", cluster.adj = FALSE), datfinal)

# attempt to regress wmn_empavg on EV ----------------------
fit5 <- feols(wmn_empavg ~ 1 | year + ccode | EV ~ l2CPcol2, cluster = ~ year + ccode, ssc = ssc(fixef.K = "nested", cluster.adj = FALSE), dat_empavg)

summary(fit5, stage = 1)

spec8_form <- glue("wmn_empavg ~ {str_c(coef_names, collapse = ' + ')} | year + ccode | EV ~ l2CPcol2")

fit6 <- feols(as.formula(spec8_form), cluster = ~ year + ccode, ssc = ssc(fixef.K = "nested", cluster.adj = FALSE), dat_empavg)

fit7 <- feols(as.formula(spec8_form), cluster = ~ year + ccode, ssc = ssc(fixef.K = "nested", cluster.adj = FALSE), dat_empavg) |>
  tidy() |>
  mutate(sevestimate7 = estimate) |>
  filter(term == "fit_EV") |>
  mutate(term = recode_factor(term, fit_EV = 0)) |>
  mutate(sevstd.error7 = std.error)

attempt <- read_csv("Data/avg education wdi.csv")

gg_miss_var(attempt, show_pct = TRUE)

# attempt to regress freedom house on EV ------------
new_col2 <-
  feols(
    rank_bi ~ 1
    | year + ccode | EV ~ l2CPcol2
    ,
    dat_empavg,
    cluster = ~ ccode + year,
    ssc = ssc(fixef.K = "nested", cluster.adj = FALSE)
  )

new_col <-
  feols(
    rank_bi ~ covihme_ayem + covihme_ayemF + covwdi_exp + covwdi_expF + covwdi_fdi + covwdi_fdiF + covwdi_imp +  covwdi_impF +
      covwvs_rel + covwvs_relF + coviNY_GDP_PETR_RT_ZS + coviNY_GDP_PETR_RT_ZSF + covdemregion + covdemregionF + covloggdp + covloggdpF + covloggdpC + covloggdpCF
    | year + ccode | EV ~ l2CPcol2
    ,
    dat_empavg,
    cluster = ~ ccode + year,
    ssc = ssc(fixef.K = "nested", cluster.adj = FALSE)
  )

# attempt to regress wmn_empavg on the Iv
feols(wmn_empavg ~ l2CPcol2 | year + ccode, dat_empavg)


# gt/modelsummary table -------------------------------------------
fit5 <- feols(wmn_empavg ~ 1 | year + ccode | EV ~ l2CPcol2, cluster = ~ year + ccode, ssc = ssc(fixef.K = "nested", cluster.adj = FALSE), dat_empavg)

summary(fit5, stage = 1)

fit_list <- list(new_col2, new_col, fit5, fit6, fit2, fit4, fit1, fit3)

modsum <- modelsummary(fit_list, 
             coef_map = c(
               "fit_EV" = "EU Aid"), 
             output = "gt", 
             gof_map = "nobs",
             add_rows = data.frame(intercept = c("Countries", "Years", "Covariates", "Year Fixed Effects", "Country Fixed Effects"),
    `1` = c("113", "22", "No", "Yes", "Yes"), 
    `2` = c("113", "22", "Yes", "Yes", "Yes"), 
    `3` = c("101", "22", "No", "Yes", "Yes"),
    `4` = c("101", "22", "Yes", "Yes", "Yes"),
    `5` = c("115", "20", "No", "Yes", "Yes"),
    `6` = c("115", "22", "Yes", "Yes", "Yes"),
    `7` = c("95", "20", "No", "Yes", "Yes"),
    `8` = c("95", "20", "Yes", "Yes", "Yes")))



gt_table <- modsum$`_data`


main <- gt_table |> 
  gt() |> 
  tab_spanner("FH Rating", columns = 2:3) |> 
  tab_spanner("V-DEM", columns = 4:5)  |> 
  tab_spanner("CIRI", columns = 6:7) |> 
  tab_spanner("PolityIV", columns = 8:9)

  


  

# attempt to create wmn_ emp event study plot --------------------

specl1_form <- glue("wmn_empl1 ~ 1 | year + ccode | EV ~ l2CPcol2")
specl2_form <- glue("wmn_empl2 ~ 1 | year + ccode | EV ~ l2CPcol2")
specl3_form <- glue("wmn_empl3 ~ 1 | year + ccode | EV ~ l2CPcol2")
specl4_form <- glue("wmn_empl4 ~ 1 | year + ccode | EV ~ l2CPcol2")
specl5_form <- glue("wmn_empl5 ~ 1 | year + ccode | EV ~ l2CPcol2")

fit8 <- feols(as.formula(specl1_form), dat_empavg) |>
  tidy() |> 
  mutate(eighestimate8 = estimate) |>
  filter(term == "fit_EV") |>
  mutate(term = recode_factor(term, fit_EV = 1)) |> 
  mutate(eighstd.error8 = std.error)
fit9 <- feols(as.formula(specl2_form), dat_empavg) |>
  tidy() |>
  mutate(ninestimate9 = estimate) |>
  filter(term == "fit_EV") |>
  mutate(term = recode_factor(term, fit_EV = 2)) |> 
  mutate(ninstd.error9 = std.error)
fit10 <- feols(as.formula(specl3_form), dat_empavg) |>
  tidy() |>
  mutate(tenestimate10 = estimate) |>
  filter(term == "fit_EV") |>
  mutate(term = recode_factor(term, fit_EV = 3)) |> 
  mutate(tenstd.error10 = std.error)
fit11 <- feols(as.formula(specl4_form), dat_empavg) |>
  tidy() |>
  mutate(elvestimate11 = estimate) |>
  filter(term == "fit_EV") |>
  mutate(term = recode_factor(term, fit_EV = 4)) |> 
  mutate(elvstd.error11 = std.error)
fit12 <- feols(as.formula(specl5_form), dat_empavg) |>
  tidy() |>
  mutate(twlestimate12 = estimate) |>
  filter(term == "fit_EV") |>
  mutate(term = recode_factor(term, fit_EV = 5)) |> 
  mutate(twlstd.error12 = std.error)

fit_evt <- fit7 |>
  full_join(fit8, by = "term") |>
  full_join(fit9, by = "term") |>
  full_join(fit10, by = "term") |>
  select(-starts_with("esti"), -starts_with("std."), -starts_with("p.valu"), -starts_with("stat"))
 
p1 <- fit_evt |>
  ggplot(aes(x = term)) +
  geom_point(aes(y = sevestimate7)) +
  geom_point(aes(y = eighestimate8)) +
  geom_point(aes(y = ninestimate9)) +
  geom_point(aes(y = tenestimate10)) +
  geom_errorbar(aes(ymin = sevestimate7 - 2*sevstd.error7, ymax = sevestimate7 + 2*sevstd.error7), width = 0.1) + 
  geom_errorbar(aes(ymin = eighestimate8 - 2*eighstd.error8, ymax = eighestimate8 + 2*eighstd.error8), width = 0.1) +
  geom_errorbar(aes(ymin = ninestimate9 - 2*ninstd.error9, ymax = ninestimate9 + 2*ninstd.error9), width = 0.1) +
  geom_errorbar(aes(ymin = tenestimate10 - 2*tenstd.error10, ymax = tenestimate10 + 2*tenstd.error10), width = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#FF0036") +
  labs(title = "Women Empowerment Index",
       y = "",
       x = "Years Forward") +
  theme_bw()


# Event study plot for c&m polity data --------
datfinal <- datfinal |>
  mutate(polity2l1 = lead(polity2, 1),
         polity2l2 = lead(polity2, 2),
         polity2l3 = lead(polity2, 3),
         polity2l4 = lead(polity2, 4),
         polity2l5 = lead(polity2, 5))

specp_form <- glue("polity2 ~ 1 | year + ccode | EV ~ l2CPcol2")
specpl1_form <- glue("polity2l1 ~ 1 | year + ccode | EV ~ l2CPcol2")
specpl2_form <- glue("polity2l2 ~ 1 | year + ccode | EV ~ l2CPcol2")
specpl3_form <- glue("polity2l3 ~ 1 | year + ccode | EV ~ l2CPcol2")
specpl4_form <- glue("polity2l4 ~ 1 | year + ccode | EV ~ l2CPcol2")
specpl5_form <- glue("polity2l5 ~ 1 | year + ccode | EV ~ l2CPcol2")

feols(polity2 ~ 1 | year + ccode | EV ~ l2CPcol2, datfinal)

cit7 <- feols(as.formula(specp_form), datfinal) |>
  tidy() |>
  mutate(sevestimate7 = estimate) |>
  filter(term == "fit_EV") |>
  mutate(term = recode_factor(term, fit_EV = 0)) |>
  mutate(sevstd.error7 = std.error)
cit8 <- feols(as.formula(specpl1_form), datfinal) |>
  tidy() |> 
  mutate(eighestimate8 = estimate) |>
  filter(term == "fit_EV") |>
  mutate(term = recode_factor(term, fit_EV = 1)) |> 
  mutate(eighstd.error8 = std.error)
cit9 <- feols(as.formula(specpl2_form), datfinal) |>
  tidy() |>
  mutate(ninestimate9 = estimate) |>
  filter(term == "fit_EV") |>
  mutate(term = recode_factor(term, fit_EV = 2)) |> 
  mutate(ninstd.error9 = std.error)
cit10 <- feols(as.formula(specpl3_form), datfinal) |>
  tidy() |>
  mutate(tenestimate10 = estimate) |>
  filter(term == "fit_EV") |>
  mutate(term = recode_factor(term, fit_EV = 3)) |> 
  mutate(tenstd.error10 = std.error)
cit11 <- feols(as.formula(specpl4_form), datfinal) |>
  tidy() |>
  mutate(elvestimate11 = estimate) |>
  filter(term == "fit_EV") |>
  mutate(term = recode_factor(term, fit_EV = 4)) |> 
  mutate(elvstd.error11 = std.error)
cit12 <- feols(as.formula(specpl5_form), datfinal) |>
  tidy() |>
  mutate(twlestimate12 = estimate) |>
  filter(term == "fit_EV") |>
  mutate(term = recode_factor(term, fit_EV = 5)) |> 
  mutate(twlstd.error12 = std.error)

cit_evt <- cit7 |>
  full_join(cit8, by = "term") |>
  full_join(cit9, by = "term") |>
  full_join(cit10, by = "term") |>
  full_join(cit11, by = "term") |>
  full_join(cit12, by = "term") |>
  select(-starts_with("esti"), -starts_with("std."), -starts_with("p.valu"), -starts_with("stat"))

p2 <- cit_evt |>
  ggplot(aes(x = term)) +
  ylim(-10, 10) +
  geom_point(aes(y = sevestimate7)) +
  geom_point(aes(y = eighestimate8)) +
  geom_point(aes(y = ninestimate9)) +
  geom_point(aes(y = tenestimate10)) +
  geom_point(aes(y = elvestimate11)) +
  geom_point(aes(y = twlestimate12)) +
  geom_errorbar(aes(ymin = sevestimate7 - 2*sevstd.error7, ymax = sevestimate7 + 2*sevstd.error7), width = 0.1) + 
  geom_errorbar(aes(ymin = eighestimate8 - 2*eighstd.error8, ymax = eighestimate8 + 2*eighstd.error8), width = 0.1) +
  geom_errorbar(aes(ymin = ninestimate9 - 2*ninstd.error9, ymax = ninestimate9 + 2*ninstd.error9), width = 0.1) +
  geom_errorbar(aes(ymin = tenestimate10 - 2*tenstd.error10, ymax = tenestimate10 + 2*tenstd.error10), width = 0.1) +
  geom_errorbar(aes(ymin = elvestimate11 - 2*elvstd.error11, ymax = elvestimate11 + 2*elvstd.error11), width = 0.1) +
  geom_errorbar(aes(ymin = twlestimate12 - 2*twlstd.error12, ymax = twlestimate12 + 2*twlstd.error12), width = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#FF0036") +
  labs(title = "Polity IV Score",
       y = "",
       x = "Years Forward") +
  theme_bw()


pprint <- p1 + p2

  


# attempt to create freedom house event study plot-------------

specb_form <-
  glue("rank_b ~ 1 | year + ccode | EV ~ l2CPcol2")
specbl1_form <-
  glue("rank_bl1 ~ 1 | year + ccode | EV ~ l2CPcol2")
specbl2_form <-
  glue("rank_bl2 ~ 1 | year + ccode | EV ~ l2CPcol2")
specbl3_form <-
  glue("rank_bl3 ~ 1 | year + ccode | EV ~ l2CPcol2")
specbl4_form <-
  glue("rank_bl4 ~ 1 | year + ccode | EV ~ l2CPcol2")
specbl5_form <-
  glue("rank_bl5 ~ 1| year + ccode | EV ~ l2CPcol2")

git7 <-
  feols(
    as.formula(specb_form),
    cluster = ~ year + ccode,
    ssc = ssc(fixef.K = "nested", cluster.adj = FALSE),
    dat_empavg
  ) |>
  tidy() |>
  mutate(sevestimate7 = estimate) |>
  filter(term == "fit_EV") |>
  mutate(term = recode_factor(term, fit_EV = 0)) |>
  mutate(sevstd.error7 = std.error)
git8 <-
  feols(
    as.formula(specbl1_form),
    cluster = ~ year + ccode,
    ssc = ssc(fixef.K = "nested", cluster.adj = FALSE),
    dat_empavg
  ) |>
  tidy() |>
  mutate(eighestimate8 = estimate) |>
  filter(term == "fit_EV") |>
  mutate(term = recode_factor(term, fit_EV = 1)) |>
  mutate(eighstd.error8 = std.error)
git9 <-
  feols(
    as.formula(specbl2_form),
    cluster = ~ year + ccode,
    ssc = ssc(fixef.K = "nested", cluster.adj = FALSE),
    dat_empavg
  ) |>
  tidy() |>
  mutate(ninestimate9 = estimate) |>
  filter(term == "fit_EV") |>
  mutate(term = recode_factor(term, fit_EV = 2)) |>
  mutate(ninstd.error9 = std.error)
git10 <-
  feols(
    as.formula(specbl3_form),
    cluster = ~ year + ccode,
    ssc = ssc(fixef.K = "nested", cluster.adj = FALSE),
    dat_empavg
  ) |>
  tidy() |>
  mutate(tenestimate10 = estimate) |>
  filter(term == "fit_EV") |>
  mutate(term = recode_factor(term, fit_EV = 3)) |>
  mutate(tenstd.error10 = std.error)
git11 <-
  feols(
    as.formula(specbl4_form),
    cluster = ~ year + ccode,
    ssc = ssc(fixef.K = "nested", cluster.adj = FALSE),
    dat_empavg
  ) |>
  tidy() |>
  mutate(elvestimate11 = estimate) |>
  filter(term == "fit_EV") |>
  mutate(term = recode_factor(term, fit_EV = 4)) |>
  mutate(elvstd.error11 = std.error)
git12 <-
  feols(
    as.formula(specbl5_form),
    cluster = ~ year + ccode,
    ssc = ssc(fixef.K = "nested", cluster.adj = FALSE),
    dat_empavg
  ) |>
  tidy() |>
  mutate(twlestimate12 = estimate) |>
  filter(term == "fit_EV") |>
  mutate(term = recode_factor(term, fit_EV = 5)) |>
  mutate(twlstd.error12 = std.error)

git_evt <- git7 |>
  full_join(git8, by = "term") |>
  full_join(git9, by = "term") |>
  full_join(git10, by = "term") |>
  select(
    -starts_with("esti"),
    -starts_with("std."),
    -starts_with("p.valu"),
    -starts_with("stat")
  )

p3 <- git_evt |>
  ggplot(aes(x = term)) +
  geom_point(aes(y = sevestimate7)) +
  geom_point(aes(y = eighestimate8)) +
  geom_point(aes(y = ninestimate9)) +
  geom_point(aes(y = tenestimate10)) +
  geom_errorbar(
    aes(
      ymin = sevestimate7 - 2 * sevstd.error7,
      ymax = sevestimate7 + 2 * sevstd.error7
    ),
    width = 0.1
  ) +
  geom_errorbar(
    aes(
      ymin = eighestimate8 - 2 * eighstd.error8,
      ymax = eighestimate8 + 2 * eighstd.error8
    ),
    width = 0.1
  ) +
  geom_errorbar(
    aes(
      ymin = ninestimate9 - 2 * ninstd.error9,
      ymax = ninestimate9 + 2 * ninstd.error9
    ),
    width = 0.1
  ) +
  geom_errorbar(
    aes(
      ymin = tenestimate10 - 2 * tenstd.error10,
      ymax = tenestimate10 + 2 * tenstd.error10
    ),
    width = 0.1
  ) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "#FF0036") +
  labs(title = "Freedom House Ratings Index",
       y = "",
       x = "Years Forward") +
  theme_bw()


# CIRI Event study plot replication--------------------------------------------------------

dat_empavg <- dat_empavg |>
  mutate(
    new_empinx_1 = lead(new_empinx, 1),
    new_empinx_2 = lead(new_empinx, 2),
    new_empinx_3 = lead(new_empinx, 3),
    new_empinx_4 = lead(new_empinx, 4),
    new_empinx_5 = lead(new_empinx, 5)
  )

specb_form <-
  glue("new_empinx ~ 1 | year + ccode | EV ~ l2CPcol2")
specbl1_form <-
  glue("new_empinx_1 ~ 1 | year + ccode | EV ~ l2CPcol2")
specbl2_form <-
  glue("new_empinx_2 ~ 1 | year + ccode | EV ~ l2CPcol2")
specbl3_form <-
  glue("new_empinx_3 ~ 1 | year + ccode | EV ~ l2CPcol2")
specbl4_form <-
  glue("new_empinx_4 ~ 1 | year + ccode | EV ~ l2CPcol2")
specbl5_form <-
  glue("new_empinx_5 ~ 1 | year + ccode | EV ~ l2CPcol2")

hit7 <-
  feols(
    as.formula(specb_form),
    cluster = ~ year + ccode,
    ssc = ssc(fixef.K = "nested", cluster.adj = FALSE),
    dat_empavg
  ) |>
  tidy() |>
  mutate(sevestimate7 = estimate) |>
  filter(term == "fit_EV") |>
  mutate(term = recode_factor(term, fit_EV = 0)) |>
  mutate(sevstd.error7 = std.error)
hit8 <-
  feols(
    as.formula(specbl1_form),
    cluster = ~ year + ccode,
    ssc = ssc(fixef.K = "nested", cluster.adj = FALSE),
    dat_empavg
  ) |>
  tidy() |>
  mutate(eighestimate8 = estimate) |>
  filter(term == "fit_EV") |>
  mutate(term = recode_factor(term, fit_EV = 1)) |>
  mutate(eighstd.error8 = std.error)
hit9 <-
  feols(
    as.formula(specbl2_form),
    cluster = ~ year + ccode,
    ssc = ssc(fixef.K = "nested", cluster.adj = FALSE),
    dat_empavg
  ) |>
  tidy() |>
  mutate(ninestimate9 = estimate) |>
  filter(term == "fit_EV") |>
  mutate(term = recode_factor(term, fit_EV = 2)) |>
  mutate(ninstd.error9 = std.error)
hit10 <-
  feols(
    as.formula(specbl3_form),
    cluster = ~ year + ccode,
    ssc = ssc(fixef.K = "nested", cluster.adj = FALSE),
    dat_empavg
  ) |>
  tidy() |>
  mutate(tenestimate10 = estimate) |>
  filter(term == "fit_EV") |>
  mutate(term = recode_factor(term, fit_EV = 3)) |>
  mutate(tenstd.error10 = std.error)
hit11 <-
  feols(
    as.formula(specbl4_form),
    cluster = ~ year + ccode,
    ssc = ssc(fixef.K = "nested", cluster.adj = FALSE),
    dat_empavg
  ) |>
  tidy() |>
  mutate(elvestimate11 = estimate) |>
  filter(term == "fit_EV") |>
  mutate(term = recode_factor(term, fit_EV = 4)) |>
  mutate(elvstd.error11 = std.error)
hit12 <-
  feols(
    as.formula(specbl5_form),
    cluster = ~ year + ccode,
    ssc = ssc(fixef.K = "nested", cluster.adj = FALSE),
    dat_empavg
  ) |>
  tidy() |>
  mutate(twlestimate12 = estimate) |>
  filter(term == "fit_EV") |>
  mutate(term = recode_factor(term, fit_EV = 5)) |>
  mutate(twlstd.error12 = std.error)


# Attempt to create event study plot ----------
hit_evt <- hit7 |>
  full_join(hit8, by = "term") |>
  full_join(hit9, by = "term") |>
  full_join(hit10, by = "term") |>
  full_join(hit11, by = "term") |>
  full_join(hit12, by = "term") |> 
  select(
    -starts_with("esti"),
    -starts_with("std."),
    -starts_with("p.valu"),
    -starts_with("stat")
  )

p4 <- hit_evt |>
  ggplot(aes(x = term)) +
  geom_point(aes(y = sevestimate7)) +
  geom_point(aes(y = eighestimate8)) +
  geom_point(aes(y = ninestimate9)) +
  geom_point(aes(y = tenestimate10)) +
  geom_point(aes(y = elvestimate11)) +
  geom_point(aes(y = twlestimate12)) +
  geom_errorbar(
    aes(
      ymin = sevestimate7 - 2 * sevstd.error7,
      ymax = sevestimate7 + 2 * sevstd.error7
    ),
    width = 0.1
  ) +
  geom_errorbar(
    aes(
      ymin = eighestimate8 - 2 * eighstd.error8,
      ymax = eighestimate8 + 2 * eighstd.error8
    ),
    width = 0.1
  ) +
  geom_errorbar(
    aes(
      ymin = ninestimate9 - 2 * ninstd.error9,
      ymax = ninestimate9 + 2 * ninstd.error9
    ),
    width = 0.1
  ) +
  geom_errorbar(
    aes(
      ymin = tenestimate10 - 2 * tenstd.error10,
      ymax = tenestimate10 + 2 * tenstd.error10
    ),
    width = 0.1
  ) +
  geom_errorbar(
    aes(
      ymin = elvestimate11 - 2 * elvstd.error11,
      ymax = elvestimate11 + 2 * elvstd.error11
    ),
    width = 0.1
  ) +
  geom_errorbar(
    aes(
      ymin = twlestimate12 - 2 * twlstd.error12,
      ymax = twlestimate12 + 2 * twlstd.error12
    ),
    width = 0.1
  ) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "#FF0036") +
  labs(title = "CIRI Original Event Sudy Plot",
       y = "",
       x = "Years Forward") +
  theme_bw()


pprint <- p3 + p4


