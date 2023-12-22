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

# Exclusion restriction test --------
rob <- feols(rank_b ~ l2CPcol2 | ccode + year, data = dat_empavg)

rob2 <- feols(wmn_empavg ~ l2CPcol2 | ccode + year, data = dat_empavg)

rob_list <- list(rob, rob2)


robust <- modelsummary(rob_list, gof_map = "nobs", output = "gt")
  gtsave(filename = "robust.png")

# missingness in the avg education for female-------------------

avg_ed <- read_csv("Data/avg education wdi.csv")

gg_miss_var(avg_ed, show_pct = TRUE) |>
  ggsave(filename = "figures/missinged.png")

## attempt to regress CIRI on Polity to check for relationship---------

coef_names <- str_subset(colnames(dat3), "cov")

datfinal <- datfinal |>
  mutate(polity2avgl = lag(polity2avg, 2))

spec4_form <- glue("new_empinxavg ~ {str_c(coef_names, collapse = ' + ')} + polity2avgl | year + ccode | EV ~ l2CPcol2")

appnew <- feols(as.formula(spec4_form), cluster = ~ year + ccode, datfinal)

# Attempt to estimate the effect of a treaty on CIRI-------------
dathr <- read_xls("data/ICCPR_ratification.xls")

colnames(dathr) <- c("wdi_nm", "dtsgn", "dtrat", "dtacpt", "year")
dathr <- dathr[-1, ]

dathr_reg <- dathr |>
  mutate(year = as.double(year)) |>
  mutate(treaty = if_else(year != "", 1, 0)) |>
  select(wdi_nm, year, treaty) |>
  left_join(dat5, by = c("wdi_nm", "year")) |>
  select(ccode, year, treaty)

dat3_hr <- dat3 |>
  left_join(dathr_reg, by = c("year", "ccode")) |>
  mutate(treaty = ifelse(is.na(treaty), 0, treaty)) |>
  mutate(treatyl = lag(treaty, 2))

coef_names <- str_subset(colnames(dat3), "cov")

spec5_form <- glue("new_empinxavg ~ {str_c(coef_names, collapse = ' + ')} + treatyl | year + ccode | EV ~ l2CPcol2")

fit13 <- feols(as.formula(spec5_form), cluster = ~ year + ccode, ssc = ssc(fixef.K = "nested", cluster.adj = FALSE), dat3_hr)

summary(lm(new_empinxavg ~ treaty, dat3_hr))

# attempt to regress wmn_emp on EV --------
app1 <- feols(wmn_emp ~ 1 | year + ccode | EV ~ l2CPcol2, cluster = c("year", "ccode"), dat_empavg)

coef_names <- str_subset(colnames(dat3), "cov")

specapp1_form <- glue("wmn_emp ~ {str_c(coef_names, collapse = ' + ')} | year + ccode | EV ~ l2CPcol2")

app2 <- feols(as.formula(specapp1_form), dat_empavg)

app3 <- feols(rank_b ~ 1 | year + ccode | EV ~ l2CPcol2, cluster = c("year", "ccode"), dat_empavg)

coef_names <- str_subset(colnames(dat3), "cov")

specapp2_form <- glue("rank_b ~ {str_c(coef_names, collapse = ' + ')} | year + ccode | EV ~ l2CPcol2")

app4 <- feols(as.formula(specapp2_form), dat_empavg)


app_list <- list(app3, app4, app1, app2)
modelsummary(app_list, 
             coef_map = c(
               "fit_EV" = "EU Aid"), 
             output = "gt", 
             gof_map = "nobs",
             add_rows = data.frame(intercept = c("Countries", "Years", "Covariates", "Year Fixed Effects", "Country Fixed Effects"),
                                   `1` = c("112", "22", "No", "Yes", "Yes"), 
                                   `2` = c("112", "22", "Yes", "Yes", "Yes"), 
                                   `3` = c("102", "22", "No", "Yes", "Yes"),
                                   `4` = c("102", "22", "Yes", "Yes", "Yes")), 
             notes = "Columns (1) and (2) are the results for the effect of EU aid on the unaveraged freedom house ranking in year t. Columns (3) and (4) present same for the V-Dem women empowerment index")

#3 attempt to know how long the effect lasts for wmn_emp --------

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

specl1_form <- glue("wmn_empl1 ~ {str_c(coef_names, collapse = ' + ')} | year + ccode | EV ~ l2CPcol2")
specl2_form <- glue("wmn_empl2 ~ {str_c(coef_names, collapse = ' + ')} | year + ccode | EV ~ l2CPcol2")
specl3_form <- glue("wmn_empl3 ~ {str_c(coef_names, collapse = ' + ')} | year + ccode | EV ~ l2CPcol2")
specl4_form <- glue("wmn_empl4 ~ {str_c(coef_names, collapse = ' + ')} | year + ccode | EV ~ l2CPcol2")
specl5_form <- glue("wmn_empl5 ~ {str_c(coef_names, collapse = ' + ')} | year + ccode | EV ~ l2CPcol2")


feols(as.formula(specl1_form), cluster = ~ year + ccode, ssc = ssc(fixef.K = "nested", cluster.adj = FALSE), dat_empavg)
feols(as.formula(specl2_form), cluster = ~ year + ccode, ssc = ssc(fixef.K = "nested", cluster.adj = FALSE), dat_empavg)
feols(as.formula(specl3_form), cluster = ~ year + ccode, ssc = ssc(fixef.K = "nested", cluster.adj = FALSE), dat_empavg)
feols(as.formula(specl4_form), dat_empavg)
feols(as.formula(specl5_form), dat_empavg)

# attempt for heterogeinity  for wmn_empavg--------
reg <- read_excel("data/region.xlsx")

world <-
  dat_empavg |>
  left_join(reg, by = c("ccode"))

#North America

northamerica <-
  world |>
  filter(region == "Noth America" | region == "North America")

north_am <-
  feols(wmn_empavg ~ covihme_ayem + covihme_ayemF + covwdi_exp + covwdi_expF + covwdi_fdi + covwdi_fdiF + covwdi_imp +  covwdi_impF +
          covwvs_rel + covwvs_relF + coviNY_GDP_PETR_RT_ZS +coviNY_GDP_PETR_RT_ZSF + covdemregion + covdemregionF + covloggdp + covloggdpF + covloggdpC + covloggdpCF
        | year + ccode | EV ~ l2CPcol2
        ,data = northamerica, cluster = ~ccode + year,
        ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))

summary(north_am)

#Africa

africa <-
  world |>
  filter(region == "Africa")

afr <-
  feols(wmn_empavg ~ covihme_ayem + covihme_ayemF + covwdi_exp + covwdi_expF + covwdi_fdi + covwdi_fdiF + covwdi_imp +  covwdi_impF +
          covwvs_rel + covwvs_relF + coviNY_GDP_PETR_RT_ZS +coviNY_GDP_PETR_RT_ZSF + covdemregion + covdemregionF + covloggdp + covloggdpF + covloggdpC + covloggdpCF 
        | year + ccode | EV ~ l2CPcol2
        ,data = africa, cluster = ~ccode + year,
        ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))

summary(afr)

#South America

southam <-
  world |>
  filter(region == "South America")


s_am <-
  feols(wmn_empavg ~ covihme_ayem + covihme_ayemF + covwdi_exp + covwdi_expF + covwdi_fdi + covwdi_fdiF + covwdi_imp +  covwdi_impF +
          covwvs_rel + covwvs_relF + coviNY_GDP_PETR_RT_ZS +coviNY_GDP_PETR_RT_ZSF + covdemregion + covdemregionF + covloggdp + covloggdpF + covloggdpC + covloggdpCF
        | year + ccode | EV ~ l2CPcol2
        ,data = southam, cluster = ~ccode + year,
        ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))

summary(s_am)

#Oceania

oceania <-
  world |>
  filter(region == "Oceania")

ocean <-
  feols(wmn_empavg ~ covihme_ayem + covihme_ayemF + covwdi_exp + covwdi_expF + covwdi_fdi + covwdi_fdiF + covwdi_imp +  covwdi_impF +
          covwvs_rel + covwvs_relF + coviNY_GDP_PETR_RT_ZS +coviNY_GDP_PETR_RT_ZSF + covdemregion + covdemregionF + covloggdp + covloggdpF + covloggdpC + covloggdpCF
        | year + ccode | EV ~ l2CPcol2
        ,data = oceania, cluster = ~ccode + year,
        ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))

summary(ocean)

#Asia

asia <-
  world |>
  filter(region == "Asia")

asia_reg <-
  feols(wmn_empavg ~ covihme_ayem + covihme_ayemF + covwdi_exp + covwdi_expF + covwdi_fdi + covwdi_fdiF + covwdi_imp +  covwdi_impF +
          covwvs_rel + covwvs_relF + coviNY_GDP_PETR_RT_ZS +coviNY_GDP_PETR_RT_ZSF + covdemregion + covdemregionF + covloggdp + covloggdpF + covloggdpC + covloggdpCF
        | year + ccode | EV ~ l2CPcol2
        ,data = asia, cluster = ~ccode + year,
        ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))

summary(asia_reg)

#Europe

euro <-
  world |>
  filter(region == "Europe")

europe <-
  feols(wmn_empavg ~ covihme_ayem + covihme_ayemF + covwdi_exp + covwdi_expF + covwdi_fdi + covwdi_fdiF + covwdi_imp +  covwdi_impF +
          covwvs_rel + covwvs_relF + coviNY_GDP_PETR_RT_ZS +coviNY_GDP_PETR_RT_ZSF + covdemregion + covdemregionF + covloggdp + covloggdpF + covloggdpC + covloggdpCF 
        | year + ccode | EV ~ l2CPcol2
        ,data = euro, cluster = ~ccode + year,
        ssc = ssc(fixef.K = "nested", cluster.adj = FALSE))


dat_emp |>
  select(v2x_gender, v2x_gencl, v2x_gencs, v2x_genpp) |>
  filter(!is.na(v2x_gender))

# Heterogeneity with freedom house ranks------------------------------------------

reg <- read_excel("data/region.xlsx")

world <-
  dat_empavgavg |>
  left_join(reg, by = c("ccode"))

#North America

northamerica <-
  world |>
  filter(region == "Noth America" | region == "North America")

north_am <-
  feols(
    rank_bi ~ covihme_ayem + covihme_ayemF + covwdi_exp + covwdi_expF + covwdi_fdi + covwdi_fdiF + covwdi_imp +  covwdi_impF +
      covwvs_rel + covwvs_relF + coviNY_GDP_PETR_RT_ZS + coviNY_GDP_PETR_RT_ZSF + covdemregion + covdemregionF + covloggdp + covloggdpF + covloggdpC + covloggdpCF
    | year + ccode | EV ~ l2CPcol2
    ,
    data = northamerica,
    cluster = ~ ccode + year,
    ssc = ssc(fixef.K = "nested", cluster.adj = FALSE)
  )

summary(north_am)

#Africa

africa <-
  world |>
  filter(region == "Africa")

afr <-
  feols(
    rank_bi ~ covihme_ayem + covihme_ayemF + covwdi_exp + covwdi_expF + covwdi_fdi + covwdi_fdiF + covwdi_imp +  covwdi_impF +
      covwvs_rel + covwvs_relF + coviNY_GDP_PETR_RT_ZS + coviNY_GDP_PETR_RT_ZSF + covdemregion + covdemregionF + covloggdp + covloggdpF + covloggdpC + covloggdpCF
    | year + ccode | EV ~ l2CPcol2
    ,
    data = africa,
    cluster = ~ ccode + year,
    ssc = ssc(fixef.K = "nested", cluster.adj = FALSE)
  )

summary(afr)

#South America

southam <-
  world |>
  filter(region == "South America")


s_am <-
  feols(
    rank_bi ~ covihme_ayem + covihme_ayemF + covwdi_exp + covwdi_expF + covwdi_fdi + covwdi_fdiF + covwdi_imp +  covwdi_impF +
      covwvs_rel + covwvs_relF + coviNY_GDP_PETR_RT_ZS + coviNY_GDP_PETR_RT_ZSF + covdemregion + covdemregionF + covloggdp + covloggdpF + covloggdpC + covloggdpCF
    | year + ccode | EV ~ l2CPcol2
    ,
    data = southam,
    cluster = ~ ccode + year,
    ssc = ssc(fixef.K = "nested", cluster.adj = FALSE)
  )

summary(s_am)

#Oceania

oceania <-
  world |>
  filter(region == "Oceania")

ocean <-
  feols(
    rank_bi ~ covihme_ayem + covihme_ayemF + covwdi_exp + covwdi_expF + covwdi_fdi + covwdi_fdiF + covwdi_imp +  covwdi_impF +
      covwvs_rel + covwvs_relF + coviNY_GDP_PETR_RT_ZS + coviNY_GDP_PETR_RT_ZSF + covdemregion + covdemregionF + covloggdp + covloggdpF + covloggdpC + covloggdpCF
    | year + ccode | EV ~ l2CPcol2
    ,
    data = oceania,
    cluster = ~ ccode + year,
    ssc = ssc(fixef.K = "nested", cluster.adj = FALSE)
  )

summary(ocean)

#Asia

asia <-
  world |>
  filter(region == "Asia")

asia_reg <-
  feols(
    rank_bi ~ covihme_ayem + covihme_ayemF + covwdi_exp + covwdi_expF + covwdi_fdi + covwdi_fdiF + covwdi_imp +  covwdi_impF +
      covwvs_rel + covwvs_relF + coviNY_GDP_PETR_RT_ZS + coviNY_GDP_PETR_RT_ZSF + covdemregion + covdemregionF + covloggdp + covloggdpF + covloggdpC + covloggdpCF
    | year + ccode | EV ~ l2CPcol2
    ,
    data = asia,
    cluster = ~ ccode + year,
    ssc = ssc(fixef.K = "nested", cluster.adj = FALSE)
  )

summary(asia_reg)

#Europe

euro <-
  world |>
  filter(region == "Europe")

europe <-
  feols(
    rank_bi ~ covihme_ayem + covihme_ayemF + covwdi_exp + covwdi_expF + covwdi_fdi + covwdi_fdiF + covwdi_imp +  covwdi_impF +
      covwvs_rel + covwvs_relF + coviNY_GDP_PETR_RT_ZS + coviNY_GDP_PETR_RT_ZSF + covdemregion + covdemregionF + covloggdp + covloggdpF + covloggdpC + covloggdpCF
    | year + ccode | EV ~ l2CPcol2
    ,
    data = euro,
    cluster = ~ ccode + year,
    ssc = ssc(fixef.K = "nested", cluster.adj = FALSE)
  )
