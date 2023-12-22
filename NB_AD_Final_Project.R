

# Calling Libraries -------------------------------------------------------

library(haven)
library(tidyverse)
library(gt)
library(modelsummary)
library(fixest)
library(glue)
library(multiwayvcov)
library(readxl)
library(panelView)
library(flextable)
library(scales)
library(broom)


final <- read_dta("data/Final_Main.dta")

figure <- read_dta("data/figuredata.dta")

freedom <- read_excel("data/FH_final.xlsx")

final <-
  figure |>
  select(year, ccode, logpop) |>
  left_join(final, by = c("year", "ccode"))

# Summary Stats -----------------------------------------------------------

stats <- function(x) {
  c(
    mean = mean(x[x != -99], na.rm = TRUE),
    std_Dev = sd(x[x != -99], na.rm = TRUE),
    max = max(x[x != -99], na.rm = TRUE),
    min = min(x[x != -99], na.rm = TRUE),
    n = n()
  )
}

results <- final |>
  summarize_at(
    vars(
      covwdi_imp,
      covwdi_exp,
      EV,
      covloggdpC,
      polity2avg,
      l2CPcol2,
      covihme_ayem,
      new_empinxavg
    ),
    stats
  ) |>
  t() |>
  as_tibble() |>
  rename(
    "Mean" = V1,
    "Std dev" = V2,
    "Max" = V3,
    "Min" = V4,
    "N" = V5
  ) |>
  mutate(
    Variables = c(
      "Import",
      "Export",
      "EU Aid",
      "Logged GDPPC",
      "Average Polity IV Scores",
      "Former Colony Status",
      "Average years of educ- male",
      "CIRI Human Empowerment Index"
    ),
    .before = 1
  )

results |>
  gt() |>
  fmt_number(decimals = 2) |>
  gtsave(filename = "desstats.png")


# FEOLS -------------------------------------------------------------------

#Recoding missing values

#final <-
#final |>
#mutate_all(~ ifelse(. == -99, NA, .))

#Running FEOLS for CIRI Index - Column 1 specifications with no co variates

filtered_data <-
  final |>
  filter(year >= "1987")

# general format (outcome ~ exogenous/controls | fixed effects | dep ~ instrument)

col1 <-
  feols(new_empinxavg ~ 1 | year + ccode | EV ~ l2CPcol2
        , filtered_data)

#col1 <-
# feols(new_empinxavg ~ 1 | year + ccode | EV ~ l2CPcol2
#, filtered_data, cluster = c("ccode", "year"))

summary(col1)

modelsummary(col1)

#Running FEOLS for CIRI Index - Column 2 specifications with co variates

col2 <-
  feols(
    new_empinxavg ~ covihme_ayem + covihme_ayemF + covwdi_exp + covwdi_expF + covwdi_fdi + covwdi_fdiF + covwdi_imp +  covwdi_impF +
      covwvs_rel + covwvs_relF + coviNY_GDP_PETR_RT_ZS + coviNY_GDP_PETR_RT_ZSF + covdemregion + covdemregionF + covloggdp + covloggdpF + covloggdpC + covloggdpCF
    | year + ccode | EV ~ l2CPcol2
    ,
    filtered_data,
    cluster = ~ ccode + year,
    ssc = ssc(fixef.K = "nested", cluster.adj = FALSE)
  )


summary(col2)

modelsummary(col2)

coef_names <- str_subset(colnames(filtered_data), "cov")
spec_form <-
  glue("new_empinxavg ~ {str_c(coef_names, collapse = ' + ')} | year + ccode | EV ~ l2CPcol2")
feols(as.formula(spec_form), filtered_data)




missing <-
  final |>
  select(new_empinxavg, polity2avg, EV, l2CPcol2) |>
  summarise(
    new_empinxavg_missing = sum(is.na(new_empinxavg)),
    polity2avg_missing = sum(is.na(polity2avg)),
    EV_missing = sum(is.na(EV)),
    l2CPcol2_missing = sum(is.na(l2CPcol2))
  )



library(naniar)

missing_plot <-
  final |>
  filter(year >= 1987) |>
  select(starts_with("cov"), new_empinxavg, polity2avg) |>
  mutate_all( ~ ifelse(. == -99, NA, .))

vis_miss(missing_plot)

gg_miss_var(missing_plot, show_pct = TRUE) +
  labs(title = "Percentage of data missing in covariates and outcome variables",
       x = "Covariates and Outcome")


#freedom wide to long

freedom_long <-
  freedom |>
  pivot_longer(
    cols = c(starts_with("19"), starts_with("20")),
    names_to = "year",
    values_to = "rank"
  )

gg_miss_var(freedom_long, show_pct = TRUE) +
  labs(title = "Percentage of data missing in covariates and outcome variables",
       x = "Covariates and Outcome")

freedom_long$year <- as.double(freedom_long$year)

new <-
  final |>
  left_join(freedom_long, by = c("ccode", "year"))


new_dat <-
  new |>
  filter(year >= "1987")


new_dat$rank <-
  relevel(new_dat$rank, ref = "NF")


library(MASS)

new_dat <-
  new_dat |>
  mutate(rank_b = case_when(rank == "F" ~ 1,
                            rank == "NF" ~ 0,
                            rank == "PF" ~ 0.5))

#regressing 2 step jump from Not Free to Free

feols(rankNPF ~ 1 | year + ccode | EV ~ l2CPcol2, new_dat)

coef_names <- str_subset(colnames(new_dat), "cov")

spec_form <-
  glue("rankNPF ~ {str_c(coef_names, collapse = ' + ')} | year + ccode | EV ~ l2CPcol2")

feols(as.formula(spec_form), new_dat)

spec_form <-
  glue("rankFNF ~ {str_c(coef_names, collapse = ' + ')} | year + ccode | EV ~ l2CPcol2")

feols(as.formula(spec_form), new_dat)


# FH avg ranks ------------------------------------------------------------


fh_avg <- read_excel("data/FH_avgranks.xlsx")

fhavg_long <-
  fh_avg |>
  pivot_longer(
    cols = c(starts_with("19"), starts_with("20")),
    names_to = "year",
    values_to = "rank_avg"
  )

fhavg_long$year <- as.double(fhavg_long$year)

fh_final <-
  final |>
  left_join(fhavg_long, by = c("ccode", "year"))


missing_plot <-
  fh_final |>
  filter(year >= 1987) |>
  select(rank_avg)

gg_miss_var(missing_plot, show_pct = TRUE)

fh_final <-
  fh_final |>
  mutate(rank_bi =
           case_when(rank_avg == "NF" ~ 0,
                     rank_avg == "PF" ~ 0.5,
                     rank_avg == "F"  ~ 1))


fh_final |>
  filter(year >= 1987)


#with religiosity
new_col <-
  feols(
    rank_bi ~ covihme_ayem + covihme_ayemF + covwdi_exp + covwdi_expF + covwdi_fdi + covwdi_fdiF + covwdi_imp +  covwdi_impF +
      covwvs_rel + covwvs_relF + coviNY_GDP_PETR_RT_ZS + coviNY_GDP_PETR_RT_ZSF + covdemregion + covdemregionF + covloggdp + covloggdpF + covloggdpC + covloggdpCF
    | year + ccode | EV ~ l2CPcol2
    ,
    fh_final,
    cluster = ~ ccode + year,
    ssc = ssc(fixef.K = "nested", cluster.adj = FALSE)
  )

summary(new_col)

modelsummary(new_col)

new_col2 <-
  feols(
    rank_bi ~ 1
    | year + ccode | EV ~ l2CPcol2
    ,
    fh_final,
    cluster = ~ ccode + year,
    ssc = ssc(fixef.K = "nested", cluster.adj = FALSE)
  )

summary(new_col2)
modelsummary(new_col2)

#without religiosity

new_col_2 <-
  feols(
    rank_bi ~ covihme_ayem + covihme_ayemF + covwdi_exp + covwdi_expF + covwdi_fdi + covwdi_fdiF + covwdi_imp +  covwdi_impF +
      coviNY_GDP_PETR_RT_ZS + coviNY_GDP_PETR_RT_ZSF + covdemregion + covdemregionF + covloggdp + covloggdpF + covloggdpC + covloggdpCF
    | year + ccode | EV ~ l2CPcol2
    ,
    fh_final,
    cluster = ~ ccode + year,
    ssc = ssc(fixef.K = "nested", cluster.adj = FALSE)
  )

summary(new_col_2)



#with democracy

fh_final <-
  fh_final |>
  mutate(politylag = lag(polity2avg, 2))

new_col_3 <-
  feols(
    rank_bi ~ covihme_ayem + covihme_ayemF + covwdi_exp + covwdi_expF + covwdi_fdi + covwdi_fdiF + covwdi_imp +  covwdi_impF +
      covwvs_rel + covwvs_relF + coviNY_GDP_PETR_RT_ZS + coviNY_GDP_PETR_RT_ZSF + covdemregion + covdemregionF + covloggdp + covloggdpF + covloggdpC + covloggdpCF + politylag
    | year + ccode | EV ~ l2CPcol2
    ,
    fh_final,
    cluster = ~ ccode + year,
    ssc = ssc(fixef.K = "nested", cluster.adj = FALSE)
  )

summary(new_col_3)

reg <- read_excel("data/region.xlsx")

world <-
  fh_final |>
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



# attempt to deal with leads-----------------------
new_dat <- new_dat |>
  mutate(
    rank_abl1 = lead(rank_b, 1),
    rank_abl2 = lead(rank_b, 2),
    rank_abl3 = lead(rank_b, 3),
    rank_abl4 = lead(rank_b, 4),
    rank_abl5 = lead(rank_b, 5),
    rank_abl6 = lead(rank_b, 6)
  )

specab_form <-
  glue("rank_b ~ {str_c(coef_names, collapse = ' + ')} | year + ccode | EV ~ l2CPcol2")
specabl1_form <-
  glue("rank_abl1 ~ {str_c(coef_names, collapse = ' + ')} | year + ccode | EV ~ l2CPcol2")
specabl2_form <-
  glue("rank_abl2 ~ {str_c(coef_names, collapse = ' + ')} | year + ccode | EV ~ l2CPcol2")
specabl3_form <-
  glue("rank_abl3 ~ {str_c(coef_names, collapse = ' + ')} | year + ccode | EV ~ l2CPcol2")
specabl4_form <-
  glue("rank_abl4 ~ {str_c(coef_names, collapse = ' + ')} | year + ccode | EV ~ l2CPcol2")
specabl5_form <-
  glue("rank_abl5 ~ {str_c(coef_names, collapse = ' + ')} | year + ccode | EV ~ l2CPcol2")
specabl6_form <-
  glue("rank_abl6 ~ {str_c(coef_names, collapse = ' + ')} | year + ccode | EV ~ l2CPcol2")


feols(
  as.formula(specab_form),
  cluster = ~ year + ccode,
  ssc = ssc(fixef.K = "nested", cluster.adj = FALSE),
  new_dat
)
feols(
  as.formula(specabl1_form),
  cluster = ~ year + ccode,
  ssc = ssc(fixef.K = "nested", cluster.adj = FALSE),
  new_dat
)
feols(
  as.formula(specabl2_form),
  cluster = ~ year + ccode,
  ssc = ssc(fixef.K = "nested", cluster.adj = FALSE),
  new_dat
)
feols(
  as.formula(specabl3_form),
  cluster = ~ year + ccode,
  ssc = ssc(fixef.K = "nested", cluster.adj = FALSE),
  new_dat
)
feols(
  as.formula(specabl4_form),
  cluster = ~ year + ccode,
  ssc = ssc(fixef.K = "nested", cluster.adj = FALSE),
  new_dat
)
feols(
  as.formula(specabl5_form),
  cluster = ~ year + ccode,
  ssc = ssc(fixef.K = "nested", cluster.adj = FALSE),
  new_dat
)




# event study plot --------------------------------------------------------

new_dat <- new_dat |>
  mutate(
    rank_bl1 = lead(rank_b, 1),
    rank_bl2 = lead(rank_b, 2),
    rank_bl3 = lead(rank_b, 3),
    rank_bl4 = lead(rank_b, 4),
    rank_bl5 = lead(rank_b, 5)
  )

specb_form <-
  glue("rank_b ~ {str_c(coef_names, collapse = ' + ')} | year + ccode | EV ~ l2CPcol2")
specbl1_form <-
  glue("rank_bl1 ~ {str_c(coef_names, collapse = ' + ')} | year + ccode | EV ~ l2CPcol2")
specbl2_form <-
  glue("rank_bl2 ~ {str_c(coef_names, collapse = ' + ')} | year + ccode | EV ~ l2CPcol2")
specbl3_form <-
  glue("rank_bl3 ~ {str_c(coef_names, collapse = ' + ')} | year + ccode | EV ~ l2CPcol2")
specbl4_form <-
  glue("rank_bl4 ~ {str_c(coef_names, collapse = ' + ')} | year + ccode | EV ~ l2CPcol2")
specbl5_form <-
  glue("rank_bl5 ~ {str_c(coef_names, collapse = ' + ')} | year + ccode | EV ~ l2CPcol2")

git7 <-
  feols(
    as.formula(specb_form),
    cluster = ~ year + ccode,
    ssc = ssc(fixef.K = "nested", cluster.adj = FALSE),
    new_dat
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
    new_dat
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
    new_dat
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
    new_dat
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
    new_dat
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
    new_dat
  ) |>
  tidy() |>
  mutate(twlestimate12 = estimate) |>
  filter(term == "fit_EV") |>
  mutate(term = recode_factor(term, fit_EV = 5)) |>
  mutate(twlstd.error12 = std.error)


# Attempt to create event study plot ----------
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

rank_plot <- git_evt |>
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
