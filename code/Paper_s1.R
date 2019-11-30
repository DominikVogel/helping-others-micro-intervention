# THIS IS THE BEGINNING OF STUDY 1 CODE XXXXXXXXXXXXXXXXXXXXXX ################

# A.1 Import data #############################################################
coltypes <- cols(
  id = col_double(),
  startdate = col_datetime(format = ""),
  submitdate = col_datetime(format = ""),
  lastpage = col_double(),
  treatment = col_double(),
  willingnessjobrec1 = col_double(),
  willingnessjobrec2 = col_double(),
  paffect1 = col_double(),
  paffect2 = col_double(),
  paffect3 = col_double(),
  paffect4 = col_double(),
  paffect5 = col_double(),
  paffect6 = col_double(),
  naffect1 = col_double(),
  naffect2 = col_double(),
  naffect3 = col_double(),
  naffect4 = col_double(),
  naffect5 = col_double(),
  naffect6 = col_double(),
  turnover1 = col_double(),
  turnover2 = col_double(),
  turnover3 = col_double(),
  prosocialimpact1 = col_double(),
  prosocialimpact2 = col_double(),
  prosocialimpact3 = col_double(),
  manipulationcheck = col_double(),
  taskvariety1 = col_double(),
  taskvariety2 = col_double(),
  taskvariety3 = col_double(),
  taskvariety4 = col_double(),
  taskvariety5 = col_double(),
  taskanalyz1 = col_double(),
  taskanalyz2 = col_double(),
  taskanalyz3 = col_double(),
  taskanalyz4 = col_double(),
  attention1 = col_double(),
  autonomy1 = col_double(),
  autonomy2 = col_double(),
  autonomy3 = col_double(),
  beneficiaries1 = col_double(),
  beneficiaries2 = col_double(),
  attention2 = col_double(),
  manager = col_double(),
  gender = col_double(),
  yearbirth = col_double(),
  educator = col_double(),
  sector = col_double(),
  subsector = col_double(),
  atwork = col_double(),
  duration_treatment_general = col_double(),
  duration_treatment_prosocial = col_double(),
  duration_treatment_societal = col_double()
)

df_merge <- read_csv(here("data", "Study1_public.csv"), col_types = coltypes)
#df_merge <- read_csv(here("data", "S1_full.csv"))





# A.5 Generate variables ######################################################

# A.5.1 Generate factor variables =============================================
df_merge$treatment_f <- factor(df_merge$treatment)
df_merge$gender_f <- factor(df_merge$gender, 
                            labels = c("Male", "Female", "Other"))


# A.5.2 Set NA categories =====================================================
df_merge <- df_merge %>% 
  mutate(educator = ifelse(educator == 2, NA, educator))
#df_merge <- df_merge %>% 
#  mutate(sector = ifelse(sector == 4, NA, sector))
df_merge <- df_merge %>% 
  mutate(subsector = ifelse(subsector == 0, NA, subsector))
df_merge <- df_merge %>% 
  mutate(atwork = ifelse(atwork == 2, NA, atwork))




# A.5.3 Reverse items =========================================================
df_merge <- df_merge %>% mutate(turnover1 = 8 - turnover1)
df_merge <- df_merge %>% mutate(willingnessjobrec2 = 6 - willingnessjobrec2)
df_merge <- df_merge %>% mutate(taskanalyz1 = 6 - taskanalyz1)
df_merge <- df_merge %>% mutate(taskanalyz2 = 6 - taskanalyz2)
df_merge <- df_merge %>% mutate(taskanalyz3 = 6 - taskanalyz3)
df_merge <- df_merge %>% mutate(taskanalyz4 = 6 - taskanalyz4)
df_merge <- df_merge %>% mutate(taskvariety1 = 6 - taskvariety1)
df_merge <- df_merge %>% mutate(taskvariety2 = 6 - taskvariety2)
df_merge <- df_merge %>% mutate(taskvariety3 = 6 - taskvariety3)
df_merge <- df_merge %>% mutate(taskvariety4 = 6 - taskvariety4)
df_merge <- df_merge %>% mutate(taskvariety5 = 6 - taskvariety5)


# A.5.4 Create Dummies ========================================================
df_merge <- df_merge %>% mutate(public = ifelse(sector == 1, 1, 0),
                                public = ifelse(is.na(sector), NA, public))
df_merge <- df_merge %>% mutate(private = ifelse(sector == 3, 1, 0),
                                private = ifelse(is.na(sector), NA, private))  
df_merge <- df_merge %>% mutate(npo = ifelse(sector == 2, 1, 0),
                                npo = ifelse(is.na(sector), NA, npo))
df_merge <- df_merge %>% mutate(sector_na = ifelse(sector == 4, 1, 0),
                                sector_na = ifelse(is.na(sector), NA, 
                                                   sector_na))
df_merge <- df_merge %>% mutate(manager_dummy = ifelse(manager == 0, 0, 1),
                                manager_dummy = ifelse(is.na(manager), NA, 
                                                       manager_dummy))
df_merge <- df_merge %>% mutate(male = ifelse(gender == 0, 1, 0),
                                male = ifelse(is.na(gender), NA, male),
                                female = ifelse(gender == 1, 1, 0),
                                female = ifelse(is.na(gender), NA, female),
                                gender_other = ifelse(gender == 2, 1, 0),
                                gender_other = ifelse(is.na(gender), 
                                                      NA, gender_other))
df_merge <- df_merge %>% 
  mutate(treatment_dummy1 = ifelse(treatment == 1, 1, 0),
         treatment_dummy2 = ifelse(treatment == 2, 1, 0),
         treatment_dummy3 = ifelse(treatment == 3, 1, 0),
         treatment_dummy4 = ifelse(treatment == 4, 1, 0))


# A.5.5 Create age ============================================================
df_merge <- df_merge %>%
  mutate(age = 2018 - yearbirth)


# A.5.7 Create completetion time ==============================================
df_merge <- df_merge %>% mutate(completion_time = submitdate - startdate)



# A.5.8 Create dependent variables ============================================

## Positive Affect
crona_paffect <- psych::alpha(select(df_merge, paffect1, paffect2, paffect3, 
                                     paffect4, paffect5, paffect6))
crona_paffect <- crona_paffect[["total"]][["raw_alpha"]]
df_merge <- mean_index(df_merge, "paffect",
                       c("paffect1", "paffect2", "paffect3", 
                         "paffect4", "paffect5", "paffect6"))
## Negative Affect
crona_naffect <- psych::alpha(select(df_merge, naffect1, naffect2, naffect3,
                                     naffect4, naffect5, naffect6))
crona_naffect <- crona_naffect[["total"]][["raw_alpha"]]
df_merge <- mean_index(df_merge, "naffect",
                       c("naffect1", "naffect2", "naffect3", 
                         "naffect4", "naffect5", "naffect6"))

## Turnover intention
crona_turnover <- psych::alpha(select(df_merge, turnover1, 
                                      turnover2, turnover3))
crona_turnover <- crona_turnover[["total"]][["raw_alpha"]]
df_merge <- mean_index(df_merge, "turnover",
                       c("turnover1", "turnover2", "turnover3"))

## willingness to recommend job
crona_will <- psych::alpha(select(df_merge, 
                                  willingnessjobrec1, willingnessjobrec2))
crona_will <- crona_will[["total"]][["raw_alpha"]]
df_merge <- mean_index(df_merge, 
                       "willingnessjobrec", 
                       c("willingnessjobrec1", 
                         "willingnessjobrec2"))


# A.5.9 Create moderator / control variables ==================================

## Contact with beneficiaries
crona_bene <- psych::alpha(select(df_merge, beneficiaries1, beneficiaries2))
crona_bene <- crona_bene[["total"]][["raw_alpha"]]
df_merge <- mean_index(df_merge, "beneficiaries",
                       c("beneficiaries1", "beneficiaries2"))

## Perceived prosocial impact
crona_psi <- psych::alpha(select(df_merge, prosocialimpact1, 
                                 prosocialimpact2, prosocialimpact3))
crona_psi <- crona_psi[["total"]][["raw_alpha"]]
df_merge <- mean_index(df_merge, "prosocialimpact",
                       c("prosocialimpact1", 
                         "prosocialimpact2",
                         "prosocialimpact3"))

## Task variety
crona_taskvar <- psych::alpha(select(df_merge, taskvariety1, taskvariety2,
                                     taskvariety3, taskvariety4, taskvariety5))
crona_taskvar <- crona_taskvar[["total"]][["raw_alpha"]]
df_merge <- mean_index(df_merge, "taskvariety",
                       c("taskvariety1", "taskvariety2",
                         "taskvariety3", "taskvariety4", 
                         "taskvariety5"))

## Task analyzability
crona_taskanalyz <- psych::alpha(select(df_merge, taskanalyz1, taskanalyz2, 
                                        taskanalyz3, taskanalyz4))
crona_taskanalyz <- crona_taskanalyz[["total"]][["raw_alpha"]]
df_merge <- mean_index(df_merge, "taskanalyz",
                       c("taskanalyz1", "taskanalyz2", 
                         "taskanalyz3", "taskanalyz4"))

## Job autonomy
crona_auto <- psych::alpha(select(df_merge, autonomy1, autonomy2, autonomy3))
crona_auto <- crona_auto[["total"]][["raw_alpha"]]
df_merge <- mean_index(df_merge, "autonomy",
                       c("autonomy1", "autonomy2", "autonomy3"))



# A.6 Clean data ##############################################################

# A.6.1 Store initial n =======================================================
n_total <- tibble(treatment = "total", initial = nrow(df_merge))
obs <- df_merge %>% 
  group_by(treatment) %>% 
  summarise(initial = n()) %>%
  mutate(treatment = as.character(treatment))
obs <- bind_rows(n_total, obs)
rm(n_total)


# A.6.2 Remove obs how did not receive treatment ==============================
df_merge <- df_merge %>% 
  mutate(no_treat = ifelse(treatment == 3 & is.na(duration_treatment_prosocial), 1, 0),
         no_treat = ifelse(treatment == 4 & is.na(duration_treatment_societal),
                           1, no_treat),
         no_treat = ifelse(treatment == 2 & is.na(duration_treatment_general),
                           1, no_treat)) %>%
  filter(no_treat != 1) %>%
  select(-no_treat)


# Store n
n_total <- tibble(treatment = "total", receive_treat = nrow(df_merge))
obs_receive <- df_merge %>% 
  group_by(treatment) %>% 
  summarise(receive_treat = n()) %>%
  mutate(treatment = as.character(treatment))
obs_receive <- bind_rows(n_total, obs_receive)
obs <- left_join(obs, obs_receive)
rm(obs_receive)



# A.6.3 Remove obs how did not answer dependent variable ====================== 
df_merge <- df_merge %>% 
  mutate(mi_dep = ifelse(is.na(paffect) | 
                           is.na(naffect) |
                           is.na(turnover) |
                           is.na(willingnessjobrec),
                         1, 0)) %>%
  filter(mi_dep != 1) %>%
  select(-mi_dep)


# A.6.4 Remove obs how did not finish questionnaire ===========================
df_merge <- df_merge %>% filter(lastpage == 11)

# Store n
n_total <- tibble(treatment = "total", complete = nrow(df_merge))
obs_complete <- df_merge %>% 
  group_by(treatment) %>% 
  summarise(complete = n()) %>%
  mutate(treatment = as.character(treatment))
obs_complete <- bind_rows(n_total, obs_complete)
obs <- left_join(obs, obs_complete)
rm(obs_complete)



# A.6.5 Remove obs with failed attention check ================================
df_merge <- df_merge %>% 
  filter(attention1 == 3) %>% 
  filter(attention2 == 1)

# Store n
n_total <- tibble(treatment = "total", attention = nrow(df_merge))
obs_attention <- df_merge %>% 
  group_by(treatment) %>% 
  summarise(attention = n()) %>%
  mutate(treatment = as.character(treatment))
obs_attention <- bind_rows(n_total, obs_attention)
obs <- left_join(obs, obs_attention)
rm(obs_attention)


# A.6.6 Remove obs who tried to cheat =========================================
df_merge <- df_merge %>% 
  filter(id != 500)

# Store n
n_total <- tibble(treatment = "total", final = nrow(df_merge))
obs_final <- df_merge %>% 
  group_by(treatment) %>% 
  summarise(final = n()) %>%
  mutate(treatment = as.character(treatment))
obs_final <- bind_rows(n_total, obs_final)
obs <- left_join(obs, obs_final)
rm(obs_final, n_total)

obs








# A.7 Descriptives ############################################################
desc <- df_merge %>% 
  group_by(treatment) %>% 
  # Means and SDs per group
  summarise(paffect_m = func_round_mean(paffect),
            paffect_sd = func_round_sd(paffect),
            naffect_m = func_round_mean(naffect),
            naffect_sd = func_round_sd(naffect),
            turnover_m = func_round_mean(turnover),
            turnover_sd = func_round_sd(turnover),
            will_m = func_round_mean(willingnessjobrec),
            will_sd = func_round_sd(willingnessjobrec),
            psi_m = func_round_mean(prosocialimpact),
            psi_sd = func_round_sd(prosocialimpact),
            age_m = func_round_mean(age),
            age_sd = func_round_sd(age),
            female_m = func_round_mean(female),
            female_sd = func_round_sd(female),
            manager_m = func_round_mean(manager_dummy),
            manager_sd = func_round_sd(manager_dummy),
            n = n()) %>%
  # put SDs in parantheses
  mutate(paffect_sd = paste0("(", paffect_sd, ")"),
         naffect_sd = paste0("(", naffect_sd, ")"),
         turnover_sd = paste0("(", turnover_sd, ")"),
         will_sd = paste0("(", will_sd, ")"),
         psi_sd = paste0("(", psi_sd, ")"),
         age_sd = paste0("(", age_sd, ")"),
         female_sd = paste0("(", female_sd, ")"),
         manager_sd = paste0("(", manager_sd, ")")) %>%
  # Transpond table so groups are columns
  rownames_to_column %>% 
  gather(treatment, value, -rowname) %>% 
  spread(rowname, value) %>%
  filter(treatment != "treatment") %>%
  # Generate factor to sort table
  mutate(var = factor(treatment, 
                      levels = c("paffect_m", "paffect_sd",
                                 "naffect_m", "naffect_sd",
                                 "turnover_m", "turnover_sd",
                                 "will_m", "will_sd",
                                 "psi_m", "psi_sd",
                                 "age_m", "age_sd",
                                 "female_m", "female_sd",
                                 "manager_m", "manager_sd",
                                 "n"))) %>%
  # Sort
  arrange(var) %>%
  select(-var)

desc





# A.8 Manipulation Check ######################################################

## A: What exactly did we ask you to reflect upon?
df_merge$manipulationcheck <- 
  factor(df_merge$manipulationcheck,
         levels = c(1, 2, 3, 4),
         labels = c("Your job", 
                    "Your job & how you help others", 
                    "Your job & how it helps society",
                    "Don't remember")) 

# Cross-Table
manipulationcheck1 <- df_merge %>% 
  filter(treatment != 1)
manipulationcheck1 <- table(manipulationcheck1$manipulationcheck, 
                            manipulationcheck1$treatment)
manipulationcheck1



## B: Perceived prosocial impact
df_merge %>% 
  group_by(treatment) %>% 
  summarise(mean = mean(prosocialimpact),
            sd = sd(prosocialimpact))

aov_prosocialimpact <- aov(prosocialimpact ~ treatment_f, data = df_merge)
summary(aov_prosocialimpact)



# A.9 ANOVA Hypotheses & descriptives #########################################

## Positive affect
aov_paffect <- aov(df_merge$paffect ~ factor(df_merge$treatment))
summary(aov_paffect)

### Effect size Cohen's f: 
effsize_paffect_aov <- sjstats::cohens_f(aov_paffect)




## Negative Affect
aov_naffect <- aov(df_merge$naffect ~ factor(df_merge$treatment))
summary(aov_naffect)

### Effect size Cohen's f: 
effsize_naffect_aov <- sjstats::cohens_f(aov_naffect)





## Turnover intention
aov_turnover <- aov(df_merge$turnover ~ factor(df_merge$treatment))
summary(aov_turnover)

### Effect size Cohen's f: 
effsize_turnover_aov <- sjstats::cohens_f(aov_turnover)





## willingness to recommend job
aov_will <- aov(df_merge$willingnessjobrec ~ factor(df_merge$treatment))
summary(aov_will)

### Effect size Cohen's f: 
effsize_will_aov <- sjstats::cohens_f(aov_will)


# A.9.1 Balance test ==========================================================
chi_female <- chisq.test(table(df_merge$gender_f, df_merge$treatment_f))
chi_manager <- chisq.test(table(df_merge$manager_dummy, df_merge$treatment_f))
aov_age <- aov(age ~ treatment_f, data = df_merge)




# A.10 Group comparisons ######################################################

# Generate treatment factor with reversed oreder of treatments
# Ensures that the mean differences have the right sign
df_merge <- df_merge %>%
  mutate(treatment_f_reversed = factor(treatment, 
                                       levels = c("4", "3", "2", "1")))


# A.10.1 Generate subsamples ==================================================
df_merge1 <- df_merge %>% filter(treatment == 1)
df_merge2 <- df_merge %>% filter(treatment == 2)
df_merge3 <- df_merge %>% filter(treatment == 3)
df_merge4 <- df_merge %>% filter(treatment == 4)
df_merge1_3 <- df_merge %>% filter(treatment == 1 | treatment == 3)
df_merge1_4 <- df_merge %>% filter(treatment == 1 | treatment == 4)
df_merge2_3 <- df_merge %>% filter(treatment == 2 | treatment == 3)
df_merge2_4 <- df_merge %>% filter(treatment == 2 | treatment == 4)


# A.10.2 Group comparison positive affect =====================================

# empty vector to store p-values
peace <- vector(mode = "numeric", length = 4)
pairwise <- tibble(comparison = c("1 vs 3", "1 vs 4", "2 vs 3", "2 vs 4"))
pairwise <- pairwise %>% mutate(t = NA)
pairwise <- pairwise %>% mutate(df = NA)
pairwise <- pairwise %>% mutate(mean.diff = NA)
pairwise <- pairwise %>% mutate(eff_size_d = NA)
pairwise <- pairwise %>% mutate(eff_size_g = NA)
pairwise <- pairwise %>% mutate(p = NA)


# A.10.2.1 1 vs 3 -------------------------------------------------------------
# calculate p-values from t-tests
tee <- t.test(paffect ~ treatment_f_reversed, data = df_merge1_3,
              alternative = "greater")

# Effect Size (Hedge's g)
effsize_g <- effsize::cohen.d(df_merge1_3$paffect, 
                              as.factor(df_merge1_3$treatment),
                              hedges.correction = TRUE,
                              na.rm = TRUE)

# Effect Size (Cohen's d)
effsize_d <- MBESS::ci.smd(ncp = tee[["statistic"]][["t"]], 
                           n.1=obs$final[2], 
                           n.2=obs$final[4], 
                           conf.level=0.95)$smd

# Write t-test, p and effect sizes in table
pairwise <- pairwise %>% 
  mutate(t = ifelse(comparison == "1 vs 3",
                    tee[["statistic"]][["t"]], t)) %>%
  mutate(df = ifelse(comparison == "1 vs 3",
                     tee[["parameter"]][["df"]], df)) %>%
  mutate(mean.diff = ifelse(comparison == "1 vs 3",
                            (tee[["estimate"]][[1]] - tee[["estimate"]][[2]]), 
                            mean.diff)) %>%
  mutate(p = ifelse(comparison == "1 vs 3",
                    tee[["p.value"]], p)) %>%
  mutate(eff_size_g = ifelse(comparison == "1 vs 3",
                             round(effsize_g[["estimate"]][["1"]], 3), 
                             eff_size_g)) %>%
  mutate(eff_size_d = ifelse(comparison == "1 vs 3",
                             round(effsize_d, 3), 
                             eff_size_d))


# Store p-value separatly (for adjustment)
peace[1] <- tee[["p.value"]]


# A.10.2.2 1 vs 4 -------------------------------------------------------------
# calculate p-values from t-tests
tee <- t.test(paffect ~ treatment_f_reversed, data = df_merge1_4,
              alternative = "greater")

# Effect Size (Hedge's g)
effsize_g <- effsize::cohen.d(df_merge1_4$paffect, 
                              as.factor(df_merge1_4$treatment),
                              hedges.correction = TRUE,
                              na.rm = TRUE)

# Effect Size (Cohen's d)
effsize_d <- MBESS::ci.smd(ncp = tee[["statistic"]][["t"]], 
                           n.1=obs$final[2], 
                           n.2=obs$final[5], 
                           conf.level=0.95)$smd

# Write t-test, p and effect sizes in table
pairwise <- pairwise %>% 
  mutate(t = ifelse(comparison == "1 vs 4",
                    tee[["statistic"]][["t"]], t)) %>%
  mutate(df = ifelse(comparison == "1 vs 4",
                     tee[["parameter"]][["df"]], df)) %>%
  mutate(mean.diff = ifelse(comparison == "1 vs 4",
                            (tee[["estimate"]][[1]] - tee[["estimate"]][[2]]), 
                            mean.diff)) %>%
  mutate(p = ifelse(comparison == "1 vs 4",
                    tee[["p.value"]], p)) %>%
  mutate(eff_size_g = ifelse(comparison == "1 vs 4",
                             round(effsize_g[["estimate"]][["1"]], 3), 
                             eff_size_g)) %>%
  mutate(eff_size_d = ifelse(comparison == "1 vs 4",
                             round(effsize_d, 3), 
                             eff_size_d))

# Store p-value separatly (for adjustment)
peace[2] <- tee[["p.value"]]

# A.10.2.3 2 vs 3 -------------------------------------------------------------
# calculate p-values from t-tests
tee <- t.test(paffect ~ treatment_f_reversed, data = df_merge2_3,
              alternative = "greater")

# Effect Size (Hedge's g)
effsize_g <- effsize::cohen.d(df_merge2_3$paffect, 
                              as.factor(df_merge2_3$treatment),
                              hedges.correction = TRUE,
                              na.rm = TRUE)

# Effect Size (Cohen's d)
effsize_d <- MBESS::ci.smd(ncp = tee[["statistic"]][["t"]], 
                           n.1=obs$final[3], 
                           n.2=obs$final[4], 
                           conf.level=0.95)$smd

# Write t-test, p and effect sizes in table
pairwise <- pairwise %>% 
  mutate(t = ifelse(comparison == "2 vs 3",
                    tee[["statistic"]][["t"]], t)) %>%
  mutate(df = ifelse(comparison == "2 vs 3",
                     tee[["parameter"]][["df"]], df)) %>%
  mutate(mean.diff = ifelse(comparison == "2 vs 3",
                            (tee[["estimate"]][[1]] - tee[["estimate"]][[2]]), 
                            mean.diff)) %>%
  mutate(p = ifelse(comparison == "2 vs 3",
                    tee[["p.value"]], p)) %>%
  mutate(eff_size_g = ifelse(comparison == "2 vs 3",
                             round(effsize_g[["estimate"]][["2"]], 3), 
                             eff_size_g)) %>%
  mutate(eff_size_d = ifelse(comparison == "2 vs 3",
                             round(effsize_d, 3), 
                             eff_size_d))

# Store p-value separatly (for adjustment)
peace[3] <- tee[["p.value"]]



# A.10.2.4 2 vs 4 -------------------------------------------------------------
# calculate p-values from t-tests
tee <- t.test(paffect ~ treatment_f_reversed, data = df_merge2_4,
              alternative = "greater")

# Effect Size (Hedge's g)
effsize_g <- effsize::cohen.d(df_merge2_4$paffect, 
                              as.factor(df_merge2_4$treatment),
                              hedges.correction = TRUE,
                              na.rm = TRUE)

# Effect Size (Cohen's d)
effsize_d <- MBESS::ci.smd(ncp = tee[["statistic"]][["t"]], 
                           n.1=obs$final[3], 
                           n.2=obs$final[5], 
                           conf.level=0.95)$smd

# Write t-test, p and effect sizes in table
pairwise <- pairwise %>% 
  mutate(t = ifelse(comparison == "2 vs 4",
                    tee[["statistic"]][["t"]], t)) %>%
  mutate(df = ifelse(comparison == "2 vs 4",
                     tee[["parameter"]][["df"]], df)) %>%
  mutate(mean.diff = ifelse(comparison == "2 vs 4",
                            (tee[["estimate"]][[1]] - tee[["estimate"]][[2]]), 
                            mean.diff)) %>%
  mutate(p = ifelse(comparison == "2 vs 4",
                    tee[["p.value"]], p)) %>%
  mutate(eff_size_g = ifelse(comparison == "2 vs 4",
                             round(effsize_g[["estimate"]][["2"]], 3), 
                             eff_size_g)) %>%
  mutate(eff_size_d = ifelse(comparison == "2 vs 4",
                             round(effsize_d, 3), 
                             eff_size_d))

# Store p-value separatly (for adjustment)
peace[4] <- tee[["p.value"]]



# A.10.2.5 Adjusted p-values --------------------------------------------------
padjust <- p.adjust(peace, method = "BH")

pairwise <- bind_cols(pairwise, as_tibble(padjust))
pairwise <- pairwise %>% rename(p.adjust = value)

pairwise <- pairwise %>% mutate(star = ifelse(p.adjust < 0.05, "*", ""))
#pairwise %>% select(comparison, mean.diff, eff_size_g, p, p.adjust, star)

pairwise_paffect <- pairwise



# A.10.3 Group comparison negative affect =====================================

# empty vector to store p-values
peace <- vector(mode = "numeric", length = 4)
pairwise <- tibble(comparison = c("1 vs 3", "1 vs 4", "2 vs 3", "2 vs 4"))
pairwise <- pairwise %>% mutate(t = NA)
pairwise <- pairwise %>% mutate(df = NA)
pairwise <- pairwise %>% mutate(mean.diff = NA)
pairwise <- pairwise %>% mutate(eff_size_d = NA)
pairwise <- pairwise %>% mutate(eff_size_g = NA)
pairwise <- pairwise %>% mutate(p = NA)


# A.10.3.1 1 vs 3 -------------------------------------------------------------
# calculate p-values from t-tests
tee <- t.test(naffect ~ treatment_f_reversed, data = df_merge1_3,
              alternative = "less")

# Effect Size (Hedge's g)
effsize_g <- effsize::cohen.d(df_merge1_3$naffect, 
                              as.factor(df_merge1_3$treatment),
                              hedges.correction = TRUE,
                              na.rm = TRUE)

# Effect Size (Cohen's d)
effsize_d <- MBESS::ci.smd(ncp = tee[["statistic"]][["t"]], 
                           n.1=obs$final[2], 
                           n.2=obs$final[4], 
                           conf.level=0.95)$smd

# Write t-test, p and effect sizes in table
pairwise <- pairwise %>% 
  mutate(t = ifelse(comparison == "1 vs 3",
                    tee[["statistic"]][["t"]], t)) %>%
  mutate(df = ifelse(comparison == "1 vs 3",
                     tee[["parameter"]][["df"]], df)) %>%
  mutate(mean.diff = ifelse(comparison == "1 vs 3",
                            (tee[["estimate"]][[1]] - tee[["estimate"]][[2]]), 
                            mean.diff)) %>%
  mutate(p = ifelse(comparison == "1 vs 3",
                    tee[["p.value"]], p)) %>%
  mutate(eff_size_g = ifelse(comparison == "1 vs 3",
                             round(effsize_g[["estimate"]][["1"]], 3), 
                             eff_size_g)) %>%
  mutate(eff_size_d = ifelse(comparison == "1 vs 3",
                             round(effsize_d, 3), 
                             eff_size_d))


# Store p-value separatly (for adjustment)
peace[1] <- tee[["p.value"]]


# A.10.3.2 1 vs 4 -------------------------------------------------------------
# calculate p-values from t-tests
tee <- t.test(naffect ~ treatment_f_reversed, data = df_merge1_4,
              alternative = "less")

# Effect Size (Hedge's g)
effsize_g <- effsize::cohen.d(df_merge1_4$naffect, 
                              as.factor(df_merge1_4$treatment),
                              hedges.correction = TRUE,
                              na.rm = TRUE)

# Effect Size (Cohen's d)
effsize_d <- MBESS::ci.smd(ncp = tee[["statistic"]][["t"]], 
                           n.1=obs$final[2], 
                           n.2=obs$final[5], 
                           conf.level=0.95)$smd

# Write t-test, p and effect sizes in table
pairwise <- pairwise %>% 
  mutate(t = ifelse(comparison == "1 vs 4",
                    tee[["statistic"]][["t"]], t)) %>%
  mutate(df = ifelse(comparison == "1 vs 4",
                     tee[["parameter"]][["df"]], df)) %>%
  mutate(mean.diff = ifelse(comparison == "1 vs 4",
                            (tee[["estimate"]][[1]] - tee[["estimate"]][[2]]), 
                            mean.diff)) %>%
  mutate(p = ifelse(comparison == "1 vs 4",
                    tee[["p.value"]], p)) %>%
  mutate(eff_size_g = ifelse(comparison == "1 vs 4",
                             round(effsize_g[["estimate"]][["1"]], 3), 
                             eff_size_g)) %>%
  mutate(eff_size_d = ifelse(comparison == "1 vs 4",
                             round(effsize_d, 3), 
                             eff_size_d))

# Store p-value separatly (for adjustment)
peace[2] <- tee[["p.value"]]

# A.10.3.3 2 vs 3 -------------------------------------------------------------
# calculate p-values from t-tests
tee <- t.test(naffect ~ treatment_f_reversed, data = df_merge2_3,
              alternative = "less")

# Effect Size (Hedge's g)
effsize_g <- effsize::cohen.d(df_merge2_3$naffect, 
                              as.factor(df_merge2_3$treatment),
                              hedges.correction = TRUE,
                              na.rm = TRUE)

# Effect Size (Cohen's d)
effsize_d <- MBESS::ci.smd(ncp = tee[["statistic"]][["t"]], 
                           n.1=obs$final[3], 
                           n.2=obs$final[4], 
                           conf.level=0.95)$smd

# Write t-test, p and effect sizes in table
pairwise <- pairwise %>% 
  mutate(t = ifelse(comparison == "2 vs 3",
                    tee[["statistic"]][["t"]], t)) %>%
  mutate(df = ifelse(comparison == "2 vs 3",
                     tee[["parameter"]][["df"]], df)) %>%
  mutate(mean.diff = ifelse(comparison == "2 vs 3",
                            (tee[["estimate"]][[1]] - tee[["estimate"]][[2]]), 
                            mean.diff)) %>%
  mutate(p = ifelse(comparison == "2 vs 3",
                    tee[["p.value"]], p)) %>%
  mutate(eff_size_g = ifelse(comparison == "2 vs 3",
                             round(effsize_g[["estimate"]][["2"]], 3), 
                             eff_size_g)) %>%
  mutate(eff_size_d = ifelse(comparison == "2 vs 3",
                             round(effsize_d, 3), 
                             eff_size_d))

# Store p-value separatly (for adjustment)
peace[3] <- tee[["p.value"]]



# A.10.3.4 2 vs 4 -------------------------------------------------------------
# calculate p-values from t-tests
tee <- t.test(naffect ~ treatment_f_reversed, data = df_merge2_4,
              alternative = "less")

# Effect Size (Hedge's g)
effsize_g <- effsize::cohen.d(df_merge2_4$naffect, 
                              as.factor(df_merge2_4$treatment),
                              hedges.correction = TRUE,
                              na.rm = TRUE)

# Effect Size (Cohen's d)
effsize_d <- MBESS::ci.smd(ncp = tee[["statistic"]][["t"]], 
                           n.1=obs$final[3], 
                           n.2=obs$final[5], 
                           conf.level=0.95)$smd

# Write t-test, p and effect sizes in table
pairwise <- pairwise %>% 
  mutate(t = ifelse(comparison == "2 vs 4",
                    tee[["statistic"]][["t"]], t)) %>%
  mutate(df = ifelse(comparison == "2 vs 4",
                     tee[["parameter"]][["df"]], df)) %>%
  mutate(mean.diff = ifelse(comparison == "2 vs 4",
                            (tee[["estimate"]][[1]] - tee[["estimate"]][[2]]), 
                            mean.diff)) %>%
  mutate(p = ifelse(comparison == "2 vs 4",
                    tee[["p.value"]], p)) %>%
  mutate(eff_size_g = ifelse(comparison == "2 vs 4",
                             round(effsize_g[["estimate"]][["2"]], 3), 
                             eff_size_g)) %>%
  mutate(eff_size_d = ifelse(comparison == "2 vs 4",
                             round(effsize_d, 3), 
                             eff_size_d))

# Store p-value separatly (for adjustment)
peace[4] <- tee[["p.value"]]



# A.10.3.5 Adjusted p-values --------------------------------------------------
padjust <- p.adjust(peace, method = "BH")

pairwise <- bind_cols(pairwise, as_tibble(padjust))
pairwise <- pairwise %>% rename(p.adjust = value)

pairwise <- pairwise %>% mutate(star = ifelse(p.adjust < 0.05, "*", ""))
#pairwise %>% select(comparison, mean.diff, eff_size_g, p, p.adjust, star)
pairwise_naffect <- pairwise






# A.10.4 Group comparison turnover intention ==================================

# empty vector to store p-values
peace <- vector(mode = "numeric", length = 4)
pairwise <- tibble(comparison = c("1 vs 3", "1 vs 4", "2 vs 3", "2 vs 4"))
pairwise <- pairwise %>% mutate(t = NA)
pairwise <- pairwise %>% mutate(df = NA)
pairwise <- pairwise %>% mutate(mean.diff = NA)
pairwise <- pairwise %>% mutate(eff_size_d = NA)
pairwise <- pairwise %>% mutate(eff_size_g = NA)
pairwise <- pairwise %>% mutate(p = NA)


# A.10.4.1 1 vs 3 -------------------------------------------------------------
# calculate p-values from t-tests
tee <- t.test(turnover ~ treatment_f_reversed, data = df_merge1_3,
              alternative = "less")

# Effect Size (Hedge's g)
effsize_g <- effsize::cohen.d(df_merge1_3$turnover, 
                              as.factor(df_merge1_3$treatment),
                              hedges.correction = TRUE,
                              na.rm = TRUE)

# Effect Size (Cohen's d)
effsize_d <- MBESS::ci.smd(ncp = tee[["statistic"]][["t"]], 
                           n.1=obs$final[2], 
                           n.2=obs$final[4], 
                           conf.level=0.95)$smd

# Write t-test, p and effect sizes in table
pairwise <- pairwise %>% 
  mutate(t = ifelse(comparison == "1 vs 3",
                    tee[["statistic"]][["t"]], t)) %>%
  mutate(df = ifelse(comparison == "1 vs 3",
                     tee[["parameter"]][["df"]], df)) %>%
  mutate(mean.diff = ifelse(comparison == "1 vs 3",
                            (tee[["estimate"]][[1]] - tee[["estimate"]][[2]]), 
                            mean.diff)) %>%
  mutate(p = ifelse(comparison == "1 vs 3",
                    tee[["p.value"]], p)) %>%
  mutate(eff_size_g = ifelse(comparison == "1 vs 3",
                             round(effsize_g[["estimate"]][["1"]], 3), 
                             eff_size_g)) %>%
  mutate(eff_size_d = ifelse(comparison == "1 vs 3",
                             round(effsize_d, 3), 
                             eff_size_d))


# Store p-value separatly (for adjustment)
peace[1] <- tee[["p.value"]]


# A.10.4.2 1 vs 4 -------------------------------------------------------------
# calculate p-values from t-tests
tee <- t.test(turnover ~ treatment_f_reversed, data = df_merge1_4,
              alternative = "less")

# Effect Size (Hedge's g)
effsize_g <- effsize::cohen.d(df_merge1_4$turnover, 
                              as.factor(df_merge1_4$treatment),
                              hedges.correction = TRUE,
                              na.rm = TRUE)

# Effect Size (Cohen's d)
effsize_d <- MBESS::ci.smd(ncp = tee[["statistic"]][["t"]], 
                           n.1=obs$final[2], 
                           n.2=obs$final[5], 
                           conf.level=0.95)$smd

# Write t-test, p and effect sizes in table
pairwise <- pairwise %>% 
  mutate(t = ifelse(comparison == "1 vs 4",
                    tee[["statistic"]][["t"]], t)) %>%
  mutate(df = ifelse(comparison == "1 vs 4",
                     tee[["parameter"]][["df"]], df)) %>%
  mutate(mean.diff = ifelse(comparison == "1 vs 4",
                            (tee[["estimate"]][[1]] - tee[["estimate"]][[2]]), 
                            mean.diff)) %>%
  mutate(p = ifelse(comparison == "1 vs 4",
                    tee[["p.value"]], p)) %>%
  mutate(eff_size_g = ifelse(comparison == "1 vs 4",
                             round(effsize_g[["estimate"]][["1"]], 3), 
                             eff_size_g)) %>%
  mutate(eff_size_d = ifelse(comparison == "1 vs 4",
                             round(effsize_d, 3), 
                             eff_size_d))

# Store p-value separatly (for adjustment)
peace[2] <- tee[["p.value"]]

# A.10.4.3 2 vs 3 -------------------------------------------------------------
# calculate p-values from t-tests
tee <- t.test(turnover ~ treatment_f_reversed, data = df_merge2_3,
              alternative = "less")

# Effect Size (Hedge's g)
effsize_g <- effsize::cohen.d(df_merge2_3$turnover, 
                              as.factor(df_merge2_3$treatment),
                              hedges.correction = TRUE,
                              na.rm = TRUE)

# Effect Size (Cohen's d)
effsize_d <- MBESS::ci.smd(ncp = tee[["statistic"]][["t"]], 
                           n.1=obs$final[3], 
                           n.2=obs$final[4], 
                           conf.level=0.95)$smd

# Write t-test, p and effect sizes in table
pairwise <- pairwise %>% 
  mutate(t = ifelse(comparison == "2 vs 3",
                    tee[["statistic"]][["t"]], t)) %>%
  mutate(df = ifelse(comparison == "2 vs 3",
                     tee[["parameter"]][["df"]], df)) %>%
  mutate(mean.diff = ifelse(comparison == "2 vs 3",
                            (tee[["estimate"]][[1]] - tee[["estimate"]][[2]]), 
                            mean.diff)) %>%
  mutate(p = ifelse(comparison == "2 vs 3",
                    tee[["p.value"]], p)) %>%
  mutate(eff_size_g = ifelse(comparison == "2 vs 3",
                             round(effsize_g[["estimate"]][["2"]], 3), 
                             eff_size_g)) %>%
  mutate(eff_size_d = ifelse(comparison == "2 vs 3",
                             round(effsize_d, 3), 
                             eff_size_d))

# Store p-value separatly (for adjustment)
peace[3] <- tee[["p.value"]]



# A.10.4.4 2 vs 4 -------------------------------------------------------------
# calculate p-values from t-tests
tee <- t.test(turnover ~ treatment_f_reversed, data = df_merge2_4,
              alternative = "less")

# Effect Size (Hedge's g)
effsize_g <- effsize::cohen.d(df_merge2_4$turnover, 
                              as.factor(df_merge2_4$treatment),
                              hedges.correction = TRUE,
                              na.rm = TRUE)

# Effect Size (Cohen's d)
effsize_d <- MBESS::ci.smd(ncp = tee[["statistic"]][["t"]], 
                           n.1=obs$final[3], 
                           n.2=obs$final[5], 
                           conf.level=0.95)$smd

# Write t-test, p and effect sizes in table
pairwise <- pairwise %>% 
  mutate(t = ifelse(comparison == "2 vs 4",
                    tee[["statistic"]][["t"]], t)) %>%
  mutate(df = ifelse(comparison == "2 vs 4",
                     tee[["parameter"]][["df"]], df)) %>%
  mutate(mean.diff = ifelse(comparison == "2 vs 4",
                            (tee[["estimate"]][[1]] - tee[["estimate"]][[2]]), 
                            mean.diff)) %>%
  mutate(p = ifelse(comparison == "2 vs 4",
                    tee[["p.value"]], p)) %>%
  mutate(eff_size_g = ifelse(comparison == "2 vs 4",
                             round(effsize_g[["estimate"]][["2"]], 3), 
                             eff_size_g)) %>%
  mutate(eff_size_d = ifelse(comparison == "2 vs 4",
                             round(effsize_d, 3), 
                             eff_size_d))

# Store p-value separatly (for adjustment)
peace[4] <- tee[["p.value"]]



# A.10.4.5 Adjusted p-values --------------------------------------------------
padjust <- p.adjust(peace, method = "BH")

pairwise <- bind_cols(pairwise, as_tibble(padjust))
pairwise <- pairwise %>% rename(p.adjust = value)

pairwise <- pairwise %>% mutate(star = ifelse(p.adjust < 0.05, "*", ""))
#pairwise %>% select(comparison, mean.diff, eff_size_g, p, p.adjust, star)
pairwise_turnover <- pairwise





# A.10.5 Group comparison willingness to recommend job ========================

# empty vector to store p-values
peace <- vector(mode = "numeric", length = 4)
pairwise <- tibble(comparison = c("1 vs 3", "1 vs 4", "2 vs 3", "2 vs 4"))
pairwise <- pairwise %>% mutate(t = NA)
pairwise <- pairwise %>% mutate(df = NA)
pairwise <- pairwise %>% mutate(mean.diff = NA)
pairwise <- pairwise %>% mutate(eff_size_d = NA)
pairwise <- pairwise %>% mutate(eff_size_g = NA)
pairwise <- pairwise %>% mutate(p = NA)


# A.10.5.1 1 vs 3 -------------------------------------------------------------
# calculate p-values from t-tests
tee <- t.test(willingnessjobrec ~ treatment_f_reversed, data = df_merge1_3,
              alternative = "greater")

# Effect Size (Hedge's g)
effsize_g <- effsize::cohen.d(df_merge1_3$willingnessjobrec, 
                              as.factor(df_merge1_3$treatment),
                              hedges.correction = TRUE,
                              na.rm = TRUE)

# Effect Size (Cohen's d)
effsize_d <- MBESS::ci.smd(ncp = tee[["statistic"]][["t"]], 
                           n.1=obs$final[2], 
                           n.2=obs$final[4], 
                           conf.level=0.95)$smd

# Write t-test, p and effect sizes in table
pairwise <- pairwise %>% 
  mutate(t = ifelse(comparison == "1 vs 3",
                    tee[["statistic"]][["t"]], t)) %>%
  mutate(df = ifelse(comparison == "1 vs 3",
                     tee[["parameter"]][["df"]], df)) %>%
  mutate(mean.diff = ifelse(comparison == "1 vs 3",
                            (tee[["estimate"]][[1]] - tee[["estimate"]][[2]]), 
                            mean.diff)) %>%
  mutate(p = ifelse(comparison == "1 vs 3",
                    tee[["p.value"]], p)) %>%
  mutate(eff_size_g = ifelse(comparison == "1 vs 3",
                             round(effsize_g[["estimate"]][["1"]], 3), 
                             eff_size_g)) %>%
  mutate(eff_size_d = ifelse(comparison == "1 vs 3",
                             round(effsize_d, 3), 
                             eff_size_d))


# Store p-value separatly (for adjustment)
peace[1] <- tee[["p.value"]]


# A.10.5.2 1 vs 4 -------------------------------------------------------------
# calculate p-values from t-tests
tee <- t.test(willingnessjobrec ~ treatment_f_reversed, data = df_merge1_4,
              alternative = "greater")

# Effect Size (Hedge's g)
effsize_g <- effsize::cohen.d(df_merge1_4$willingnessjobrec, 
                              as.factor(df_merge1_4$treatment),
                              hedges.correction = TRUE,
                              na.rm = TRUE)

# Effect Size (Cohen's d)
effsize_d <- MBESS::ci.smd(ncp = tee[["statistic"]][["t"]], 
                           n.1=obs$final[2], 
                           n.2=obs$final[5], 
                           conf.level=0.95)$smd

# Write t-test, p and effect sizes in table
pairwise <- pairwise %>% 
  mutate(t = ifelse(comparison == "1 vs 4",
                    tee[["statistic"]][["t"]], t)) %>%
  mutate(df = ifelse(comparison == "1 vs 4",
                     tee[["parameter"]][["df"]], df)) %>%
  mutate(mean.diff = ifelse(comparison == "1 vs 4",
                            (tee[["estimate"]][[1]] - tee[["estimate"]][[2]]), 
                            mean.diff)) %>%
  mutate(p = ifelse(comparison == "1 vs 4",
                    tee[["p.value"]], p)) %>%
  mutate(eff_size_g = ifelse(comparison == "1 vs 4",
                             round(effsize_g[["estimate"]][["1"]], 3), 
                             eff_size_g)) %>%
  mutate(eff_size_d = ifelse(comparison == "1 vs 4",
                             round(effsize_d, 3), 
                             eff_size_d))

# Store p-value separatly (for adjustment)
peace[2] <- tee[["p.value"]]

# A.10.5.3 2 vs 3 -------------------------------------------------------------
# calculate p-values from t-tests
tee <- t.test(willingnessjobrec ~ treatment_f_reversed, data = df_merge2_3,
              alternative = "greater")

# Effect Size (Hedge's g)
effsize_g <- effsize::cohen.d(df_merge2_3$willingnessjobrec, 
                              as.factor(df_merge2_3$treatment),
                              hedges.correction = TRUE,
                              na.rm = TRUE)

# Effect Size (Cohen's d)
effsize_d <- MBESS::ci.smd(ncp = tee[["statistic"]][["t"]], 
                           n.1=obs$final[3], 
                           n.2=obs$final[4], 
                           conf.level=0.95)$smd

# Write t-test, p and effect sizes in table
pairwise <- pairwise %>% 
  mutate(t = ifelse(comparison == "2 vs 3",
                    tee[["statistic"]][["t"]], t)) %>%
  mutate(df = ifelse(comparison == "2 vs 3",
                     tee[["parameter"]][["df"]], df)) %>%
  mutate(mean.diff = ifelse(comparison == "2 vs 3",
                            (tee[["estimate"]][[1]] - tee[["estimate"]][[2]]), 
                            mean.diff)) %>%
  mutate(p = ifelse(comparison == "2 vs 3",
                    tee[["p.value"]], p)) %>%
  mutate(eff_size_g = ifelse(comparison == "2 vs 3",
                             round(effsize_g[["estimate"]][["2"]], 3), 
                             eff_size_g)) %>%
  mutate(eff_size_d = ifelse(comparison == "2 vs 3",
                             round(effsize_d, 3), 
                             eff_size_d))

# Store p-value separatly (for adjustment)
peace[3] <- tee[["p.value"]]



# A.10.5.4 2 vs 4 -------------------------------------------------------------
# calculate p-values from t-tests
tee <- t.test(willingnessjobrec ~ treatment_f_reversed, data = df_merge2_4,
              alternative = "greater")

# Effect Size (Hedge's g)
effsize_g <- effsize::cohen.d(df_merge2_4$willingnessjobrec, 
                              as.factor(df_merge2_4$treatment),
                              hedges.correction = TRUE,
                              na.rm = TRUE)

# Effect Size (Cohen's d)
effsize_d <- MBESS::ci.smd(ncp = tee[["statistic"]][["t"]], 
                           n.1=obs$final[3], 
                           n.2=obs$final[5], 
                           conf.level=0.95)$smd

# Write t-test, p and effect sizes in table
pairwise <- pairwise %>% 
  mutate(t = ifelse(comparison == "2 vs 4",
                    tee[["statistic"]][["t"]], t)) %>%
  mutate(df = ifelse(comparison == "2 vs 4",
                     tee[["parameter"]][["df"]], df)) %>%
  mutate(mean.diff = ifelse(comparison == "2 vs 4",
                            (tee[["estimate"]][[1]] - tee[["estimate"]][[2]]), 
                            mean.diff)) %>%
  mutate(p = ifelse(comparison == "2 vs 4",
                    tee[["p.value"]], p)) %>%
  mutate(eff_size_g = ifelse(comparison == "2 vs 4",
                             round(effsize_g[["estimate"]][["2"]], 3), 
                             eff_size_g)) %>%
  mutate(eff_size_d = ifelse(comparison == "2 vs 4",
                             round(effsize_d, 3), 
                             eff_size_d))

# Store p-value separatly (for adjustment)
peace[4] <- tee[["p.value"]]



# A.10.5.5 Adjusted p-values --------------------------------------------------
padjust <- p.adjust(peace, method = "BH")

pairwise <- bind_cols(pairwise, as_tibble(padjust))
pairwise <- pairwise %>% rename(p.adjust = value)

pairwise <- pairwise %>% mutate(star = ifelse(p.adjust < 0.05, "*", ""))
#pairwise %>% select(comparison, mean.diff, eff_size_g, p, p.adjust, star)
pairwise_will <- pairwise


# remove unused objects
rm(effsize_d, effsize_g, pairwise, tee, padjust, peace)










# A.11 ANOVA for Descriptives #################################################
aov_paffect_result <- aov_result(aov_paffect)
aov_naffect_result <- aov_result(aov_naffect)
aov_turnover_result <- aov_result(aov_turnover)
aov_will_result <- aov_result(aov_will)
aov_bene_result <- aov_result(aov(df_merge$beneficiaries ~ 
                                    df_merge$treatment_f))
aov_prosocialimpact_result <- aov_result(aov_prosocialimpact)
aov_age_result <- aov_result(aov_age)
chi_female_result <- chi_result(df_merge$gender_f, df_merge$treatment_f)
chi_manager_result <- chi_result(df_merge$manager_dummy, df_merge$treatment_f)

desc <-  desc %>%
  mutate(Diff = "") %>%
  mutate(Diff = ifelse(treatment == "paffect_m",
                       aov_paffect_result, Diff),
         Diff = ifelse(treatment == "naffect_m",
                       aov_naffect_result, Diff),
         Diff = ifelse(treatment == "turnover_m",
                       aov_turnover_result, Diff),
         Diff = ifelse(treatment == "will_m",
                       aov_will_result, Diff),
         Diff = ifelse(treatment == "psi_m",
                       aov_prosocialimpact_result, Diff),
         Diff = ifelse(treatment == "age_m",
                       aov_age_result, Diff),
         Diff = ifelse(treatment == "female_m",
                       chi_female_result, Diff),
         Diff = ifelse(treatment == "manager_m",
                       chi_manager_result, Diff)
  )

desc <- desc %>%
  mutate(treatment = ifelse(treatment == "paffect_m",
                            "Positive affect", treatment),
         treatment = ifelse(treatment == "naffect_m",
                            "Negative affect", treatment),
         treatment = ifelse(treatment == "turnover_m",
                            "Turnover intention", treatment),
         treatment = ifelse(treatment == "will_m",
                            "Willingness to recommend job", treatment),
         treatment = ifelse(treatment == "psi_m",
                            "Prosocial impact", treatment),
         treatment = ifelse(treatment == "age_m",
                            "Age", treatment),
         treatment = ifelse(treatment == "female_m",
                            "Female", treatment),
         treatment = ifelse(treatment == "manager_m",
                            "Leadership responsibilities", treatment),
         treatment = ifelse(treatment == "paffect_sd",
                            "", treatment),
         treatment = ifelse(treatment == "naffect_sd",
                            "", treatment),
         treatment = ifelse(treatment == "turnover_sd",
                            "", treatment),
         treatment = ifelse(treatment == "will_sd",
                            "", treatment),
         treatment = ifelse(treatment == "psi_sd",
                            "", treatment),
         treatment = ifelse(treatment == "age_sd",
                            "", treatment),
         treatment = ifelse(treatment == "female_sd",
                            "", treatment),
         treatment = ifelse(treatment == "manager_sd",
                            "", treatment)
  )

desc





# A.12 Plot with mean values per group (Meanplot) #############################

# A.12.1 Prepare data (sd, n, se, ci per treatment per variable) ==============
df_merge_sum <- df_merge %>% 
  select(treatment, paffect, naffect, 
         turnover, willingnessjobrec) %>%
  group_by(treatment) %>%
  summarise_all(funs(mean, 
                     sd, 
                     n = n(), 
                     se_func,
                     lower_ci_func,
                     upper_ci_func))

# Transform data to long (means)
df_merge_sum2 <- df_merge_sum %>% 
  select(treatment, ends_with("_mean")) %>%
  rename(paffect = paffect_mean,
         naffect = naffect_mean,
         turnover = turnover_mean,
         will = willingnessjobrec_mean) %>%
  gather("paffect", 
         "naffect", 
         "turnover", 
         "will", 
         key = "var", 
         value = "mean")

# Transform data to long (CI lower bound) 
df_merge_sum3 <- df_merge_sum %>% 
  select(treatment, ends_with("_lower_ci_func")) %>%
  rename(paffect = paffect_lower_ci_func,
         naffect = naffect_lower_ci_func,
         turnover = turnover_lower_ci_func,
         will = willingnessjobrec_lower_ci_func) %>%
  gather("paffect", 
         "naffect", 
         "turnover", 
         "will",
         key = "var", 
         value = "lower_ci")

# Transform data to long (CI upper bound)
df_merge_sum4 <- df_merge_sum %>% 
  select(treatment, ends_with("_upper_ci_func")) %>%
  rename(paffect = paffect_upper_ci_func,
         naffect = naffect_upper_ci_func,
         turnover = turnover_upper_ci_func,
         will = willingnessjobrec_upper_ci_func) %>%
  gather("paffect", 
         "naffect", 
         "turnover", 
         "will",
         key = "var", 
         value = "upper_ci")

# Merge data
df_merge_sum2 <- left_join(df_merge_sum2, df_merge_sum3,
                           key = c("treatment", "var"))
df_merge_sum2 <- left_join(df_merge_sum2, df_merge_sum4,
                           key = c("treatment", "var"))


# Add factor variables (for captions and sorting)
df_merge_sum2 <- df_merge_sum2 %>%
  mutate(treatment_f = factor(treatment,
                              labels = c("Passive\nControl",
                                         "Active\nControl",
                                         "Prosocial\nImpact", 
                                         "Societal\nImpact")),
         var_f = factor(var, 
                        levels = c("paffect",
                                   "naffect",
                                   "turnover",
                                   "will")))

# A.12.2 Plot =================================================================
# Define jitter so it is the same for means and CI
dodge <- position_dodge(.8) # how much jitter on the x-axis?


# Plot
meanplot <- ggplot(df_merge_sum2, 
                   aes(x = var_f, 
                       y = mean, 
                       color = treatment_f)) + 
  geom_errorbar(aes(ymin = lower_ci,
                    ymax = upper_ci), 
                width=.5,
                size = .7,
                position = dodge,
                show.legend=FALSE) +
  geom_point(aes(shape = treatment_f), 
             position = dodge, size = 2) +
  scale_fill_continuous(guide = guide_legend()) +
  theme(legend.position="bottom") +
  labs(title = "Figure 1: Differences in dependent variables\nbetween experimental groups in Study 1",
       x = "Dependent Variables", 
       y = "Mean", 
       color = "Treatments",
       shape = "Treatments") +
  scale_x_discrete(labels=c("naffect" = "Negative\nAffect", 
                            "paffect" = "Positive\nAffect",
                            "turnover" = "Turnover\nIntention",
                            "will" = "Willingness to\nrecommend job")) +
  coord_cartesian(ylim = c(1, 7)) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     labels = c("1", "2", "3", "4", "5", "6", "7")) +
  scale_color_manual(values = c("#1380A1", "#990000", "#FAAB18", "#588300")) +
  scale_shape_manual(values = c(15, 16, 17, 18)) +
  # Negative Affect
  geom_signif(annotations = "**",
            y_position = 1.35, 
            xmin= 1.71, xmax = 2.1, 
            tip_length = c(-0.12, -0.02),
            vjust = 2.3,
            color='black') +
  geom_signif(annotations = "*",
              y_position = 1.2, 
              xmin= 1.69, xmax = 2.3, 
              tip_length = c(-0.155, -0.1),
              vjust = 3.8,
              color='black') +
  geom_signif(annotations = "*",
              y_position = 2.6, 
              xmin= 1.9, xmax = 2.1, 
              tip_length = c(0.1, 0.17),
              vjust = 0.5,
              color='black') + 
  # Turnover
  geom_signif(annotations = "*",
              y_position = 2.1,
              xmin= 2.71, xmax = 3.1,
              tip_length = c(-0.225, -0.1),
              vjust = 3.6,
              color='black') +
  geom_signif(annotations = "***",
              y_position = 1.95,
              xmin= 2.69, xmax = 3.3,
              tip_length = c(-0.26, -0.045),
              vjust = 5.2,
              color='black') +  
  geom_signif(annotations = "**",
              y_position = 4.1,
              xmin= 2.9, xmax = 3.3,
              tip_length = c(0.13, 0.31),
              vjust = 0.5,
              color='black') +
  # Will
  geom_signif(annotations = "*",
              y_position = 3.05,
              xmin= 3.71, xmax = 4.1,
              tip_length = c(-0.02, -0.11),
              vjust = 2.2,
              color='black') +
  geom_signif(annotations = "***",
              y_position = 2.9,
              xmin= 3.69, xmax = 4.3,
              tip_length = c(-0.057, -0.22),
              vjust = 4.7,
              color='black') +
  geom_signif(annotations = "*",
              y_position = 4.4,
              xmin= 3.9, xmax = 4.3,
              tip_length = c(0.09, 0.02),
              vjust = 0.5,
              color='black')

rm(df_merge_sum, df_merge_sum2, df_merge_sum3, df_merge_sum4)






# A.13 Table with results of group comparisons ################################
pairwise_paffect2 <- 
  pairwise_paffect %>%
  mutate(var = "paffect") %>%
  mutate(test = paste0("t(",
                       broman::myround(df, 1),
                       ") = ",
                       broman::myround(t, 2),
                       ", ", pformat(p.adjust))) %>%
  select(var, comparison, mean.diff, test, eff_size_g)

pairwise_naffect2 <- 
  pairwise_naffect %>%
  mutate(var = "naffect") %>%
  mutate(test = paste0("t(",
                       broman::myround(df, 1),
                       ") = ",
                       broman::myround(t, 2),
                       ", ", pformat(p.adjust))) %>%
  select(var, comparison, mean.diff, test, eff_size_g)

pairwise_turnover2 <- 
  pairwise_turnover %>%
  mutate(var = "turnover") %>%
  mutate(test = paste0("t(",
                       broman::myround(df, 1),
                       ") = ",
                       broman::myround(t, 2),
                       ", ", pformat(p.adjust))) %>%
  select(var, comparison, mean.diff, test, eff_size_g)

pairwise_will2 <- 
  pairwise_will %>%
  mutate(var = "will") %>%
  mutate(test = paste0("t(",
                       broman::myround(df, 1),
                       ") = ",
                       broman::myround(t, 2),
                       ", ", pformat(p.adjust))) %>%
  select(var, comparison, mean.diff, test, eff_size_g)

pairwise2 <- bind_rows(pairwise_paffect2, pairwise_naffect2,
                       pairwise_turnover2, pairwise_will2)
pairwise2
pairwise3 <- pairwise2 %>% 
  select(-test, -eff_size_g) %>% 
  mutate(mean.diff = 
           as.character(broman::myround(mean.diff,2))) %>%
  spread(comparison, mean.diff) %>%
  mutate(value = "mean.diff")

pairwise4 <- pairwise2 %>% 
  select(-mean.diff, -test) %>%
  mutate(eff_size_g = abs(eff_size_g)) %>%
  mutate(eff_size_g = 
           as.character(broman::myround(eff_size_g,2))) %>%
  spread(comparison, eff_size_g) %>%
  mutate(value = "eff_size_g")

pairwise5 <- pairwise2 %>% 
  select(-mean.diff, -eff_size_g) %>%
  spread(comparison, test) %>%
  mutate(value = "test")

results_table <- bind_rows(pairwise3, pairwise4, pairwise5) %>%
  select(var, value, everything()) %>% 
  mutate(var = factor(var,
                      levels = c("paffect",
                                 "naffect",
                                 "turnover",
                                 "will"))) %>%
  mutate(value = factor(value, 
                        levels = c("mean.diff",
                                   "eff_size_g",
                                   "test"))) %>%
  arrange(var, value)

results_table
rm(pairwise2, pairwise3, pairwise4, pairwise5,
   pairwise_paffect2, pairwise_naffect2,
   pairwise_turnover2, pairwise_will2)









# A.15 Regression with beneficiary contact (H4) ###############################
# Model 1: Treatment only
reg_paffect <- lm(paffect ~ treatment_f, data = df_merge)
reg_naffect <- lm(naffect ~ treatment_f, data = df_merge)
reg_turnover <- lm(turnover ~ treatment_f, data = df_merge)
reg_will <- lm(willingnessjobrec ~ treatment_f, data = df_merge)

# Model 2: Add contact with beneficiaries
reg_paffect2 <- update(reg_paffect, . ~ . + beneficiaries)
reg_naffect2 <- update(reg_naffect, . ~ . + beneficiaries)
reg_turnover2 <- update(reg_turnover, . ~ . + beneficiaries)
reg_will2 <- update(reg_will, . ~ . + beneficiaries)

# Model 3: Add beneficiaries * treatment
reg_paffect3 <- update(reg_paffect, . ~ . + beneficiaries*treatment_f)
reg_naffect3 <- update(reg_naffect, . ~ . + beneficiaries*treatment_f)
reg_turnover3 <- update(reg_turnover, . ~ . + beneficiaries*treatment_f)
reg_will3 <- update(reg_will, . ~ . + beneficiaries*treatment_f)

regtab <- stargazer::stargazer(reg_paffect3, reg_naffect3,
                               reg_turnover3, reg_will3,
                               type = "latex", 
                               dep.var.labels.include = TRUE,
                               dep.var.caption = "",
                               star.cutoffs = c(.05, .01, .001))


