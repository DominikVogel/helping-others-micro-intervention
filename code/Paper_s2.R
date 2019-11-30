###############################################################################
################ CODE FOR STUDY 2 #############################################
###############################################################################

# B.1 Import data #############################################################
coltypes <- cols(
  id = col_double(),
  startdate = col_character(),
  enddate = col_character(),
  treatment = col_double(),
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
  willingnessjobrec1 = col_double(),
  willingnessjobrec2 = col_double(),
  turnover1 = col_double(),
  turnover2 = col_double(),
  turnover3 = col_double(),
  manipulationcheck = col_double(),
  prosocialimpact1 = col_double(),
  prosocialimpact2 = col_double(),
  prosocialimpact3 = col_double(),
  societalimpact1 = col_double(),
  societalimpact2 = col_double(),
  societalimpact3 = col_double(),
  beneficiaries_obj = col_double(),
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
  sector = col_double(),
  atwork = col_double(),
  timebeforetreatment = col_time(format = ""),
  timeaftertreatment = col_time(format = ""),
  finished = col_double()
)

df_merge_s2 <- read_csv(here("data", "Study2_public.csv"),
                        col_types = coltypes)






# B.2 Generate variables ######################################################

# B.2.1 Generate factor variables =============================================
df_merge_s2$treatment_f <- factor(df_merge_s2$treatment)
df_merge_s2$gender_f <- factor(df_merge_s2$gender, 
                               labels = c("Male", "Female", "Other"))


# B.2.2 Set NA categories =====================================================
df_merge_s2 <- df_merge_s2 %>% 
  mutate(atwork = ifelse(atwork == 2, NA, atwork))




# B.2.3 Reverse items =========================================================
df_merge_s2 <- df_merge_s2 %>% mutate(turnover1 = 8 - turnover1)
df_merge_s2 <- df_merge_s2 %>% 
  mutate(willingnessjobrec2 = 6 - willingnessjobrec2)
df_merge_s2 <- df_merge_s2 %>% mutate(taskanalyz1 = 6 - taskanalyz1)
df_merge_s2 <- df_merge_s2 %>% mutate(taskanalyz2 = 6 - taskanalyz2)
df_merge_s2 <- df_merge_s2 %>% mutate(taskanalyz3 = 6 - taskanalyz3)
df_merge_s2 <- df_merge_s2 %>% mutate(taskanalyz4 = 6 - taskanalyz4)
df_merge_s2 <- df_merge_s2 %>% mutate(taskvariety1 = 6 - taskvariety1)
df_merge_s2 <- df_merge_s2 %>% mutate(taskvariety2 = 6 - taskvariety2)
df_merge_s2 <- df_merge_s2 %>% mutate(taskvariety3 = 6 - taskvariety3)
df_merge_s2 <- df_merge_s2 %>% mutate(taskvariety4 = 6 - taskvariety4)
df_merge_s2 <- df_merge_s2 %>% mutate(taskvariety5 = 6 - taskvariety5)


# B.2.4 Create Dummies ========================================================
df_merge_s2 <- df_merge_s2 %>% mutate(public = ifelse(sector == 1, 1, 0),
                                      public = ifelse(is.na(sector), 
                                                      NA, 
                                                      public))
df_merge_s2 <- df_merge_s2 %>% mutate(private = ifelse(sector == 3, 1, 0),
                                      private = ifelse(is.na(sector), 
                                                       NA, 
                                                       private))  
df_merge_s2 <- df_merge_s2 %>% mutate(npo = ifelse(sector == 2, 1, 0),
                                      npo = ifelse(is.na(sector), NA, npo))
df_merge_s2 <- df_merge_s2 %>% mutate(sector_na = ifelse(sector == 4, 1, 0),
                                      sector_na = ifelse(is.na(sector), NA, 
                                                         sector_na))
df_merge_s2 <- df_merge_s2 %>% mutate(manager_dummy = ifelse(manager == 0, 
                                                             0, 
                                                             1),
                                      manager_dummy = ifelse(is.na(manager), 
                                                             NA, 
                                                             manager_dummy))
df_merge_s2 <- df_merge_s2 %>% mutate(male = ifelse(gender == 1, 1, 0),
                                      male = ifelse(is.na(gender), NA, male),
                                      female = ifelse(gender == 2, 1, 0),
                                      female = ifelse(is.na(gender), 
                                                      NA, 
                                                      female),
                                      gender_other = ifelse(gender == 3, 1, 0),
                                      gender_other = ifelse(is.na(gender), 
                                                            NA, 
                                                            gender_other))
df_merge_s2 <- df_merge_s2 %>% 
  mutate(treatment_dummy1 = ifelse(treatment == 1, 1, 0),
         treatment_dummy2 = ifelse(treatment == 2, 1, 0),
         treatment_dummy3 = ifelse(treatment == 3, 1, 0),
         treatment_dummy4 = ifelse(treatment == 4, 1, 0))






# B.2.5 Create treatment duration =============================================
df_merge_s2 <- df_merge_s2 %>% 
  mutate(treatment_duration = timeaftertreatment - timebeforetreatment)

# B.2.6 Create dependent variables ============================================

## Positive Affect
crona_paffect_s2 <- psych::alpha(select(df_merge_s2, 
                                        paffect1, paffect2, 
                                        paffect3, paffect4, 
                                        paffect5, paffect6))
crona_paffect_s2 <- crona_paffect_s2[["total"]][["raw_alpha"]]
df_merge_s2 <- mean_index(df_merge_s2, "paffect",
                          c("paffect1", "paffect2", "paffect3", 
                            "paffect4", "paffect5", "paffect6"))
## Negative Affect
crona_naffect_s2 <- psych::alpha(select(df_merge_s2, 
                                        naffect1, naffect2, 
                                        naffect3, naffect4, 
                                        naffect5, naffect6))
crona_naffect_s2 <- crona_naffect_s2[["total"]][["raw_alpha"]]
df_merge_s2 <- mean_index(df_merge_s2, "naffect",
                          c("naffect1", "naffect2", "naffect3", 
                            "naffect4", "naffect5", "naffect6"))

## Turnover intention
crona_turnover_s2 <- psych::alpha(select(df_merge_s2, turnover1, 
                                         turnover2, turnover3))
crona_turnover_s2 <- crona_turnover_s2[["total"]][["raw_alpha"]]
df_merge_s2 <- mean_index(df_merge_s2, "turnover",
                          c("turnover1", "turnover2", "turnover3"))

## Willigness to recommend job
crona_will_s2 <- psych::alpha(select(df_merge_s2, 
                                     willingnessjobrec1, willingnessjobrec2))
crona_will_s2 <- crona_will_s2[["total"]][["raw_alpha"]]
df_merge_s2 <- mean_index(df_merge_s2, 
                          "willingnessjobrec", 
                          c("willingnessjobrec1", 
                            "willingnessjobrec2"))


# B.2.7 Create moderator / control variables ==================================

## Contact with beneficiaries
crona_bene_s2 <- psych::alpha(select(df_merge_s2, 
                                     beneficiaries1, beneficiaries2))
crona_bene_s2 <- crona_bene_s2[["total"]][["raw_alpha"]]
df_merge_s2 <- mean_index(df_merge_s2, "beneficiaries",
                          c("beneficiaries1", "beneficiaries2"))

## Perceived prosocial impact
crona_psi_s2 <- psych::alpha(select(df_merge_s2, prosocialimpact1, 
                                    prosocialimpact2, prosocialimpact3))
crona_psi_s2 <- crona_psi_s2[["total"]][["raw_alpha"]]
df_merge_s2 <- mean_index(df_merge_s2, "prosocialimpact",
                          c("prosocialimpact1", 
                            "prosocialimpact2",
                            "prosocialimpact3"))


## Perceived societal impact
crona_si_s2 <- psych::alpha(select(df_merge_s2, societalimpact1, 
                                   societalimpact2, societalimpact3))
crona_si_s2 <- crona_si_s2[["total"]][["raw_alpha"]]
df_merge_s2 <- mean_index(df_merge_s2, "societalimpact",
                          c("societalimpact1", 
                            "societalimpact2",
                            "societalimpact3"))


## Task variety
crona_taskvar_s2 <- psych::alpha(select(df_merge_s2, 
                                        taskvariety1, taskvariety2,
                                        taskvariety3, taskvariety4,
                                        taskvariety5))
crona_taskvar_s2 <- crona_taskvar_s2[["total"]][["raw_alpha"]]
df_merge_s2 <- mean_index(df_merge_s2, "taskvariety",
                          c("taskvariety1", "taskvariety2",
                            "taskvariety3", "taskvariety4", 
                            "taskvariety5"))

## Task analyzability
crona_taskanalyz_s2 <- psych::alpha(select(df_merge_s2, 
                                           taskanalyz1, 
                                           taskanalyz2, 
                                           taskanalyz3, 
                                           taskanalyz4))
crona_taskanalyz_s2 <- crona_taskanalyz_s2[["total"]][["raw_alpha"]]
df_merge_s2 <- mean_index(df_merge_s2, "taskanalyz",
                          c("taskanalyz1", "taskanalyz2", 
                            "taskanalyz3", "taskanalyz4"))

## Job autonomy
crona_auto_s2 <- psych::alpha(select(df_merge_s2, 
                                     autonomy1, autonomy2, autonomy3))
crona_auto_s2 <- crona_auto_s2[["total"]][["raw_alpha"]]
df_merge_s2 <- mean_index(df_merge_s2, "autonomy",
                          c("autonomy1", "autonomy2", "autonomy3"))


## Age
df_merge_s2 <- df_merge_s2 %>%
  mutate(age = 2018 - yearbirth)








# B.3 Clean data ##############################################################

# B.3.1 Store initial n =======================================================
n_total <- tibble(treatment = "total", initial = nrow(df_merge_s2))
obs_s2 <- df_merge_s2 %>% 
  group_by(treatment) %>% 
  summarise(initial = n()) %>%
  mutate(treatment = as.character(treatment))
obs_s2 <- bind_rows(n_total, obs_s2)
rm(n_total)


# B.3.2 Remove obs how did not receive treatment ==============================
df_merge_s2 <- df_merge_s2 %>% 
  mutate(no_treat = ifelse(treatment > 1 & is.na(treatment_duration), 1, 0),
         no_treat = ifelse(treatment > 1 & treatment_duration == 0, 
                           1, 
                           no_treat),
         no_treat = ifelse(treatment == 1, NA, no_treat)) %>%
  filter(no_treat != 1 | is.na(no_treat)) %>%
  select(-no_treat)


# Store n
n_total <- tibble(treatment = "total", receive_treat = nrow(df_merge_s2))
obs_receive <- df_merge_s2 %>% 
  group_by(treatment) %>% 
  summarise(receive_treat = n()) %>%
  mutate(treatment = as.character(treatment))
obs_receive <- bind_rows(n_total, obs_receive)
obs_s2 <- left_join(obs_s2, obs_receive)
rm(obs_receive)






# B.3.3 Remove obs how did not answer dependent variable ======================
df_merge_s2 <- df_merge_s2 %>% 
  mutate(mi_dep = ifelse(is.na(paffect) | 
                           is.na(naffect) |
                           is.na(turnover) |
                           is.na(willingnessjobrec),
                         1, 0)) %>%
  filter(mi_dep != 1) %>%
  select(-mi_dep)


# B.3.4 Remove obs how did not finish questionnaire ===========================
df_merge_s2 <- df_merge_s2 %>% filter(finished == 1)

# Store n
n_total <- tibble(treatment = "total", complete = nrow(df_merge_s2))
obs_complete <- df_merge_s2 %>% 
  group_by(treatment) %>% 
  summarise(complete = n()) %>%
  mutate(treatment = as.character(treatment))
obs_complete <- bind_rows(n_total, obs_complete)
obs_s2 <- left_join(obs_s2, obs_complete)
rm(obs_complete)

# B.3.5 Remove participants who do not work in the public sector ==============
df_merge_s2 <- df_merge_s2 %>%
  filter(sector == 1)

# B.3.6 Remove participants whose answers indicate that they are not working in the public sector ====
df_merge_s2 <- df_merge_s2 %>%
  filter(id != 61,
         id != 86,
         id != 151,
         id != 189,
         id != 266,	
         id != 298,
         id != 330,
         id != 408,
         id != 430,
         id != 462,
         id != 481,
         id != 551,
         id != 598
         )


# Store n
n_total <- tibble(treatment = "total", public = nrow(df_merge_s2))
obs_public <- df_merge_s2 %>% 
  group_by(treatment) %>% 
  summarise(public = n()) %>%
  mutate(treatment = as.character(treatment))
obs_public <- bind_rows(n_total, obs_public)
obs_s2 <- left_join(obs_s2, obs_public)
rm(obs_public)




# B.3.7 Remove obs with failed attention check ================================
df_merge_s2 <- df_merge_s2 %>% 
  filter(attention1 == 3) %>% 
  filter(attention2 == 7)

# Store n
n_total <- tibble(treatment = "total", attention = nrow(df_merge_s2))
obs_attention <- df_merge_s2 %>% 
  group_by(treatment) %>% 
  summarise(attention = n()) %>%
  mutate(treatment = as.character(treatment))
obs_attention <- bind_rows(n_total, obs_attention)
obs_s2 <- left_join(obs_s2, obs_attention)
rm(obs_attention)



# B.3.8 Remove participants whose answers to the reflection task are nonsense =====
df_merge_s2 <- df_merge_s2 %>%
  filter(id != 304, 
         id != 313,
         id != 409, 
         id != 586, 
         id != 633, 
         id != 640 
         )

# Store n
n_total <- tibble(treatment = "total", nonsense = nrow(df_merge_s2))
obs_nonsense <- df_merge_s2 %>% 
  group_by(treatment) %>% 
  summarise(nonsense = n()) %>%
  mutate(treatment = as.character(treatment))
obs_nonsense <- bind_rows(n_total, obs_nonsense)
obs_s2 <- left_join(obs_s2, obs_nonsense)
rm(obs_nonsense)



# Store n
n_total <- tibble(treatment = "total", final = nrow(df_merge_s2))
obs_final <- df_merge_s2 %>% 
  group_by(treatment) %>% 
  summarise(final = n()) %>%
  mutate(treatment = as.character(treatment))
obs_final <- bind_rows(n_total, obs_final)
obs_s2 <- left_join(obs_s2, obs_final)
rm(obs_final, n_total)

obs_s2








# B.4 Descriptives (Prepare Table 3) ##########################################
desc_s2 <- df_merge_s2 %>% 
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
            si_m = func_round_mean(societalimpact),
            si_sd = func_round_sd(societalimpact),
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
         si_sd = paste0("(", si_sd, ")"),
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
                                 "si_m", "si_sd",
                                 "age_m", "age_sd",
                                 "female_m", "female_sd",
                                 "manager_m", "manager_sd",
                                 "n"))) %>%
  # Sort
  arrange(var) %>%
  select(-var)

desc_s2







# B.5 Manipulation Check ######################################################

## A: What exactly did we ask you to reflect upon?
df_merge_s2$manipulationcheck <- 
  factor(df_merge_s2$manipulationcheck,
         levels = c(1, 2, 3, 4),
         labels = c("Your job", 
                    "Your job & how you help others", 
                    "Your job & how it helps society",
                    "Don't remember")) 

# Cross-Table
manipulationcheck1_s2 <- df_merge_s2 %>% 
  filter(treatment != 1)
manipulationcheck1_s2 <- table(manipulationcheck1_s2$manipulationcheck, 
                               manipulationcheck1_s2$treatment)
manipulationcheck1_s2



## B: Perceived prosocial impact
df_merge_s2 %>% 
  group_by(treatment) %>% 
  summarise(mean = mean(prosocialimpact),
            sd = sd(prosocialimpact))

aov_prosocialimpact_s2 <- aov(prosocialimpact ~ treatment_f, 
                              data = df_merge_s2)
aov_societalimpact_s2 <- aov(societalimpact ~ treatment_f, 
                             data = df_merge_s2)






# B.6 Group differences DVs and controls (for Table 3) ########################

## Positive affect
aov_paffect_s2 <- aov(df_merge_s2$paffect ~ factor(df_merge_s2$treatment))
summary(aov_paffect_s2)

### Effect size Cohen's f: 
effsize_paffect_aov_s2 <- sjstats::cohens_f(aov_paffect_s2)




## Negative Affect
aov_naffect_s2 <- aov(df_merge_s2$naffect ~ factor(df_merge_s2$treatment))
summary(aov_naffect_s2)

### Effect size Cohen's f: 
effsize_naffect_aov_s2 <- sjstats::cohens_f(aov_naffect_s2)





## Turnover intention
aov_turnover_s2 <- aov(df_merge_s2$turnover ~ factor(df_merge_s2$treatment))
summary(aov_turnover_s2)

### Effect size Cohen's f: 
effsize_turnover_aov_s2 <- sjstats::cohens_f(aov_turnover_s2)





## Willigness to recommend job
aov_will_s2 <- aov(df_merge_s2$willingnessjobrec ~ factor(df_merge_s2$treatment))
summary(aov_will_s2)

### Effect size Cohen's f: 
effsize_will_aov_s2 <- sjstats::cohens_f(aov_will_s2)




# B.6.1 Balance test ==========================================================
chi_female_s2 <- chisq.test(table(df_merge_s2$gender_f, 
                                  df_merge_s2$treatment_f))
chi_manager_s2 <- chisq.test(table(df_merge_s2$manager_dummy, 
                                   df_merge_s2$treatment_f))
aov_age_s2 <- aov(age ~ treatment_f, data = df_merge_s2)






# B.7 Group comparisons #######################################################

# Generate treatment factor with reversed order of treatments
# Ensures that the mean differences have the right sign
df_merge_s2 <- df_merge_s2 %>%
  mutate(treatment_f_reversed = factor(treatment, 
                                       levels = c("4", "3", "2", "1")))


# B.7.1 Generate subsamples ===================================================
df_merge1_s2 <- df_merge_s2 %>% filter(treatment == 1)
df_merge2_s2 <- df_merge_s2 %>% filter(treatment == 2)
df_merge3_s2 <- df_merge_s2 %>% filter(treatment == 3)
df_merge4_s2 <- df_merge_s2 %>% filter(treatment == 4)
df_merge1_3_s2  <- df_merge_s2 %>% filter(treatment == 1 | treatment == 3)
df_merge1_4_s2  <- df_merge_s2 %>% filter(treatment == 1 | treatment == 4)
df_merge2_3_s2  <- df_merge_s2 %>% filter(treatment == 2 | treatment == 3)
df_merge2_4_s2  <- df_merge_s2 %>% filter(treatment == 2 | treatment == 4)


# B.7.2 Group comparison positive affect ======================================

# empty vector to store p-values
peace <- vector(mode = "numeric", length = 4)
pairwise <- tibble(comparison = c("1 vs 3", "1 vs 4", "2 vs 3", "2 vs 4"))
pairwise <- pairwise %>% mutate(t = NA)
pairwise <- pairwise %>% mutate(df = NA)
pairwise <- pairwise %>% mutate(mean.diff = NA)
pairwise <- pairwise %>% mutate(eff_size_d = NA)
pairwise <- pairwise %>% mutate(eff_size_g = NA)
pairwise <- pairwise %>% mutate(p = NA)


# B.7.2.1 Passive Control vs. Prosocial ---------------------------------------
# calculate p-values from t-tests
tee <- t.test(paffect ~ treatment_f_reversed, data = df_merge1_3_s2,
              alternative = "greater")

# Effect Size (Hedge's g)
effsize_g <- effsize::cohen.d(df_merge1_3_s2 $paffect, 
                              as.factor(df_merge1_3_s2$treatment),
                              hedges.correction = TRUE,
                              na.rm = TRUE)

# Effect Size (Cohen's d)
effsize_d <- MBESS::ci.smd(ncp = tee[["statistic"]][["t"]], 
                           n.1=obs_s2$final[2], 
                           n.2=obs_s2$final[4], 
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


# B.7.2.2 Passive Control vs. Societal ----------------------------------------
# calculate p-values from t-tests
tee <- t.test(paffect ~ treatment_f_reversed, data = df_merge1_4_s2 ,
              alternative = "greater")

# Effect Size (Hedge's g)
effsize_g <- effsize::cohen.d(df_merge1_4_s2 $paffect, 
                              as.factor(df_merge1_4_s2 $treatment),
                              hedges.correction = TRUE,
                              na.rm = TRUE)

# Effect Size (Cohen's d)
effsize_d <- MBESS::ci.smd(ncp = tee[["statistic"]][["t"]], 
                           n.1=obs_s2$final[2], 
                           n.2=obs_s2$final[5], 
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

# B.7.2.3 Active Control vs. Prosocial ----------------------------------------
# calculate p-values from t-tests
tee <- t.test(paffect ~ treatment_f_reversed, data = df_merge2_3_s2 ,
              alternative = "greater")

# Effect Size (Hedge's g)
effsize_g <- effsize::cohen.d(df_merge2_3_s2 $paffect, 
                              as.factor(df_merge2_3_s2 $treatment),
                              hedges.correction = TRUE,
                              na.rm = TRUE)

# Effect Size (Cohen's d)
effsize_d <- MBESS::ci.smd(ncp = tee[["statistic"]][["t"]], 
                           n.1=obs_s2$final[3], 
                           n.2=obs_s2$final[4], 
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



# B.7.2.4 Active Control vs. Societal -----------------------------------------
# calculate p-values from t-tests
tee <- t.test(paffect ~ treatment_f_reversed, data = df_merge2_4_s2 ,
              alternative = "greater")

# Effect Size (Hedge's g)
effsize_g <- effsize::cohen.d(df_merge2_4_s2 $paffect, 
                              as.factor(df_merge2_4_s2 $treatment),
                              hedges.correction = TRUE,
                              na.rm = TRUE)

# Effect Size (Cohen's d)
effsize_d <- MBESS::ci.smd(ncp = tee[["statistic"]][["t"]], 
                           n.1=obs_s2$final[3], 
                           n.2=obs_s2$final[5], 
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



# B.7.2.5 Adjusted p-values ---------------------------------------------------
padjust <- p.adjust(peace, method = "BH")

pairwise <- bind_cols(pairwise, as_tibble(padjust))
pairwise <- pairwise %>% rename(p.adjust = value)

pairwise <- pairwise %>% mutate(star = ifelse(p.adjust < 0.05, "*", ""))
#pairwise %>% select(comparison, mean.diff, eff_size_g, p, p.adjust, star)

pairwise_paffect_s2 <- pairwise




# B.7.3 Group comparison negative affect ======================================

# empty vector to store p-values
peace <- vector(mode = "numeric", length = 4)
pairwise <- tibble(comparison = c("1 vs 3", "1 vs 4", "2 vs 3", "2 vs 4"))
pairwise <- pairwise %>% mutate(t = NA)
pairwise <- pairwise %>% mutate(df = NA)
pairwise <- pairwise %>% mutate(mean.diff = NA)
pairwise <- pairwise %>% mutate(eff_size_d = NA)
pairwise <- pairwise %>% mutate(eff_size_g = NA)
pairwise <- pairwise %>% mutate(p = NA)


# B.7.3.1 Passive Control vs. Prosocial ---------------------------------------
# calculate p-values from t-tests
tee <- t.test(naffect ~ treatment_f_reversed, data = df_merge1_3_s2 ,
              alternative = "less")

# Effect Size (Hedge's g)
effsize_g <- effsize::cohen.d(df_merge1_3_s2 $naffect, 
                              as.factor(df_merge1_3_s2 $treatment),
                              hedges.correction = TRUE,
                              na.rm = TRUE)

# Effect Size (Cohen's d)
effsize_d <- MBESS::ci.smd(ncp = tee[["statistic"]][["t"]], 
                           n.1=obs_s2$final[2], 
                           n.2=obs_s2$final[4], 
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


# B.7.3.2 Passive Control vs. Societal ----------------------------------------
# calculate p-values from t-tests
tee <- t.test(naffect ~ treatment_f_reversed, data = df_merge1_4_s2 ,
              alternative = "less")

# Effect Size (Hedge's g)
effsize_g <- effsize::cohen.d(df_merge1_4_s2 $naffect, 
                              as.factor(df_merge1_4_s2 $treatment),
                              hedges.correction = TRUE,
                              na.rm = TRUE)

# Effect Size (Cohen's d)
effsize_d <- MBESS::ci.smd(ncp = tee[["statistic"]][["t"]], 
                           n.1=obs_s2$final[2], 
                           n.2=obs_s2$final[5], 
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

# B.7.3.3 Active Control vs. Prosocial ----------------------------------------
# calculate p-values from t-tests
tee <- t.test(naffect ~ treatment_f_reversed, data = df_merge2_3_s2 ,
              alternative = "less")

# Effect Size (Hedge's g)
effsize_g <- effsize::cohen.d(df_merge2_3_s2 $naffect, 
                              as.factor(df_merge2_3_s2 $treatment),
                              hedges.correction = TRUE,
                              na.rm = TRUE)

# Effect Size (Cohen's d)
effsize_d <- MBESS::ci.smd(ncp = tee[["statistic"]][["t"]], 
                           n.1=obs_s2$final[3], 
                           n.2=obs_s2$final[4], 
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



# B.7.3.4 Active Control vs. Societal -----------------------------------------
# calculate p-values from t-tests
tee <- t.test(naffect ~ treatment_f_reversed, data = df_merge2_4_s2 ,
              alternative = "less")

# Effect Size (Hedge's g)
effsize_g <- effsize::cohen.d(df_merge2_4_s2 $naffect, 
                              as.factor(df_merge2_4_s2 $treatment),
                              hedges.correction = TRUE,
                              na.rm = TRUE)

# Effect Size (Cohen's d)
effsize_d <- MBESS::ci.smd(ncp = tee[["statistic"]][["t"]], 
                           n.1=obs_s2$final[3], 
                           n.2=obs_s2$final[5], 
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



# B.7.3.5 Adjusted p-values ---------------------------------------------------
padjust <- p.adjust(peace, method = "BH")

pairwise <- bind_cols(pairwise, as_tibble(padjust))
pairwise <- pairwise %>% rename(p.adjust = value)

pairwise <- pairwise %>% mutate(star = ifelse(p.adjust < 0.05, "*", ""))
#pairwise %>% select(comparison, mean.diff, eff_size_g, p, p.adjust, star)
pairwise_naffect_s2 <- pairwise




# B.7.4 Group comparison turnover intention ===================================

# empty vector to store p-values
peace <- vector(mode = "numeric", length = 4)
pairwise <- tibble(comparison = c("1 vs 3", "1 vs 4", "2 vs 3", "2 vs 4"))
pairwise <- pairwise %>% mutate(t = NA)
pairwise <- pairwise %>% mutate(df = NA)
pairwise <- pairwise %>% mutate(mean.diff = NA)
pairwise <- pairwise %>% mutate(eff_size_d = NA)
pairwise <- pairwise %>% mutate(eff_size_g = NA)
pairwise <- pairwise %>% mutate(p = NA)


# B.7.4.1 Passive Control vs. Prosocial ---------------------------------------
# calculate p-values from t-tests
tee <- t.test(turnover ~ treatment_f_reversed, data = df_merge1_3_s2 ,
              alternative = "less")

# Effect Size (Hedge's g)
effsize_g <- effsize::cohen.d(df_merge1_3_s2 $turnover, 
                              as.factor(df_merge1_3_s2 $treatment),
                              hedges.correction = TRUE,
                              na.rm = TRUE)

# Effect Size (Cohen's d)
effsize_d <- MBESS::ci.smd(ncp = tee[["statistic"]][["t"]], 
                           n.1=obs_s2$final[2], 
                           n.2=obs_s2$final[4], 
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


# B.7.4.2 Passive Control vs. Societal ----------------------------------------
# calculate p-values from t-tests
tee <- t.test(turnover ~ treatment_f_reversed, data = df_merge1_4_s2 ,
              alternative = "less")

# Effect Size (Hedge's g)
effsize_g <- effsize::cohen.d(df_merge1_4_s2 $turnover, 
                              as.factor(df_merge1_4_s2 $treatment),
                              hedges.correction = TRUE,
                              na.rm = TRUE)

# Effect Size (Cohen's d)
effsize_d <- MBESS::ci.smd(ncp = tee[["statistic"]][["t"]], 
                           n.1=obs_s2$final[2], 
                           n.2=obs_s2$final[5], 
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

# B.7.4.3 Active Control vs. Prosocial ----------------------------------------
# calculate p-values from t-tests
tee <- t.test(turnover ~ treatment_f_reversed, data = df_merge2_3_s2 ,
              alternative = "less")

# Effect Size (Hedge's g)
effsize_g <- effsize::cohen.d(df_merge2_3_s2 $turnover, 
                              as.factor(df_merge2_3_s2 $treatment),
                              hedges.correction = TRUE,
                              na.rm = TRUE)

# Effect Size (Cohen's d)
effsize_d <- MBESS::ci.smd(ncp = tee[["statistic"]][["t"]], 
                           n.1=obs_s2$final[3], 
                           n.2=obs_s2$final[4], 
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



# B.7.4.4 Active Control vs. Societal -----------------------------------------
# calculate p-values from t-tests
tee <- t.test(turnover ~ treatment_f_reversed, data = df_merge2_4_s2 ,
              alternative = "less")

# Effect Size (Hedge's g)
effsize_g <- effsize::cohen.d(df_merge2_4_s2 $turnover, 
                              as.factor(df_merge2_4_s2 $treatment),
                              hedges.correction = TRUE,
                              na.rm = TRUE)

# Effect Size (Cohen's d)
effsize_d <- MBESS::ci.smd(ncp = tee[["statistic"]][["t"]], 
                           n.1=obs_s2$final[3], 
                           n.2=obs_s2$final[5], 
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



# B.7.4.5 Adjusted p-values ---------------------------------------------------
padjust <- p.adjust(peace, method = "BH")

pairwise <- bind_cols(pairwise, as_tibble(padjust))
pairwise <- pairwise %>% rename(p.adjust = value)

pairwise <- pairwise %>% mutate(star = ifelse(p.adjust < 0.05, "*", ""))
#pairwise %>% select(comparison, mean.diff, eff_size_g, p, p.adjust, star)
pairwise_turnover_s2 <- pairwise





# B.7.5 Group comparison willigness to recommend job ==========================

# empty vector to store p-values
peace <- vector(mode = "numeric", length = 4)
pairwise <- tibble(comparison = c("1 vs 3", "1 vs 4", "2 vs 3", "2 vs 4"))
pairwise <- pairwise %>% mutate(t = NA)
pairwise <- pairwise %>% mutate(df = NA)
pairwise <- pairwise %>% mutate(mean.diff = NA)
pairwise <- pairwise %>% mutate(eff_size_d = NA)
pairwise <- pairwise %>% mutate(eff_size_g = NA)
pairwise <- pairwise %>% mutate(p = NA)


# B.7.5.1 Passive Control vs. Prosocial ---------------------------------------
# calculate p-values from t-tests
tee <- t.test(willingnessjobrec ~ treatment_f_reversed, data = df_merge1_3_s2 ,
              alternative = "greater")

# Effect Size (Hedge's g)
effsize_g <- effsize::cohen.d(df_merge1_3_s2 $willingnessjobrec, 
                              as.factor(df_merge1_3_s2 $treatment),
                              hedges.correction = TRUE,
                              na.rm = TRUE)

# Effect Size (Cohen's d)
effsize_d <- MBESS::ci.smd(ncp = tee[["statistic"]][["t"]], 
                           n.1=obs_s2$final[2], 
                           n.2=obs_s2$final[4], 
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


# B.7.5.2 Passive Control vs. Societal ----------------------------------------
# calculate p-values from t-tests
tee <- t.test(willingnessjobrec ~ treatment_f_reversed, data = df_merge1_4_s2 ,
              alternative = "greater")

# Effect Size (Hedge's g)
effsize_g <- effsize::cohen.d(df_merge1_4_s2 $willingnessjobrec, 
                              as.factor(df_merge1_4_s2 $treatment),
                              hedges.correction = TRUE,
                              na.rm = TRUE)

# Effect Size (Cohen's d)
effsize_d <- MBESS::ci.smd(ncp = tee[["statistic"]][["t"]], 
                           n.1=obs_s2$final[2], 
                           n.2=obs_s2$final[5], 
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

# B.7.5.3 Active Control vs. Prosocial ----------------------------------------
# calculate p-values from t-tests
tee <- t.test(willingnessjobrec ~ treatment_f_reversed, data = df_merge2_3_s2 ,
              alternative = "greater")

# Effect Size (Hedge's g)
effsize_g <- effsize::cohen.d(df_merge2_3_s2 $willingnessjobrec, 
                              as.factor(df_merge2_3_s2 $treatment),
                              hedges.correction = TRUE,
                              na.rm = TRUE)

# Effect Size (Cohen's d)
effsize_d <- MBESS::ci.smd(ncp = tee[["statistic"]][["t"]], 
                           n.1=obs_s2$final[3], 
                           n.2=obs_s2$final[4], 
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



# B.7.5.4 Active Control vs. Societal -----------------------------------------
# calculate p-values from t-tests
tee <- t.test(willingnessjobrec ~ treatment_f_reversed, data = df_merge2_4_s2 ,
              alternative = "greater")

# Effect Size (Hedge's g)
effsize_g <- effsize::cohen.d(df_merge2_4_s2 $willingnessjobrec, 
                              as.factor(df_merge2_4_s2 $treatment),
                              hedges.correction = TRUE,
                              na.rm = TRUE)

# Effect Size (Cohen's d)
effsize_d <- MBESS::ci.smd(ncp = tee[["statistic"]][["t"]], 
                           n.1=obs_s2$final[3], 
                           n.2=obs_s2$final[5], 
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



# B.7.5.5 Adjusted p-values ---------------------------------------------------
padjust <- p.adjust(peace, method = "BH")

pairwise <- bind_cols(pairwise, as_tibble(padjust))
pairwise <- pairwise %>% rename(p.adjust = value)

pairwise <- pairwise %>% mutate(star = ifelse(p.adjust < 0.05, "*", ""))
#pairwise %>% select(comparison, mean.diff, eff_size_g, p, p.adjust, star)
pairwise_will_s2 <- pairwise


# remove unused objects
rm(effsize_d, effsize_g, pairwise, tee, padjust, peace)









# B.8 Descriptives (Table 3) ##################################################
aov_paffect_result_s2 <- aov_result(aov_paffect_s2)
aov_naffect_result_s2 <- aov_result(aov_naffect_s2)
aov_turnover_result_s2 <- aov_result(aov_turnover_s2)
aov_will_result_s2 <- aov_result(aov_will_s2)
aov_bene_result_s2 <- 
  aov_result(aov(df_merge_s2$beneficiaries ~ df_merge_s2$treatment_f))
aov_obj_bene_result_s2 <- 
  aov_result(aov(df_merge_s2$beneficiaries_obj ~ df_merge_s2$treatment_f))
aov_prosocialimpact_result_s2 <- aov_result(aov_prosocialimpact_s2)
aov_societalimpact_result_s2 <- aov_result(aov_societalimpact_s2)

aov_age_result_s2 <- aov_result(aov_age_s2)
chi_female_result_s2 <- chi_result(df_merge_s2$gender_f, 
                                   df_merge_s2$treatment_f)
chi_manager_result_s2 <- chi_result(df_merge_s2$manager_dummy, 
                                    df_merge_s2$treatment_f)


desc_s2 <-  desc_s2 %>%
  mutate(Diff = "") %>%
  mutate(Diff = ifelse(treatment == "paffect_m",
                       aov_paffect_result_s2, Diff),
         Diff = ifelse(treatment == "naffect_m",
                       aov_naffect_result_s2, Diff),
         Diff = ifelse(treatment == "turnover_m",
                       aov_turnover_result_s2, Diff),
         Diff = ifelse(treatment == "will_m",
                       aov_will_result_s2, Diff),
         Diff = ifelse(treatment == "psi_m",
                       aov_prosocialimpact_result_s2, Diff),
         Diff = ifelse(treatment == "si_m",
                       aov_societalimpact_result_s2, Diff),
         Diff = ifelse(treatment == "age_m",
                       aov_age_result_s2, Diff),
         Diff = ifelse(treatment == "female_m",
                       chi_female_result_s2, Diff),
         Diff = ifelse(treatment == "manager_m",
                       chi_manager_result_s2, Diff)
         )



desc_s2 <- desc_s2 %>%
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
         treatment = ifelse(treatment == "si_m",
                            "Societal impact", treatment),
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
         treatment = ifelse(treatment == "si_sd",
                            "", treatment),
         treatment = ifelse(treatment == "age_sd",
                            "", treatment),
         treatment = ifelse(treatment == "female_sd",
                            "", treatment),
         treatment = ifelse(treatment == "manager_sd",
                            "", treatment)
  )

desc_s2








# B.9 Plot with mean values per group (Figure 2) ##############################

# B.9.1 Prepare data (sd, n, se, ci per treatment per variable) ===============
df_merge_sum_s2 <- df_merge_s2 %>% 
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
df_merge_sum2_s2 <- df_merge_sum_s2 %>% 
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
df_merge_sum3_s2 <- df_merge_sum_s2 %>% 
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
df_merge_sum4_s2 <- df_merge_sum_s2 %>% 
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
df_merge_sum2_s2 <- left_join(df_merge_sum2_s2, df_merge_sum3_s2,
                              key = c("treatment", "var"))
df_merge_sum2_s2 <- left_join(df_merge_sum2_s2, df_merge_sum4_s2,
                              key = c("treatment", "var"))


# Add factor variables (for captions and sorting)
df_merge_sum2_s2 <- df_merge_sum2_s2 %>%
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

# B.9.2 Plot ==================================================================

# Define jitter so it is the same for means and CI
dodge <- position_dodge(.8) # how much jitter on the x-axis?


meanplot_s2 <- ggplot(df_merge_sum2_s2, aes(x=var_f, y=mean, 
                                            color =treatment_f)) + 
  geom_errorbar(aes(ymin=lower_ci,
                    ymax=upper_ci), 
                width=.5,
                size = .7,
                position = dodge,
                show.legend=FALSE) +
  geom_point(aes(shape = treatment_f),
             position = dodge, 
             size = 2) +
  scale_fill_continuous(guide = guide_legend()) +
  theme(legend.position="bottom") +
  labs(title = "Figure 2: Differences in dependent variables\nbetween experimental groups in Study 2",
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
  scale_shape_manual(values = c(15, 16, 17, 18)) 

rm(df_merge_sum_s2, df_merge_sum2_s2, df_merge_sum3_s2, df_merge_sum4_s2)








# B.10 Table with results of group comparisons ################################
pairwise_paffect2_s2 <- 
  pairwise_paffect_s2 %>%
  mutate(var = "paffect") %>%
  mutate(test = paste0("t(",
                       broman::myround(df, 1),
                       ") = ",
                       broman::myround(t, 2),
                       ", ", pformat(p.adjust))) %>%
  select(var, comparison, mean.diff, test, eff_size_g)

pairwise_naffect2_s2 <- 
  pairwise_naffect_s2 %>%
  mutate(var = "naffect") %>%
  mutate(test = paste0("t(",
                       broman::myround(df, 1),
                       ") = ",
                       broman::myround(t, 2),
                       ", ", pformat(p.adjust))) %>%
  select(var, comparison, mean.diff, test, eff_size_g)

pairwise_turnover2_s2 <- 
  pairwise_turnover_s2 %>%
  mutate(var = "turnover") %>%
  mutate(test = paste0("t(",
                       broman::myround(df, 1),
                       ") = ",
                       broman::myround(t, 2),
                       ", ", pformat(p.adjust))) %>%
  select(var, comparison, mean.diff, test, eff_size_g)

pairwise_will2_s2 <- 
  pairwise_will_s2 %>%
  mutate(var = "will") %>%
  mutate(test = paste0("t(",
                       broman::myround(df, 1),
                       ") = ",
                       broman::myround(t, 2),
                       ", ", pformat(p.adjust))) %>%
  select(var, comparison, mean.diff, test, eff_size_g)

pairwise2_s2 <- bind_rows(pairwise_paffect2_s2, pairwise_naffect2_s2,
                          pairwise_turnover2_s2, pairwise_will2_s2)
pairwise2_s2
pairwise3_s2 <- pairwise2_s2 %>% 
  select(-test, -eff_size_g) %>% 
  mutate(mean.diff = 
           as.character(broman::myround(mean.diff,2))) %>%
  spread(comparison, mean.diff) %>%
  mutate(value = "mean.diff")

pairwise4_s2 <- pairwise2_s2 %>% 
  select(-mean.diff, -test) %>%
  mutate(eff_size_g = 
           as.character(broman::myround(eff_size_g,2))) %>%
  spread(comparison, eff_size_g) %>%
  mutate(value = "eff_size_g")

pairwise5_s2 <- pairwise2_s2 %>% 
  select(-mean.diff, -eff_size_g) %>%
  spread(comparison, test) %>%
  mutate(value = "test")

results_table_s2 <- bind_rows(pairwise3_s2, pairwise4_s2, pairwise5_s2) %>%
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

results_table_s2
rm(pairwise2_s2, pairwise3_s2, pairwise4_s2, pairwise5_s2,
   pairwise_paffect2_s2, pairwise_naffect2_s2,
   pairwise_turnover2_s2, pairwise_will2_s2)







# B.11 Regression with beneficiary contact (H4) ###############################
# Model 1: Treatment only
reg_paffect_s2 <- lm(paffect ~ treatment_f, data = df_merge_s2)
reg_naffect_s2 <- lm(naffect ~ treatment_f, data = df_merge_s2)
reg_turnover_s2 <- lm(turnover ~ treatment_f, data = df_merge_s2)
reg_will_s2 <- lm(willingnessjobrec ~ treatment_f, data = df_merge_s2)

# Model 2: Add contact with beneficiaries
reg_paffect2_s2 <- update(reg_paffect_s2, . ~ . + beneficiaries)
reg_naffect2_s2 <- update(reg_naffect_s2, . ~ . + beneficiaries)
reg_turnover2_s2 <- update(reg_turnover_s2, . ~ . + beneficiaries)
reg_will2_s2 <- update(reg_will_s2, . ~ . + beneficiaries)

# Model 3: Add beneficiaries * treatment
reg_paffect3_s2 <- update(reg_paffect_s2, . ~ . + beneficiaries*treatment_f)
reg_naffect3_s2 <- update(reg_naffect_s2, . ~ . + beneficiaries*treatment_f)
reg_turnover3_s2 <- update(reg_turnover_s2, . ~ . + beneficiaries*treatment_f)
reg_will3_s2 <- update(reg_will_s2, . ~ . + beneficiaries*treatment_f)

# Model 4: Add objective beneficiaries * treatment to model 1
reg_paffect4_s2 <- update(reg_paffect_s2, . ~ . + 
                            beneficiaries_obj*treatment_f)
reg_naffect4_s2 <- update(reg_naffect_s2, . ~ . + 
                            beneficiaries_obj*treatment_f)
reg_turnover4_s2 <- update(reg_turnover_s2, . ~ . + 
                             beneficiaries_obj*treatment_f)
reg_will4_s2 <- update(reg_will_s2, . ~ . + beneficiaries_obj*treatment_f)



