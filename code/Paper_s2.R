# THIS IS THE BEGINNING OF STUDY 2 CODE XXXXXXXXXXXXXXXXXXXXXXXXX -------------

# B.1 Import data #############################################################
coltypes <- cols(
  V1 = col_character(),
  V2 = col_character(),
  V3 = col_character(),
  V4 = col_character(),
  V5 = col_character(),
  V6 = col_character(),
  V7 = col_integer(),
  V8 = col_character(),
  V9 = col_character(),
  V10 = col_integer(),
  workerId = col_character(),
  assignmentId = col_character(),
  hitId = col_character(),
  timebeforetreatment = col_time(format = ""),
  timeaftertreatment = col_time(format = ""),
  intro = col_integer(),
  reflectiongeneral = col_character(),
  reflectionprosocial = col_character(),
  reflectionsocietal = col_character(),
  paffect1 = col_integer(),
  paffect2 = col_integer(),
  paffect3 = col_integer(),
  paffect4 = col_integer(),
  paffect5 = col_integer(),
  paffect6 = col_integer(),
  naffect1 = col_integer(),
  naffect2 = col_integer(),
  naffect3 = col_integer(),
  naffect4 = col_integer(),
  naffect5 = col_integer(),
  naffect6 = col_integer(),
  willingnessjobrec = col_integer(),
  willingnessjobrec2 = col_integer(),
  turnover1 = col_integer(),
  turnover2 = col_integer(),
  turnover3 = col_integer(),
  manipulationcheck = col_integer(),
  prosocialimpact1 = col_integer(),
  prosocialimpact2 = col_integer(),
  prosocialimpact3 = col_integer(),
  societalimpact1 = col_integer(),
  societalimpact2 = col_integer(),
  societalimpact3 = col_integer(),
  beneficiaries_obj = col_integer(),
  taskvariety1 = col_integer(),
  taskvariety2 = col_integer(),
  taskvariety3 = col_integer(),
  taskvariety4 = col_integer(),
  taskvariety5 = col_integer(),
  taskanalyz_28 = col_integer(),
  taskanalyz_29 = col_integer(),
  taskanalyz_30 = col_integer(),
  taskanalyz_31 = col_integer(),
  attention1 = col_integer(),
  autonomy1 = col_integer(),
  autonomy2 = col_integer(),
  autonomy3 = col_integer(),
  beneficiaries1 = col_integer(),
  beneficiaries2 = col_integer(),
  ttention2 = col_integer(),
  jobtitle = col_character(),
  manager = col_integer(),
  gender = col_integer(),
  yearbirth = col_integer(),
  sector = col_integer(),
  atwork = col_integer(),
  openfinal = col_character(),
  DO_BR_FL_13 = col_character(),
  DO_BL_Control = col_character(),
  DO_Q_prosocialimpact = col_character(),
  DO_Q_beneficiaries = col_character(),
  DO_Q_taskvariety = col_character(),
  DO_Q_taskanalyz = col_character(),
  DO_Q_autonomy = col_character(),
  DO_Q_affect = col_character(),
  DO_Q_turnover = col_character(),
  DO_Q_willingnessjobrec = col_character(),
  LocationLatitude = col_double(),
  LocationLongitude = col_double(),
  LocationAccuracy = col_integer()
)

coltypes2 <- cols(
  StartDate = col_character(),
  EndDate = col_character(),
  Status = col_integer(),
  IPAddress = col_character(),
  Progress = col_integer(),
  Duration__in_seconds_ = col_integer(),
  Finished = col_integer(),
  RecordedDate = col_character(),
  ResponseId = col_character(),
  RecipientLastName = col_character(),
  RecipientFirstName = col_character(),
  RecipientEmail = col_character(),
  ExternalReference = col_character(),
  LocationLatitude = col_double(),
  LocationLongitude = col_double(),
  DistributionChannel = col_character(),
  UserLanguage = col_character(),
  reflectiongeneral = col_character(),
  reflectionprosocial = col_character(),
  reflectionsocietal = col_character(),
  affect_paffect1 = col_integer(),
  affect_paffect2 = col_integer(),
  affect_paffect3 = col_integer(),
  affect_paffect4 = col_integer(),
  affect_paffect5 = col_integer(),
  affect_paffect6 = col_integer(),
  affect_naffect1 = col_integer(),
  affect_naffect2 = col_integer(),
  affect_naffect3 = col_integer(),
  affect_naffect4 = col_integer(),
  affect_naffect5 = col_integer(),
  affect_naffect6 = col_integer(),
  affect_DO_paffect1 = col_integer(),
  affect_DO_paffect2 = col_integer(),
  affect_DO_paffect3 = col_integer(),
  affect_DO_paffect4 = col_integer(),
  affect_DO_paffect5 = col_integer(),
  affect_DO_paffect6 = col_integer(),
  affect_DO_naffect1 = col_integer(),
  affect_DO_naffect2 = col_integer(),
  affect_DO_naffect3 = col_integer(),
  affect_DO_naffect4 = col_integer(),
  affect_DO_naffect5 = col_integer(),
  affect_DO_naffect6 = col_integer(),
  willingnessjobrec_willingnessjobrec = col_integer(),
  willingnessjobrec_willingnessjobrec2 = col_integer(),
  willingnessjobrec_DO_willingnessjobrec = col_integer(),
  willingnessjobrec_DO_willingnessjobrec2 = col_integer(),
  turnover_turnover1 = col_integer(),
  turnover_turnover2 = col_integer(),
  turnover_turnover3 = col_integer(),
  turnover_DO_turnover1 = col_integer(),
  turnover_DO_turnover2 = col_integer(),
  turnover_DO_turnover3 = col_integer(),
  manipulationcheck = col_integer(),
  prosocialimpact_prosocialimpact1 = col_integer(),
  prosocialimpact_prosocialimpact2 = col_integer(),
  prosocialimpact_prosocialimpact3 = col_integer(),
  prosocialimpact_DO_prosocialimpact1 = col_integer(),
  prosocialimpact_DO_prosocialimpact2 = col_integer(),
  prosocialimpact_DO_prosocialimpact3 = col_integer(),
  societalimpact_societalimpact1 = col_integer(),
  societalimpact_societalimpact2 = col_integer(),
  societalimpact_societalimpact3 = col_integer(),
  beneficiaries_obj = col_integer(),
  taskvariety_taskvariety1 = col_integer(),
  taskvariety_taskvariety2 = col_integer(),
  taskvariety_taskvariety3 = col_integer(),
  taskvariety_taskvariety4 = col_integer(),
  taskvariety_taskvariety5 = col_integer(),
  taskvariety_DO_taskvariety1 = col_integer(),
  taskvariety_DO_taskvariety2 = col_integer(),
  taskvariety_DO_taskvariety3 = col_integer(),
  taskvariety_DO_taskvariety4 = col_integer(),
  taskvariety_DO_taskvariety5 = col_integer(),
  taskanalyz_taskanalyz_28 = col_integer(),
  taskanalyz_taskanalyz_29 = col_integer(),
  taskanalyz_taskanalyz_30 = col_integer(),
  taskanalyz_taskanalyz_31 = col_integer(),
  taskanalyz_attention1 = col_integer(),
  taskanalyz_DO_taskanalyz_28 = col_integer(),
  taskanalyz_DO_taskanalyz_29 = col_integer(),
  taskanalyz_DO_taskanalyz_30 = col_integer(),
  taskanalyz_DO_taskanalyz_31 = col_integer(),
  taskanalyz_DO_attention1 = col_integer(),
  autonomy_autonomy1 = col_integer(),
  autonomy_autonomy2 = col_integer(),
  autonomy_autonomy3 = col_integer(),
  autonomy_DO_autonomy1 = col_integer(),
  autonomy_DO_autonomy2 = col_integer(),
  autonomy_DO_autonomy3 = col_integer(),
  beneficiaries_beneficiaries1 = col_integer(),
  beneficiaries_beneficiaries2 = col_integer(),
  beneficiaries_ttention2 = col_integer(),
  beneficiaries_DO_beneficiaries1 = col_integer(),
  beneficiaries_DO_beneficiaries2 = col_integer(),
  beneficiaries_DO_ttention2 = col_integer(),
  jobtitle = col_character(),
  manager = col_integer(),
  gender = col_integer(),
  yearbirth = col_integer(),
  sector = col_integer(),
  atwork = col_integer(),
  openfinal = col_character(),
  workerId = col_character(),
  assignmentId = col_character(),
  hitId.0 = col_character(),
  timebeforetreatment = col_time(format = ""),
  timeaftertreatment = col_time(format = ""),
  FL_13_DO_ControlGroup = col_integer(),
  FL_13_DO_GeneralReflectionTask = col_integer(),
  FL_13_DO_ProsocialReflection = col_integer(),
  FL_13_DO_SocietalReflection = col_integer(),
  Control_DO_sector = col_integer(),
  Control_DO_yearbirth = col_integer(),
  Control_DO_gender = col_integer(),
  Control_DO_manager = col_integer(),
  Control_DO_jobtitle = col_integer(),
  Control_DO_atwork = col_integer()
)

coltypes_turkprime <- cols(
  AmazonIdentifier = col_character(),
  AssignmentId = col_character(),
  DurationInSeconds = col_integer(),
  StartTime = col_character(),
  CompletionTime = col_character(),
  ApprovalStatus = col_character(),
  AmountPaid = col_double(),
  AmountBonused = col_double(),
  `Actual Completion Code` = col_character(),
  `Expected Completion Code` = col_character(),
  Gender = col_character(),
  GenderConsistency = col_character(),
  Feedback = col_character()
)

# Import data in two different Qualtric flavors (different additional vars)
df_s2 <- read_csv(here("data", "Data_Study2_v2_180306.csv"), 
                  col_types = coltypes)
df2_s2 <- read_csv(here("data", "Data_Study2_v1_180306.csv"), 
                   col_types = coltypes2)


# Import TurkPrime Approval data
turkprime1_s2 <- read_csv(here("data", "TurkPrime_worker_Study2_Part1.csv"),
                          col_types = coltypes_turkprime)
turkprime1_s2 <- turkprime1_s2 %>% mutate(part = 1)

turkprime2_s2 <- read_csv(here("data", "TurkPrime_worker_Study2_Part2.csv"),
                          col_types = coltypes_turkprime)
turkprime2_s2 <- turkprime2_s2 %>% mutate(part = 2)

turkprime3_s2 <- read_csv(here("data", "TurkPrime_worker_Study2_Part3.csv"),
                          col_types = coltypes_turkprime)
turkprime3_s2 <- turkprime3_s2 %>% mutate(part = 3)



# merge TurkPrime data 
turkprime_s2 <- bind_rows(turkprime1_s2, turkprime2_s2, turkprime3_s2)




# B.2 Clean up and rename #####################################################

# B.2.1 Remove empty / irrelevant variables ===================================
# Remove empty / irrelevant variables from Turkprime
turkprime_s2 <- turkprime_s2 %>% select(-AmountPaid, -AmountBonused, 
                                        -ApprovalStatus)

# Keep only relevant vars from df2
df2_s2 <- df2_s2 %>% select(workerId, Finished, Duration__in_seconds_, 
                            affect_paffect1, gender, reflectiongeneral) %>%
  rename(workerid = workerId,
         finished = Finished,
         duration_sec = `Duration__in_seconds_` ,
         paffect1 = affect_paffect1)

# Remove irrelevant variables from survey 
df_s2 <- df_s2 %>% select(-V1, -V2, -V3, -V4, -V5, -V7, -V10)


# B.2.2 rename variables =======================================================
df_s2 <- df_s2 %>% rename(attention2 = ttention2,
                          ipaddr = V6,
                          startdate = V8,
                          enddate = V9,
                          workerid = workerId,
                          taskanalyz1 = taskanalyz_28,
                          taskanalyz2 = taskanalyz_29,
                          taskanalyz3 = taskanalyz_30,
                          taskanalyz4 =taskanalyz_31,
                          willingnessjobrec1 = willingnessjobrec)





# B.2.3 generate treatment var ================================================
df_s2 <- df_s2 %>% 
  mutate(treatment = ifelse(DO_BR_FL_13 == "Control Group",
                            1, NA),
         treatment = ifelse(DO_BR_FL_13 == "General Reflection Task",
                            2, treatment),
         treatment = ifelse(DO_BR_FL_13 == "Prosocial Reflection",
                            3, treatment),
         treatment = ifelse(DO_BR_FL_13 == "Societal Reflection",
                            4, treatment))




# B.2.4 Find duplicates =======================================================
dubli_s2 <- df_s2 %>% 
  group_by(workerid) %>% 
  mutate(n = n()) %>% 
  arrange(desc(n), workerid) %>% 
  select(workerid, n)

# Remove duplicates 
worker1 <- df_s2 %>% filter(workerid == "A13CXQGLSH6QY8")
worker2 <- df_s2 %>% filter(workerid == "A269YMD69ZOXIK")
worker3 <- df_s2 %>% filter(workerid == "A300U36A83F1TQ")
worker4 <- df_s2 %>% filter(workerid == "A3KKPCU6AHF1ZV")
worker5 <- df_s2 %>% filter(workerid == "A1VPSUPBTA6Q87")

tibble_print_vars(worker1)
# second entry of worker 1 is empty
df_s2 <- df_s2 %>% mutate(drop = ifelse(workerid == "A13CXQGLSH6QY8" & 
                                          is.na(paffect1), 1, 0))

tibble_print_vars(worker2)
# second entry of worker 2 is empty
df_s2 <- df_s2 %>% mutate(drop = ifelse(workerid == "A269YMD69ZOXIK" & 
                                          is.na(paffect1), 1, drop))

tibble_print_vars(worker3)
# second entry of worker 3 is missing socio-demographics
df_s2 <- df_s2 %>% mutate(drop = ifelse(workerid == "A300U36A83F1TQ" & 
                                          is.na(gender), 1, drop))

tibble_print_vars(worker4)
# worker 4 participate twice. Will use the first one
df_s2 <- df_s2 %>% mutate(drop = ifelse(workerid == "A3KKPCU6AHF1ZV" & 
                                          is.na(reflectiongeneral), 1, drop))

tibble_print_vars(worker5)
# worker 5 participate twice. Second one was to get completion code
df_s2 <- df_s2 %>% mutate(drop = ifelse(workerid == "A1VPSUPBTA6Q87" & 
                                          !is.na(reflectionprosocial),
                                        1, drop))

df_s2 <- df_s2 %>% filter(drop == 0) %>%
  select(-drop)

# Remove duplicates from df2 
df2_s2 <- df2_s2 %>% mutate(drop = ifelse(workerid == "A13CXQGLSH6QY8" & 
                                            is.na(paffect1), 1, 0),
                            drop = ifelse(workerid == "A269YMD69ZOXIK" & 
                                            is.na(paffect1), 1, drop),
                            drop = ifelse(workerid == "A300U36A83F1TQ" & 
                                            is.na(gender), 1, drop),
                            drop = ifelse(workerid == "A3KKPCU6AHF1ZV" & 
                                            is.na(reflectiongeneral), 1, drop))

df2_s2 <- df2_s2 %>% 
  filter(drop == 0) %>% 
  select(-reflectiongeneral, -gender, -paffect1, -drop)



# B.3 Merge data ##############################################################

# Merge df with df2 
df_s2 <- left_join(df_s2, df2_s2, by = "workerid")

# Merge survey data and TurkPrime data 
df_merge_s2 <- left_join(df_s2, turkprime_s2, 
                         by = c("workerid" = "AmazonIdentifier"))

rm(df_s2, df2_s2,
   turkprime_s2,
   turkprime1_s2, turkprime2_s2, turkprime3_s2,
   dubli_s2,
   worker1, worker2, worker3, worker4, worker5,
   coltypes, coltypes2, coltypes_turkprime)


# Fix yearbirth error
df_merge_s2 <- df_merge_s2 %>%
  mutate(yearbirth = 2012 - yearbirth)







# B.4 Generate variables ######################################################

# B.4.1 Generate factor variables =============================================
df_merge_s2$treatment_f <- factor(df_merge_s2$treatment)
df_merge_s2$gender_f <- factor(df_merge_s2$gender, 
                               labels = c("Male", "Female", "Other"))


# B.4.2 Set NA categories =====================================================
df_merge_s2 <- df_merge_s2 %>% 
  mutate(atwork = ifelse(atwork == 2, NA, atwork))




# B.4.3 Reverse items =========================================================
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


# B.4.4 Create Dummies ========================================================
df_merge_s2 <- df_merge_s2 %>% mutate(public = ifelse(sector == 1, 1, 0),
                                      public = ifelse(is.na(sector), NA, public))
df_merge_s2 <- df_merge_s2 %>% mutate(private = ifelse(sector == 3, 1, 0),
                                      private = ifelse(is.na(sector), NA, private))  
df_merge_s2 <- df_merge_s2 %>% mutate(npo = ifelse(sector == 2, 1, 0),
                                      npo = ifelse(is.na(sector), NA, npo))
df_merge_s2 <- df_merge_s2 %>% mutate(sector_na = ifelse(sector == 4, 1, 0),
                                      sector_na = ifelse(is.na(sector), NA, 
                                                         sector_na))
df_merge_s2 <- df_merge_s2 %>% mutate(manager_dummy = ifelse(manager == 0, 0, 1),
                                      manager_dummy = ifelse(is.na(manager), NA, 
                                                             manager_dummy))
df_merge_s2 <- df_merge_s2 %>% mutate(male = ifelse(gender == 1, 1, 0),
                                      male = ifelse(is.na(gender), NA, male),
                                      female = ifelse(gender == 2, 1, 0),
                                      female = ifelse(is.na(gender), NA, female),
                                      gender_other = ifelse(gender == 3, 1, 0),
                                      gender_other = ifelse(is.na(gender), 
                                                            NA, gender_other))
df_merge_s2 <- df_merge_s2 %>% 
  mutate(treatment_dummy1 = ifelse(treatment == 1, 1, 0),
         treatment_dummy2 = ifelse(treatment == 2, 1, 0),
         treatment_dummy3 = ifelse(treatment == 3, 1, 0),
         treatment_dummy4 = ifelse(treatment == 4, 1, 0))



# B.4.5 Count words in reflection task ========================================
df_merge_s2 <- df_merge_s2 %>% mutate(word_count = NA)
df_merge_s2 <- df_merge_s2 %>% 
  mutate(word_count = 
           ifelse(treatment == 2, 
                  sapply(gregexpr("[[:alpha:]]+", 
                                  df_merge_s2$reflectiongeneral),
                         function(x) sum(x > 0)), 
                  word_count))
df_merge_s2 <- df_merge_s2 %>% 
  mutate(word_count = ifelse(treatment == 3, 
                             sapply(gregexpr("[[:alpha:]]+", 
                                             df_merge_s2$reflectionprosocial), 
                                    function(x) sum(x > 0)),
                             word_count)) 
df_merge_s2 <- df_merge_s2 %>% 
  mutate(word_count = ifelse(treatment == 4, 
                             sapply(gregexpr("[[:alpha:]]+", 
                                             df_merge_s2$reflectionsocietal), 
                                    function(x) sum(x > 0)), 
                             word_count))

df_merge_s2 <- df_merge_s2 %>% 
  mutate(word_count = ifelse(treatment != 1 & is.na(word_count),
                             0, word_count))





# B.4.6 Create treatment duration =============================================
df_merge_s2 <- df_merge_s2 %>% 
  mutate(treatment_duration = timeaftertreatment - timebeforetreatment)

# B.4.7 Create dependent variables ============================================

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


# B.4.8 Create moderator / control variables ==================================

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








# B.5 Clean data ##############################################################

# B.5.1 Store initial n =======================================================
n_total <- tibble(treatment = "total", initial = nrow(df_merge_s2))
obs_s2 <- df_merge_s2 %>% 
  group_by(treatment) %>% 
  summarise(initial = n()) %>%
  mutate(treatment = as.character(treatment))
obs_s2 <- bind_rows(n_total, obs_s2)
rm(n_total)


# B.5.2 Remove obs how did not receive treatment ==============================
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






# B.5.3 Remove obs how did not answer dependent variable ======================
df_merge_s2 <- df_merge_s2 %>% 
  mutate(mi_dep = ifelse(is.na(paffect) | 
                           is.na(naffect) |
                           is.na(turnover) |
                           is.na(willingnessjobrec),
                         1, 0)) %>%
  filter(mi_dep != 1) %>%
  select(-mi_dep)


# B.5.4 Remove obs how did not finish questionnaire ===========================
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

# B.5.5 Remove participants who do not work in the public sector ==============
df_merge_s2 <- df_merge_s2 %>%
  filter(sector == 1)

# B.5.6 Remove participants whose answers indicate that they are not working in the public sector ====
df_merge_s2 <- df_merge_s2 %>%
  filter(workerid != "APF1EAZT104LQ", # I am a merchandiser for Nestle and the job I do gives me great satisfaction. I went into a store last week and saw the difference I made because of how well our merchandise was placed on the shelves. It made me feel good to know I was making a difference.
         workerid != "A2QKM3JUFWBSMO", #	I work in the insurance industry. I am an independent agent that represents one of the large national carriers. Last week I had the pleasure of delivering a claims check for a family that lost their home in the California wildfires. It was a great relief or the family as they are now able to begin the process of rebuilding their lives.
         workerid != "A3VPTQQ4NQDVNZ",	# I am a welder fabricator I build anything from structural steel beams for new building to small air craft parts. I remember one time I got called out to a job were we had to build a hand rail that lead into a pool. The tenant of the home was disabled and he best therapy she could do was to go in the pool. So when we installed her hand rail she was really happy.
         workerid != "A1QP59PUWJJMJY", #	I'm an IT Coordinating Manager for a small tech company, last week I had to schedule the work for 4 technical support technicians they go on call to the local Kansas City area and help set up computers, software and servers. I'm in charge of outsourcing tem and teaching them.
         workerid != "AZHZ1GC6RWSRC",	# I work in a deli, and recently helped a customer find deli meats that had the lowest amount of sodium. They had problems with their blood pressure, and had to avoid sodium but didn't know which products had the lowest. I went through the labels of every poduct in our front case and helped them decide which product, per serving, was the healthiest for their condition.
         workerid != "A389KAGDNVULOJ", # I received notification through a system that we use that a new person was to be hired into our business. I created the offer letter and sent it to the new employee. they responded and accepted and then i entered him into our system so that he could gain ccess to various different and a work email address. he then came in on his start date and completed the new hire paperwork.
         workerid != "A3A05FW2HAWPBM", # I work in finance focused on customer service. A large part of my job has to do with helping customers access online accounts and answer questions about securities. Last week I assisted an elderly customer with logging into an online account. This took ovr an hour but it was very rewarding in that we were both pleased with him eventually being able to log in.
         workerid != "A290UTHIBLPO52", #I'm homeschool my 4 kids and help them learn how to be independent, think critical, and work as a team. I would have the older kids help the younger kids to introduce teamwork. Certain subjects they are allowed to work on their own and I will follow up wih questions to ensure they have comprehended what they read and discussions will take place to see how it effects their life.
         workerid != "A3C2J2DZDXKV34", # I draw blueprints of the products that the company I work for makes. I continually communicate with people all over the country at different plants through the phone and teaching them how to read the prints that I make. I always enjoy when someone gets wht I was trying to tell them through a visual aid. This is the joy I get in the job that I do.
         workerid != "AZ4MMVSAY9L3X", #	I gave a presentation to our management team about the analytics of our website. This information will help investors, it will help our client's perform better, and it will give everyone insight into a part of our company they might not normally think abot.
         workerid != "A2TN5WX8SSRW0", # I am a supervisor at a Mortgage Title company, we are responsible for clearing any title defects on a borrower's property when they are refinancing. There was an awful file where the lender was willing to cancel their refinance due to an outstanding judgmnt. I was able to convince the creditor to subordinate for this refinance to continue.
         workerid != "A1WAH8PEKTJ0GP", #	I am a chef and last week i had the honor to cook for the new president of one of the oldest school in America. I did not realize at the time that this was the first time the school had ever had a woman president. I was asked the day before the dinner if  would assist at the president house now i have been to the president house before serving the old president but this was a new president of the school so you can imagine the nervousness i felt. What if she does not like the meal what kind of food do theylike all this was going through my head. So I assembled all my ingredients and started preparing dinner as I was preparing the meal the new elected president walked into the kitchen and introduced themselves and i felt more comfortable after that . The diner went was a hit . And what better way to welcome the new president.
         workerid != "A17JS48GBE7C19" #	Well I work at Pizza Hut and I make pizzas I clean the restaurant I make the dough I make the sauce I put it all together I send it to the fryer I'll find some wings I make the text Jeannie pastas I also make desserts which is the cookies and brownies andthen I clean up that's all I do
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




# B.5.7 Remove obs with failed attention check ================================
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



# B.5.8 Remove participants whose answers to the reflection task are nonsense =====
df_merge_s2 <- df_merge_s2 %>%
  filter(workerid != "A1C6NTK19FBQO0", #nterested in your everyday work experiences. Please take a moment and think about a work-related event from the last weeks that gives a good impression of what work you do.
         workerid != "AZ9V8MHARJQU2", #I choose to not participate in society as well as others, thus contributing to society as neutral as possible. I do this in my everyday lifestyle, this meaning my lack of participation in the social aspect of society. This happens everywhere I go, or so Itry to manage.
         workerid != "A36D7P81YSP3VZ", #Attitudes, orientations, or behaviors which take the interests, intentions, or needs of other people into account (in contrast to anti-social behaviour) has played some role in defining the idea or the principle. For instance terms like social realism, soial justice, social constructivism, social psychology, social anarchism and social capital imply that there is some social process involved or considered, a process that is not there in regular, "non-social" realism, justice, constructivism, psychology, aarchism, or capital. The adjective "social" is also used often in politics, although its meaning in a context depends heavily on who is using it. In left-wing circles it is often used to imply a liberal characteristic, while in right-wing circles it is gnerally used to imply a conservative characteristic. This adjective is used much more often by those on the political left than by those on the political right. For these reasons, those seeking to avoid association with the left-right political debates ofen seek to label their work with phrases that do not include the word "social". An example is quasi-empiricism in mathematics which is sometimes labelled social constructivism by those who see it as an unwarranted intrusion of social considerations in matematical practice.
         workerid != "A3P6BRCL0IC22S", #In psychology and cognitive science, the positivity effect is the ability to constructively analyze a situation where the desired results are not achieved; but still obtain positive feedback that assists our future progression. When a person is considerin people they like (including themselves), the person tends to make situational attributions about their negative behaviors and dispositional attributions about their positive behaviors. The reverse may be true for people that the person dislikes. This maywell be because of the dissonance between liking a person and seeing them behave negatively. Example: If a friend hits someone, one would tell them the other guy deserved it or that he had to defend himself.
         workerid != "A9K63G3FV7Q1R", #More improve digital life . work simplification work security done. easy moved . simple transaction and highly secured . public services social media to help us the government based the scheme .
         workerid != "A355PGLV2ID2SX" #festivals also so exited,That time also so enjoyed my family and friends. that time also so many importance of my family
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








# B.6 Descriptives ############################################################
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







# B.7 Manipulation Check ######################################################

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

# frequencies::freq_two_vects(df_merge_s2, treatment_f, manipulationcheck,
#                             separate_tables = TRUE)


## B: Perceived prosocial impact
df_merge_s2 %>% 
  group_by(treatment) %>% 
  summarise(mean = mean(prosocialimpact),
            sd = sd(prosocialimpact))

aov_prosocialimpact_s2 <- aov(prosocialimpact ~ treatment_f, 
                              data = df_merge_s2)
aov_societalimpact_s2 <- aov(societalimpact ~ treatment_f, 
                             data = df_merge_s2)






# B.8 ANOVA Hypotheses ########################################################

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




# B.8.1 Balance test ==========================================================
chi_female_s2 <- chisq.test(table(df_merge_s2$gender_f, 
                                  df_merge_s2$treatment_f))
chi_manager_s2 <- chisq.test(table(df_merge_s2$manager_dummy, 
                                   df_merge_s2$treatment_f))
aov_age_s2 <- aov(age ~ treatment_f, data = df_merge_s2)






# B.9 Group comparisons #######################################################

# Generate treatment factor with reversed order of treatments
# Ensures that the mean differences have the right sign
df_merge_s2 <- df_merge_s2 %>%
  mutate(treatment_f_reversed = factor(treatment, 
                                       levels = c("4", "3", "2", "1")))


# B.9.1 Generate subsamples ===================================================
df_merge1_s2 <- df_merge_s2 %>% filter(treatment == 1)
df_merge2_s2 <- df_merge_s2 %>% filter(treatment == 2)
df_merge3_s2 <- df_merge_s2 %>% filter(treatment == 3)
df_merge4_s2 <- df_merge_s2 %>% filter(treatment == 4)
df_merge1_3_s2  <- df_merge_s2 %>% filter(treatment == 1 | treatment == 3)
df_merge1_4_s2  <- df_merge_s2 %>% filter(treatment == 1 | treatment == 4)
df_merge2_3_s2  <- df_merge_s2 %>% filter(treatment == 2 | treatment == 3)
df_merge2_4_s2  <- df_merge_s2 %>% filter(treatment == 2 | treatment == 4)


# B.9.2 Group comparison positive affect ======================================

# empty vector to store p-values
peace <- vector(mode = "numeric", length = 4)
pairwise <- tibble(comparison = c("1 vs 3", "1 vs 4", "2 vs 3", "2 vs 4"))
pairwise <- pairwise %>% mutate(t = NA)
pairwise <- pairwise %>% mutate(df = NA)
pairwise <- pairwise %>% mutate(mean.diff = NA)
pairwise <- pairwise %>% mutate(eff_size_d = NA)
pairwise <- pairwise %>% mutate(eff_size_g = NA)
pairwise <- pairwise %>% mutate(p = NA)


# B.9.2.1 1 vs 3 --------------------------------------------------------------
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


# B.9.2.2 1 vs 4 --------------------------------------------------------------
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

# B.9.2.3 2 vs 3 --------------------------------------------------------------
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



# B.9.2.4 2 vs 4 --------------------------------------------------------------
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



# B.9.2.5 Adjusted p-values ---------------------------------------------------
padjust <- p.adjust(peace, method = "BH")

pairwise <- bind_cols(pairwise, as_tibble(padjust))
pairwise <- pairwise %>% rename(p.adjust = value)

pairwise <- pairwise %>% mutate(star = ifelse(p.adjust < 0.05, "*", ""))
#pairwise %>% select(comparison, mean.diff, eff_size_g, p, p.adjust, star)

pairwise_paffect_s2 <- pairwise




# B.9.3 Group comparison negative affect ======================================

# empty vector to store p-values
peace <- vector(mode = "numeric", length = 4)
pairwise <- tibble(comparison = c("1 vs 3", "1 vs 4", "2 vs 3", "2 vs 4"))
pairwise <- pairwise %>% mutate(t = NA)
pairwise <- pairwise %>% mutate(df = NA)
pairwise <- pairwise %>% mutate(mean.diff = NA)
pairwise <- pairwise %>% mutate(eff_size_d = NA)
pairwise <- pairwise %>% mutate(eff_size_g = NA)
pairwise <- pairwise %>% mutate(p = NA)


# B.9.3.1 1 vs 3 --------------------------------------------------------------
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


# B.9.3.2 1 vs 4 --------------------------------------------------------------
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

# B.9.3.3 2 vs 3 --------------------------------------------------------------
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



# B.9.3.4 2 vs 4 --------------------------------------------------------------
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



# B.9.3.5 Adjusted p-values ---------------------------------------------------
padjust <- p.adjust(peace, method = "BH")

pairwise <- bind_cols(pairwise, as_tibble(padjust))
pairwise <- pairwise %>% rename(p.adjust = value)

pairwise <- pairwise %>% mutate(star = ifelse(p.adjust < 0.05, "*", ""))
#pairwise %>% select(comparison, mean.diff, eff_size_g, p, p.adjust, star)
pairwise_naffect_s2 <- pairwise




# B.9.4 Group comparison turnover intention ===================================

# empty vector to store p-values
peace <- vector(mode = "numeric", length = 4)
pairwise <- tibble(comparison = c("1 vs 3", "1 vs 4", "2 vs 3", "2 vs 4"))
pairwise <- pairwise %>% mutate(t = NA)
pairwise <- pairwise %>% mutate(df = NA)
pairwise <- pairwise %>% mutate(mean.diff = NA)
pairwise <- pairwise %>% mutate(eff_size_d = NA)
pairwise <- pairwise %>% mutate(eff_size_g = NA)
pairwise <- pairwise %>% mutate(p = NA)


# B.9.4.1 1 vs 3 --------------------------------------------------------------
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


# B.9.4.2 1 vs 4 --------------------------------------------------------------
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

# B.9.4.3 2 vs 3 --------------------------------------------------------------
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



# B.9.4.4 2 vs 4 --------------------------------------------------------------
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



# B.9.4.5 Adjusted p-values ---------------------------------------------------
padjust <- p.adjust(peace, method = "BH")

pairwise <- bind_cols(pairwise, as_tibble(padjust))
pairwise <- pairwise %>% rename(p.adjust = value)

pairwise <- pairwise %>% mutate(star = ifelse(p.adjust < 0.05, "*", ""))
#pairwise %>% select(comparison, mean.diff, eff_size_g, p, p.adjust, star)
pairwise_turnover_s2 <- pairwise





# B.9.5 Group comparison willigness to recommend job ==========================

# empty vector to store p-values
peace <- vector(mode = "numeric", length = 4)
pairwise <- tibble(comparison = c("1 vs 3", "1 vs 4", "2 vs 3", "2 vs 4"))
pairwise <- pairwise %>% mutate(t = NA)
pairwise <- pairwise %>% mutate(df = NA)
pairwise <- pairwise %>% mutate(mean.diff = NA)
pairwise <- pairwise %>% mutate(eff_size_d = NA)
pairwise <- pairwise %>% mutate(eff_size_g = NA)
pairwise <- pairwise %>% mutate(p = NA)


# B.9.5.1 1 vs 3 --------------------------------------------------------------
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


# B.9.5.2 1 vs 4 --------------------------------------------------------------
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

# B.9.5.3 2 vs 3 --------------------------------------------------------------
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



# B.9.5.4 2 vs 4 --------------------------------------------------------------
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



# B.9.5.5 Adjusted p-values ---------------------------------------------------
padjust <- p.adjust(peace, method = "BH")

pairwise <- bind_cols(pairwise, as_tibble(padjust))
pairwise <- pairwise %>% rename(p.adjust = value)

pairwise <- pairwise %>% mutate(star = ifelse(p.adjust < 0.05, "*", ""))
#pairwise %>% select(comparison, mean.diff, eff_size_g, p, p.adjust, star)
pairwise_will_s2 <- pairwise


# remove unused objects
rm(effsize_d, effsize_g, pairwise, tee, padjust, peace)









# A.10 ANOVA for Descriptives #################################################
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








# A.11 Plot with mean values per group (Meanplot) #############################

# A.11.1 Prepare data (sd, n, se, ci per treatment per variable) ==============
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

# A.11.2 Plot =================================================================

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








# A.12 Table with results of group comparisons ################################
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







# A.13 Regression with beneficiary contact (H4) ###############################
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





# A.14 Regression with exogeneous control variables only ######################

# Model 2: Add exogenous control variables
reg_paffect_control_s2 <- update(reg_paffect_s2, . ~ . + 
                                   female + age + manager)
reg_naffect_control_s2 <- update(reg_naffect_s2, . ~ . + 
                                   female + age + manager)
reg_turnover_control_s2 <- update(reg_turnover_s2, . ~ . + 
                                    female + age + manager)
reg_will_control_s2 <- update(reg_will_s2, . ~ . + 
                                female + age + manager)

stargazer::stargazer(reg_paffect_s2, reg_paffect_control_s2, 
                     reg_naffect_s2, reg_naffect_control_s2,
                     reg_turnover_s2, reg_turnover_control_s2,
                     reg_will_s2, reg_will_control_s2,
                     type = "html",
                     out = here("analysis", "S2_Reg_Controls.html"),
                     title = "Study 2: OLS regression with exogenous control variables",
                     dep.var.labels.include = TRUE,
                     dep.var.caption = "",
                     dep.var.labels = c("Positive affect",
                                        "Negative affect",
                                        "Turnover intent.",
                                        "Will. rec. job"),
                     star.cutoffs = c(.05, .01, .001),
                     covariate.labels = c("Treat. B (Active control)",
                                          "Treat. C (Prosocial impact)",
                                          "Treat. D (Societal impact)",
                                          "Gender (1 = female)",
                                          "Age",
                                          "Leadership position"),
                     keep.stat = c("n", "rsq", "adj.rsq"),
                     notes = "Standard errors in parantheses",
                     notes.append = TRUE,
                     report = "vc*s",
                     font.size = "small",
                     align = TRUE)




# A.15 Correlation table ######################################################
# Prepare data
df_s2_cor <- df_merge_s2 %>%
  select(paffect,
         naffect,
         turnover,
         willingnessjobrec,
         prosocialimpact,
         societalimpact)
df_s2_cor <- as.data.frame(df_s2_cor)

corstars(df_s2_cor, 
         method="pearson", 
         removeTriangle="upper",
         result="html",
         cap = "Correlations matrix", 
         filename = here("analysis", "corr_s2.html"),
         labels_rows = c("(1) Positive Affect", 
                         "(2) Negative Affect", 
                         "(3) Turnover Int.", 
                         "(4) Will. to recommend job", 
                         "(5) Prosocial Impact",
                         "(6) Societal Impact"),
         labels_cols = 1:5)

