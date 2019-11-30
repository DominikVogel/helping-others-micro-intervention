
# THIS IS THE BEGINNING OF STUDY 3 CODE XXXXXXXXXXXXXXXXXXXXXXXXX -------------
# C.2.1 Import pre data =======================================================
# Define variable types to assure consistency
coltypes_pre <- cols(
  V1 = col_character(),
  V2 = col_character(),
  V3 = col_character(),
  V4 = col_character(),
  V5 = col_character(),
  V6 = col_character(),
  V7 = col_integer(),
  V8 = col_datetime(format = ""),
  V9 = col_datetime(format = ""),
  V10 = col_integer(),
  MTurkCode = col_integer(),
  intro = col_integer(),
  employment = col_integer(),
  sector = col_integer(),
  agree_pre_test = col_integer(),
  Q29 = col_integer(),
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
  willingnessjobrec1 = col_integer(),
  willingnessjobrec2 = col_integer(),
  attention1 = col_integer(),
  turnover1 = col_integer(),
  turnover2 = col_integer(),
  turnover3 = col_integer(),
  prosocialimpact1 = col_integer(),
  prosocialimpact2 = col_integer(),
  prosocialimpact3 = col_integer(),
  societalimpact1 = col_integer(),
  societalimpact2 = col_integer(),
  societalimpact3 = col_integer(),
  attention2 = col_integer(),
  beneficiaries_obj = col_integer(),
  beneficiaries1 = col_integer(),
  beneficiaries2 = col_integer(),
  openfinal = col_character(),
  LocationLatitude = col_double(),
  LocationLongitude = col_double(),
  LocationAccuracy = col_integer(),
  AssignmentId = col_character(),
  WorkerId = col_character(),
  HITId = col_character(),
  AssignmentStatus = col_character(),
  AutoApprovalTime = col_datetime(format = ""),
  AcceptTime = col_datetime(format = ""),
  SubmitTime = col_datetime(format = ""),
  ApprovalTime = col_datetime(format = ""),
  RejectionTime = col_datetime(format = ""),
  RequesterFeedback = col_character(),
  ApprovalRejectionTime = col_integer(),
  SecondsOnHIT = col_integer(),
  pre = col_integer()
)

pre1 <- read_csv(here("data/s3-pre", "part1.csv"), col_types = coltypes_pre)
pre2 <- read_csv(here("data/s3-pre", "part2.csv"), col_types = coltypes_pre)
pre3 <- read_csv(here("data/s3-pre", "part3.csv"), col_types = coltypes_pre)
pre4 <- read_csv(here("data/s3-pre", "part4.csv"), col_types = coltypes_pre)
pre5 <- read_csv(here("data/s3-pre", "part5.csv"), col_types = coltypes_pre)
pre6 <- read_csv(here("data/s3-pre", "part6.csv"), col_types = coltypes_pre)
pre7 <- read_csv(here("data/s3-pre", "part7.csv"), col_types = coltypes_pre)
pre8 <- read_csv(here("data/s3-pre", "part8.csv"), col_types = coltypes_pre)
pre9 <- read_csv(here("data/s3-pre", "part9.csv"), col_types = coltypes_pre)






# C.2.1.1 Merge pre data ------------------------------------------------------
pre <- bind_rows(pre1, pre2, pre3, pre4, pre5, pre6, pre7, pre8, pre9)
rm(pre1, pre2, pre3, pre4, pre5, pre6, pre7, pre8, pre9)


# C.2.1.2 Keep participants of pre-test only ----------------------------------
# This excludes those who only participated in the screening
pre <- pre %>% filter(pre == 1)




# C.2.1.4 Drop dublicates -----------------------------------------------------
pre %>% group_by(WorkerId) %>% count(WorkerId) %>% arrange(desc(n))
# No duplicates found
# pre <- distinct(pre, WorkerId, .keep_all = TRUE)


# C.2.1.5 Export pre data -----------------------------------------------------
#write_csv(pre, "../data/s3-pre.csv")







# C.2.2 Import post data ======================================================
# Define variable types to assure consistency
coltypes_post <- cols(
  V1 = col_character(),
  V2 = col_character(),
  V3 = col_character(),
  V4 = col_character(),
  V5 = col_character(),
  V6 = col_character(),
  V7 = col_integer(),
  V8 = col_datetime(format = ""),
  V9 = col_datetime(format = ""),
  V10 = col_integer(),
  MTurkCode = col_integer(),
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
  willingnessjobrec1 = col_integer(),
  willingnessjobrec2 = col_integer(),
  turnover1 = col_integer(),
  turnover2 = col_integer(),
  turnover3 = col_integer(),
  manipulationcheck = col_integer(),
  prosocialimpact1 = col_integer(),
  prosocialimpact2 = col_integer(),
  prosocialimpact3 = col_integer(),
  attention1 = col_integer(),
  societalimpact1 = col_integer(),
  societalimpact2 = col_integer(),
  societalimpact3 = col_integer(),
  taskvariety1 = col_integer(),
  taskvariety2 = col_integer(),
  taskvariety3 = col_integer(),
  taskvariety4 = col_integer(),
  taskvariety5 = col_integer(),
  taskanalyz_1 = col_integer(),
  taskanalyz_2 = col_integer(),
  taskanalyz_3 = col_integer(),
  taskanalyz_4 = col_integer(),
  attention2 = col_integer(),
  autonomy1 = col_integer(),
  autonomy2 = col_integer(),
  autonomy3 = col_integer(),
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
  DO_Q_taskvariety = col_character(),
  DO_Q_taskanalyz = col_character(),
  DO_Q_autonomy = col_character(),
  DO_Q_affect = col_character(),
  DO_Q_turnover = col_character(),
  DO_Q_willingnessjobrec = col_character(),
  LocationLatitude = col_double(),
  LocationLongitude = col_double(),
  LocationAccuracy = col_integer(),
  treatment = col_integer(),
  AssignmentId = col_character(),
  WorkerId = col_character(),
  HITId = col_character(),
  AssignmentStatus = col_character(),
  AutoApprovalTime = col_datetime(format = ""),
  AcceptTime = col_datetime(format = ""),
  SubmitTime = col_datetime(format = ""),
  ApprovalTime = col_datetime(format = ""),
  RejectionTime = col_datetime(format = ""),
  RequesterFeedback = col_character(),
  ApprovalRejectionTime = col_integer(),
  SecondsOnHIT = col_integer(),
  attention_passed = col_integer()
)

post1 <- read_csv(here("data/s3-post", "part1.csv"), col_types = coltypes_post)
post2_1 <- read_csv(here("data/s3-post", "part2-1.csv"), col_types = coltypes_post)
post2_2 <- read_csv(here("data/s3-post", "part2-2.csv"), col_types = coltypes_post)
post3 <- read_csv(here("data/s3-post", "part3.csv"), col_types = coltypes_post)
post4 <- read_csv(here("data/s3-post", "part4.csv"), col_types = coltypes_post)
post5 <- read_csv(here("data/s3-post", "part5.csv"), col_types = coltypes_post)
post6 <- read_csv(here("data/s3-post", "part6.csv"), col_types = coltypes_post)
post7 <- read_csv(here("data/s3-post", "part7.csv"), col_types = coltypes_post)
post8 <- read_csv(here("data/s3-post", "part8.csv"), col_types = coltypes_post)









# C.2.2.1 Merge post data -----------------------------------------------------
post <- bind_rows(post1, post2_1, post2_2, post3, post4, post5, 
                  post6, post7, post8)
rm(post1, post2_1, post2_2, post3, post4, post5, post6, post7, post8)

# C.2.2.2 Fix yearbirth error -------------------------------------------------
post <- post %>%
  mutate(yearbirth = 2012 - yearbirth)







# C.2.2.5 Drop dublicates -----------------------------------------------------
post %>% group_by(WorkerId) %>% count(WorkerId) %>% arrange(desc(n))
# No dublicate found. First entry would have been used
#post <- distinct(post, WorkerId, .keep_all = TRUE)

# C.2.2.6 Check responses to reflection task ----------------------------------
# tibble_print_all(post %>% select(WorkerId, reflectiongeneral))
# tibble_print_all(post %>% select(WorkerId, reflectionprosocial))
# tibble_print_all(post %>% select(WorkerId, reflectionsocietal))






# C.2.2.8 Export post data-----------------------------------------------------
# write_csv(post, "../data/s3-post.csv")







# C.2.3 Merge pre and post ====================================================
df_merge_s3 <- inner_join(post, pre, 
                          by = "WorkerId", 
                          suffix = c("_post", "_pre"))
rm(post, pre,
   coltypes_pre, coltypes_post)








# C.2.4 Clean data ============================================================
# C.2.4.1 Store initial n -----------------------------------------------------
n_total <- tibble(treatment = "total", initial = nrow(df_merge_s3))
obs_s3 <- df_merge_s3 %>% 
  group_by(treatment) %>% 
  summarise(initial = n()) %>%
  mutate(treatment = as.character(treatment))
obs_s3 <- bind_rows(n_total, obs_s3)
rm(n_total)


# C.2.4.2 Remove obs how did not receive treatment-----------------------------
df_merge_s3 <- df_merge_s3 %>%
  mutate(treatment_duration = timeaftertreatment - timebeforetreatment)
df_merge_s3 <- df_merge_s3 %>% 
  mutate(no_treat = ifelse(treatment > 1 & is.na(treatment_duration), 1, 0),
         no_treat = ifelse(treatment > 1 & treatment_duration == 0, 1, no_treat),
         no_treat = ifelse(treatment == 1, NA, no_treat)) %>%
  filter(no_treat != 1 | is.na(no_treat)) %>%
  select(-no_treat)


# Store n
n_total <- tibble(treatment = "total", receive_treat = nrow(df_merge_s3))
obs_receive <- df_merge_s3 %>% 
  group_by(treatment) %>% 
  summarise(receive_treat = n()) %>%
  mutate(treatment = as.character(treatment))
obs_receive <- bind_rows(n_total, obs_receive)
obs_s3 <- left_join(obs_s3, obs_receive)
rm(obs_receive)






# C.2.4.3 Remove obs how did not answer dep var -------------------------------
df_merge_s3 <- df_merge_s3 %>% 
  mutate(mi_dep = ifelse(is.na(paffect1_post) | is.na(paffect1_pre) |
                           is.na(naffect1_post) | is.na(naffect1_pre) |
                           is.na(turnover1_post) | is.na(turnover1_pre) |
                           is.na(willingnessjobrec1_post) |
                           is.na(willingnessjobrec1_pre),
                         1, 0)) %>%
  filter(mi_dep != 1) %>%
  select(-mi_dep)


# C.2.4.4 Remove participants who do not work in the public sector ------------
table(df_merge_s3$sector_post)
# Sector composition: 418 public, 19 nonprofit, 39 private, 7 NA
df_merge_s3 <- df_merge_s3 %>%
  filter(sector_post == 1)

# Store n
n_total <- tibble(treatment = "total", public = nrow(df_merge_s3))
obs_public <- df_merge_s3 %>% 
  group_by(treatment) %>% 
  summarise(public = n()) %>%
  mutate(treatment = as.character(treatment))
obs_public <- bind_rows(n_total, obs_public)
obs_s3 <- left_join(obs_s3, obs_public)
rm(obs_public)

# C.2.4.5 Remove obs with failed attention check ------------------------------
df_merge_s3 <- df_merge_s3 %>% 
  filter(attention1_pre == 5, 
         attention2_pre == 4,
         attention1_post == 4, 
         attention2_post == 3)

table(df_merge_s3$attention2_post)


# Store n
n_total <- tibble(treatment = "total", attention = nrow(df_merge_s3))
obs_attention <- df_merge_s3 %>% 
  group_by(treatment) %>% 
  summarise(attention = n()) %>%
  mutate(treatment = as.character(treatment))
obs_attention <- bind_rows(n_total, obs_attention)
obs_s3 <- left_join(obs_s3, obs_attention)
rm(obs_attention)


# C.2.4.6 Remove obs who tried to cheat ---------------------------------------
# Drop obs because they did not reflect or answer indicate private sector -----
df_merge_s3 <- df_merge_s3 %>% 
  filter(WorkerId != "A2QWVKYC4RSJKM",
         WorkerId != "A2FIBJ5O89L3XB",
         WorkerId != "AAE1COF4E8740",
         WorkerId != "AN4D1WRTKLUYZ",
         WorkerId != "A1ZCZK4LR2U5AW",
         WorkerId != "A2174DULPU1NR6",
         WorkerId != "A2LI51CANHJ00M",
         WorkerId != "A3329CH71VMUBQ",
         WorkerId != "A2WKN97R9EXE6B",
         WorkerId != "A2RHJT0OMA09YH")

# Store n
n_total <- tibble(treatment = "total", final = nrow(df_merge_s3))
obs_final <- df_merge_s3 %>% 
  group_by(treatment) %>% 
  summarise(final = n()) %>%
  mutate(treatment = as.character(treatment))
obs_final <- bind_rows(n_total, obs_final)
obs_s3 <- left_join(obs_s3, obs_final)
rm(obs_final, n_total)

obs_s3










# C.3 rename variables ########################################################
df_merge_s3 <- df_merge_s3 %>% rename(taskanalyz1 = taskanalyz_1,
                                      taskanalyz2 = taskanalyz_2,
                                      taskanalyz3 = taskanalyz_3,
                                      taskanalyz4 = taskanalyz_4)



# C.4 Generate and modify variables ###########################################

# C.4.1 Generate factor variables =============================================
df_merge_s3$treatment_f <- factor(df_merge_s3$treatment)
df_merge_s3$gender_f <- factor(df_merge_s3$gender, 
                               labels = c("Male", "Female", "Other"))

# C.4.2 Set NA categories =====================================================
df_merge_s3 <- df_merge_s3 %>% 
  mutate(atwork = ifelse(atwork == 2, NA, atwork))




# C.4.3 Reverse items =========================================================
df_merge_s3 <- df_merge_s3 %>% mutate(turnover1_pre = 8 - turnover1_pre)
df_merge_s3 <- df_merge_s3 %>% mutate(turnover1_post = 8 - turnover1_post)
df_merge_s3 <- df_merge_s3 %>% 
  mutate(willingnessjobrec2_pre = 6 - willingnessjobrec2_pre)
df_merge_s3 <- df_merge_s3 %>% 
  mutate(willingnessjobrec2_post = 6 - willingnessjobrec2_post)
df_merge_s3 <- df_merge_s3 %>% mutate(taskanalyz1 = 6 - taskanalyz1)
df_merge_s3 <- df_merge_s3 %>% mutate(taskanalyz2 = 6 - taskanalyz2)
df_merge_s3 <- df_merge_s3 %>% mutate(taskanalyz3 = 6 - taskanalyz3)
df_merge_s3 <- df_merge_s3 %>% mutate(taskanalyz4 = 6 - taskanalyz4)
df_merge_s3 <- df_merge_s3 %>% mutate(taskvariety1 = 6 - taskvariety1)
df_merge_s3 <- df_merge_s3 %>% mutate(taskvariety2 = 6 - taskvariety2)
df_merge_s3 <- df_merge_s3 %>% mutate(taskvariety3 = 6 - taskvariety3)
df_merge_s3 <- df_merge_s3 %>% mutate(taskvariety4 = 6 - taskvariety4)
df_merge_s3 <- df_merge_s3 %>% mutate(taskvariety5 = 6 - taskvariety5)

# C.4.4 Create Dummies ========================================================
df_merge_s3 <- df_merge_s3 %>% 
  mutate(manager_dummy = ifelse(manager == 0, 0, 1),
         manager_dummy = ifelse(is.na(manager), NA, 
                                manager_dummy))
df_merge_s3 <- df_merge_s3 %>% 
  mutate(male = ifelse(gender == 1, 1, 0),
         male = ifelse(is.na(gender), NA, male),
         female = ifelse(gender == 2, 1, 0),
         female = ifelse(is.na(gender), NA, female),
         gender_other = ifelse(gender == 3, 1, 0),
         gender_other = ifelse(is.na(gender), 
                               NA, gender_other))

df_merge_s3 <- df_merge_s3 %>% 
  mutate(treatment_dummy1 = ifelse(treatment == 1, 1, 0),
         treatment_dummy2 = ifelse(treatment == 2, 1, 0),
         treatment_dummy3 = ifelse(treatment == 3, 1, 0),
         treatment_dummy4 = ifelse(treatment == 4, 1, 0))



# C.4.5 Count words in reflection task ========================================
df_merge_s3 <- df_merge_s3 %>% mutate(word_count = NA)
df_merge_s3 <- df_merge_s3 %>% 
  mutate(word_count = 
           ifelse(treatment == 2, 
                  sapply(gregexpr("[[:alpha:]]+", 
                                  df_merge_s3$reflectiongeneral),
                         function(x) sum(x > 0)), 
                  word_count))
df_merge_s3 <- df_merge_s3 %>% 
  mutate(word_count = ifelse(treatment == 3, 
                             sapply(gregexpr("[[:alpha:]]+", 
                                             df_merge_s3$reflectionprosocial), 
                                    function(x) sum(x > 0)),
                             word_count)) 
df_merge_s3 <- df_merge_s3 %>% 
  mutate(word_count = ifelse(treatment == 4, 
                             sapply(gregexpr("[[:alpha:]]+", 
                                             df_merge_s3$reflectionsocietal), 
                                    function(x) sum(x > 0)), 
                             word_count))

df_merge_s3 <- df_merge_s3 %>% 
  mutate(word_count = ifelse(treatment != 1 & is.na(word_count),
                             0, word_count))





# C.4.6 Treatment duration ====================================================

df_merge_s3 <- df_merge_s3 %>% 
  mutate(treatment_duration = timeaftertreatment - timebeforetreatment)



# C.4.7 Create dependent variables ============================================

# C.4.7.1 Positive Affect -----------------------------------------------------
crona_paffect_pre_s3 <- psych::alpha(select(df_merge_s3, paffect1_pre, 
                                            paffect2_pre, paffect3_pre, 
                                            paffect4_pre, paffect5_pre, 
                                            paffect6_pre))
crona_paffect_pre_s3 <- crona_paffect_pre_s3[["total"]][["raw_alpha"]]
df_merge_s3 <- mean_index(df_merge_s3, "paffect_pre",
                          c("paffect1_pre", "paffect2_pre", "paffect3_pre", 
                            "paffect4_pre", "paffect5_pre", "paffect6_pre"))

crona_paffect_post_s3 <- psych::alpha(select(df_merge_s3, paffect1_post, 
                                             paffect2_post, paffect3_post, 
                                             paffect4_post, paffect5_post, 
                                             paffect6_post))
crona_paffect_post_s3 <- crona_paffect_post_s3[["total"]][["raw_alpha"]]
df_merge_s3 <- mean_index(df_merge_s3, "paffect_post",
                          c("paffect1_post", "paffect2_post", "paffect3_post", 
                            "paffect4_post", "paffect5_post", "paffect6_post"))


# C.4.7.2 Negative Affect -----------------------------------------------------
crona_naffect_pre_s3 <- psych::alpha(select(df_merge_s3, naffect1_pre, 
                                            naffect2_pre, naffect3_pre,
                                            naffect4_pre, naffect5_pre, 
                                            naffect6_pre))
crona_naffect_pre_s3 <- crona_naffect_pre_s3[["total"]][["raw_alpha"]]
df_merge_s3 <- mean_index(df_merge_s3, "naffect_pre",
                          c("naffect1_pre", "naffect2_pre", "naffect3_pre", 
                            "naffect4_pre", "naffect5_pre", "naffect6_pre"))

crona_naffect_post_s3 <- psych::alpha(select(df_merge_s3, naffect1_post, 
                                             naffect2_post, naffect3_post,
                                             naffect4_post, naffect5_post, 
                                             naffect6_post))
crona_naffect_post_s3 <- crona_naffect_post_s3[["total"]][["raw_alpha"]]
df_merge_s3 <- mean_index(df_merge_s3, "naffect_post",
                          c("naffect1_post", "naffect2_post", "naffect3_post", 
                            "naffect4_post", "naffect5_post", "naffect6_post"))


# C.4.7.3 Turnover intention --------------------------------------------------
crona_turnover_pre_s3 <- psych::alpha(select(df_merge_s3, turnover1_pre, 
                                             turnover2_pre, turnover3_pre))
crona_turnover_pre_s3 <- crona_turnover_pre_s3[["total"]][["raw_alpha"]]
df_merge_s3 <- mean_index(df_merge_s3, "turnover_pre",
                          c("turnover1_pre", "turnover2_pre", 
                            "turnover3_pre"))

crona_turnover_post_s3 <- psych::alpha(select(df_merge_s3, turnover1_post, 
                                              turnover2_post, turnover3_post))
crona_turnover_post_s3 <- crona_turnover_post_s3[["total"]][["raw_alpha"]]
df_merge_s3 <- mean_index(df_merge_s3, "turnover_post",
                          c("turnover1_post", "turnover2_post", 
                            "turnover3_post"))

# C.4.7.4 Willigness to recommend job -----------------------------------------
crona_will_pre_s3 <- psych::alpha(select(df_merge_s3, 
                                         willingnessjobrec1_pre, 
                                         willingnessjobrec2_pre))
crona_will_pre_s3 <- crona_will_pre_s3[["total"]][["raw_alpha"]]
df_merge_s3 <- mean_index(df_merge_s3, 
                          "willingnessjobrec_pre", 
                          c("willingnessjobrec1_pre", 
                            "willingnessjobrec2_pre"))

crona_will_post_s3 <- psych::alpha(select(df_merge_s3, 
                                          willingnessjobrec1_post, 
                                          willingnessjobrec2_post))
crona_will_post_s3 <- crona_will_post_s3[["total"]][["raw_alpha"]]
df_merge_s3 <- mean_index(df_merge_s3, 
                          "willingnessjobrec_post", 
                          c("willingnessjobrec1_post", 
                            "willingnessjobrec2_post"))


# C.4.5 Create moderator / control variables ==================================

## Contact with beneficiaries
crona_bene_s3 <- psych::alpha(select(df_merge_s3, beneficiaries1, beneficiaries2))
crona_bene_s3 <- crona_bene_s3[["total"]][["raw_alpha"]]
df_merge_s3 <- mean_index(df_merge_s3, "beneficiaries",
                          c("beneficiaries1", "beneficiaries2"))

## Perceived prosocial impact
crona_psi_pre_s3 <- psych::alpha(select(df_merge_s3, 
                                        prosocialimpact1_pre, 
                                        prosocialimpact2_pre, 
                                        prosocialimpact3_pre))
crona_psi_pre_s3 <- crona_psi_pre_s3[["total"]][["raw_alpha"]]
df_merge_s3 <- mean_index(df_merge_s3, "prosocialimpact_pre",
                          c("prosocialimpact1_pre", 
                            "prosocialimpact2_pre",
                            "prosocialimpact3_pre"))

crona_psi_post_s3 <- psych::alpha(select(df_merge_s3, 
                                         prosocialimpact1_post, 
                                         prosocialimpact2_post, 
                                         prosocialimpact3_post))
crona_psi_post_s3 <- crona_psi_post_s3[["total"]][["raw_alpha"]]
df_merge_s3 <- mean_index(df_merge_s3, "prosocialimpact_post",
                          c("prosocialimpact1_post", 
                            "prosocialimpact2_post",
                            "prosocialimpact3_post"))

## Perceived societal impact
crona_si_pre_s3 <- psych::alpha(select(df_merge_s3, 
                                       societalimpact1_pre, 
                                       societalimpact2_pre, 
                                       societalimpact3_pre))
crona_si_pre_s3 <- crona_si_pre_s3[["total"]][["raw_alpha"]]
df_merge_s3 <- mean_index(df_merge_s3, "societalimpact_pre",
                          c("societalimpact1_pre", 
                            "societalimpact2_pre",
                            "societalimpact3_pre"))

crona_si_post_s3 <- psych::alpha(select(df_merge_s3, 
                                        societalimpact1_post, 
                                        societalimpact2_post, 
                                        societalimpact3_post))
crona_si_post_s3 <- crona_si_post_s3[["total"]][["raw_alpha"]]
df_merge_s3 <- mean_index(df_merge_s3, "societalimpact_post",
                          c("societalimpact1_post", 
                            "societalimpact2_post",
                            "societalimpact3_post"))


## Task variety
crona_taskvar_s3 <- psych::alpha(select(df_merge_s3, taskvariety1, taskvariety2,
                                        taskvariety3, taskvariety4, taskvariety5))
crona_taskvar_s3 <- crona_taskvar_s3[["total"]][["raw_alpha"]]
df_merge_s3 <- mean_index(df_merge_s3, "taskvariety",
                          c("taskvariety1", "taskvariety2",
                            "taskvariety3", "taskvariety4", 
                            "taskvariety5"))

## Task analyzability
crona_taskanalyz_s3 <- psych::alpha(select(df_merge_s3, taskanalyz1, 
                                           taskanalyz2, taskanalyz3, 
                                           taskanalyz4))
crona_taskanalyz_s3 <- crona_taskanalyz_s3[["total"]][["raw_alpha"]]
df_merge_s3 <- mean_index(df_merge_s3, "taskanalyz",
                          c("taskanalyz1", "taskanalyz2", 
                            "taskanalyz3", "taskanalyz4"))

## Job autonomy
crona_auto_s3 <- psych::alpha(select(df_merge_s3, autonomy1, autonomy2, autonomy3))
crona_auto_s3 <- crona_auto_s3[["total"]][["raw_alpha"]]
df_merge_s3 <- mean_index(df_merge_s3, "autonomy",
                          c("autonomy1", "autonomy2", "autonomy3"))

## Age
df_merge_s3 <- df_merge_s3 %>%
  mutate(age = 2018 - yearbirth)









# C.5 Descriptives ############################################################

desc_s3 <- df_merge_s3 %>% 
  group_by(treatment) %>% 
  # Means and SDs per group
  summarise(paffect_m = func_round_mean(paffect_post),
            paffect_sd = func_round_sd(paffect_post),
            naffect_m = func_round_mean(naffect_post),
            naffect_sd = func_round_sd(naffect_post),
            turnover_m = func_round_mean(turnover_post),
            turnover_sd = func_round_sd(turnover_post),
            will_m = func_round_mean(willingnessjobrec_post),
            will_sd = func_round_sd(willingnessjobrec_post),
            psi_m = func_round_mean(prosocialimpact_post),
            psi_sd = func_round_sd(prosocialimpact_post),
            si_m = func_round_mean(societalimpact_post),
            si_sd = func_round_sd(societalimpact_post),
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


desc_s3










# C.6 Manipulation Check ######################################################

# C.6.1 A: What exactly did we ask you to reflect upon? =======================
df_merge_s3$manipulationcheck <- 
  factor(df_merge_s3$manipulationcheck,
         levels = c(1, 2, 3, 4),
         labels = c("Your job", 
                    "Your job & how you help others", 
                    "Your job & how it helps society",
                    "Don't remember")) 

# Cross-Table
manipulationcheck1_s3 <- df_merge_s3 %>% 
  filter(treatment != 1)
manipulationcheck1_s3 <- table(manipulationcheck1_s3$manipulationcheck, 
                               manipulationcheck1_s3$treatment)
manipulationcheck1_s3

# frequencies::freq_two_vects(df_merge_s3, treatment_f, manipulationcheck,
#                             separate_tables = TRUE)


# C.6.2 B: Perceived prosocial impact =========================================
df_merge_s3 %>% 
  group_by(treatment) %>% 
  summarise(mean = mean(prosocialimpact_post),
            sd = sd(prosocialimpact_post))

anova_prosocialimpact_s3 <- aov(prosocialimpact_post ~ 
                                  treatment_f, 
                                data = df_merge_s3)
summary(anova_prosocialimpact_s3)


# C.6.3 C: Perceived societal impact ==========================================
df_merge_s3 %>% 
  group_by(treatment) %>% 
  summarise(mean = mean(societalimpact_post),
            sd = sd(societalimpact_post))

anova_societalimpact_s3 <- aov(societalimpact_post ~ 
                                 treatment_f, 
                               data = df_merge_s3)
summary(anova_societalimpact_s3)













# C.7 ANOVA Hypotheses (Post-test only) #######################################
# Those analyses are not adviced in general as they ignore the pre-test data
# Tests are made to compare the results to S1 and S2

# C.7.1 Positive affect =======================================================
df_merge_s3 %>% 
  group_by(treatment) %>% 
  summarise(mean = mean(paffect_post),
            sd = sd(paffect_post))
aov_paffect_post_s3 <- aov(df_merge_s3$paffect_post ~ 
                             factor(df_merge_s3$treatment))
summary(aov_paffect_post_s3)

### Effect size Cohen's F: 
effsize_paffect_aov_post_s3 <- sjstats::cohens_f(aov_paffect_post_s3)




# C.7.2 Negative Affect =======================================================
df_merge_s3 %>% 
  group_by(treatment) %>% 
  summarise(mean = mean(naffect_post),
            sd = sd(naffect_post))
aov_naffect_post_s3 <- aov(df_merge_s3$naffect_post ~
                             factor(df_merge_s3$treatment))
summary(aov_naffect_post_s3)

### Effect size Cohen's F: 
effsize_naffect_aov_post_s3 <- sjstats::cohens_f(aov_naffect_post_s3)





# C.7.3 Turnover intention ====================================================
df_merge_s3 %>% 
  group_by(treatment) %>% 
  summarise(mean = mean(turnover_post),
            sd = sd(turnover_post))
aov_turnover_post_s3 <- aov(df_merge_s3$turnover_post ~ 
                              factor(df_merge_s3$treatment))
summary(aov_turnover_post_s3)

### Effect size Cohen's F: 
effsize_turnover_aov_post_s3 <- sjstats::cohens_f(aov_turnover_post_s3)





# C.7.4 Willigness to recommend job ===========================================
df_merge_s3 %>% 
  group_by(treatment) %>% 
  summarise(mean = mean(willingnessjobrec_post),
            sd = sd(willingnessjobrec_post))
aov_will_post_s3 <- aov(df_merge_s3$willingnessjobrec_post ~ 
                          factor(df_merge_s3$treatment))
summary(aov_will_post_s3)

### Effect size Cohen's F: 
effsize_will_aov_post_s3 <- sjstats::cohens_f(aov_will_post_s3)




# C.7.5 Prosocial impact ======================================================
aov_prosocialimpact_post_s3 <- aov(prosocialimpact_post ~ treatment_f, 
                                   data = df_merge_s3)


# C.7.6 Societal impact =======================================================
aov_societalimpact_post_s3 <- aov(societalimpact_post ~ treatment_f, 
                                  data = df_merge_s3)



# C.7.7 Balance test ==========================================================
chi_female_s3 <- chisq.test(table(df_merge_s3$gender_f, 
                                  df_merge_s3$treatment_f))
chi_manager_s3 <- chisq.test(table(df_merge_s3$manager_dummy, 
                                   df_merge_s3$treatment_f))
aov_age_s3 <- aov(age ~ treatment_f, data = df_merge_s3)

reg_balance_paffect1 <- lm(paffect_post ~ treatment_f, data = df_merge_s3)
reg_balance_paffect2 <- lm(paffect_post ~ treatment_f + manager_dummy, 
                           data = df_merge_s3)
reg_balance_naffect1 <- lm(naffect_post ~ treatment_f, data = df_merge_s3)
reg_balance_naffect2 <- lm(naffect_post ~ treatment_f + manager_dummy, 
                           data = df_merge_s3)
reg_balance_turn1 <- lm(turnover_post ~ treatment_f, data = df_merge_s3)
reg_balance_turn2 <- lm(turnover_post ~ treatment_f + manager_dummy, 
                        data = df_merge_s3)
reg_balance_will1 <- lm(willingnessjobrec_post ~ treatment_f, 
                        data = df_merge_s3)
reg_balance_will2 <- lm(willingnessjobrec_post ~ treatment_f + manager_dummy, 
                        data = df_merge_s3)

stargazer::stargazer(reg_balance_paffect1, reg_balance_paffect2,
                     reg_balance_naffect1, reg_balance_naffect2,
                     type = "text")
stargazer::stargazer(reg_balance_turn1, reg_balance_turn2,
                     reg_balance_will1, reg_balance_will2,
                     type = "text")








# C.8 Group comparisons (post-test only) ######################################

# Generate treatment factor with reversed oreder of treatments
# Ensures that the mean differences have the right sign
df_merge_s3 <- df_merge_s3 %>%
  mutate(treatment_f_reversed = factor(treatment, 
                                       levels = c("4", "3", "2", "1")))


# Generate subsamples
df_merge1_s3 <- df_merge_s3 %>% filter(treatment == 1)
df_merge2_s3 <- df_merge_s3 %>% filter(treatment == 2)
df_merge3_s3 <- df_merge_s3 %>% filter(treatment == 3)
df_merge4_s3 <- df_merge_s3 %>% filter(treatment == 4)
df_merge1_3_s3  <- df_merge_s3 %>% filter(treatment == 1 | treatment == 3)
df_merge1_4_s3  <- df_merge_s3 %>% filter(treatment == 1 | treatment == 4)
df_merge2_3_s3  <- df_merge_s3 %>% filter(treatment == 2 | treatment == 3)
df_merge2_4_s3  <- df_merge_s3 %>% filter(treatment == 2 | treatment == 4)


# C.8.1 Group comparison positive affect ======================================
# empty vector to store p-values
peace <- vector(mode = "numeric", length = 4)

# Clear the results table
pairwise <- func_empty_resultstable_postonly()

# C.8.1.1 1 vs 3 --------------------------------------------------------------
tee <- func_compare_ttest_postonly(df_merge1_3_s3, 
                                   df_merge1_3_s3$paffect_post, 
                                   df_merge1_3_s3$treatment_f_reversed,
                                   side = "greater")

effsize_g <- func_compare_effsize_postonly(df_merge1_3_s3$paffect_post, 
                                           df_merge1_3_s3$treatment, 
                                           hedge = TRUE, 
                                           paired = FALSE)
effsize_d <- MBESS::ci.smd(ncp = tee[["statistic"]][["t"]], 
                           n.1=obs_s3$final[3], 
                           n.2=obs_s3$final[5], 
                           conf.level=0.95)$smd

pairwise <- func_compare_writetable_postonly(pairwise, 
                                             tee, 
                                             effsize_g, 
                                             effsize_d, 
                                             group = "1 vs 3")

# Store p-value separatly (for adjustment)
peace[1] <- tee[["p.value"]]


# C.8.1.2 1 vs 4 --------------------------------------------------------------
tee <- func_compare_ttest_postonly(df_merge1_4_s3, 
                                   df_merge1_4_s3$paffect_post, 
                                   df_merge1_4_s3$treatment_f_reversed,
                                   side = "greater")

effsize_g <- func_compare_effsize_postonly(df_merge1_4_s3$paffect_post, 
                                           df_merge1_4_s3$treatment, 
                                           hedge = TRUE, 
                                           paired = FALSE)
effsize_d <- MBESS::ci.smd(ncp = tee[["statistic"]][["t"]], 
                           n.1=obs_s3$final[3], 
                           n.2=obs_s3$final[5], 
                           conf.level=0.95)$smd

pairwise <- func_compare_writetable_postonly(pairwise, 
                                             tee, 
                                             effsize_g, 
                                             effsize_d, 
                                             group = "1 vs 4")

# Store p-value separatly (for adjustment)
peace[2] <- tee[["p.value"]]


# C.8.1.3 2 vs 3 --------------------------------------------------------------
tee <- func_compare_ttest_postonly(df_merge2_3_s3, 
                                   df_merge2_3_s3$paffect_post, 
                                   df_merge2_3_s3$treatment_f_reversed,
                                   side = "greater")

effsize_g <- func_compare_effsize_postonly(df_merge2_3_s3$paffect_post, 
                                           df_merge2_3_s3$treatment, 
                                           hedge = TRUE, 
                                           paired = FALSE)
effsize_d <- MBESS::ci.smd(ncp = tee[["statistic"]][["t"]], 
                           n.1=obs_s3$final[3], 
                           n.2=obs_s3$final[5], 
                           conf.level=0.95)$smd

pairwise <- func_compare_writetable_postonly(pairwise, 
                                             tee, 
                                             effsize_g, 
                                             effsize_d, 
                                             group = "2 vs 3")

# Store p-value separatly (for adjustment)
peace[3] <- tee[["p.value"]]

# C.8.1.4 2 vs 4 --------------------------------------------------------------
tee <- func_compare_ttest_postonly(df_merge2_4_s3, 
                                   df_merge2_4_s3$paffect_post, 
                                   df_merge2_4_s3$treatment_f_reversed,
                                   side = "greater")

effsize_g <- func_compare_effsize_postonly(df_merge2_4_s3$paffect_post, 
                                           df_merge2_4_s3$treatment, 
                                           hedge = TRUE, 
                                           paired = FALSE)
effsize_d <- MBESS::ci.smd(ncp = tee[["statistic"]][["t"]], 
                           n.1=obs_s3$final[3], 
                           n.2=obs_s3$final[5], 
                           conf.level=0.95)$smd

pairwise <- func_compare_writetable_postonly(pairwise, 
                                             tee, 
                                             effsize_g, 
                                             effsize_d, 
                                             group = "2 vs 4")

# Store p-value separatly (for adjustment)
peace[4] <- tee[["p.value"]]


# C.8.1.5 Adjusted p-values ---------------------------------------------------
padjust <- p.adjust(peace, method = "BH")

pairwise <- bind_cols(pairwise, as_tibble(padjust))
pairwise <- pairwise %>% rename(p.adjust = value)

pairwise <- pairwise %>% mutate(star = ifelse(p.adjust < 0.05, "*", ""))
#pairwise %>% select(comparison, mean.diff, eff_size_g, p, p.adjust, star)

pairwise_paffect_post_s3 <- pairwise















## C.8.2 Group comparison negative affect =====================================
# empty vector to store p-values
peace <- vector(mode = "numeric", length = 4)

# Clear the results table
pairwise <- func_empty_resultstable_postonly()

# C.8.2.1 1 vs 3 --------------------------------------------------------------
tee <- func_compare_ttest_postonly(df_merge1_3_s3, 
                                   df_merge1_3_s3$naffect_post, 
                                   df_merge1_3_s3$treatment_f_reversed,
                                   side = "less")

effsize_g <- func_compare_effsize_postonly(df_merge1_3_s3$naffect_post, 
                                           df_merge1_3_s3$treatment, 
                                           hedge = TRUE, 
                                           paired = FALSE)
effsize_d <- MBESS::ci.smd(ncp = tee[["statistic"]][["t"]], 
                           n.1=obs_s3$final[3], 
                           n.2=obs_s3$final[5], 
                           conf.level=0.95)$smd

pairwise <- func_compare_writetable_postonly(pairwise, 
                                             tee, 
                                             effsize_g, 
                                             effsize_d, 
                                             group = "1 vs 3")

# Store p-value separatly (for adjustment)
peace[1] <- tee[["p.value"]]


# C.8.2.2 1 vs 4 --------------------------------------------------------------
tee <- func_compare_ttest_postonly(df_merge1_4_s3, 
                                   df_merge1_4_s3$naffect_post, 
                                   df_merge1_4_s3$treatment_f_reversed,
                                   side = "less")

effsize_g <- func_compare_effsize_postonly(df_merge1_4_s3$naffect_post, 
                                           df_merge1_4_s3$treatment, 
                                           hedge = TRUE, 
                                           paired = FALSE)
effsize_d <- MBESS::ci.smd(ncp = tee[["statistic"]][["t"]], 
                           n.1=obs_s3$final[3], 
                           n.2=obs_s3$final[5], 
                           conf.level=0.95)$smd

pairwise <- func_compare_writetable_postonly(pairwise, 
                                             tee, 
                                             effsize_g, 
                                             effsize_d, 
                                             group = "1 vs 4")

# Store p-value separatly (for adjustment)
peace[2] <- tee[["p.value"]]


# C.8.2.3 2 vs 3 --------------------------------------------------------------
tee <- func_compare_ttest_postonly(df_merge2_3_s3, 
                                   df_merge2_3_s3$naffect_post, 
                                   df_merge2_3_s3$treatment_f_reversed,
                                   side = "less")

effsize_g <- func_compare_effsize_postonly(df_merge2_3_s3$naffect_post, 
                                           df_merge2_3_s3$treatment, 
                                           hedge = TRUE, 
                                           paired = FALSE)
effsize_d <- MBESS::ci.smd(ncp = tee[["statistic"]][["t"]], 
                           n.1=obs_s3$final[3], 
                           n.2=obs_s3$final[5], 
                           conf.level=0.95)$smd

pairwise <- func_compare_writetable_postonly(pairwise, 
                                             tee, 
                                             effsize_g, 
                                             effsize_d, 
                                             group = "2 vs 3")

# Store p-value separatly (for adjustment)
peace[3] <- tee[["p.value"]]

# C.8.2.4 2 vs 4 --------------------------------------------------------------
tee <- func_compare_ttest_postonly(df_merge2_4_s3, 
                                   df_merge2_4_s3$naffect_post, 
                                   df_merge2_4_s3$treatment_f_reversed,
                                   side = "less")

effsize_g <- func_compare_effsize_postonly(df_merge2_4_s3$naffect_post, 
                                           df_merge2_4_s3$treatment, 
                                           hedge = TRUE, 
                                           paired = FALSE)
effsize_d <- MBESS::ci.smd(ncp = tee[["statistic"]][["t"]], 
                           n.1=obs_s3$final[3], 
                           n.2=obs_s3$final[5], 
                           conf.level=0.95)$smd

pairwise <- func_compare_writetable_postonly(pairwise, 
                                             tee, 
                                             effsize_g, 
                                             effsize_d, 
                                             group = "2 vs 4")

# Store p-value separatly (for adjustment)
peace[4] <- tee[["p.value"]]


# C.8.2.5 Adjusted p-values ---------------------------------------------------
padjust <- p.adjust(peace, method = "BH")

pairwise <- bind_cols(pairwise, as_tibble(padjust))
pairwise <- pairwise %>% rename(p.adjust = value)

pairwise <- pairwise %>% mutate(star = ifelse(p.adjust < 0.05, "*", ""))
#pairwise %>% select(comparison, mean.diff, eff_size_g, p, p.adjust, star)

pairwise_naffect_post_s3 <- pairwise









## C.8.3 Group comparison turnover intention ==================================
# empty vector to store p-values
peace <- vector(mode = "numeric", length = 4)

# Clear the results table
pairwise <- func_empty_resultstable_postonly()

# C.8.3.1 1 vs 3 --------------------------------------------------------------
tee <- func_compare_ttest_postonly(df_merge1_3_s3, 
                                   df_merge1_3_s3$turnover_post, 
                                   df_merge1_3_s3$treatment_f_reversed,
                                   side = "less")

effsize_g <- func_compare_effsize_postonly(df_merge1_3_s3$turnover_post, 
                                           df_merge1_3_s3$treatment, 
                                           hedge = TRUE, 
                                           paired = FALSE)
effsize_d <- MBESS::ci.smd(ncp = tee[["statistic"]][["t"]], 
                           n.1=obs_s3$final[3], 
                           n.2=obs_s3$final[5], 
                           conf.level=0.95)$smd

pairwise <- func_compare_writetable_postonly(pairwise, 
                                             tee, 
                                             effsize_g, 
                                             effsize_d, 
                                             group = "1 vs 3")

# Store p-value separatly (for adjustment)
peace[1] <- tee[["p.value"]]


# C.8.3.2 1 vs 4 --------------------------------------------------------------
tee <- func_compare_ttest_postonly(df_merge1_4_s3, 
                                   df_merge1_4_s3$turnover_post, 
                                   df_merge1_4_s3$treatment_f_reversed,
                                   side = "less")

effsize_g <- func_compare_effsize_postonly(df_merge1_4_s3$turnover_post, 
                                           df_merge1_4_s3$treatment, 
                                           hedge = TRUE, 
                                           paired = FALSE)
effsize_d <- MBESS::ci.smd(ncp = tee[["statistic"]][["t"]], 
                           n.1=obs_s3$final[3], 
                           n.2=obs_s3$final[5], 
                           conf.level=0.95)$smd

pairwise <- func_compare_writetable_postonly(pairwise, 
                                             tee, 
                                             effsize_g, 
                                             effsize_d, 
                                             group = "1 vs 4")

# Store p-value separatly (for adjustment)
peace[2] <- tee[["p.value"]]


# C.8.3.3 2 vs 3 --------------------------------------------------------------
tee <- func_compare_ttest_postonly(df_merge2_3_s3, 
                                   df_merge2_3_s3$turnover_post, 
                                   df_merge2_3_s3$treatment_f_reversed,
                                   side = "less")

effsize_g <- func_compare_effsize_postonly(df_merge2_3_s3$turnover_post, 
                                           df_merge2_3_s3$treatment, 
                                           hedge = TRUE, 
                                           paired = FALSE)
effsize_d <- MBESS::ci.smd(ncp = tee[["statistic"]][["t"]], 
                           n.1=obs_s3$final[3], 
                           n.2=obs_s3$final[5], 
                           conf.level=0.95)$smd

pairwise <- func_compare_writetable_postonly(pairwise, 
                                             tee, 
                                             effsize_g, 
                                             effsize_d, 
                                             group = "2 vs 3")

# Store p-value separatly (for adjustment)
peace[3] <- tee[["p.value"]]

# C.8.3.4 2 vs 4 --------------------------------------------------------------
tee <- func_compare_ttest_postonly(df_merge2_4_s3, 
                                   df_merge2_4_s3$turnover_post, 
                                   df_merge2_4_s3$treatment_f_reversed,
                                   side = "less")

effsize_g <- func_compare_effsize_postonly(df_merge2_4_s3$turnover_post, 
                                           df_merge2_4_s3$treatment, 
                                           hedge = TRUE, 
                                           paired = FALSE)
effsize_d <- MBESS::ci.smd(ncp = tee[["statistic"]][["t"]], 
                           n.1=obs_s3$final[3], 
                           n.2=obs_s3$final[5], 
                           conf.level=0.95)$smd

pairwise <- func_compare_writetable_postonly(pairwise, 
                                             tee, 
                                             effsize_g, 
                                             effsize_d, 
                                             group = "2 vs 4")

# Store p-value separatly (for adjustment)
peace[4] <- tee[["p.value"]]


# C.8.3.5 Adjusted p-values ---------------------------------------------------
padjust <- p.adjust(peace, method = "BH")

pairwise <- bind_cols(pairwise, as_tibble(padjust))
pairwise <- pairwise %>% rename(p.adjust = value)

pairwise <- pairwise %>% mutate(star = ifelse(p.adjust < 0.05, "*", ""))
#pairwise %>% select(comparison, mean.diff, eff_size_g, p, p.adjust, star)

pairwise_turnover_post_s3 <- pairwise






## C.8.4 Group comparison willigness to recommend job =========================
# empty vector to store p-values
peace <- vector(mode = "numeric", length = 4)

# Clear the results table
pairwise <- func_empty_resultstable_postonly()

# C.8.4.1 1 vs 3 --------------------------------------------------------------
tee <- func_compare_ttest_postonly(df_merge1_3_s3, 
                                   df_merge1_3_s3$willingnessjobrec_post, 
                                   df_merge1_3_s3$treatment_f_reversed,
                                   side = "greater")

effsize_g <- func_compare_effsize_postonly(df_merge1_3_s3$willingnessjobrec_post, 
                                           df_merge1_3_s3$treatment, 
                                           hedge = TRUE, 
                                           paired = FALSE)
effsize_d <- MBESS::ci.smd(ncp = tee[["statistic"]][["t"]], 
                           n.1=obs_s3$final[3], 
                           n.2=obs_s3$final[5], 
                           conf.level=0.95)$smd

pairwise <- func_compare_writetable_postonly(pairwise, 
                                             tee, 
                                             effsize_g, 
                                             effsize_d, 
                                             group = "1 vs 3")

# Store p-value separatly (for adjustment)
peace[1] <- tee[["p.value"]]


# C.8.4.2 1 vs 4 --------------------------------------------------------------
tee <- func_compare_ttest_postonly(df_merge1_4_s3, 
                                   df_merge1_4_s3$willingnessjobrec_post, 
                                   df_merge1_4_s3$treatment_f_reversed,
                                   side = "greater")

effsize_g <- func_compare_effsize_postonly(df_merge1_4_s3$willingnessjobrec_post, 
                                           df_merge1_4_s3$treatment, 
                                           hedge = TRUE, 
                                           paired = FALSE)
effsize_d <- MBESS::ci.smd(ncp = tee[["statistic"]][["t"]], 
                           n.1=obs_s3$final[3], 
                           n.2=obs_s3$final[5], 
                           conf.level=0.95)$smd

pairwise <- func_compare_writetable_postonly(pairwise, 
                                             tee, 
                                             effsize_g, 
                                             effsize_d, 
                                             group = "1 vs 4")

# Store p-value separatly (for adjustment)
peace[2] <- tee[["p.value"]]


# C.8.4.3 2 vs 3 --------------------------------------------------------------
tee <- func_compare_ttest_postonly(df_merge2_3_s3, 
                                   df_merge2_3_s3$willingnessjobrec_post, 
                                   df_merge2_3_s3$treatment_f_reversed,
                                   side = "greater")

effsize_g <- func_compare_effsize_postonly(df_merge2_3_s3$willingnessjobrec_post, 
                                           df_merge2_3_s3$treatment, 
                                           hedge = TRUE, 
                                           paired = FALSE)
effsize_d <- MBESS::ci.smd(ncp = tee[["statistic"]][["t"]], 
                           n.1=obs_s3$final[3], 
                           n.2=obs_s3$final[5], 
                           conf.level=0.95)$smd

pairwise <- func_compare_writetable_postonly(pairwise, 
                                             tee, 
                                             effsize_g, 
                                             effsize_d, 
                                             group = "2 vs 3")

# Store p-value separatly (for adjustment)
peace[3] <- tee[["p.value"]]

# C.8.4.4 2 vs 4 --------------------------------------------------------------
tee <- func_compare_ttest_postonly(df_merge2_4_s3, 
                                   df_merge2_4_s3$willingnessjobrec_post, 
                                   df_merge2_4_s3$treatment_f_reversed,
                                   side = "greater")

effsize_g <- func_compare_effsize_postonly(df_merge2_4_s3$willingnessjobrec_post, 
                                           df_merge2_4_s3$treatment, 
                                           hedge = TRUE, 
                                           paired = FALSE)
effsize_d <- MBESS::ci.smd(ncp = tee[["statistic"]][["t"]], 
                           n.1=obs_s3$final[3], 
                           n.2=obs_s3$final[5], 
                           conf.level=0.95)$smd

pairwise <- func_compare_writetable_postonly(pairwise, 
                                             tee, 
                                             effsize_g, 
                                             effsize_d, 
                                             group = "2 vs 4")

# Store p-value separatly (for adjustment)
peace[4] <- tee[["p.value"]]


# C.8.4.5 Adjusted p-values ---------------------------------------------------
padjust <- p.adjust(peace, method = "BH")

pairwise <- bind_cols(pairwise, as_tibble(padjust))
pairwise <- pairwise %>% rename(p.adjust = value)

pairwise <- pairwise %>% mutate(star = ifelse(p.adjust < 0.05, "*", ""))
#pairwise %>% select(comparison, mean.diff, eff_size_g, p, p.adjust, star)

pairwise_will_post_s3 <- pairwise








# remove unused objects
rm(effsize_d, effsize_g, pairwise, tee, padjust, peace)

pairwise_paffect_post_s3
pairwise_naffect_post_s3
pairwise_turnover_post_s3
pairwise_will_post_s3







# C.9 Results table ANOVA #####################################################
aov_paffect_result_s3 <- aov_result(aov_paffect_post_s3)
aov_naffect_result_s3 <- aov_result(aov_naffect_post_s3)
aov_turnover_result_s3 <- aov_result(aov_turnover_post_s3)
aov_will_result_s3 <- aov_result(aov_will_post_s3)
aov_bene_result_s3 <- 
  aov_result(aov(df_merge_s3$beneficiaries ~ df_merge_s3$treatment_f))
aov_obj_bene_result_s3 <- 
  aov_result(aov(df_merge_s3$beneficiaries_obj ~ df_merge_s3$treatment_f))
aov_prosocialimpact_result_s3 <- aov_result(aov_prosocialimpact_post_s3)
aov_societalimpact_result_s3 <- aov_result(aov_societalimpact_post_s3)

aov_age_result_s3 <- aov_result(aov_age_s3)
chi_female_result_s3 <- chi_result(df_merge_s3$gender_f, 
                                   df_merge_s3$treatment_f)
chi_manager_result_s3 <- chi_result(df_merge_s3$manager_dummy, 
                                    df_merge_s3$treatment_f)



desc_s3 <-  desc_s3 %>%
  mutate(Diff = "") %>%
  mutate(Diff = ifelse(treatment == "paffect_m",
                       aov_paffect_result_s3, Diff),
         Diff = ifelse(treatment == "naffect_m",
                       aov_naffect_result_s3, Diff),
         Diff = ifelse(treatment == "turnover_m",
                       aov_turnover_result_s3, Diff),
         Diff = ifelse(treatment == "will_m",
                       aov_will_result_s3, Diff),
         Diff = ifelse(treatment == "psi_m",
                       aov_prosocialimpact_result_s3, Diff),
         Diff = ifelse(treatment == "si_m",
                       aov_societalimpact_result_s3, Diff),
         Diff = ifelse(treatment == "age_m",
                       aov_age_result_s3, Diff),
         Diff = ifelse(treatment == "female_m",
                       chi_female_result_s3, Diff),
         Diff = ifelse(treatment == "manager_m",
                       chi_manager_result_s3, Diff)
         )


desc_s3 <- desc_s3 %>%
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

desc_s3






# C.10 Plot DVs (post-test only) ##############################################
# Prepare data (sd, n, se, ci per treatment per variable)
df_merge_sum_s3 <- df_merge_s3 %>% 
  select(treatment, paffect_post, naffect_post, 
         turnover_post, willingnessjobrec_post) %>%
  group_by(treatment) %>%
  summarise_all(funs(mean, 
                     sd, 
                     n = n(), 
                     se_func,
                     lower_ci_func,
                     upper_ci_func))

# Transform data to long (means)
df_merge_sum2_s3 <- df_merge_sum_s3 %>% 
  select(treatment, ends_with("_mean")) %>%
  rename(paffect_post = paffect_post_mean,
         naffect_post = naffect_post_mean,
         turnover_post = turnover_post_mean,
         will_post = willingnessjobrec_post_mean) %>%
  gather("paffect_post", 
         "naffect_post", 
         "turnover_post", 
         "will_post", 
         key = "var", 
         value = "mean")

# Transform data to long (CI lower bound) 
df_merge_sum3_s3 <- df_merge_sum_s3 %>% 
  select(treatment, ends_with("_lower_ci_func")) %>%
  rename(paffect_post = paffect_post_lower_ci_func,
         naffect_post = naffect_post_lower_ci_func,
         turnover_post = turnover_post_lower_ci_func,
         will_post = willingnessjobrec_post_lower_ci_func) %>%
  gather("paffect_post", 
         "naffect_post", 
         "turnover_post", 
         "will_post",
         key = "var", 
         value = "lower_ci")

# Transform data to long (CI upper bound)
df_merge_sum4_s3 <- df_merge_sum_s3 %>% 
  select(treatment, ends_with("_upper_ci_func")) %>%
  rename(paffect_post = paffect_post_upper_ci_func,
         naffect_post = naffect_post_upper_ci_func,
         turnover_post = turnover_post_upper_ci_func,
         will_post = willingnessjobrec_post_upper_ci_func) %>%
  gather("paffect_post", 
         "naffect_post", 
         "turnover_post", 
         "will_post",
         key = "var", 
         value = "upper_ci")

# Merge data
df_merge_sum2_s3 <- left_join(df_merge_sum2_s3, df_merge_sum3_s3,
                              key = c("treatment", "var"))
df_merge_sum2_s3 <- left_join(df_merge_sum2_s3, df_merge_sum4_s3,
                              key = c("treatment", "var"))


# Add factor variables (for captions and sorting)
df_merge_sum2_s3 <- df_merge_sum2_s3 %>%
  mutate(treatment_f = factor(treatment,
                              labels = c("Passive\nControl",
                                         "Active\nControl",
                                         "Prosocial\nImpact", 
                                         "Societal\nImpact")),
         var_f = factor(var, 
                        levels = c("paffect_post",
                                   "naffect_post",
                                   "turnover_post",
                                   "will_post")))

# Define jitter so it is the same for means and CI
dodge <- position_dodge(.8) # how much jitter on the x-axis?


# Plot
meanplot_s3 <- ggplot(df_merge_sum2_s3, aes(x=var_f, y=mean, 
                                            color =treatment_f)) + 
  geom_errorbar(aes(ymin=lower_ci,
                    ymax=upper_ci), 
                width=.5,
                size = .6,
                position = dodge,
                show.legend=FALSE) +
  geom_point(aes(shape = treatment_f),
             position = dodge, 
             size = 2) +
  scale_fill_continuous(guide = guide_legend()) +
  theme(legend.position="bottom") +
  labs(title = "Figure 3: Differences in dependent variables\nbetween experimental groups in Study 3",
       x = "Dependent Variables", 
       y = "Mean", 
       color = "Treatments",
       shape = "Treatments") +
  scale_x_discrete(labels=c("naffect_post" = "Negative\nAffect", 
                            "paffect_post" = "Positive\nAffect",
                            "turnover_post" = "Turnover\nIntention",
                            "will_post" = "Willingness to\nrecommend job")) +
  coord_cartesian(ylim = c(1, 7)) + 
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                     labels = c("1", "2", "3", "4", "5", "6", "7")) +
  scale_color_manual(values = c("#1380A1", "#990000", "#FAAB18", "#588300")) +
  scale_shape_manual(values = c(15, 16, 17, 18)) +
  # Positive Affect
  geom_signif(annotations = "**",
            y_position = 4.3,
            xmin= 0.7, xmax = 1.3,
            tip_length = c(-0.08, -0.2),
            vjust = 4.5,
            color='black') +
  geom_signif(annotations = "**",
            y_position = 5.9,
            xmin= 0.9, xmax = 1.3,
            tip_length = c(0.14, 0.02),
            vjust = 0.5,
            color='black') +
# Negative Affect
  geom_signif(annotations = "**",
              y_position = 1.3,
              xmin= 1.7, xmax = 2.3,
              tip_length = c(-0.10, -0.01),
              vjust = 3.0,
              color='black') +
  geom_signif(annotations = "*",
              y_position = 2.5,
              xmin= 1.9, xmax = 2.3,
              tip_length = c(0.03, 0.13),
              vjust = 0.5,
              color='black') +
#   # Turnover
  geom_signif(annotations = "**",
              y_position = 2.35,
              xmin= 2.7, xmax = 3.3,
              tip_length = c(-0.19, -0.02),
              vjust = 4.1,
              color='black') +
#   # Will
  geom_signif(annotations = "*",
              y_position = 2.8,
              xmin= 3.71, xmax = 4.1,
              tip_length = c(-0.02, -0.10),
              vjust = 1.9,
              color='black') +
  geom_signif(annotations = "***",
              y_position = 2.65,
              xmin= 3.69, xmax = 4.3,
              tip_length = c(-0.057, -0.22),
              vjust = 4.7,
              color='black') +
  geom_signif(annotations = "*",
              y_position = 4.3,
              xmin= 3.9, xmax = 4.3,
              tip_length = c(0.07, 0.02),
              vjust = 0.5,
              color='black')


rm(df_merge_sum_s3, df_merge_sum2_s3, df_merge_sum3_s3, df_merge_sum4_s3)











# C.11 Results table group compare (post-test only) ###########################
pairwise_paffect_post2_s3 <- 
  pairwise_paffect_post_s3 %>%
  mutate(var = "paffect_post") %>%
  mutate(test = paste0("t(",
                       broman::myround(df, 1),
                       ") = ",
                       broman::myround(t, 2),
                       ", ", pformat(p.adjust))) %>%
  select(var, comparison, mean.diff, test, eff_size_g)

pairwise_naffect_post2_s3 <- 
  pairwise_naffect_post_s3 %>%
  mutate(var = "naffect_post") %>%
  mutate(test = paste0("t(",
                       broman::myround(df, 1),
                       ") = ",
                       broman::myround(t, 2),
                       ", ", pformat(p.adjust))) %>%
  select(var, comparison, mean.diff, test, eff_size_g)

pairwise_turnover_post2_s3 <- 
  pairwise_turnover_post_s3 %>%
  mutate(var = "turnover_post") %>%
  mutate(test = paste0("t(",
                       broman::myround(df, 1),
                       ") = ",
                       broman::myround(t, 2),
                       ", ", pformat(p.adjust))) %>%
  select(var, comparison, mean.diff, test, eff_size_g)

pairwise_will_post2_s3 <- 
  pairwise_will_post_s3 %>%
  mutate(var = "will_post") %>%
  mutate(test = paste0("t(",
                       broman::myround(df, 1),
                       ") = ",
                       broman::myround(t, 2),
                       ", ", pformat(p.adjust))) %>%
  select(var, comparison, mean.diff, test, eff_size_g)

pairwise2_s3 <- bind_rows(pairwise_paffect_post2_s3, pairwise_naffect_post2_s3,
                          pairwise_turnover_post2_s3, pairwise_will_post2_s3)
pairwise2_s3
pairwise3_s3 <- pairwise2_s3 %>% 
  select(-test, -eff_size_g) %>% 
  mutate(mean.diff = 
           as.character(broman::myround(mean.diff,2))) %>%
  spread(comparison, mean.diff) %>%
  mutate(value = "mean.diff")

pairwise4_s3 <- pairwise2_s3 %>% 
  select(-mean.diff, -test) %>%
  mutate(eff_size_g = abs(eff_size_g)) %>%
  mutate(eff_size_g = 
           as.character(broman::myround(eff_size_g,2))) %>%
  spread(comparison, eff_size_g) %>%
  mutate(value = "eff_size_g")

pairwise5_s3 <- pairwise2_s3 %>% 
  select(-mean.diff, -eff_size_g) %>%
  spread(comparison, test) %>%
  mutate(value = "test")

results_table_s3 <- bind_rows(pairwise3_s3, pairwise4_s3, pairwise5_s3) %>%
  select(var, value, everything()) %>% 
  mutate(var = factor(var,
                      levels = c("paffect_post",
                                 "naffect_post",
                                 "turnover_post",
                                 "will_post"))) %>%
  mutate(value = factor(value, 
                        levels = c("mean.diff",
                                   "eff_size_g",
                                   "test"))) %>%
  arrange(var, value)

results_table_s3
rm(pairwise2_s3, pairwise3_s3, pairwise4_s3, pairwise5_s3,
   pairwise_paffect_post2_s3, pairwise_naffect_post2_s3,
   pairwise_turnover_post2_s3, pairwise_will_post2_s3)

results_table2_s3 <- results_table_s3 %>%
  mutate(var = as.character(var)) %>%
  mutate(var = ifelse(value == "eff_size_g",
                      NA, var),
         var = ifelse(value == "test",
                      NA, var)) %>%
  mutate(var = factor(var,
                      levels = c("paffect_post", "naffect_post", "turnover_post", "will_post"),
                      labels= c("Positive Affect", 
                                "Negative Affect", 
                                "Turnover Intention",
                                "Will. to rec. job"))) %>%
  mutate(value = factor(value,
                        labels = c("Mean diff.",
                                   "Hedge's g",
                                   "Welch t-test"))) #%>%
#mutate(var = as.character(var)) %>%

results_table2_s3





# C.12 Regression with beneficiary contact (H4) ###############################
# Model 1: Treatment only
reg_paffect_s3 <- lm(paffect_post ~ treatment_f, data = df_merge_s3)
reg_naffect_s3 <- lm(naffect_post ~ treatment_f, data = df_merge_s3)
reg_turnover_s3 <- lm(turnover_post ~ treatment_f, data = df_merge_s3)
reg_will_s3 <- lm(willingnessjobrec_post ~ treatment_f, data = df_merge_s3)

# Model 2: Add contact with beneficiaries
reg_paffect2_s3 <- update(reg_paffect_s3, . ~ . + beneficiaries)
reg_naffect2_s3 <- update(reg_naffect_s3, . ~ . + beneficiaries)
reg_turnover2_s3 <- update(reg_turnover_s3, . ~ . + beneficiaries)
reg_will2_s3 <- update(reg_will_s3, . ~ . + beneficiaries)

# Model 3: Add beneficiaries * treatment
reg_paffect3_s3 <- update(reg_paffect_s3, . ~ . + beneficiaries*treatment_f)
reg_naffect3_s3 <- update(reg_naffect_s3, . ~ . + beneficiaries*treatment_f)
reg_turnover3_s3 <- update(reg_turnover_s3, . ~ . + beneficiaries*treatment_f)
reg_will3_s3 <- update(reg_will_s3, . ~ . + beneficiaries*treatment_f)

# Model 4: Add objective beneficiaries * treatment to model 1
reg_paffect4_s3 <- update(reg_paffect_s3, . ~ . + beneficiaries_obj*treatment_f)
reg_naffect4_s3 <- update(reg_naffect_s3, . ~ . + beneficiaries_obj*treatment_f)
reg_turnover4_s3 <- update(reg_turnover_s3, . ~ . + beneficiaries_obj*treatment_f)
reg_will4_s3 <- update(reg_will_s3, . ~ . + beneficiaries_obj*treatment_f)


# C.12.1 Regression with exogeneous control variables only ####################

# Model 2: Add exogenous control variables
reg_paffect_control_s3 <- update(reg_paffect_s3, . ~ . + female + age + manager)
reg_naffect_control_s3 <- update(reg_naffect_s3, . ~ . + female + age + manager)
reg_turnover_control_s3 <- update(reg_turnover_s3, . ~ . + female + age + manager)
reg_will_control_s3 <- update(reg_will_s3, . ~ . + female + age + manager)

stargazer::stargazer(reg_paffect_s3, reg_paffect_control_s3, 
                     reg_naffect_s3, reg_naffect_control_s3,
                     reg_turnover_s3, reg_turnover_control_s3,
                     reg_will_s3, reg_will_control_s3,
                     type = "html",
                     out = here("analysis", "S3_Reg_Controls.html"),
                     title = "Study 3: OLS regression with exogenous control variables",
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







# C.13 Analyse pre/post-test differences (mixed effects model) ################

# C.13.1 Mixed effect positive affect =========================================
# C.13.1.1 Prepare data -------------------------------------------------------
# Select paffect only and transform from wide to long
df_merge_s3_paffect_long <- df_merge_s3 %>% 
  select(WorkerId, paffect_pre, paffect_post, treatment_f) %>%
  gather(-WorkerId, -treatment_f, 
         key = "time", 
         value = "paffect",
         factor_key = TRUE) %>%
  mutate(WorkerId = as.factor(WorkerId)) %>%
  arrange(WorkerId)


# C.13.1.2 mixed effect ANOVA --------------------------------------------
# Option A (Standard aov) - seems to be easier to interpret
rmaov_paffect <- aov(paffect ~ treatment_f * time + Error(WorkerId/time), 
                     data = df_merge_s3_paffect_long)
summary(rmaov_paffect)

# First part (Error: WorkerId): test for between-differences
# ==> If we ignore time, the treatment groups do not differ significantly
# Second part (Error: WorkerId:time): test for within differences
# ==> If we ignore the treatment groups, paffect does not differ over time 
# (between pre and post measure)
# ==> The effect of time  differs between treatment groups 
# That's what we are interested in!
# Now we can test differences between pre and post for each group
# (t-tests for pre-/post means per group) = post-hoc tests of within-effects
# as well as (pairwise t-tests of gain scores) =
# post-hoc test of interaction effects

# Option B (ezANOVA) : easier to spacify but more difficult to interpret
rmezaov_paffect <- ez::ezANOVA(data = df_merge_s3_paffect_long, 
                               dv = .(paffect), 
                               wid = .(WorkerId), 
                               between = .(treatment_f), 
                               within = .(time),
                               type = 2,
                               detailed = TRUE)

rmezaov_paffect

# # HLM
# # There is also a HLM option, but I do not understand
# nullmodel <- nlme::lme(paffect ~ 1, 
#                        random = ~1|WorkerId/time, 
#                        data=new2,
#                        method = "ML")
# model <- nlme::lme(paffect ~ treatment_f + time, 
#                    random = ~1|WorkerId/time, 
#                    data=new2,
#                    method = "ML")
# anova(nullmodel, model)
# summary(model)




# # ANCOVA
# anova(lm(df_merge_s3$paffect_post ~ df_merge_s3$paffect_pre + 
#            df_merge_s3$treatment_f))










# C.13.2 Mixed effect negative affect =========================================
# C.13.2.1 Prepare data -------------------------------------------------------
# Select naffect only and transform from wide to long
df_merge_s3_naffect_long <- df_merge_s3 %>% 
  select(WorkerId, naffect_pre, naffect_post, treatment_f) %>%
  gather(-WorkerId, -treatment_f, 
         key = "time", 
         value = "naffect",
         factor_key = TRUE) %>%
  mutate(WorkerId = as.factor(WorkerId)) %>%
  arrange(WorkerId)


# C.13.2.2 mixed effect ANOVA -------------------------------------------------
rmaov_naffect <- aov(naffect ~ treatment_f * time + Error(WorkerId/time), 
                     data = df_merge_s3_naffect_long)
summary(rmaov_naffect)

rmezaov_naffect <- ez::ezANOVA(data = df_merge_s3_naffect_long, 
                               dv = .(naffect), 
                               wid = .(WorkerId), 
                               between = .(treatment_f), 
                               within = .(time),
                               type = 2,
                               detailed = TRUE)






# C.13.3 Mixed effect turnover intention ======================================
# C.13.3.1 Prepare data -------------------------------------------------------
# Select naffect only and transform from wide to long
df_merge_s3_turnover_long <- df_merge_s3 %>% 
  select(WorkerId, turnover_pre, turnover_post, treatment_f) %>%
  gather(-WorkerId, -treatment_f, 
         key = "time", 
         value = "turnover",
         factor_key = TRUE) %>%
  mutate(WorkerId = as.factor(WorkerId)) %>%
  arrange(WorkerId)


# C.13.3.2 mixed effect ANOVA -------------------------------------------------
rmaov_turnover <- aov(turnover ~ treatment_f * time + Error(WorkerId/time), 
                      data = df_merge_s3_turnover_long)
summary(rmaov_turnover)

rmezaov_turnover <- ez::ezANOVA(data = df_merge_s3_turnover_long, 
                                dv = .(turnover), 
                                wid = .(WorkerId), 
                                between = .(treatment_f), 
                                within = .(time),
                                type = 2,
                                detailed = TRUE)








# C.13.4 Mixed effect willingness to recommend job ============================
# C.13.4.1 Prepare data -------------------------------------------------------
# Select naffect only and transform from wide to long
df_merge_s3_will_long <- df_merge_s3 %>% 
  select(WorkerId, willingnessjobrec_pre, willingnessjobrec_post, 
         treatment_f) %>%
  gather(-WorkerId, -treatment_f, 
         key = "time", 
         value = "will",
         factor_key = TRUE) %>%
  mutate(WorkerId = as.factor(WorkerId)) %>%
  arrange(WorkerId)


# C.13.4.2 mixed effect ANOVA -------------------------------------------------
rmaov_will <- aov(will ~ treatment_f * time + Error(WorkerId/time), 
                  data = df_merge_s3_will_long)
summary(rmaov_will)

rmezaov_will <- ez::ezANOVA(data = df_merge_s3_will_long, 
                            dv = .(will), 
                            wid = .(WorkerId), 
                            between = .(treatment_f), 
                            within = .(time),
                            type = 2,
                            detailed = TRUE)




# C.13.5 Results ==============================================================
apaTables::apa.ezANOVA.table(rmezaov_paffect,
                             table.title = "Positive Affect (Mixed effects ANOVA)",
                             correction = "none")
apaTables::apa.ezANOVA.table(rmezaov_naffect,
                             table.title = "Negative Affect (Mixed effects ANOVA)",
                             correction = "none")
apaTables::apa.ezANOVA.table(rmezaov_turnover,
                             table.title = "Turnover Intention (Mixed effects ANOVA)",
                             correction = "none")
apaTables::apa.ezANOVA.table(rmezaov_will,
                             table.title = "Willingness rec. job (Mixed effects ANOVA)",
                             correction = "none")



rmaov_table <- tibble(Effect = c("Treatment", "Time", "Treatment * Time"),
                      `paffect` = c(rmaov_results(rmezaov_paffect, 2), 
                                            rmaov_results(rmezaov_paffect, 3),
                                            rmaov_results(rmezaov_paffect, 4)),
                      `naffect` = c(rmaov_results(rmezaov_naffect, 2), 
                                            rmaov_results(rmezaov_naffect, 3),
                                            rmaov_results(rmezaov_naffect, 4)),
                      `turnover` = c(rmaov_results(rmezaov_turnover, 2), 
                                               rmaov_results(rmezaov_turnover, 3),
                                               rmaov_results(rmezaov_turnover, 4)),
                      `will` = c(rmaov_results(rmezaov_will, 2), 
                                                   rmaov_results(rmezaov_will, 3),
                                                   rmaov_results(rmezaov_will, 4))
                      )


# C.14 Group comparisons time (paired t-tests) ################################
df_merge1_s3 <- df_merge_s3 %>% filter(treatment == 1)
df_merge2_s3 <- df_merge_s3 %>% filter(treatment == 2)
df_merge3_s3 <- df_merge_s3 %>% filter(treatment == 3)
df_merge4_s3 <- df_merge_s3 %>% filter(treatment == 4)

# C.14.1 Group comparison positive affect =====================================

# empty vector to store p-values
peace <- vector(mode = "numeric", length = 4)


# Empty results table
pairwise <- func_empty_resultstable()




# C.14.1.1 Treatment 1 --------------------------------------------------------
tee <- func_compare_ttest(df_merge1_s3, 
                          df_merge1_s3$paffect_post, 
                          df_merge1_s3$paffect_pre,
                          side = "greater")

effsize_g <- func_compare_effsize(df_merge1_s3, 
                                  df_merge1_s3$paffect_post, 
                                  df_merge1_s3$paffect_pre,
                                  "greater")
effsize_d <- func_compare_effsize(df_merge1_s3, 
                                  df_merge1_s3$paffect_post, 
                                  df_merge1_s3$paffect_pre,
                                  "greater")

pairwise <- func_compare_writetable(pairwise, tee, effsize_g, effsize_d, "1")


# Store p-value separatly (for adjustment)
peace[1] <- tee[["p.value"]]



# C.14.1.2 Treatment 2 --------------------------------------------------------
tee <- func_compare_ttest(df_merge2_s3, 
                          df_merge2_s3$paffect_post, 
                          df_merge2_s3$paffect_pre,
                          "greater")

effsize_g <- func_compare_effsize(df_merge2_s3, 
                                  df_merge2_s3$paffect_post, 
                                  df_merge2_s3$paffect_pre,
                                  "greater")
effsize_d <- func_compare_effsize(df_merge2_s3, 
                                  df_merge2_s3$paffect_post, 
                                  df_merge2_s3$paffect_pre,
                                  "greater")

pairwise <- func_compare_writetable(pairwise, tee, effsize_g, effsize_d, "2")

# Store p-value separatly (for adjustment)
peace[2] <- tee[["p.value"]]




# C.14.1.3 Treatment 3 --------------------------------------------------------
tee <- func_compare_ttest(df_merge3_s3, 
                          df_merge3_s3$paffect_post, 
                          df_merge3_s3$paffect_pre,
                          "greater")

effsize_g <- func_compare_effsize(df_merge3_s3, 
                                  df_merge3_s3$paffect_post, 
                                  df_merge3_s3$paffect_pre,
                                  "greater")
effsize_d <- func_compare_effsize(df_merge3_s3, 
                                  df_merge3_s3$paffect_post, 
                                  df_merge3_s3$paffect_pre,
                                  "greater")

pairwise <- func_compare_writetable(pairwise, tee, effsize_g, effsize_d, "3")

# Store p-value separatly (for adjustment)
peace[3] <- tee[["p.value"]]



# C.14.1.4 Treatment 4 --------------------------------------------------------
tee <- func_compare_ttest(df_merge4_s3, 
                          df_merge4_s3$paffect_post, 
                          df_merge4_s3$paffect_pre,
                          "greater")

effsize_g <- func_compare_effsize(df_merge4_s3, 
                                  df_merge4_s3$paffect_post, 
                                  df_merge4_s3$paffect_pre,
                                  "greater")
effsize_d <- func_compare_effsize(df_merge4_s3, 
                                  df_merge4_s3$paffect_post, 
                                  df_merge4_s3$paffect_pre,
                                  "greater")

pairwise <- func_compare_writetable(pairwise, tee, effsize_g, effsize_d, "4")

# Store p-value separatly (for adjustment)
peace[4] <- tee[["p.value"]]



# C.14.1.5 Adjusted p-values -------------------------------------------------
padjust <- p.adjust(peace, method = "BH")

pairwise <- bind_cols(pairwise, as_tibble(padjust))
pairwise <- pairwise %>% rename(p.adjust = value)

pairwise <- pairwise %>% mutate(star = ifelse(p.adjust < 0.05, "*", ""))

pairwise_paffect_time_s3 <- pairwise





# C.14.2 Group comparison negative affect =====================================
# empty vector to store p-values
peace <- vector(mode = "numeric", length = 4)

pairwise <- func_empty_resultstable()

# C.14.2.1 Treatment 1 --------------------------------------------------------
tee <- func_compare_ttest(df_merge1_s3, 
                          df_merge1_s3$naffect_post, 
                          df_merge1_s3$naffect_pre,
                          "less")

effsize_g <- func_compare_effsize(df_merge1_s3, 
                                  df_merge1_s3$naffect_post, 
                                  df_merge1_s3$naffect_pre,
                                  "less")
effsize_d <- func_compare_effsize(df_merge1_s3, 
                                  df_merge1_s3$naffect_post, 
                                  df_merge1_s3$naffect_pre,
                                  "less")

pairwise <- func_compare_writetable(pairwise, tee, effsize_g, effsize_d, "1")


# Store p-value separatly (for adjustment)
peace[1] <- tee[["p.value"]]



# C.14.2.2 Treatment 2 --------------------------------------------------------
tee <- func_compare_ttest(df_merge2_s3, 
                          df_merge2_s3$naffect_post, 
                          df_merge2_s3$naffect_pre,
                          "less")

effsize_g <- func_compare_effsize(df_merge2_s3, 
                                  df_merge2_s3$naffect_post, 
                                  df_merge2_s3$naffect_pre,
                                  "less")
effsize_d <- func_compare_effsize(df_merge2_s3, 
                                  df_merge2_s3$naffect_post, 
                                  df_merge2_s3$naffect_pre,
                                  "less")

pairwise <- func_compare_writetable(pairwise, tee, effsize_g, effsize_d, 
                                    "2")

# Store p-value separatly (for adjustment)
peace[2] <- tee[["p.value"]]




# C.14.2.3 Treatment 3 --------------------------------------------------------
tee <- func_compare_ttest(df_merge3_s3, 
                          df_merge3_s3$naffect_post, 
                          df_merge3_s3$naffect_pre,
                          "less")

effsize_g <- func_compare_effsize(df_merge3_s3, 
                                  df_merge3_s3$naffect_post, 
                                  df_merge3_s3$naffect_pre,
                                  "less")
effsize_d <- func_compare_effsize(df_merge3_s3, 
                                  df_merge3_s3$naffect_post, 
                                  df_merge3_s3$naffect_pre,
                                  "less")

pairwise <- func_compare_writetable(pairwise, tee, effsize_g, effsize_d, 
                                    "3")

# Store p-value separatly (for adjustment)
peace[3] <- tee[["p.value"]]



# C.14.2.4 Treatment 4 --------------------------------------------------------
tee <- func_compare_ttest(df_merge4_s3, 
                          df_merge4_s3$naffect_post, 
                          df_merge4_s3$naffect_pre,
                          "less")

effsize_g <- func_compare_effsize(df_merge4_s3, 
                                  df_merge4_s3$naffect_post, 
                                  df_merge4_s3$naffect_pre,
                                  "less")
effsize_d <- func_compare_effsize(df_merge4_s3, 
                                  df_merge4_s3$naffect_post, 
                                  df_merge4_s3$naffect_pre,
                                  "less")

pairwise <- func_compare_writetable(pairwise, tee, effsize_g, effsize_d, 
                                    "4")

# Store p-value separatly (for adjustment)
peace[4] <- tee[["p.value"]]



# C.14.2.5 Adjusted p-values --------------------------------------------------
padjust <- p.adjust(peace, method = "BH")

pairwise <- bind_cols(pairwise, as_tibble(padjust))
pairwise <- pairwise %>% rename(p.adjust = value)

pairwise <- pairwise %>% mutate(star = ifelse(p.adjust < 0.05, "*", ""))

pairwise_naffect_time_s3 <- pairwise





# C.14.3 Group comparison turnover intention ==================================
# empty vector to store p-values
peace <- vector(mode = "numeric", length = 4)

pairwise <- func_empty_resultstable()

# C.14.3.1 Treatment 1 --------------------------------------------------------
tee <- func_compare_ttest(df_merge1_s3, 
                          df_merge1_s3$turnover_post, 
                          df_merge1_s3$turnover_pre,
                          "less")

effsize_g <- func_compare_effsize(df_merge1_s3, 
                                  df_merge1_s3$turnover_post, 
                                  df_merge1_s3$turnover_pre,
                                  "less")
effsize_d <- func_compare_effsize(df_merge1_s3, 
                                  df_merge1_s3$turnover_post, 
                                  df_merge1_s3$turnover_pre,
                                  "less")

pairwise <- func_compare_writetable(pairwise, tee, effsize_g, effsize_d, 
                                    "1")


# Store p-value separatly (for adjustment)
peace[1] <- tee[["p.value"]]



# C.14.3.2 Treatment 2 --------------------------------------------------------
tee <- func_compare_ttest(df_merge2_s3, 
                          df_merge2_s3$turnover_post, 
                          df_merge2_s3$turnover_pre,
                          "less")

effsize_g <- func_compare_effsize(df_merge2_s3, 
                                  df_merge2_s3$turnover_post, 
                                  df_merge2_s3$turnover_pre,
                                  "less")
effsize_d <- func_compare_effsize(df_merge2_s3, 
                                  df_merge2_s3$turnover_post, 
                                  df_merge2_s3$turnover_pre,
                                  "less")

pairwise <- func_compare_writetable(pairwise, tee, effsize_g, effsize_d, 
                                    "2")

# Store p-value separatly (for adjustment)
peace[2] <- tee[["p.value"]]




# C.14.3.3 Treatment 3 --------------------------------------------------------
tee <- func_compare_ttest(df_merge3_s3, 
                          df_merge3_s3$turnover_post, 
                          df_merge3_s3$turnover_pre,
                          "less")

effsize_g <- func_compare_effsize(df_merge3_s3, 
                                  df_merge3_s3$turnover_post, 
                                  df_merge3_s3$turnover_pre,
                                  "less")
effsize_d <- func_compare_effsize(df_merge3_s3, 
                                  df_merge3_s3$turnover_post, 
                                  df_merge3_s3$turnover_pre,
                                  "less")

pairwise <- func_compare_writetable(pairwise, tee, effsize_g, effsize_d, 
                                    "3")

# Store p-value separatly (for adjustment)
peace[3] <- tee[["p.value"]]



# C.14.3.4 Treatment 4 --------------------------------------------------------
tee <- func_compare_ttest(df_merge4_s3, 
                          df_merge4_s3$turnover_post, 
                          df_merge4_s3$turnover_pre,
                          "less")

effsize_g <- func_compare_effsize(df_merge4_s3, 
                                  df_merge4_s3$turnover_post, 
                                  df_merge4_s3$turnover_pre,
                                  "less")
effsize_d <- func_compare_effsize(df_merge4_s3, 
                                  df_merge4_s3$turnover_post, 
                                  df_merge4_s3$turnover_pre,
                                  "less")

pairwise <- func_compare_writetable(pairwise, tee, effsize_g, effsize_d, 
                                    "4")

# Store p-value separatly (for adjustment)
peace[4] <- tee[["p.value"]]



# C.14.3.5 Adjusted p-values --------------------------------------------------
padjust <- p.adjust(peace, method = "BH")

pairwise <- bind_cols(pairwise, as_tibble(padjust))
pairwise <- pairwise %>% rename(p.adjust = value)

pairwise <- pairwise %>% mutate(star = ifelse(p.adjust < 0.05, "*", ""))

pairwise_turnover_time_s3 <- pairwise












# C.14.4 Group comparison will ================================================
# empty vector to store p-values
peace <- vector(mode = "numeric", length = 4)

pairwise <- func_empty_resultstable()

# C.14.4.1 Treatment 1 --------------------------------------------------------
tee <- func_compare_ttest(df_merge1_s3, 
                          df_merge1_s3$willingnessjobrec_post, 
                          df_merge1_s3$willingnessjobrec_pre,
                          "greater")

effsize_g <- func_compare_effsize(df_merge1_s3, 
                                  df_merge1_s3$willingnessjobrec_post, 
                                  df_merge1_s3$willingnessjobrec_pre,
                                  "greater")
effsize_d <- func_compare_effsize(df_merge1_s3, 
                                  df_merge1_s3$willingnessjobrec_post, 
                                  df_merge1_s3$willingnessjobrec_pre,
                                  "greater")

pairwise <- func_compare_writetable(pairwise, tee, effsize_g, effsize_d, 
                                    "1")


# Store p-value separatly (for adjustment)
peace[1] <- tee[["p.value"]]



# C.14.4.2 Treatment 2 --------------------------------------------------------
tee <- func_compare_ttest(df_merge2_s3, 
                          df_merge2_s3$willingnessjobrec_post, 
                          df_merge2_s3$willingnessjobrec_pre,
                          "greater")

effsize_g <- func_compare_effsize(df_merge2_s3, 
                                  df_merge2_s3$willingnessjobrec_post, 
                                  df_merge2_s3$willingnessjobrec_pre,
                                  "greater")
effsize_d <- func_compare_effsize(df_merge2_s3, 
                                  df_merge2_s3$willingnessjobrec_post, 
                                  df_merge2_s3$willingnessjobrec_pre,
                                  "greater")

pairwise <- func_compare_writetable(pairwise, tee, effsize_g, effsize_d, 
                                    "2")

# Store p-value separatly (for adjustment)
peace[2] <- tee[["p.value"]]




# C.14.4.3 Treatment 3 --------------------------------------------------------
tee <- func_compare_ttest(df_merge3_s3, 
                          df_merge3_s3$willingnessjobrec_post, 
                          df_merge3_s3$willingnessjobrec_pre,
                          "greater")

effsize_g <- func_compare_effsize(df_merge3_s3, 
                                  df_merge3_s3$willingnessjobrec_post, 
                                  df_merge3_s3$willingnessjobrec_pre,
                                  "greater")
effsize_d <- func_compare_effsize(df_merge3_s3, 
                                  df_merge3_s3$willingnessjobrec_post, 
                                  df_merge3_s3$willingnessjobrec_pre,
                                  "greater")

pairwise <- func_compare_writetable(pairwise, tee, effsize_g, effsize_d, 
                                    "3")

# Store p-value separatly (for adjustment)
peace[3] <- tee[["p.value"]]



# C.14.4.4 Treatment 4 -------------------------------------------------------- 
tee <- func_compare_ttest(df_merge4_s3, 
                          df_merge4_s3$willingnessjobrec_post, 
                          df_merge4_s3$willingnessjobrec_pre,
                          "greater")

effsize_g <- func_compare_effsize(df_merge4_s3, 
                                  df_merge4_s3$willingnessjobrec_post, 
                                  df_merge4_s3$willingnessjobrec_pre,
                                  "greater")
effsize_d <- func_compare_effsize(df_merge4_s3, 
                                  df_merge4_s3$willingnessjobrec_post, 
                                  df_merge4_s3$willingnessjobrec_pre,
                                  "greater")

pairwise <- func_compare_writetable(pairwise, tee, effsize_g, effsize_d, 
                                    "4")

# Store p-value separatly (for adjustment)
peace[4] <- tee[["p.value"]]



# C.14.4.5 Adjusted p-values --------------------------------------------------
padjust <- p.adjust(peace, method = "BH")

pairwise <- bind_cols(pairwise, as_tibble(padjust))
pairwise <- pairwise %>% rename(p.adjust = value)

pairwise <- pairwise %>% mutate(star = ifelse(p.adjust < 0.05, "*", ""))

pairwise_will_time_s3 <- pairwise




# remove unused objects
rm(effsize_d, effsize_g, pairwise, tee, padjust, peace)



pairwise_paffect_time_s3
pairwise_naffect_time_s3
pairwise_turnover_time_s3
pairwise_will_time_s3







# C.15 Results table paired t-tests ###########################################
pairwise_paffect_time2_s3 <- 
  pairwise_paffect_time_s3 %>%
  mutate(var = "paffect") %>%
  mutate(test = paste0("t(",
                       myround1(df),
                       ") = ",
                       myround2(t),
                       ", ", pformat(p.adjust))) %>%
  select(var, comparison, mean.diff, test, eff_size_d)

pairwise_naffect_time2_s3 <- 
  pairwise_naffect_time_s3 %>%
  mutate(var = "naffect") %>%
  mutate(test = paste0("t(",
                       myround1(df),
                       ") = ",
                       myround2(t),
                       ", ", pformat(p.adjust))) %>%
  select(var, comparison, mean.diff, test, eff_size_d)

pairwise_turnover_time2_s3 <- 
  pairwise_turnover_time_s3 %>%
  mutate(var = "turnover") %>%
  mutate(test = paste0("t(",
                       myround1(df),
                       ") = ",
                       myround2(t),
                       ", ", pformat(p.adjust))) %>%
  select(var, comparison, mean.diff, test, eff_size_d)

pairwise_will_time2_s3 <- 
  pairwise_will_time_s3 %>%
  mutate(var = "will") %>%
  mutate(test = paste0("t(",
                       myround1(df),
                       ") = ",
                       myround2(t),
                       ", ", pformat(p.adjust))) %>%
  select(var, comparison, mean.diff, test, eff_size_d)

pairwise2_s3 <- bind_rows(pairwise_paffect_time2_s3, 
                          pairwise_naffect_time2_s3,
                          pairwise_turnover_time2_s3, 
                          pairwise_will_time2_s3)
pairwise2_s3
pairwise3_s3 <- pairwise2_s3 %>% 
  select(-test, -eff_size_d) %>% 
  mutate(mean.diff = 
           as.character(broman::myround(mean.diff,2))) %>%
  spread(comparison, mean.diff) %>%
  mutate(value = "mean.diff")

pairwise4_s3 <- pairwise2_s3 %>% 
  select(-mean.diff, -test) %>%
  #mutate(eff_size_d = abs(eff_size_d)) %>% # absolute effect size
  mutate(eff_size_d = 
           as.character(broman::myround(eff_size_d,2))) %>%
  spread(comparison, eff_size_d) %>%
  mutate(value = "eff_size_d")

pairwise5_s3 <- pairwise2_s3 %>% 
  select(-mean.diff, -eff_size_d) %>%
  spread(comparison, test) %>%
  mutate(value = "test")

results_table_time_s3 <- bind_rows(pairwise3_s3, pairwise4_s3, pairwise5_s3) %>%
  select(var, value, everything()) %>% 
  mutate(var = factor(var,
                      levels = c("paffect",
                                 "naffect",
                                 "turnover",
                                 "will"))) %>%
  mutate(value = factor(value, 
                        levels = c("mean.diff",
                                   "eff_size_d",
                                   "test"))) %>%
  arrange(var, value)

results_table_time_s3

rm(pairwise2_s3, pairwise3_s3, pairwise4_s3, pairwise5_s3,
   pairwise_paffect_time2_s3, pairwise_naffect_time2_s3,
   pairwise_turnover_time2_s3, pairwise_will_time2_s3)

results_table2_time_s3 <- results_table_time_s3 %>%
  mutate(var = as.character(var)) %>%
  mutate(var = ifelse(value == "eff_size_d",
                      NA, var),
         var = ifelse(value == "test",
                      NA, var)) %>%
  mutate(var = factor(var,
                      levels = c("paffect", "naffect", "turnover", "will"),
                      labels= c("Positive Affect", 
                                "Negative Affect", 
                                "Turnover Intention",
                                "Will. to rec. job"))) %>%
  mutate(value = factor(value,
                        labels = c("Mean diff.",
                                   "Cohan's d",
                                   "Welch t-test"))) #%>%
#mutate(var = as.character(var)) %>%

results_table2_time_s3







# C.16 Plot Time ##############################################################
# Positive affect
timeplot_paffect <- time_plot_func("paffect_pre", 
                                   "paffect_post",
                                   "Positive affect", 
                                   adjust = "cm")
timeplot_naffect <- time_plot_func("naffect_pre",
                                   "naffect_post",
                                   "Negative affect", 
                                   adjust = "cm")
timeplot_turnover <- time_plot_func("turnover_pre",
                                    "turnover_post",
                                    "Turnover intention",
                                    adjust = "cm")
timeplot_will <- time_plot_func("willingnessjobrec_pre", 
                                "willingnessjobrec_post", 
                                "Willingness to recommend job", 
                                adjust = "cm")

timeplot_paffect
timeplot_naffect
timeplot_turnover
timeplot_will


# C.17 Correlation table ######################################################
# Prepare data
df_s3_cor <- df_merge_s3 %>%
  select(paffect_pre, paffect_post,
         naffect_pre, naffect_post,
         turnover_pre, turnover_post,
         willingnessjobrec_pre, willingnessjobrec_post,
         prosocialimpact_pre, prosocialimpact_post,
         societalimpact_pre, societalimpact_post)
df_s3_cor <- as.data.frame(df_s3_cor)

corstars(df_s3_cor, 
         method="pearson", 
         removeTriangle="upper",
         result="html",
         cap = "Correlations matrix", 
         filename = here("analysis", "corr_s3.html"),
         labels_rows = c("(1) Positive Affect (pre)", 
                         "(2) Positive Affect (post)", 
                         "(3) Negative Affect (pre)", 
                         "(4) Negative Affect (post)", 
                         "(5) Turnover Int. (pre)", 
                         "(6) Turnover Int. (pre)", 
                         "(7) Will. to recommend job (pre)", 
                         "(8) Will. to recommend job (post)", 
                         "(9) Prosocial Impact (pre)",
                         "(10) Prosocial Impact (ppost)",
                         "(11) Societal Impact (pre)",
                         "(12) Societal Impact (post)"),
         labels_cols = 1:11)



# C.18 Differences betwwen groups with respect to contact with bene ###########



ttest_bene_s1_13 <- t.test(df_merge1_3$beneficiaries ~ 
                             df_merge1_3$treatment)
ttest_bene_s1_14 <- t.test(df_merge1_4$beneficiaries ~ 
                             df_merge1_4$treatment)
ttest_bene_s2_13 <- t.test(df_merge1_3_s2$beneficiaries ~ 
                             df_merge1_3_s2$treatment)
ttest_bene_s2_14 <- t.test(df_merge1_4_s2$beneficiaries ~ 
                             df_merge1_4_s2$treatment)
ttest_bene_s3_13 <- t.test(df_merge1_3_s3$beneficiaries ~ 
                             df_merge1_3_s3$treatment)
ttest_bene_s3_14 <- t.test(df_merge1_4_s3$beneficiaries ~ 
                             df_merge1_4_s3$treatment)

table_ttest_bene <- tibble(Study = c("Study 1", "Study 3"),
                           prosocial = c(ttest_bene_s1_13[["p.value"]],
                                   ttest_bene_s3_13[["p.value"]]),
                           societal = c(ttest_bene_s1_14[["p.value"]],
                                   ttest_bene_s3_14[["p.value"]]))


# write_csv(table_ttest_bene, "table_bene.csv")

