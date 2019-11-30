# Forest plot prosocial



# Effect sizes ################################################################

# Positive Affect =============================================================
eff_size_paffect_psi_passive_meta <- func_meta_effsize(
  pairwise1 = pairwise_paffect,
  pairwise2 = pairwise_paffect_s2,
  pairwise3 = pairwise_paffect_post_s3,
  control = "passive",
  treat = "prosocial")

eff_size_paffect_psi_passive <- c(
  as.numeric(pairwise_paffect$eff_size_d[1]),
  as.numeric(pairwise_paffect_s2$eff_size_d[1]),
  as.numeric(pairwise_paffect_post_s3$eff_size_d[1]),
  as.numeric(eff_size_paffect_psi_passive_meta), 
  pairwise_paffect_time_s3$eff_size_d[3]
)

eff_size_paffect_psi_active_meta <- func_meta_effsize(
  pairwise1 = pairwise_paffect,
  pairwise2 = pairwise_paffect_s2,
  pairwise3 = pairwise_paffect_post_s3,
  control = "active",
  treat = "prosocial")

eff_size_paffect_psi_active <- c(
  as.numeric(pairwise_paffect$eff_size_d[3]),
  as.numeric(pairwise_paffect_s2$eff_size_d[3]),
  as.numeric(pairwise_paffect_post_s3$eff_size_d[3]),
  as.numeric(eff_size_paffect_psi_active_meta), 
  NA
)

# Negative Affect =============================================================
eff_size_naffect_psi_passive_meta <- func_meta_effsize(
  pairwise1 = pairwise_naffect,
  pairwise2 = pairwise_naffect_s2,
  pairwise3 = pairwise_naffect_post_s3,
  control = "passive",
  treat = "prosocial")

eff_size_naffect_psi_passive <- c(
  as.numeric(pairwise_naffect$eff_size_d[1]),
  as.numeric(pairwise_naffect_s2$eff_size_d[1]),
  as.numeric(pairwise_naffect_post_s3$eff_size_d[1]),
  as.numeric(eff_size_naffect_psi_passive_meta), 
  pairwise_naffect_time_s3$eff_size_d[3]
)

eff_size_naffect_psi_active_meta <- func_meta_effsize(
  pairwise1 = pairwise_naffect,
  pairwise2 = pairwise_naffect_s2,
  pairwise3 = pairwise_naffect_post_s3,
  control = "active",
  treat = "prosocial")

eff_size_naffect_psi_active <- c(
  as.numeric(pairwise_naffect$eff_size_d[3]),
  as.numeric(pairwise_naffect_s2$eff_size_d[3]),
  as.numeric(pairwise_naffect_post_s3$eff_size_d[3]),
  as.numeric(eff_size_naffect_psi_active_meta), 
  NA
)


# Turnover intention ==========================================================
eff_size_turnover_psi_passive_meta <- func_meta_effsize(
  pairwise1 = pairwise_turnover,
  pairwise2 = pairwise_turnover_s2,
  pairwise3 = pairwise_turnover_post_s3,
  control = "passive",
  treat = "prosocial")

eff_size_turnover_psi_passive <- c(
  as.numeric(pairwise_turnover$eff_size_d[1]),
  as.numeric(pairwise_turnover_s2$eff_size_d[1]),
  as.numeric(pairwise_turnover_post_s3$eff_size_d[1]),
  as.numeric(eff_size_turnover_psi_passive_meta), 
  pairwise_turnover_time_s3$eff_size_d[3]
)

eff_size_turnover_psi_active_meta <- func_meta_effsize(
  pairwise1 = pairwise_turnover,
  pairwise2 = pairwise_turnover_s2,
  pairwise3 = pairwise_turnover_post_s3,
  control = "active",
  treat = "prosocial")

eff_size_turnover_psi_active <- c(
  as.numeric(pairwise_turnover$eff_size_d[3]),
  as.numeric(pairwise_turnover_s2$eff_size_d[3]),
  as.numeric(pairwise_turnover_post_s3$eff_size_d[3]),
  as.numeric(eff_size_turnover_psi_active_meta), 
  NA
)


# Willingness to rec. job  ====================================================
eff_size_will_psi_passive_meta <- func_meta_effsize(
  pairwise1 = pairwise_will,
  pairwise2 = pairwise_will_s2,
  pairwise3 = pairwise_will_post_s3,
  control = "passive",
  treat = "prosocial")

eff_size_will_psi_passive <- c(
  as.numeric(pairwise_will$eff_size_d[1]),
  as.numeric(pairwise_will_s2$eff_size_d[1]),
  as.numeric(pairwise_will_post_s3$eff_size_d[1]),
  as.numeric(eff_size_will_psi_passive_meta), 
  pairwise_will_time_s3$eff_size_d[3]
)

eff_size_will_psi_active_meta <- func_meta_effsize(
  pairwise1 = pairwise_will,
  pairwise2 = pairwise_will_s2,
  pairwise3 = pairwise_will_post_s3,
  control = "active",
  treat = "prosocial")

eff_size_will_psi_active <- c(
  as.numeric(pairwise_will$eff_size_d[3]),
  as.numeric(pairwise_will_s2$eff_size_d[3]),
  as.numeric(pairwise_will_post_s3$eff_size_d[3]),
  as.numeric(eff_size_will_psi_active_meta), 
  NA
)


# Upper CI ####################################################################
# Positive Affect =============================================================
upper_paffect_psi_passive <- c(
  func_upper_ci_t(pairwise = pairwise_paffect,
                  obs = obs,
                  control = "passive",
                  treat = "prosocial"),
  func_upper_ci_t(pairwise = pairwise_paffect_s2,
                  obs = obs_s2,
                  control = "passive",
                  treat = "prosocial"),
  func_upper_ci_t(pairwise = pairwise_paffect_post_s3,
                  obs = obs_s3,
                  control = "passive",
                  treat = "prosocial"),
  func_meta_upper(pairwise1 = pairwise_paffect,
                  pairwise2 = pairwise_paffect_s2,
                  pairwise3 = pairwise_paffect_post_s3,
                  control = "passive",
                  treat = "prosocial"), 
  func_upper_ci_t_paired(pairwise = pairwise_paffect_time_s3,
                         treat = "prosocial")
)

upper_paffect_psi_active <- c(
  func_upper_ci_t(pairwise = pairwise_paffect,
                  obs = obs,
                  control = "active",
                  treat = "prosocial"),
  func_upper_ci_t(pairwise = pairwise_paffect_s2,
                  obs = obs_s2,
                  control = "active",
                  treat = "prosocial"),
  func_upper_ci_t(pairwise = pairwise_paffect_post_s3,
                  obs = obs_s3,
                  control = "active",
                  treat = "prosocial"),
  func_meta_upper(pairwise1 = pairwise_paffect,
                  pairwise2 = pairwise_paffect_s2,
                  pairwise3 = pairwise_paffect_post_s3,
                  control = "active",
                  treat = "prosocial"), 
  NA
)



# Negative Affect =============================================================
upper_naffect_psi_passive <- c(
  func_upper_ci_t(pairwise = pairwise_naffect,
                  obs = obs,
                  control = "passive",
                  treat = "prosocial"),
  func_upper_ci_t(pairwise = pairwise_naffect_s2,
                  obs = obs_s2,
                  control = "passive",
                  treat = "prosocial"),
  func_upper_ci_t(pairwise = pairwise_naffect_post_s3,
                  obs = obs_s3,
                  control = "passive",
                  treat = "prosocial"),
  func_meta_upper(pairwise1 = pairwise_naffect,
                  pairwise2 = pairwise_naffect_s2,
                  pairwise3 = pairwise_naffect_post_s3,
                  control = "passive",
                  treat = "prosocial"), 
  func_upper_ci_t_paired(pairwise = pairwise_naffect_time_s3,
                         treat = "prosocial")
)

upper_naffect_psi_active <- c(
  func_upper_ci_t(pairwise = pairwise_naffect,
                  obs = obs,
                  control = "active",
                  treat = "prosocial"),
  func_upper_ci_t(pairwise = pairwise_naffect_s2,
                  obs = obs_s2,
                  control = "active",
                  treat = "prosocial"),
  func_upper_ci_t(pairwise = pairwise_naffect_post_s3,
                  obs = obs_s3,
                  control = "active",
                  treat = "prosocial"),
  func_meta_upper(pairwise1 = pairwise_naffect,
                  pairwise2 = pairwise_naffect_s2,
                  pairwise3 = pairwise_naffect_post_s3,
                  control = "active",
                  treat = "prosocial"), 
  NA
)



# Turnover =============================================================
upper_turnover_psi_passive <- c(
  func_upper_ci_t(pairwise = pairwise_turnover,
                  obs = obs,
                  control = "passive",
                  treat = "prosocial"),
  func_upper_ci_t(pairwise = pairwise_turnover_s2,
                  obs = obs_s2,
                  control = "passive",
                  treat = "prosocial"),
  func_upper_ci_t(pairwise = pairwise_turnover_post_s3,
                  obs = obs_s3,
                  control = "passive",
                  treat = "prosocial"),
  func_meta_upper(pairwise1 = pairwise_turnover,
                  pairwise2 = pairwise_turnover_s2,
                  pairwise3 = pairwise_turnover_post_s3,
                  control = "passive",
                  treat = "prosocial"), 
  func_upper_ci_t_paired(pairwise = pairwise_turnover_time_s3,
                         treat = "prosocial")
)

upper_turnover_psi_active <- c(
  func_upper_ci_t(pairwise = pairwise_turnover,
                  obs = obs,
                  control = "active",
                  treat = "prosocial"),
  func_upper_ci_t(pairwise = pairwise_turnover_s2,
                  obs = obs_s2,
                  control = "active",
                  treat = "prosocial"),
  func_upper_ci_t(pairwise = pairwise_turnover_post_s3,
                  obs = obs_s3,
                  control = "active",
                  treat = "prosocial"),
  func_meta_upper(pairwise1 = pairwise_turnover,
                  pairwise2 = pairwise_turnover_s2,
                  pairwise3 = pairwise_turnover_post_s3,
                  control = "active",
                  treat = "prosocial"), 
  NA
)



# Will ========================================================================
upper_will_psi_passive <- c(
  func_upper_ci_t(pairwise = pairwise_will,
                  obs = obs,
                  control = "passive",
                  treat = "prosocial"),
  func_upper_ci_t(pairwise = pairwise_will_s2,
                  obs = obs_s2,
                  control = "passive",
                  treat = "prosocial"),
  func_upper_ci_t(pairwise = pairwise_will_post_s3,
                  obs = obs_s3,
                  control = "passive",
                  treat = "prosocial"),
  func_meta_upper(pairwise1 = pairwise_will,
                  pairwise2 = pairwise_will_s2,
                  pairwise3 = pairwise_will_post_s3,
                  control = "passive",
                  treat = "prosocial"), 
  func_upper_ci_t_paired(pairwise = pairwise_will_time_s3,
                         treat = "prosocial")
)

upper_will_psi_active <- c(
  func_upper_ci_t(pairwise = pairwise_will,
                  obs = obs,
                  control = "active",
                  treat = "prosocial"),
  func_upper_ci_t(pairwise = pairwise_will_s2,
                  obs = obs_s2,
                  control = "active",
                  treat = "prosocial"),
  func_upper_ci_t(pairwise = pairwise_will_post_s3,
                  obs = obs_s3,
                  control = "active",
                  treat = "prosocial"),
  func_meta_upper(pairwise1 = pairwise_will,
                  pairwise2 = pairwise_will_s2,
                  pairwise3 = pairwise_will_post_s3,
                  control = "active",
                  treat = "prosocial"), 
  NA
)


# Lower CI ####################################################################
# Positive Affect =============================================================
lower_paffect_psi_passive <- c(
  func_lower_ci_t(pairwise = pairwise_paffect,
                  obs = obs,
                  control = "passive",
                  treat = "prosocial"),
  func_lower_ci_t(pairwise = pairwise_paffect_s2,
                  obs = obs_s2,
                  control = "passive",
                  treat = "prosocial"),
  func_lower_ci_t(pairwise = pairwise_paffect_post_s3,
                  obs = obs_s3,
                  control = "passive",
                  treat = "prosocial"),
  func_meta_lower(pairwise1 = pairwise_paffect,
                  pairwise2 = pairwise_paffect_s2,
                  pairwise3 = pairwise_paffect_post_s3,
                  control = "passive",
                  treat = "prosocial"), 
  func_lower_ci_t_paired(pairwise = pairwise_paffect_time_s3,
                         treat = "prosocial")
)

lower_paffect_psi_active <- c(
  func_lower_ci_t(pairwise = pairwise_paffect,
                  obs = obs,
                  control = "active",
                  treat = "prosocial"),
  func_lower_ci_t(pairwise = pairwise_paffect_s2,
                  obs = obs_s2,
                  control = "active",
                  treat = "prosocial"),
  func_lower_ci_t(pairwise = pairwise_paffect_post_s3,
                  obs = obs_s3,
                  control = "active",
                  treat = "prosocial"),
  func_meta_lower(pairwise1 = pairwise_paffect,
                  pairwise2 = pairwise_paffect_s2,
                  pairwise3 = pairwise_paffect_post_s3,
                  control = "active",
                  treat = "prosocial"), 
  NA
)


# Negative Affect =============================================================
lower_naffect_psi_passive <- c(
  func_lower_ci_t(pairwise = pairwise_naffect,
                  obs = obs,
                  control = "passive",
                  treat = "prosocial"),
  func_lower_ci_t(pairwise = pairwise_naffect_s2,
                  obs = obs_s2,
                  control = "passive",
                  treat = "prosocial"),
  func_lower_ci_t(pairwise = pairwise_naffect_post_s3,
                  obs = obs_s3,
                  control = "passive",
                  treat = "prosocial"),
  func_meta_lower(pairwise1 = pairwise_naffect,
                  pairwise2 = pairwise_naffect_s2,
                  pairwise3 = pairwise_naffect_post_s3,
                  control = "passive",
                  treat = "prosocial"), 
  func_lower_ci_t_paired(pairwise = pairwise_naffect_time_s3,
                         treat = "prosocial")
)

lower_naffect_psi_active <- c(
  func_lower_ci_t(pairwise = pairwise_naffect,
                  obs = obs,
                  control = "active",
                  treat = "prosocial"),
  func_lower_ci_t(pairwise = pairwise_naffect_s2,
                  obs = obs_s2,
                  control = "active",
                  treat = "prosocial"),
  func_lower_ci_t(pairwise = pairwise_naffect_post_s3,
                  obs = obs_s3,
                  control = "active",
                  treat = "prosocial"),
  func_meta_lower(pairwise1 = pairwise_naffect,
                  pairwise2 = pairwise_naffect_s2,
                  pairwise3 = pairwise_naffect_post_s3,
                  control = "active",
                  treat = "prosocial"), 
  NA
)



# Turnover ====================================================================
lower_turnover_psi_passive <- c(
  func_lower_ci_t(pairwise = pairwise_turnover,
                  obs = obs,
                  control = "passive",
                  treat = "prosocial"),
  func_lower_ci_t(pairwise = pairwise_turnover_s2,
                  obs = obs_s2,
                  control = "passive",
                  treat = "prosocial"),
  func_lower_ci_t(pairwise = pairwise_turnover_post_s3,
                  obs = obs_s3,
                  control = "passive",
                  treat = "prosocial"),
  func_meta_lower(pairwise1 = pairwise_turnover,
                  pairwise2 = pairwise_turnover_s2,
                  pairwise3 = pairwise_turnover_post_s3,
                  control = "passive",
                  treat = "prosocial"), 
  func_lower_ci_t_paired(pairwise = pairwise_turnover_time_s3,
                         treat = "prosocial")
)

lower_turnover_psi_active <- c(
  func_lower_ci_t(pairwise = pairwise_turnover,
                  obs = obs,
                  control = "active",
                  treat = "prosocial"),
  func_lower_ci_t(pairwise = pairwise_turnover_s2,
                  obs = obs_s2,
                  control = "active",
                  treat = "prosocial"),
  func_lower_ci_t(pairwise = pairwise_turnover_post_s3,
                  obs = obs_s3,
                  control = "active",
                  treat = "prosocial"),
  func_meta_lower(pairwise1 = pairwise_turnover,
                  pairwise2 = pairwise_turnover_s2,
                  pairwise3 = pairwise_turnover_post_s3,
                  control = "active",
                  treat = "prosocial"), 
  NA
)





# Will ========================================================================
lower_will_psi_passive <- c(
  func_lower_ci_t(pairwise = pairwise_will,
                  obs = obs,
                  control = "passive",
                  treat = "prosocial"),
  func_lower_ci_t(pairwise = pairwise_will_s2,
                  obs = obs_s2,
                  control = "passive",
                  treat = "prosocial"),
  func_lower_ci_t(pairwise = pairwise_will_post_s3,
                  obs = obs_s3,
                  control = "passive",
                  treat = "prosocial"),
  func_meta_lower(pairwise1 = pairwise_will,
                  pairwise2 = pairwise_will_s2,
                  pairwise3 = pairwise_will_post_s3,
                  control = "passive",
                  treat = "prosocial"), 
  func_lower_ci_t_paired(pairwise = pairwise_will_time_s3,
                         treat = "prosocial")
)

lower_will_psi_active <- c(
  func_lower_ci_t(pairwise = pairwise_will,
                  obs = obs,
                  control = "active",
                  treat = "prosocial"),
  func_lower_ci_t(pairwise = pairwise_will_s2,
                  obs = obs_s2,
                  control = "active",
                  treat = "prosocial"),
  func_lower_ci_t(pairwise = pairwise_will_post_s3,
                  obs = obs_s3,
                  control = "active",
                  treat = "prosocial"),
  func_meta_lower(pairwise1 = pairwise_will,
                  pairwise2 = pairwise_will_s2,
                  pairwise3 = pairwise_will_post_s3,
                  control = "active",
                  treat = "prosocial"), 
  NA
)







# Combine everything ##########################################################
mean  <- cbind(c(NA, # Passive Control
                 eff_size_paffect_psi_passive,  # paffect
                 eff_size_naffect_psi_passive,  # naffect
                 eff_size_turnover_psi_passive,  # turnover
                 eff_size_will_psi_passive), # will
               c(NA, # Active Control
                 eff_size_paffect_psi_active,  # paffect
                 eff_size_naffect_psi_active,  # naffect
                 eff_size_turnover_psi_active,  # turnover
                 eff_size_will_psi_active) # will
)

lower <- cbind(c(NA, # Passive Control
                lower_paffect_psi_passive,  # paffect
                lower_naffect_psi_passive,  # naffect
                lower_turnover_psi_passive,  # turnover
                lower_will_psi_passive), # will
              c(NA, # Active Control
                lower_paffect_psi_active,  # paffect
                lower_naffect_psi_active,  # naffect
                lower_turnover_psi_active,  # turnover
                lower_will_psi_active)  # will
)

upper <- cbind(c(NA, # Passive Control
                 upper_paffect_psi_passive,   # paffect 
                 upper_naffect_psi_passive,   # naffect
                 upper_turnover_psi_passive,   # turnover
                 upper_will_psi_passive), # will
               c(NA, # Active Control
                 upper_paffect_psi_active,   # paffect 
                 upper_naffect_psi_active,   # naffect
                 upper_turnover_psi_active,   # turnover
                 upper_will_psi_active))

rownames(mean) <- c(1:21)
rownames(lower) <- c(1:21)
rownames(upper) <- c(1:21)

# Forestplot ##################################################################

tabletext<-cbind(
  # Every line is one column
  c(1:21),
  c("Variable", "Positive affect", NA, NA, NA, NA, 
    "Negative affect", NA, NA, NA, NA,
    "Turnover intention", NA, NA, NA, NA,
    "Will. to rec. job", NA, NA, NA, NA),
  c(NA, "Study 1", "Study 2", "Study 3 (b)", "Meta between", "Study 3 (w)", 
    "Study 1", "Study 2", "Study 3 (b)", "Meta between", "Study 3 (w)",
    "Study 1", "Study 2", "Study 3 (b)", "Meta between", "Study 3 (w)",
    "Study 1", "Study 2", "Study 3 (b)", "Meta between", "Study 3 (w)"),
  c("d", 
    paste0(myround2(eff_size_paffect_psi_passive[1]), "/",             # paffect
           myround2(eff_size_paffect_psi_active[1])), 
    paste0(myround2(eff_size_paffect_psi_passive[2]), "/",
           myround2(eff_size_paffect_psi_active[2])), 
    paste0(myround2(eff_size_paffect_psi_passive[3]), "/",
           myround2(eff_size_paffect_psi_active[3])),
    paste0(myround2(eff_size_paffect_psi_passive[4]), "/",
           myround2(eff_size_paffect_psi_active[4])),
    paste0(myround2(eff_size_paffect_psi_passive[5])),  
    paste0(myround2(eff_size_naffect_psi_passive[1]), "/",             # naffect
           myround2(eff_size_naffect_psi_active[1])), 
    paste0(myround2(eff_size_naffect_psi_passive[2]), "/",
           myround2(eff_size_naffect_psi_active[2])), 
    paste0(myround2(eff_size_naffect_psi_passive[3]), "/",
           myround2(eff_size_naffect_psi_active[3])),
    paste0(myround2(eff_size_naffect_psi_passive[4]), "/",
           myround2(eff_size_naffect_psi_active[4])),
    paste0(myround2(eff_size_naffect_psi_passive[5])),  
    paste0(myround2(eff_size_turnover_psi_passive[1]), "/",             # turnover
           myround2(eff_size_turnover_psi_active[1])), 
    paste0(myround2(eff_size_turnover_psi_passive[2]), "/",
           myround2(eff_size_turnover_psi_active[2])), 
    paste0(myround2(eff_size_turnover_psi_passive[3]), "/",
           myround2(eff_size_turnover_psi_active[3])),
    paste0(myround2(eff_size_turnover_psi_passive[4]), "/",
           myround2(eff_size_turnover_psi_active[4])),
    paste0(myround2(eff_size_turnover_psi_passive[5])),  
    paste0(myround2(eff_size_will_psi_passive[1]), "/",             # will
           myround2(eff_size_will_psi_active[1])), 
    paste0(myround2(eff_size_will_psi_passive[2]), "/",
           myround2(eff_size_will_psi_active[2])), 
    paste0(myround2(eff_size_will_psi_passive[3]), "/",
           myround2(eff_size_will_psi_active[3])),
    paste0(myround2(eff_size_will_psi_passive[4]), "/",
           myround2(eff_size_will_psi_active[4])),
    paste0(myround2(eff_size_will_psi_passive[5]))),  
  c("BF", 
    paste0(bf_paffect_psi_passive, "/",             # paffect
           bf_paffect_psi_active),
    paste0(bf_paffect_psi_passive_s2, "/",
           bf_paffect_psi_active_s2),
    paste0(bf_paffect_psi_passive_s3, "/",
           bf_paffect_psi_active_s3),
    paste0(bf_paffect_psi_passive_meta, "/", 
           bf_paffect_psi_active_meta), 
    paste0(bf_paired_paffect_psi), 
    paste0(bf_naffect_psi_passive, "/",             # naffect
           bf_naffect_psi_active),
    paste0(bf_naffect_psi_passive_s2, "/",
           bf_naffect_psi_active_s2),
    paste0(bf_naffect_psi_passive_s3, "/",
           bf_naffect_psi_active_s3),
    paste0(bf_naffect_psi_passive_meta, "/", 
           bf_naffect_psi_active_meta), 
    paste0(bf_paired_naffect_psi),
    paste0(bf_turnover_psi_passive, "/",             # turnover
           bf_turnover_psi_active),
    paste0(bf_turnover_psi_passive_s2, "/",
           bf_turnover_psi_active_s2),
    paste0(bf_turnover_psi_passive_s3, "/",
           bf_turnover_psi_active_s3),
    paste0(bf_turnover_psi_passive_meta, "/", 
           bf_turnover_psi_active_meta), 
    paste0(bf_paired_turnover_psi),
    paste0(bf_will_psi_passive, "/",             # will
           bf_will_psi_active),
    paste0(bf_will_psi_passive_s2, "/",
           bf_will_psi_active_s2),
    paste0(bf_will_psi_passive_s3, "/",
           bf_will_psi_active_s3),
    paste0(bf_will_psi_passive_meta, "/", 
           bf_will_psi_active_meta), 
    paste0(bf_paired_will_psi)
    ))


# Add lines between variables
lines <- list(NULL, 
              TRUE, NULL, NULL, NULL, NULL, 
              TRUE, NULL, NULL, NULL, NULL,
              TRUE, NULL, NULL, NULL, NULL,
              TRUE, NULL, NULL, NULL, NULL, NULL)

forestplot::forestplot(tabletext,
                       fn.ci_norm = c(fpDrawNormalCI, fpDrawCircleCI),
                       mean = mean,
                       upper = upper,
                       lower = lower, new_page = TRUE,
                       # Which rows are Summary
                       is.summary=c(TRUE, 
                                    FALSE, FALSE, FALSE, TRUE, FALSE,
                                    FALSE, FALSE, FALSE, TRUE, FALSE,
                                    FALSE, FALSE, FALSE, TRUE, FALSE,
                                    FALSE, FALSE, FALSE, TRUE, FALSE),
                       clip=c(-2,2), 
                       hrzl_lines = lines,
                       boxsize = .25,
                       graphwidth = unit(5, "cm"),
                       lineheight = unit(8.5, "mm"),
                       line.margin = unit(10, "mm"),
                       colgap = unit(4, "mm"),
                       txt_gp = fpTxtGp(label = list(gpar(cex = 1))),
                       xlog = FALSE,
                       col = fpColors(box = c("#1380A1", "#990000"),
                                      lines = c("gray", "gray"),
                                      summary = c("#1380A1", "#990000")),
                       legend = c("Passive control", "Active control"),
                       xticks = c(-1, -0.5, 0, .5, 1)
)
