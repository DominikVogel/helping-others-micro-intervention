###############################################################################
################### Custom functions ##########################################
###############################################################################

# 1. Data management ##########################################################

# 1.1 Build mean indices from multiple variables ==============================
mean_index <- function (df, name, vars) {
  M1 <- dplyr::select(df, vars)
  M2 <- rowMeans(M1, na.rm = TRUE)
  M2 <- tibble::tibble(M2)
  colnames(M2) <- name
  df <- dplyr::bind_cols(df, M2)
  return(df)
}


# 1.2 Fix variable names from LimeSurvey (lime_names) =========================
lime_names <- function (df) {
  df <- stats::setNames(df, gsub(".*\\[(.+)\\]", "\\1", names(df)))
  return(df)
}


# 1.3 Print all columns of a tibble (tibble_print_vars) =======================
tibble_print_vars <- function (tibble) 
{
  default <- getOption("tibble.width")
  options(tibble.width = Inf)
  print(tibble)
  options(tibble.width = default)
}

# 2. Stats ####################################################################

# 2.1 Calculate standard error (se_func) ======================================
se_func <- function(var) {
  sd <- sd(var)
  n <- length(var)
  se <- sd / sqrt(n)
  return(se)
}

# 2.2. Calculate confidence interval (lower_ci_func, upper_ci_func) ===========

# Calculate confidence interval (lower bound)
lower_ci_func <- function(var) {
  m <- mean(var)
  sd <- sd(var)
  n <- length(var)
  se <- sd / sqrt(n)
  lower_ci <- m - qt(1 - (0.05 / 2), n - 1) * se
  return(lower_ci)
}

# Calculate confidence interval (upper bound)
upper_ci_func <- function(var) {
  m <- mean(var)
  sd <- sd(var)
  n <- length(var)
  se <- sd / sqrt(n)
  lower_ci <- m + qt(1 - (0.05 / 2), n - 1) * se
  return(lower_ci)
}


# 2.3. Adjusted confidence intervals for within-design ========================
# Adjustment procedure by Cousineau-Morey
# R adotion by Baguley, T. (2012).
#    Calculating and graphing within-subject confidence intervals for ANOVA. 
#    Behavior Research Methods, 44(1), 158–175. 
#    https://doi.org/10.3758/s13428-011-0123-7
# https://www.r-bloggers.com/calculating-and-graphing-within-subject-confidence-intervals-for-anova/

cm.ci <- function(data.frame, conf.level = 0.95, difference = FALSE) {
  #cousineau-morey within-subject CIs
  k = ncol(data.frame)
  if (difference == TRUE) 
    diff.factor = 2^0.5/2
  else diff.factor = 1
  n <- nrow(data.frame)
  df.stack <- stack(data.frame)
  index <- rep(1:n, k)
  p.means <- tapply(df.stack$values, index, mean)
  norm.df <- data.frame - p.means + (sum(data.frame)/(n * k))
  t.mat <- matrix(NA, k, 1)
  mean.mat <- matrix(NA, k, 1)
  for (i in 1:k) t.mat[i, ] <- t.test(norm.df[i])$statistic[1]
  for (i in 1:k) mean.mat[i, ] <- colMeans(norm.df[i])
  c.factor <- (k/(k - 1))^0.5
  moe.mat <- mean.mat/t.mat * qt(1 - (1 - conf.level)/2, n - 1) * c.factor * 
    diff.factor
  ci.mat <- matrix(NA, k, 2)
  dimnames(ci.mat) <- list(names(data.frame), c("lower", "upper"))
  for (i in 1:k) {
    ci.mat[i, 1] <- mean.mat[i] - moe.mat[i]
    ci.mat[i, 2] <- mean.mat[i] + moe.mat[i]
  }
  ci.mat
}

lm.ci <- function(data.frame, conf.level = 0.95, difference = FALSE) {
  #loftus-masson within-subject CIs
  data.frame <- as.data.frame(data.frame)
  k = ncol(data.frame)
  n <- nrow(data.frame)
  df.stack <- stack(data.frame)
  require(nlme)
  parts <- rep(1:n, k)
  root.ms.error <- lme(values ~ 0 + ind, random = ~1 | parts, 
                       cbind(parts, df.stack))[[6]]
  detach(package:nlme)
  mean.mat <- matrix(NA, k, 1)
  ci.mat <- matrix(NA, k, 2)
  if (difference == TRUE) diff.factor = 2^0.5/2 else diff.factor = 1
  moe <- root.ms.error/n^0.5 * qt(1 - (1 - conf.level)/2, 
                                  (n - 1) * (k - 1)) * diff.factor
  for (i in 1:k) mean.mat[i, ] <- colMeans(data.frame[i])
  for (i in 1:k) {
    ci.mat[i, 1] <- mean.mat[i] - moe
    ci.mat[i, 2] <- mean.mat[i] + moe
  }
  dimnames(ci.mat) <- list(names(data.frame), c("lower", "upper"))
  ci.mat
}



# 2.4 Meta-analytic Bayes factor (func_meta_bf) -------------------------------
func_meta_bf <- function(df1 = pairwise_paffect, 
                         df2 = pairwise_paffect_s2,
                         df3 = pairwise_paffect_post_s3,
                         dv = "paffect", 
                         control = "passive", treat = "prosocial") {
  
  # Get group number for control group (1=passive, 2=active)
  cgroup <- ifelse(control == "passive", 1, 
                   ifelse(control == "active", 2, 0))
  
  # Get group number for contorl group (3=prosocial, 4=societal)
  tgroup <- ifelse(treat == "prosocial", 3, 
                   ifelse(treat == "societal", 4, 0))
  
  # Get line number for t value in pairwise df
  line <- ifelse(cgroup == 1 & tgroup == 3, 1,
                 ifelse(cgroup == 1 & tgroup == 4, 2,
                        ifelse(cgroup == 2 & tgroup == 3, 3,
                               ifelse(cgroup == 2 & tgroup == 4, 4,
                                      0))))
  
  # For one-sided tests: get boundaries
  alternative1 = ifelse(dv == "paffect", 0,
                        ifelse(dv == "naffect", -Inf,
                               ifelse(dv == "turnover", -Inf,
                                      ifelse(dv == "will", 0, 
                                             -Inf))))
  alternative2 = ifelse(dv == "paffect", Inf,
                        ifelse(dv == "naffect", 0,
                               ifelse(dv == "turnover", 0,
                                      ifelse(dv == "will", Inf, 
                                             Inf))))
  
  # Get t values from pairwise df
  t <- c(df1$t[line], df2$t[line], df3$t[line])
  
  # Get n from obs df
  n1 <- c(obs$final[cgroup+1], obs_s2$final[cgroup+1], obs_s3$final[cgroup+1])
  n2 <- c(obs$final[tgroup+1], obs_s2$final[tgroup+1], obs_s3$final[tgroup+1])
  
  # Calculate meta-analytic Bayes factor
  bf <- BayesFactor::meta.ttestBF(t = t, n1 = n1, n2 = n2,
                                  nullInterval= c(alternative1, alternative2)
  )
  
  bf2 <- BayesFactor::extractBF(bf)
  bf2 <- bf2[1,1]
  bf2 <- broman::myround(bf2, 2)
  return(bf2)
}



# 2.5 Bayes Factor independent t-test (func_ttest_bf) -------------------------
func_ttest_bf <- function(control = df_merge1$paffect, 
                          treat = df_merge3$paffect,
                          dv = "paffect") {
  
  # For one-sided tests: get boundaries
  alternative1 = ifelse(dv == "paffect", 0,
                        ifelse(dv == "naffect", -Inf,
                               ifelse(dv == "turnover", -Inf,
                                      ifelse(dv == "will", 0, 
                                             -Inf))))
  
  alternative2 = ifelse(dv == "paffect", Inf,
                        ifelse(dv == "naffect", 0,
                               ifelse(dv == "turnover", 0,
                                      ifelse(dv == "will", Inf, 
                                             Inf))))
  
  # Calculate  Bayes factor
  bf <- BayesFactor::ttestBF(treat, control,
                             nullInterval = c(alternative1, alternative2),
                             paired = FALSE)
  
  bf2 <- BayesFactor::extractBF(bf)
  bf2 <- bf2[1,1]
  bf2 <- broman::myround(bf2, 2)
  return(bf2)
}


# 2.6 Bayes Factor paired t-test (func_ttest_paired_bf) -----------------------
func_ttest_paired_bf <- function(pre = df_merge1_s3$paffect_pre, 
                                 post = df_merge1_s3$paffect_post,
                                 dv = "paffect") {
  
  # For one-sided tests: get boundaries
  alternative1 = ifelse(dv == "paffect", 0,
                        ifelse(dv == "naffect", -Inf,
                               ifelse(dv == "turnover", -Inf,
                                      ifelse(dv == "will", 0, 
                                             -Inf))))
  
  alternative2 = ifelse(dv == "paffect", Inf,
                        ifelse(dv == "naffect", 0,
                               ifelse(dv == "turnover", 0,
                                      ifelse(dv == "will", Inf, 
                                             Inf))))
  
  # Calculate  Bayes factor
  bf <- BayesFactor::ttestBF(post, pre,
                             nullInterval = c(alternative1, alternative2),
                             paired = TRUE)
  
  bf2 <- BayesFactor::extractBF(bf)
  bf2 <- bf2[1,1]
  bf2 <- broman::myround(bf2, 2)
  return(bf2)
}



# 2.7 Effect sizes for meta-analysis (func_meta_effsize) ######################
func_meta_effsize <- function(pairwise1 = pairwise_paffect,
                              pairwise2 = pairwise_paffect_s2,
                              pairwise3 = pairwise_paffect_post_s3,
                              control = "passive",
                              treat = "prosocial") {
  # Get group number for control group (1=passive, 2=active)
  cgroup <- ifelse(control == "passive", 1, 
                   ifelse(control == "active", 2, 0))
  
  # Get group number for contorl group (3=prosocial, 4=societal)
  tgroup <- ifelse(treat == "prosocial", 3, 
                   ifelse(treat == "societal", 4, 0))
  
  # Get line number for t value in pairwise df
  line <- ifelse(cgroup == 1 & tgroup == 3, 1,
                 ifelse(cgroup == 1 & tgroup == 4, 2,
                        ifelse(cgroup == 2 & tgroup == 3, 3,
                               ifelse(cgroup == 2 & tgroup == 4, 4,
                                      0))))
  
  d <- as.numeric(pairwise1$eff_size_d[line])
  n1 <- as.numeric(obs$final[cgroup+1])
  n2 <- as.numeric(obs$final[tgroup+1])
  meta11 <- (d * (1 / (sqrt((n1 + n2) / (n1 * n2)) + ((d^2) / (2 * (n1 + n2))))^2)) 
  meta21 <- (1 / (sqrt((n1 + n2) / (n1 * n2)) + ((d^2) / (2 * (n1 + n2))))^2) 
  
  d <- as.numeric(pairwise2$eff_size_d[line])
  n1 <- as.numeric(obs_s2$final[cgroup+1])
  n2 <- as.numeric(obs_s2$final[tgroup+1])
  meta12 <- (d * (1 / (sqrt((n1 + n2) / (n1 * n2)) + ((d^2) / (2 * (n1 + n2))))^2)) 
  meta22 <- (1 / (sqrt((n1 + n2) / (n1 * n2)) + ((d^2) / (2 * (n1 + n2))))^2) 
  
  d <- as.numeric(pairwise3$eff_size_d[line])
  n1 <- as.numeric(obs_s3$final[cgroup+1])
  n2 <- as.numeric(obs_s3$final[tgroup+1])
  meta13 <- (d * (1 / (sqrt((n1 + n2) / (n1 * n2)) + ((d^2) / (2 * (n1 + n2))))^2))
  meta23 <- (1 / (sqrt((n1 + n2) / (n1 * n2)) + ((d^2) / (2 * (n1 + n2))))^2) 
  
  meta <- (meta11 + meta12 + meta13) / (meta21 + meta22 + meta23)
  return(meta)
}


func_meta_upper <- function(pairwise1 = pairwise_paffect,
                            pairwise2 = pairwise_paffect_s2,
                            pairwise3 = pairwise_paffect_post_s3,
                            control = "passive",
                            treat = "prosocial") {
  # Get group number for control group (1=passive, 2=active)
  cgroup <- ifelse(control == "passive", 1, 
                   ifelse(control == "active", 2, 0))
  
  # Get group number for contorl group (3=prosocial, 4=societal)
  tgroup <- ifelse(treat == "prosocial", 3, 
                   ifelse(treat == "societal", 4, 0))
  
  # Get line number for t value in pairwise df
  line <- ifelse(cgroup == 1 & tgroup == 3, 1,
                 ifelse(cgroup == 1 & tgroup == 4, 2,
                        ifelse(cgroup == 2 & tgroup == 3, 3,
                               ifelse(cgroup == 2 & tgroup == 4, 4,
                                      0))))
  
  d <- as.numeric(pairwise1$eff_size_d[line])
  n1 <- as.numeric(obs$final[cgroup+1])
  n2 <- as.numeric(obs$final[tgroup+1])
  meta11 <- (d * (1 / (sqrt((n1 + n2) / (n1 * n2)) + ((d^2) / (2 * (n1 + n2))))^2)) 
  meta21 <- (1 / (sqrt((n1 + n2) / (n1 * n2)) + ((d^2) / (2 * (n1 + n2))))^2) 
  
  d <- as.numeric(pairwise2$eff_size_d[line])
  n1 <- as.numeric(obs_s2$final[cgroup+1])
  n2 <- as.numeric(obs_s2$final[tgroup+1])
  meta12 <- (d * (1 / (sqrt((n1 + n2) / (n1 * n2)) + ((d^2) / (2 * (n1 + n2))))^2)) 
  meta22 <- (1 / (sqrt((n1 + n2) / (n1 * n2)) + ((d^2) / (2 * (n1 + n2))))^2) 
  
  d <- as.numeric(pairwise3$eff_size_d[line])
  n1 <- as.numeric(obs_s3$final[cgroup+1])
  n2 <- as.numeric(obs_s3$final[tgroup+1])
  meta13 <- (d * (1 / (sqrt((n1 + n2) / (n1 * n2)) + ((d^2) / (2 * (n1 + n2))))^2)) 
  meta23 <- (1 / (sqrt((n1 + n2) / (n1 * n2)) + ((d^2) / (2 * (n1 + n2))))^2) 
  
  meta1 <- (meta11 + meta12 + meta13)
  meta2 <- (meta21 + meta22 + meta23)
  meta <- meta1 / meta2
  
  upper <- meta + 1.96 * (sqrt(1 /meta2))
  return(upper)
}

func_meta_lower <- function(pairwise1 = pairwise_paffect,
                            pairwise2 = pairwise_paffect_s2,
                            pairwise3 = pairwise_paffect_post_s3,
                            control = "passive",
                            treat = "prosocial") {
  # Get group number for control group (1=passive, 2=active)
  cgroup <- ifelse(control == "passive", 1, 
                   ifelse(control == "active", 2, 0))
  
  # Get group number for contorl group (3=prosocial, 4=societal)
  tgroup <- ifelse(treat == "prosocial", 3, 
                   ifelse(treat == "societal", 4, 0))
  
  # Get line number for t value in pairwise df
  line <- ifelse(cgroup == 1 & tgroup == 3, 1,
                 ifelse(cgroup == 1 & tgroup == 4, 2,
                        ifelse(cgroup == 2 & tgroup == 3, 3,
                               ifelse(cgroup == 2 & tgroup == 4, 4,
                                      0))))
  
  d <- as.numeric(pairwise1$eff_size_d[line])
  n1 <- as.numeric(obs$final[cgroup+1])
  n2 <- as.numeric(obs$final[tgroup+1])
  meta11 <- (d * (1 / (sqrt((n1 + n2) / (n1 * n2)) + ((d^2) / (2 * (n1 + n2))))^2))
  meta21 <- (1 / (sqrt((n1 + n2) / (n1 * n2)) + ((d^2) / (2 * (n1 + n2))))^2) 
  
  d <- as.numeric(pairwise2$eff_size_d[line])
  n1 <- as.numeric(obs_s2$final[cgroup+1])
  n2 <- as.numeric(obs_s2$final[tgroup+1])
  meta12 <- (d * (1 / (sqrt((n1 + n2) / (n1 * n2)) + ((d^2) / (2 * (n1 + n2))))^2)) 
  meta22 <- (1 / (sqrt((n1 + n2) / (n1 * n2)) + ((d^2) / (2 * (n1 + n2))))^2) 
  
  d <- as.numeric(pairwise3$eff_size_d[line])
  n1 <- as.numeric(obs_s3$final[cgroup+1])
  n2 <- as.numeric(obs_s3$final[tgroup+1])
  meta13 <- (d * (1 / (sqrt((n1 + n2) / (n1 * n2)) + ((d^2) / (2 * (n1 + n2))))^2)) 
  meta23 <- (1 / (sqrt((n1 + n2) / (n1 * n2)) + ((d^2) / (2 * (n1 + n2))))^2) 
  
  meta1 <- (meta11 + meta12 + meta13)
  meta2 <- (meta21 + meta22 + meta23)
  meta <- meta1 / meta2
  
  lower <- meta - 1.96 * (sqrt(1 /meta2))
  return(lower)
}




# 2.8 CIs for effet sizes #####################################################
func_upper_ci_t <- function(pairwise = pairwise_paffect,
                            obs = obs,
                            control = "passive",
                            treat = "prosocial") {
  
  # Get group number for control group (1=passive, 2=active)
  cgroup <- ifelse(control == "passive", 1, 
                   ifelse(control == "active", 2, 0))
  
  # Get group number for contorl group (3=prosocial, 4=societal)
  tgroup <- ifelse(treat == "prosocial", 3, 
                   ifelse(treat == "societal", 4, 0))
  
  # Get line number for t value in pairwise df
  line <- ifelse(cgroup == 1 & tgroup == 3, 1,
                 ifelse(cgroup == 1 & tgroup == 4, 2,
                        ifelse(cgroup == 2 & tgroup == 3, 3,
                               ifelse(cgroup == 2 & tgroup == 4, 4,
                                      0))))
  
  t <- pairwise$t[line]
  n1 <- as.numeric(obs$final[cgroup+1])
  n2 <- as.numeric(obs$final[tgroup+1]) 
  
  upper <- MBESS::ci.smd(ncp=t, n.1=n1, n.2=n2, conf.level=0.95)
  return(upper$Upper.Conf.Limit.smd)
}


func_lower_ci_t <- function(pairwise = pairwise_paffect,
                            obs = obs,
                            control = "passive",
                            treat = "prosocial") {
  
  # Get group number for control group (1=passive, 2=active)
  cgroup <- ifelse(control == "passive", 1, 
                   ifelse(control == "active", 2, 0))
  
  # Get group number for contorl group (3=prosocial, 4=societal)
  tgroup <- ifelse(treat == "prosocial", 3, 
                   ifelse(treat == "societal", 4, 0))
  
  # Get line number for t value in pairwise df
  line <- ifelse(cgroup == 1 & tgroup == 3, 1,
                 ifelse(cgroup == 1 & tgroup == 4, 2,
                        ifelse(cgroup == 2 & tgroup == 3, 3,
                               ifelse(cgroup == 2 & tgroup == 4, 4,
                                      0))))
  
  t <- pairwise$t[line]
  n1 <- as.numeric(obs$final[cgroup+1])
  n2 <- as.numeric(obs$final[tgroup+1]) 
  
  lower <- MBESS::ci.smd(ncp=t, n.1=n1, n.2=n2, conf.level=0.95)
  return(lower$Lower.Conf.Limit.smd)
}





func_upper_ci_t_paired <- function(pairwise = pairwise_paffect_time_s3,
                                   treat = "prosocial") {
  # Get group number for contorl group (3=prosocial, 4=societal)
  tgroup <- ifelse(treat == "prosocial", 3, 
                   ifelse(treat == "societal", 4, 0))
  
  t <- pairwise$t[tgroup]
  n1 <- as.numeric(obs_s3$final[tgroup+1])
  
  # Code from https://github.com/Lakens/perfect-t-test/blob/master/Perfect_dependent_t-test.Rmd
  nct_limits <- MBESS::conf.limits.nct(t.value = t, df=n1-1, conf.level = 0.95)
  ci_u_d_z <- nct_limits$Upper.Limit/sqrt(n1) #Not sure about this formula
  
  return(ci_u_d_z)
}

func_lower_ci_t_paired <- function(pairwise = pairwise_paffect_time_s3,
                                   treat = "prosocial") {
  # Get group number for contorl group (3=prosocial, 4=societal)
  tgroup <- ifelse(treat == "prosocial", 3, 
                   ifelse(treat == "societal", 4, 0))
  
  t <- pairwise$t[tgroup]
  n1 <- as.numeric(obs_s3$final[tgroup+1])
  
  # Code from https://github.com/Lakens/perfect-t-test/blob/master/Perfect_dependent_t-test.Rmd
  nct_limits <- MBESS::conf.limits.nct(t.value = t, df=n1-1, conf.level = 0.95)
  ci_l_d_z <- nct_limits$Lower.Limit/sqrt(n1) #Not sure about this formula, but gives same results as Wuensch's files
  
  return(ci_l_d_z)
}




# 3. Format statistical results ###############################################

# 3.1. format p values in APA format (pformat) ================================
pformat <- function(p) {
  paste0("p ",
         ifelse(p >= 0.001,
                paste0("= ",
                       weights::rd(p, 3)),
                paste0("< .001")))
}

# 3.2 Chi2 results in APA format (chi_result) =================================
chi_result <- function (x, y) 
{
  suppressWarnings(chi_result <- stats::chisq.test(x, y))
  p <- pformat(chi_result[["p.value"]]) 
  result <- 
    paste0("Chi2(", 
           as.integer(chi_result[["parameter"]][["df"]]),
           ") = ",
           broman::myround(
             chi_result[["statistic"]][["X-squared"]], 
             2),
           ", ",
           p)
  return(result)
}

# 3.3. ANOVA result in APA format (aov_results) ===============================
aov_result <- function (aov_object) {
  anova_result <- summary(aov_object)
  p <- pformat(anova_result[[1]][["Pr(>F)"]][1])
  result <- paste0("F(", as.integer(anova_result[[1]][["Df"]][1]), 
                   ",", as.integer(anova_result[[1]][["Df"]][2]), ") = ", 
                   broman::myround(anova_result[[1]][["F value"]][1], 2), 
                   ", ", p)
  return(result)
}


# 3.4 Mixed-effects ANOVA results in APA format (rmaov_results) ===============
rmaov_results <- function(rmezaov, row) {
  paste0("F(", 
         round(rmezaov$ANOVA$DFn[row],2),
         ",", 
         rmezaov$ANOVA$DFd[row],
         ") = ", 
         myround2(rmezaov$ANOVA$F[row]),
         ", ", 
         pformat(rmezaov$ANOVA$p[row]))
}



# Clear results table (group comparison)
func_empty_resultstable_postonly <- function() {
  pairwise <- tibble(comparison = c("1 vs 3", "1 vs 4", "2 vs 3", "2 vs 4"))
  pairwise <- pairwise %>% mutate(t = NA)
  pairwise <- pairwise %>% mutate(df = NA)
  pairwise <- pairwise %>% mutate(mean.diff = NA)
  pairwise <- pairwise %>% mutate(eff_size_d = NA)
  pairwise <- pairwise %>% mutate(eff_size_g = NA)
  pairwise <- pairwise %>% mutate(p = NA)
  return(pairwise)
}

# Function to calculate t-test (group comparison)
func_compare_ttest_postonly <- function(df, dep_var, group_var, side) {
  # calculate p-values from t-tests
  tee <- with(df, t.test(formula = dep_var ~ group_var,
                         paired = FALSE,
                         alternative = side))
  return(tee)
}


# Function to calculate effect sizes (group comparison)
func_compare_effsize_postonly <- function(dep_var, group_var, 
                                          hedge = FALSE,
                                          paired = FALSE) {
  effsize <- effsize::cohen.d(dep_var,
                              as.factor(group_var),
                              hedges.correction = hedge,
                              paired = paired,
                              na.rm = TRUE)
  return(effsize)
}

# Function to write t-test, p and effect sizes in table (group comparison)
func_compare_writetable_postonly <- function(pairwise, tee, 
                                             eff_size_g, eff_size_d, group) {
  pairwise <- pairwise %>% 
    mutate(t = ifelse(comparison == group,
                      tee[["statistic"]][["t"]], t)) %>%
    mutate(df = ifelse(comparison == group,
                       tee[["parameter"]][["df"]], df)) %>%
    mutate(mean.diff = ifelse(comparison == group,
                              (tee[["estimate"]][[1]] - 
                                 tee[["estimate"]][[2]]),
                              mean.diff)) %>%
    mutate(p = ifelse(comparison == group,
                      tee[["p.value"]], p)) %>%
    mutate(eff_size_g = ifelse(comparison == group,
                               round(as.double(effsize_g[["estimate"]]), 3),
                               eff_size_g)) %>%
    mutate(eff_size_d = ifelse(comparison == group,
                               round(as.double(effsize_d), 3),
                               eff_size_d))
  return(pairwise)
}



# Clear results table (pre/post)
func_empty_resultstable <- function() {
  pairwise <- tibble(comparison = c("1", "2", "3", "4"))
  pairwise <- pairwise %>% mutate(t = NA)
  pairwise <- pairwise %>% mutate(df = NA)
  pairwise <- pairwise %>% mutate(mean.diff = NA)
  pairwise <- pairwise %>% mutate(eff_size_d = NA)
  pairwise <- pairwise %>% mutate(eff_size_g = NA)
  pairwise <- pairwise %>% mutate(p = NA)
  return(pairwise)
}


# Paired t-test for pre/post-test
func_compare_ttest <- function(df, var_pre, var_post, side) {
  # calculate p-values from t-tests
  tee <- with(df, t.test(x = var_pre, 
                         y = var_post,
                         paired = TRUE,
                         alternative = side))
}

# Effect size for paired t-test for pre/post-test
func_compare_effsize <- function(df, var_pre, var_post, side) {
  # calculate p-values from t-tests
  tee <- with(df, t.test(x = var_pre, 
                         y = var_post,
                         paired = TRUE,
                         alternative = side))
  tee <- tee[["statistic"]][["t"]]
  effsize <- tee / sqrt(length(var_pre))
  return(effsize)
}


# Write t-test, p and effect sizes in table (pre/post-test)
func_compare_writetable <- function(pairwise, tee, 
                                    eff_size_g, eff_size_d, group) {
  pairwise <- pairwise %>% 
    mutate(t = ifelse(comparison == group,
                      tee[["statistic"]][["t"]], t)) %>%
    mutate(df = ifelse(comparison == group,
                       tee[["parameter"]][["df"]], df)) %>%
    mutate(mean.diff = ifelse(comparison == group,
                              (tee[["estimate"]][["mean of the differences"]]),
                              mean.diff)) %>%
    mutate(p = ifelse(comparison == group,
                      tee[["p.value"]], p)) %>%
    mutate(eff_size_g = ifelse(comparison == group,
                               round(as.double(effsize_g), 3),
                               eff_size_g)) %>%
    mutate(eff_size_d = ifelse(comparison == group,
                               round(as.double(effsize_d), 3),
                               eff_size_d))
  return(pairwise)
}

# Plotting pre-test and post-test data per group for one variable
time_plot_func <- function(var_pre, var_post, ylabel, 
                           adjust = c("cm", "no", "lm")) {
  # Requires package rlang
  # var_pre: Pre-Test variable (in quotation marks)
  # var_post Post-Test variable (in quotation marks)
  # ylabel: label of y-axis
  # adjust: Adjust confidence interval? 
  # cm: Adjustment based on Cousineau (2005) with the adoption suggest by Morrey (default)
  # lm: Adjustment based on Loftus & Masson (1994)
  # no: no adjustment
  
  if (adjust == "no") {
    new1 <- df_merge_s3 %>% 
      select(id, treatment, !!sym(var_pre), !!sym(var_post)) %>% 
      gather(-id, -treatment, 
             key = "time", 
             value = "var", 
             factor_key = TRUE) %>% 
      arrange(id)
    
    df_merge_sum_s3 <- new1 %>% 
      select(treatment, time, var) %>%
      group_by(time, treatment) %>%
      summarise_all(funs(mean, 
                         sd, 
                         n = n(), 
                         se_func,
                         lower_ci_func,
                         upper_ci_func))
  }
  
  # Calculate confidence interval (lower bound)
  # Adjustment procedure by Cousineau-Morey
  if (adjust == "cm") {
    new1 <- df_merge_s3 %>% 
      select(id, treatment, !!sym(var_pre), !!sym(var_post)) %>% 
      gather(-id, -treatment, 
             key = "time", 
             value = "var", 
             factor_key = TRUE) %>% 
      arrange(id)
    
    df_merge_sum_s3 <- new1 %>% 
      select(treatment, time, var) %>%
      group_by(time, treatment) %>%
      summarise_all(funs(mean, 
                         sd, 
                         n = n(), 
                         se_func,
                         lower_ci_func,
                         upper_ci_func))
    
    for (i in 1:4) {
      cis1 <- df_merge_s3 %>% 
        filter(treatment == i) %>%
        select(!!sym(var_pre), !!sym(var_post)) 
      
      cis1 <- cm.ci(cis1)
      
      
      df_merge_sum_s3 <- df_merge_sum_s3  %>%
        mutate(lower_ci_func =
                 ifelse(treatment == i & as.numeric(time) == 1,
                        cis1[1,1], 
                        lower_ci_func),
               upper_ci_func =
                 ifelse(treatment == i & as.numeric(time) == 1,
                        cis1[1,2], 
                        upper_ci_func),
               lower_ci_func =
                 ifelse(treatment == i & as.numeric(time) == 2,
                        cis1[2,1], 
                        lower_ci_func),
               upper_ci_func =
                 ifelse(treatment == i & as.numeric(time) == 2,
                        cis1[2,2], 
                        upper_ci_func))
    }
    
  } 
  #loftus-masson within-subject CIs
  if (adjust == "lm") {
    new1 <- df_merge_s3 %>% 
      select(id, treatment, !!sym(var_pre), !!sym(var_post)) %>% 
      gather(-id, -treatment, 
             key = "time", 
             value = "var", 
             factor_key = TRUE) %>% 
      arrange(id)
    
    df_merge_sum_s3 <- new1 %>% 
      select(treatment, time, var) %>%
      group_by(time, treatment) %>%
      summarise_all(funs(mean, 
                         sd, 
                         n = n(), 
                         se_func,
                         lower_ci_func,
                         upper_ci_func))
    
    for (i in 1:4) {
      cis1 <- df_merge_s3 %>% 
        filter(treatment == i) %>%
        select(!!sym(var_pre), !!sym(var_post)) 
      
      cis1 <- lm.ci(cis1)
      
      
      df_merge_sum_s3 <- df_merge_sum_s3  %>%
        mutate(lower_ci_func =
                 ifelse(treatment == i & as.numeric(time) == 1,
                        cis1[1,1], 
                        lower_ci_func),
               upper_ci_func =
                 ifelse(treatment == i & as.numeric(time) == 1,
                        cis1[1,2], 
                        upper_ci_func),
               lower_ci_func =
                 ifelse(treatment == i & as.numeric(time) == 2,
                        cis1[2,1], 
                        lower_ci_func),
               upper_ci_func =
                 ifelse(treatment == i & as.numeric(time) == 2,
                        cis1[2,2], 
                        upper_ci_func))
    }
    
  } 
  
  
  # Add factor variables (for captions and sorting)
  df_merge_sum2_s3 <- df_merge_sum_s3 %>%
    mutate(treatment_f = factor(treatment,
                                labels = c("Passive\nControl",
                                           "Active\nControl",
                                           "Prosocial\nImpact", 
                                           "Societal\nImpact")))
  
  df_merge_sum2_s3 <- df_merge_sum_s3 %>% 
    ungroup() %>%
    mutate(time = factor(time, labels = c("Pre-Test", 
                                          "Post-Test")))
  
  # Define Position on x axis
  df_merge_sum2_s3 <-  df_merge_sum2_s3 %>%
    arrange(treatment, time) %>%
    mutate(treatment_jitter = c(0.25, 0.75,
                                2.25, 2.75,
                                4.25, 4.75,
                                6.25, 6.75))
  
  meanplot_s3_time <- 
    ggplot(df_merge_sum2_s3, 
           aes(x=treatment_jitter, 
               y=mean,
               color = time, 
               shape = time
           )) + 
    geom_errorbar(aes(ymin=lower_ci_func,
                      ymax=upper_ci_func),
                  width=.3,
                  #size = .6,
                  position = dodge) +
    geom_point(
      #size = 4
    ) +
    geom_line(aes(group=treatment), color = "black") +
    scale_fill_continuous(guide = guide_legend()) +
    theme(legend.position="bottom",
          plot.title = element_text(margin = margin(b = -10), hjust = 0.1)
          #axis.text = element_text(size = 16),
          #axis.title = element_text(size = 16)
    ) +
    labs(x = "Treatments",
         y = ylabel,
         color = "Time") +
    coord_cartesian(ylim = c(1, 7)) +
    scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                       labels = c("1", "2", "3", "4", "5", "6", "7")) +
    scale_x_continuous(breaks = c(0.5, 2.5, 4.5, 6.5),
                       labels = c("Passive\nControl", "Active\nControl", 
                                  "Prosocial\nImpact", "Societal\nImpact")) +
    scale_color_manual(name = "",
                       labels = c("Pre-test", "Post-test"),
                       values=c("#0A6AA6", "#CD171D")) +
    scale_shape_manual(name = "",
                       labels = c("Pre-test", "Post-test"),
                       values = c(16, 15)) #+
    #guides(shape = FALSE)
  
  return(meanplot_s3_time)
}


# 3.5 Rounded means (for results table) ---------------------------------------
func_round_mean <- function(x) {
  broman::myround(mean(x, na.rm = TRUE), digits = 2)
}

# 3.6 Rounded sds (for results table) -----------------------------------------
func_round_sd <- function(x) {
  broman::myround(sd(x, na.rm = TRUE), digits = 2)
}





# 3.7 Round values ############################################################

# Round to 2 digits
myround2 <- function(x) {
  broman::myround(x, digits = 2)
}

myround1 <- function(x) {
  broman::myround(x, digits = 1)
}

# Round to APA standard
myround_apa <- function(x) {
  x <- as.numeric(x)
  dig <- ifelse(abs(x) >=  100, 0,
                  ifelse(abs(x) < 100 & abs(x) >= 10, 1,
                         ifelse(abs(x) < 10 & abs(x) >= 0.1, 2, 
                                ifelse(abs(x) < 0.1, 3, 0))))
  
  if (dig == 0) {y <- round(x, digits = 0)}
  if (dig >= 1) {y <- broman::myround(x, digits = dig)}
  
  return(y)
}


