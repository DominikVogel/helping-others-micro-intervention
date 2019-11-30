###############################################################################
################ Bayes Factors for Figure 5 ###################################
###############################################################################


# 1. Meta-analytic Bayes factor ###############################################
# 1.1 Positive Affect =========================================================

# Prosocial vs. Passive control
bf_paffect_psi_passive_meta <- func_meta_bf(df1 = pairwise_paffect, 
                            df2 = pairwise_paffect_s2,
                            df3 = pairwise_paffect_post_s3,
                            dv = "paffect", 
                            control = "passive", 
                            treat = "prosocial")

# Prosocial vs. Active control
bf_paffect_psi_active_meta <- func_meta_bf(df1 = pairwise_paffect, 
                            df2 = pairwise_paffect_s2,
                            df3 = pairwise_paffect_post_s3,
                            dv = "paffect", 
                            control = "active", 
                            treat = "prosocial")

# Societal vs. passive control
bf_paffect_si_passive_meta <- func_meta_bf(df1 = pairwise_paffect, 
                            df2 = pairwise_paffect_s2,
                            df3 = pairwise_paffect_post_s3,
                            dv = "paffect", 
                            control = "passive", 
                            treat = "societal")

# Societal vs. active control
bf_paffect_si_active_meta <- func_meta_bf(df1 = pairwise_paffect, 
                            df2 = pairwise_paffect_s2,
                            df3 = pairwise_paffect_post_s3,
                            dv = "paffect", 
                            control = "active", 
                            treat = "societal")


# 1.2 Negative Affect =========================================================
# Prosocial vs. Passive control
bf_naffect_psi_passive_meta <- func_meta_bf(df1 = pairwise_naffect, 
                            df2 = pairwise_naffect_s2,
                            df3 = pairwise_naffect_post_s3,
                            dv = "naffect", 
                            control = "passive", 
                            treat = "prosocial")

# Prosocial vs. Active control
bf_naffect_psi_active_meta <- func_meta_bf(df1 = pairwise_naffect, 
                            df2 = pairwise_naffect_s2,
                            df3 = pairwise_naffect_post_s3,
                            dv = "naffect", 
                            control = "active", 
                            treat = "prosocial")

# Societal vs. passive control
bf_naffect_si_passive_meta <- func_meta_bf(df1 = pairwise_naffect, 
                            df2 = pairwise_naffect_s2,
                            df3 = pairwise_naffect_post_s3,
                            dv = "naffect", 
                            control = "passive", 
                            treat = "societal")

# Societal vs. active control
bf_naffect_si_active_meta <- func_meta_bf(df1 = pairwise_naffect, 
                            df2 = pairwise_naffect_s2,
                            df3 = pairwise_naffect_post_s3,
                            dv = "naffect", 
                            control = "active", 
                            treat = "societal")

# 1.3 Turnover intention ======================================================
# Prosocial vs. Passive control
bf_turnover_psi_passive_meta <- func_meta_bf(df1 = pairwise_turnover, 
                             df2 = pairwise_turnover_s2,
                             df3 = pairwise_turnover_post_s3,
                             dv = "turnover", 
                             control = "passive", 
                             treat = "prosocial")

# Prosocial vs. Active control
bf_turnover_psi_active_meta <- func_meta_bf(df1 = pairwise_turnover, 
                             df2 = pairwise_turnover_s2,
                             df3 = pairwise_turnover_post_s3,
                             dv = "turnover", 
                             control = "active", 
                             treat = "prosocial")

# Societal vs. passive control
bf_turnover_si_passive_meta <- func_meta_bf(df1 = pairwise_turnover, 
                             df2 = pairwise_turnover_s2,
                             df3 = pairwise_turnover_post_s3,
                             dv = "turnover", 
                             control = "passive", 
                             treat = "societal")

# Societal vs. active control
bf_turnover_si_active_meta <- func_meta_bf(df1 = pairwise_turnover, 
                             df2 = pairwise_turnover_s2,
                             df3 = pairwise_turnover_post_s3,
                             dv = "turnover", 
                             control = "active", 
                             treat = "societal")


# 1.4 Willingness to recommend job ============================================
# Prosocial vs. Passive control
bf_will_psi_passive_meta <- func_meta_bf(df1 = pairwise_will, 
                         df2 = pairwise_will_s2,
                         df3 = pairwise_will_post_s3,
                         dv = "will", 
                         control = "passive", 
                         treat = "prosocial")

# Prosocial vs. Active control
bf_will_psi_active_meta <- func_meta_bf(df1 = pairwise_will, 
                         df2 = pairwise_will_s2,
                         df3 = pairwise_will_post_s3,
                         dv = "will", 
                         control = "active", 
                         treat = "prosocial")

# Societal vs. passive control
bf_will_si_passive_meta <- func_meta_bf(df1 = pairwise_will, 
                         df2 = pairwise_will_s2,
                         df3 = pairwise_will_post_s3,
                         dv = "will", 
                         control = "passive", 
                         treat = "societal")

# Societal vs. active control
bf_will_si_active_meta <- func_meta_bf(df1 = pairwise_will, 
                         df2 = pairwise_will_s2,
                         df3 = pairwise_will_post_s3,
                         dv = "will", 
                         control = "active", 
                         treat = "societal")




# 2. Bayes factor independent t-test ##########################################

# 2.1 Study 1 =================================================================

# Positive Affect
bf_paffect_psi_passive <- func_ttest_bf(df_merge1$paffect, 
                                              df_merge3$paffect,
                                              dv = "paffect")
bf_paffect_psi_active <- func_ttest_bf(df_merge2$paffect, 
                                             df_merge3$paffect,
                                             dv = "paffect")
bf_paffect_si_passive <- func_ttest_bf(df_merge1$paffect, 
                                        df_merge4$paffect,
                                        dv = "paffect")
bf_paffect_si_active <- func_ttest_bf(df_merge2$paffect, 
                                       df_merge4$paffect,
                                       dv = "paffect")


# Negative Affect
bf_naffect_psi_passive <- func_ttest_bf(df_merge1$naffect, 
                                        df_merge3$naffect,
                                        dv = "naffect")
bf_naffect_psi_active <- func_ttest_bf(df_merge2$naffect, 
                                       df_merge3$naffect,
                                       dv = "naffect")
bf_naffect_si_passive <- func_ttest_bf(df_merge1$naffect, 
                                       df_merge4$naffect,
                                       dv = "naffect")
bf_naffect_si_active <- func_ttest_bf(df_merge2$naffect, 
                                      df_merge4$naffect,
                                      dv = "naffect")


# Turnover
bf_turnover_psi_passive <- func_ttest_bf(df_merge1$turnover, 
                                        df_merge3$turnover,
                                        dv = "turnover")
bf_turnover_psi_active <- func_ttest_bf(df_merge2$turnover, 
                                       df_merge3$turnover,
                                       dv = "turnover")
bf_turnover_si_passive <- func_ttest_bf(df_merge1$turnover, 
                                       df_merge4$turnover,
                                       dv = "turnover")
bf_turnover_si_active <- func_ttest_bf(df_merge2$turnover, 
                                      df_merge4$turnover,
                                      dv = "turnover")

# Will
bf_will_psi_passive <- func_ttest_bf(df_merge1$willingnessjobrec, 
                                        df_merge3$willingnessjobrec,
                                        dv = "will")
bf_will_psi_active <- func_ttest_bf(df_merge2$willingnessjobrec, 
                                       df_merge3$willingnessjobrec,
                                       dv = "will")
bf_will_si_passive <- func_ttest_bf(df_merge1$willingnessjobrec, 
                                       df_merge4$willingnessjobrec,
                                       dv = "will")
bf_will_si_active <- func_ttest_bf(df_merge2$willingnessjobrec, 
                                      df_merge4$willingnessjobrec,
                                      dv = "will")

# 2.2 Study 2 =================================================================

# Positive Affect
bf_paffect_psi_passive_s2 <- func_ttest_bf(df_merge1_s2$paffect, 
                                        df_merge3_s2$paffect,
                                        dv = "paffect")
bf_paffect_psi_active_s2 <- func_ttest_bf(df_merge2_s2$paffect, 
                                       df_merge3_s2$paffect,
                                       dv = "paffect")
bf_paffect_si_passive_s2 <- func_ttest_bf(df_merge1_s2$paffect, 
                                       df_merge4_s2$paffect,
                                       dv = "paffect")
bf_paffect_si_active_s2 <- func_ttest_bf(df_merge2_s2$paffect, 
                                      df_merge4_s2$paffect,
                                      dv = "paffect")


# Negative Affect
bf_naffect_psi_passive_s2 <- func_ttest_bf(df_merge1_s2$naffect, 
                                        df_merge3_s2$naffect,
                                        dv = "naffect")
bf_naffect_psi_active_s2 <- func_ttest_bf(df_merge2_s2$naffect, 
                                       df_merge3_s2$naffect,
                                       dv = "naffect")
bf_naffect_si_passive_s2 <- func_ttest_bf(df_merge1_s2$naffect, 
                                       df_merge4_s2$naffect,
                                       dv = "naffect")
bf_naffect_si_active_s2 <- func_ttest_bf(df_merge2_s2$naffect, 
                                      df_merge4_s2$naffect,
                                      dv = "naffect")


# Turnover
bf_turnover_psi_passive_s2 <- func_ttest_bf(df_merge1_s2$turnover, 
                                         df_merge3_s2$turnover,
                                         dv = "turnover")
bf_turnover_psi_active_s2 <- func_ttest_bf(df_merge2_s2$turnover, 
                                        df_merge3_s2$turnover,
                                        dv = "turnover")
bf_turnover_si_passive_s2 <- func_ttest_bf(df_merge1_s2$turnover, 
                                        df_merge4_s2$turnover,
                                        dv = "turnover")
bf_turnover_si_active_s2 <- func_ttest_bf(df_merge2_s2$turnover, 
                                       df_merge4_s2$turnover,
                                       dv = "turnover")

# Will
bf_will_psi_passive_s2 <- func_ttest_bf(df_merge1_s2$willingnessjobrec, 
                                     df_merge3_s2$willingnessjobrec,
                                     dv = "will")
bf_will_psi_active_s2 <- func_ttest_bf(df_merge2_s2$willingnessjobrec, 
                                    df_merge3_s2$willingnessjobrec,
                                    dv = "will")
bf_will_si_passive_s2 <- func_ttest_bf(df_merge1_s2$willingnessjobrec, 
                                    df_merge4_s2$willingnessjobrec,
                                    dv = "will")
bf_will_si_active_s2 <- func_ttest_bf(df_merge2_s2$willingnessjobrec, 
                                   df_merge4_s2$willingnessjobrec,
                                   dv = "will")


# 2.3 Study 3 =================================================================

# Positive Affect
bf_paffect_psi_passive_s3 <- func_ttest_bf(df_merge1_s3$paffect_post, 
                                           df_merge3_s3$paffect_post,
                                           dv = "paffect")
bf_paffect_psi_active_s3 <- func_ttest_bf(df_merge2_s3$paffect_post, 
                                          df_merge3_s3$paffect_post,
                                          dv = "paffect")
bf_paffect_si_passive_s3 <- func_ttest_bf(df_merge1_s3$paffect_post, 
                                          df_merge4_s3$paffect_post,
                                          dv = "paffect")
bf_paffect_si_active_s3 <- func_ttest_bf(df_merge2_s3$paffect_post, 
                                         df_merge4_s3$paffect_post,
                                         dv = "paffect")


# Negative Affect
bf_naffect_psi_passive_s3 <- func_ttest_bf(df_merge1_s3$naffect_post, 
                                           df_merge3_s3$naffect_post,
                                           dv = "naffect")
bf_naffect_psi_active_s3 <- func_ttest_bf(df_merge2_s3$naffect_post, 
                                          df_merge3_s3$naffect_post,
                                          dv = "naffect")
bf_naffect_si_passive_s3 <- func_ttest_bf(df_merge1_s3$naffect_post, 
                                          df_merge4_s3$naffect_post,
                                          dv = "naffect")
bf_naffect_si_active_s3 <- func_ttest_bf(df_merge2_s3$naffect_post, 
                                         df_merge4_s3$naffect_post,
                                         dv = "naffect")


# Turnover
bf_turnover_psi_passive_s3 <- func_ttest_bf(df_merge1_s3$turnover_post, 
                                            df_merge3_s3$turnover_post,
                                            dv = "turnover")
bf_turnover_psi_active_s3 <- func_ttest_bf(df_merge2_s3$turnover_post, 
                                           df_merge3_s3$turnover_post,
                                           dv = "turnover")
bf_turnover_si_passive_s3 <- func_ttest_bf(df_merge1_s3$turnover_post, 
                                           df_merge4_s3$turnover_post,
                                           dv = "turnover")
bf_turnover_si_active_s3 <- func_ttest_bf(df_merge2_s3$turnover_post, 
                                          df_merge4_s3$turnover_post,
                                          dv = "turnover")

# Will
bf_will_psi_passive_s3 <- func_ttest_bf(df_merge1_s3$willingnessjobrec_post, 
                                        df_merge3_s3$willingnessjobrec_post,
                                        dv = "will")
bf_will_psi_active_s3 <- func_ttest_bf(df_merge2_s3$willingnessjobrec_post, 
                                       df_merge3_s3$willingnessjobrec_post,
                                       dv = "will")
bf_will_si_passive_s3 <- func_ttest_bf(df_merge1_s3$willingnessjobrec_post, 
                                       df_merge4_s3$willingnessjobrec_post,
                                       dv = "will")
bf_will_si_active_s3 <- func_ttest_bf(df_merge2_s3$willingnessjobrec_post, 
                                      df_merge4_s3$willingnessjobrec_post,
                                      dv = "will")


# 3. Bayes factor paired t-test ###############################################
# Positive Affect
bf_paired_paffect_psi <- func_ttest_paired_bf(df_merge3_s3$paffect_pre, 
                                    df_merge3_s3$paffect_post,
                                    dv = "paffect")
bf_paired_paffect_si <- func_ttest_paired_bf(df_merge4_s3$paffect_pre, 
                                    df_merge4_s3$paffect_post,
                                    dv = "paffect")

# Negative affect
bf_paired_naffect_psi <- func_ttest_paired_bf(df_merge3_s3$naffect_pre, 
                                    df_merge3_s3$naffect_post,
                                    dv = "naffect")
bf_paired_naffect_si <- func_ttest_paired_bf(df_merge4_s3$naffect_pre, 
                                    df_merge4_s3$naffect_post,
                                    dv = "naffect")

# Turnover
bf_paired_turnover_psi <- func_ttest_paired_bf(df_merge3_s3$turnover_pre, 
                                     df_merge3_s3$turnover_post,
                                     dv = "turnover")
bf_paired_turnover_si <- func_ttest_paired_bf(df_merge4_s3$turnover_pre, 
                                     df_merge4_s3$turnover_post,
                                     dv = "turnover")

# Will
bf_paired_will_psi <- func_ttest_paired_bf(df_merge3_s3$willingnessjobrec_pre, 
                                 df_merge3_s3$willingnessjobrec_post,
                                 dv = "will")
bf_paired_will_si <- func_ttest_paired_bf(df_merge4_s3$willingnessjobrec_pre, 
                                 df_merge4_s3$willingnessjobrec_post,
                                 dv = "will")
