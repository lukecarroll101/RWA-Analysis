columns_to_analyse <- list(
  PF1OrgSup = c('X.PF1_OrgSup_1', 'X.PF1_OrgSup_2', 'X.PF1_OrgSup_3'),
  PF2Culture = c('X.PF2_Cult_1', 'X.PF2_Cult_2', 'X.PF2_Cult_3'),
  PF3Ldshp = c('X.PF3_Ldshp_1', 'X.PF3_Ldshp_2', 'X.PF3_Ldshp_3'),
  PF4Respect = c('X.PF4_Respect_1', 'X.PF4_Respect_2', 'X.PF4_Respect_3'),
  PF5Fit = c('X.PF5_Fit_1', 'X.PF5_Fit_2', 'X.PF5_Fit_3'),
  PF6Growth = c('X.PF6_Growth_1', 'X.PF6_Growth_2', 'X.PF6_Growth_3'),
  PF7RecRew = c('X.PF7_RecRew_1', 'X.PF7_RecRew_2', 'X.PF7_RecRew_3'),
  PF8AutInv = c('X.PF8_AutInv_1', 'X.PF8_AutInv_2', 'X.PF8_AutInv_3'),
  PF9WkLoad = c('X.PF9_WkLoad_1', 'X.PF9_WkLoad_2', 'X.PF9_WkLoad_3'),
  PF10Engage = c('X.PF10_Engage_1', 'X.PF10_Engage_2', 'X.PF10_Engage_3'),
  PF11WLB = c('X.PF11_WLB_1', 'X.PF11_WLB_2', 'X.PF11_WLB_3'),
  PF12PsySafety = c('X.PF12_PsySafety_1', 'X.PF12_PsySafety_2', 'X.PF12_PsySafety_3'),
  PF13OrgSafety = c('X.PF13_OrgSafety_1', 'X.PF13_OrgSafety_2', 'X.PF13_OrgSafety_3'),
  K6 = c('X.14_K6_1', 'X.14_K6_2', 'X.14_K6_3', 'X.14_K6_4', 'X.14_K6_5', 'X.14_K6_6'),
  DASS = c('X.14_DASS_1', 'X.14_DASS_2', 'X.14_DASS_3', 'X.14_DASS_4', 'X.14_DASS_5', 'X.14_DASS_6', 'X.14_DASS_7')
)


# meansdf <- dplyr::data_frame(variables)
# for (org in 1:4){
#   print(org)
#   meansdf[,org+1] <- as.numeric(lapply(df[df$OrgID == org, variables], mean, na.rm = TRUE))
# }
# names(meansdf) <- c("Predictors", "1", "2", "3", "4")
# meansdf

# ggplot(K6_means, aes(means, weights)) +
#   geom_point(colour = "blue", size = 3) +
#   labs(title = "Plot of Weights v Mean for DASS",
#        x = "Mean",
#        y = "Weight") +
#   xlim(descriptives["K6_sum", "min"], descriptives["K6_sum", "max"]) +
#   ylim(0, max(rwa_models$K6_sum$result[,3])+5)
# 
# ggplot(DASS_means, aes(means, weights), label = predictors) +
#   geom_point(colour = "blue", size = 3) +
#   labs(title = "Plot of Weights v Mean for DASS",
#        x = "Mean",
#        y = "Weight") +
#   xlim(descriptives["DASS_sum", "min"], descriptives["DASS_sum", "max"]) +
#   ylim(0, max(rwa_models$DASS_sum$result[,3])+5)

# write.csv(df[!names(df) %in% names(df[57:71])], file = "RWA CSV 2024 UPDATED.csv", row.names = FALSE)

# variables <- c(colnames(df)[57:(ncol(df)-2)], "K6", "DASS")