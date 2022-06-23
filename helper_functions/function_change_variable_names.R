# Function to change variable names
.changenames <- function(s, capitalize = TRUE) {
  n <- case_when(
    s == "rel_1" ~ "service attendance", 
    s == "rel_2" ~ "prayer",
    s == "rel_3" ~ "religious status",
    s == "rel_4" ~ "membership",
    s == "rel_5" ~ "belief God",
    s == "rel_6" ~ "belief afterlife",
    s == "rel_7" ~ "spirituality",
    s == "rel_8" ~ "norms lifestyle self",
    s == "rel_9" ~ "norms God self",
    s == "cnorm_1" ~ "norms lifestyle country",
    s == "cnorm_2" ~ "norms God country",
    s == "wb_gen_1" ~ "quality life",
    s == "wb_gen_2" ~ "statisfaction health",
    s == "wb_phys_1" ~ "pain",
    s == "wb_phys_2" ~ "medication",
    s == "wb_phys_3" ~ "energy",
    s == "wb_phys_4" ~ "mobility",
    s == "wb_phys_5" ~ "sleep",
    s == "wb_phys_6" ~ "activities",
    s == "wb_phys_7" ~ "work ability",
    s == "wb_psych_1" ~ "enjoy life",
    s == "wb_psych_2" ~ "meaningfulness",
    s == "wb_psych_3" ~ "concentration",
    s == "wb_psych_4" ~ "statisfaction appearance",
    s == "wb_psych_5" ~ "self esteem",
    s == "wb_psych_6" ~ "negative affect",
    s == "wb_soc_1" ~ "relationships",
    s == "wb_soc_2" ~ "social support",
    s == "wb_soc_3" ~ "sexual satisfaction",
    s == "wb_overall_mean" ~ "mean overall",
    s == "wb_phys_mean" ~ "mean physical",
    s == "wb_psych_mean" ~ "mean psychological",
    s == "wb_soc_mean" ~ "mean social",
    s == "external_norms" ~ "external norms",
    s == "external_n_subjs" ~ "external n subjects",
    s == "gdp" ~ "GDP",
    s == "ses" ~ "SES",
    s == "gdp_scaled" ~ "GDP Scaled",
    TRUE ~ s
  )
  if(capitalize) n <- .capwords(n)
  return(n)
}

.changenames2 = function(s){
  old <- c("rel_1","rel_2","rel_3","rel_4","rel_5","rel_6","rel_7","rel_8","rel_9",
           "cnorm_1" ,"cnorm_2",
           "wb_gen_1","wb_gen_2",
           "wb_phys_1","wb_phys_2","wb_phys_3","wb_phys_4","wb_phys_5","wb_phys_6","wb_phys_7",
           "wb_psych_1","wb_psych_2","wb_psych_3","wb_psych_4","wb_psych_5","wb_psych_6",
           "wb_soc_1","wb_soc_2","wb_soc_3",
           "wb_overall_mean","wb_phys_mean","wb_psych_mean","wb_soc_mean")
  new <- c("service_attendance","prayer","religious_status","denomination","belief_God","belief_afterlife","spiritual","norms_lifestyle_self","norms_God_self",
           "norms_lifestyle_country","norms_God_country",
           "quality_life","statisfaction_health",
           "pain","medication","energy","mobility","sleep","activities","work_ability",
           "enjoy_life","meaningfulness","concentration","statisfaction_appearance","self_esteem","negative_affect",
           "relationships","social_support","sexual_satisfaction",
           "mean_overall","mean_physical","mean_psychological","mean_social")
  out <- gsub(old, new, s, fixed = TRUE)
  return(out)
}
