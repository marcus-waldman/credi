utils::globalVariables(
  c(".GUI", "AGE", "COG", "COG_SE", "COG_flag", "COG_mu", "COG_sigma", "ID",
    "LANG", "LANG_SE", "LANG_flag", "LANG_mu", "LANG_sigma", "MOT", "MOT_SE",
    "MOT_flag", "MOT_mu", "MOT_sigma", "Number", "OK", "OVERALL",
    "OVERALL_flag", "OVERALL_mu", "OVERALL_sigma", "Pct_Missing", "SEM",
    "SEM_SE", "SEM_flag", "SEM_mu", "SEM_sigma", "Z_COG", "Z_LANG", "Z_MOT",
    "Z_OVERALL", "Z_SEM", "age_group", "check.var", "id", "var.class",
    # dscore package column names used in rename() inside clean()
    "d", "daz", "sem", "DAZ", "dscore_sem",
    # backtick-quoted column name produced by as.data.frame(sapply(...))
    "sapply(input_df, class)")
  )