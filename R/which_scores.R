#' Determine which scales should be scored given an individual's responses.
#'
#' This function determines whether there are a sufficient number of item responses
#' per scale in order to calculate a score
#' @param Yi (numeric vector) with response data. Must be named according to CREDI_code.
#' @param mest_df (data.frame) Measurement parameter estimates.
#' @param min_items (integer) Minimum number of items that must have responses to score.

which_scores<-function(Yi, mest_df = mest_df, min_items = min_items){

  #Identify items answered
  yes_items_i = names(Yi[Yi==1])
  no_items_i = names(Yi[Yi==0])
  items_i =  c(yes_items_i, no_items_i)

  #Identify the number of items by scale (e.g., domain or "form")
  J_mot_i = sum(items_i %in% mest_df[mest_df$MOT>0,"CREDI_code"] )
  J_cog_i = sum(items_i %in% mest_df[mest_df$COG>0,"CREDI_code"] )
  J_lang_i = sum(items_i %in% mest_df[mest_df$LANG>0,"CREDI_code"] )
  J_sem_i = sum(items_i %in% mest_df[mest_df$SEM>0,"CREDI_code"] )
  J_sf_i = sum(items_i %in% mest_df[mest_df$ShortForm==T,"CREDI_code"] )
  J_overall_i = sum(items_i %in%mest_df[mest_df$alpha>0,"CREDI_code"])

  #Return a logical data frame with scales to include for the individual
  return(data.frame(SF = J_sf_i>=min_items,
                    MOT = J_mot_i>=min_items,
                    COG = J_cog_i>=min_items,
                    LANG = J_lang_i>=min_items,
                    SEM = J_sem_i>=min_items,
                    OVERALL = J_overall_i>=min_items))
}
