#' Score CREDI response data
#'
#' This function calculates the posterior density function and provides CREDI Overall, Short Form, and domain-specific scores. 
#' CREDI variables must be input according to standardized variable names. Four types of naming conventions are supported by this package:
#' \itemize{
#'  \item{"Long Form (October 2017)" variable names starting with "LF"}
#'  \item{"Short Form" variables starting with "CREDI_A01", "CREDI_B01", etc.}
#'  \item{"Long Form (April 2017)" variables starting with "CREDI_LM", "CREDI_LC", etc.}
#'  \item{"CREDI Pilot 4" variables starting with "QC", "QS", and "QM"}
#' }
#' The function also requires a unique ID variable and an AGE variable. Only scores with a non-missing AGE variables will be scored. 
#' 
#' @param data (data.frame) Defaults to NULL. Response data. If NULL, then user is prompted to identify a .csv file with response data. Defaults to NULL.
#' @param reverse_code (Logical) Defaults to TRUE. If TRUE, then reverse coding is automated to appropriately handle the negatively worded items LF9, LF102, LFMH1, LFMH2, LFMH3, LFMH4, LFMH5, LFMH7, LFMH8, & LFMH9. If FALSE, then the package assumes items are already reverse coded and no changes are applied prior to scoring.
#' @param interactive (Logical) Defaults to TRUE. If TRUE, the user may be prompted with caution messages regarding whether scoring should be continued, where to save the scores, where to save a logfile, etc. If FALSE, continuation is assumed and scores and the user is not prompted to save scores or a logfile.
#' @param min_items (integer) Defaults to 5. The minimum number of scale-specific items (e.g. SEM, MOT, etc.) required for a score to be calculated.
#' @importFrom magrittr %>%
#' @importFrom readr read_csv
#' @importFrom readr write_csv
#' @importFrom dplyr case_when
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr n_distinct
#' @importFrom dplyr left_join
#' @importFrom tibble rownames_to_column
#' @export
#' @examples
#' 
#' #Create a sample dataframe
#' dat <- data.frame(ID = 1:3, AGE = c(3,5,4), LF1 = c(1,0,NA) , LF2 = c(0,0,0), LF3 = c(1,0,1), LF4 = c(1,1,1), LF5 = c(1,0,0))
#' 
#' #Score the dat
#' scored_dat <- credi::score(data = dat, reverse_code = FALSE, interactive = FALSE, min_items = 5)
#' 
#' #Print out domain scores:
#' scored_dat$scores[,c("MOT", "LANG", "SEM", "COG", "OVERALL")]
#' MOT   LANG    SEM    COG OVERALL
#' 43.489 45.968 44.626 45.091  40.079
#' 42.058 45.049 43.755 44.250  38.16
#' #One observation did not have at least 5 items responded to, so is not included in the results

score <- function(data = NULL, reverse_code = TRUE, interactive = TRUE, min_items = 5){

  # Identify if dialog specifying .csv file should be bypassed.
  bypass = ifelse(is.null(data), FALSE, TRUE)
  if (bypass == TRUE){
    if (!is.data.frame(data)){
      stop("data argument must be type data.frame")
    }
  }

  # Load required packages

  require("stats")
  require("svDialogs")
  require("tidyverse")

  # Created log file
  time1 = proc.time()
  log = list(c("------------------------------------"), c("Log for CREDI Scoring Messages"),
             paste("Date:", Sys.time()), c("------------------------------------"))


  # Load in the response data, if not bypassed
  csv_wd = getwd()
  if(bypass == FALSE){
    out_dlgOpen = dlgOpen(title = "Select the .csv file with the CREDI response data",
                          filters = c("csv", "*.csv"))
    csv_file = out_dlgOpen$res
    if (!endsWith(tolower(csv_file), ".csv")){stop("Selected file is not a .csv file.", call. = FALSE)}
    csv_wd = paste(strsplit(csv_file,"/")[[1]][-length(strsplit(csv_file,"/")[[1]])],collapse = "/")
    setwd(csv_wd)
    input_df = readr::read_csv(file = csv_file, col_types = readr::cols())
  } else {
    input_df = data
  }

  # Clean the input data
  #Uppercase id in the input_df if no ID variable exists
  if("id" %in% names(input_df) & is.na(match("ID", names(input_df)))){
    input_df <- input_df %>%
      mutate(ID = id)
  }

  #Use the clean function to clean the DF
  list_cleaned = clean(input_df = input_df, mest_df = mest_df, reverse_code = reverse_code,
                       interactive = interactive, log = log, min_items = min_items)
  log = list_cleaned$log
  if(list_cleaned$stop!=0){

    #print("*Error: Processing the provided response data resulted in errors. See log for more details.")
    return(list(log = log))

    if (interactive == TRUE){
      x<-as.character(readline(prompt = "Would you like to save a log file of warning and error messages? [Y/N]: "))
      x <- toupper(x)
      cut = 0
      while(cut == 0){
        if (x == "Y"){
          cut = 1;
        } else if (x == "N"){
          cut = 1
          stop("Scoring canceled.", call. = FALSE)
        } else {
          x<-as.character(readline(prompt = "Would you like to continue? [Y/N]:"))
          x <- toupper(x)
          cut = 0
        }
      } #end while
      write_log(log = log, folder = csv_wd)
    } #End if interactive

    #return(list(log = log))

  } # End if stop != 0

  cleaned_df = list_cleaned$cleaned_df
  sf_df = list_cleaned$sf_df
  is_sf = list_cleaned$is_sf
  items_noresponse = list_cleaned$items_noresponse

  # Create data matricies
  X = model.matrix(~1 + I( (AGE-18)/10.39 ) + I( ((AGE-18)/10.39)^2 ) + I( ((AGE-18)/10.39)^3 ), data = cleaned_df)
  X_4 = model.matrix(~1 + I( (AGE-18)/10.39 ) + I( ((AGE-18)/10.39)^2 ) + I( ((AGE-18)/10.39)^3 ) + I( ((AGE-18)/10.39)^4 ), data = cleaned_df)
  Y = as.matrix(cleaned_df[,-match(c("ID","AGE",items_noresponse), names(cleaned_df))]); Y[is.na(Y)] = -9L
  Y_sf = as.matrix(sf_df[,-na.omit(match(c("ID","AGE","age_group",items_noresponse), names(sf_df)))]); Y_sf[is.na(Y_sf)] = -9L
  MU_LF = X%*%as.matrix(B) #NxK (matrix)
  MU_SF = X%*%as.numeric(beta) #Nx1

  # Obtain necessary parameter matrices
  inds_exclude = match(items_noresponse, mest_df$CREDI_code)
  if (length(inds_exclude)==0){
    LAMBDA = as.matrix(mest_df[,c("MOT","COG","LANG","SEM")])
    TAU = as.vector(mest_df$tau)
    ALPHA = as.vector(mest_df$alpha)
    DELTA = as.vector(mest_df$delta)
  }else{
    LAMBDA = as.matrix(mest_df[-inds_exclude,c("MOT","COG","LANG","SEM")])
    TAU = as.vector(mest_df$tau[-inds_exclude])
    ALPHA = as.vector(mest_df$alpha[-inds_exclude])
    DELTA = as.vector(mest_df$delta[-inds_exclude])
  }

  # Obtain necessary constants
  J = ncol(Y);
  K = 4L
  P = 3L
  N = as.integer(nrow(Y))
  invS = as.matrix(invS)
  SIGMA_SQ= exp(X%*%as.numeric(gamma))

  # initialize the theta values
  THETA0_LF = MU_LF #NxK (matrix)
  THETA0_SF = MU_SF #Nx1 (matrix)

  # Conduct the optimization
  MAP_LF = 0.*THETA0_LF + NA
  MAP_SF = 0.*THETA0_SF + NA
  MAP_OVERALL = 0.*THETA0_SF + NA
  # Z_LF = MAP_LF
  SE_LF = MAP_LF
  SE_SF = MAP_SF
  OVERALL_SE = MAP_OVERALL
  NOTES = rep("", nrow(Y))
  writeLines(paste("\nScoring ", N, " observations:"))
  pb<-txtProgressBar(min = 0, max = N, initial = 0, style = 3)

  #Get the flags for the domain scores
  get.flags <- function(x){
    dat <- as.numeric(!which_scores(Y[x,], mest_df, min_items))
    names(dat) <- c("SF_flag", "MOT_flag", "COG_flag", "LANG_flag", "SEM_flag", "OVERALL_flag")
    return(dat)
  }

  FLAGS <- t(sapply(1:nrow(Y), get.flags))

#i = 1
  for (i in 1:N){

    scales_i = which_scores(Y[i,], mest_df, min_items)

    notes_i = ""

    if(prod(as.numeric(scales_i[1,]))==0){
      if(list_cleaned$is_sf){
        notes_i = paste0(notes_i, "Only responses to short form items detected. Therefore, scoring will produce only a CREDI-SF score.")
        if (scales_i$SF==F){
          notes_i = paste0(notes_i, "Warning: Fewer than ", min_items," were recorded for this observation. No score generated.")
        }
      } else {
        notes_i = paste0(notes_i, "The following domains did not have at least ", min_items," observed for this observation and may be innacurate: ", paste0(names(scales_i)[scales_i[1,]==FALSE], collapse = ", "),"." )
      }

    }

    #Calculate short form scores, as appropriate
    if(scales_i$SF == TRUE | is_sf == TRUE | scales_i$OVERALL == TRUE){

      js_SF = which(names(Y[i,]) %in% mest_df[mest_df$ShortForm==TRUE,"CREDI_code"])
      out_SF = optim(par = as.vector(THETA0_SF[i,]),
                          fn = sf_posterior_density,
                          Yi = as.vector(Y[i,js_SF]),
                          MUi = as.vector(MU_SF[i,]),
                          SIGMA_SQi = as.numeric(SIGMA_SQ[i]),
                          DELTA = as.vector(DELTA)[js_SF],
                          ALPHA = as.vector(ALPHA)[js_SF],
                          J = length(js_SF), #as.integer(J),
                          method = "BFGS",
                          hessian = TRUE)

      if(out_SF$convergence==0){ #If converged, produce score.
        MAP_SF[i,] = out_SF$par
        SE_SF[i,] = 1/sqrt(out_SF$hessian)
      }  else {
        notes_i = paste0(notes_i, "Scoring procedure for SF scale did not converge; SF score not provided.")
      }
    }

    # Calculate an overall score
    if(scales_i$OVERALL==TRUE){

      js_OVERALL = which(names(Y[i,]) %in% mest_df[mest_df$alpha>0,"CREDI_code"])
      out_OVERALL = optim(par = as.vector(THETA0_SF[i,]),
                          fn = sf_posterior_density,
                          Yi = as.vector(Y[i,js_OVERALL]),
                          MUi = as.vector(MU_SF[i,]),
                          SIGMA_SQi = as.numeric(SIGMA_SQ[i]),
                          DELTA = as.vector(DELTA)[js_OVERALL],
                          ALPHA = as.vector(ALPHA)[js_OVERALL],
                          J = length(js_OVERALL), #as.integer(J),
                          method = "BFGS",
                          hessian = TRUE)

      if(out_OVERALL$convergence==0){ #If converged, produce score.
        MAP_OVERALL[i,] = out_OVERALL$par
        OVERALL_SE[i,] = 1/sqrt(out_OVERALL$hessian)
      } else {
        notes_i = paste0(notes_i, "Scoring procedure for OVERALL scale did not converge; OVERALL score not produced.")
      }


    }

    # Calculate long form, domain specific subscores
    if(with(scales_i, MOT==T | COG==T | LANG == T | SEM == T | OVERALL == T)){

      out_LF = optim(par = as.vector(THETA0_LF[i,]),
                     fn = lf_posterior_density,
                     gr = lf_grad_posterior_density,
                     Yi = as.vector(Y[i,]),
                     MUi = as.vector(MU_LF[i,]),
                     invS =invS,
                     TAU = TAU,
                     LAMBDA = LAMBDA,
                     J = J,
                     K = K,
                     method = "BFGS",
                     hessian = TRUE)

      if(out_LF$convergence == 0){
        MAP_LF[i,] = out_LF$par
        SE_LF[i,] = sqrt(diag(solve(out_LF$hessian,diag(K))))

      } else {
        notes_i = paste0(notes_i, "Multidimensional scoring procedure scale did not converge; subscores not produced.")
      }

    }

    if(!is.null(notes_i)){
      NOTES[i] = notes_i
    }

    setTxtProgressBar(pb, i)
  }

  # Clean up the MAP_LF and SE_LF
  MAP_LF = data.frame(round(MAP_LF,3)+50)
  SE_LF = data.frame(round(SE_LF,3)); names(SE_LF) = paste(names(SE_LF),"_SE", sep = "")
  MAP_LF$OVERALL = round(MAP_OVERALL,3)+50
  SE_LF$OVERALL_SE = round(OVERALL_SE,3)

  # Put in the input depending on whether dataset is SF or LF
  output_scored = cbind(data.frame(ID = cleaned_df$ID), MAP_LF, SE_LF, FLAGS, NOTES) %>% 
    select(-starts_with("SF")) %>% 
    select(-ends_with("SF"))

  # Calculate in the standardized estimates
  AGE <- cleaned_df %>%
    select(c(ID, AGE)) %>%
    mutate(AGE = floor(AGE))

  output_scored <- output_scored %>%
    merge(AGE, by = "ID") %>%
    merge(zscoredat, by = "AGE") %>%
    mutate(Z_OVERALL = (OVERALL - OVERALL_mu) / OVERALL_sigma,
           Z_COG = (COG - COG_mu) / COG_sigma,
           Z_LANG = (LANG - LANG_mu) / LANG_sigma,
           Z_SEM = (SEM - SEM_mu) / SEM_sigma,
           Z_MOT = (MOT - MOT_mu) / MOT_sigma) %>%
    select(ID, AGE, COG, LANG, MOT, SEM, OVERALL,
             Z_COG, Z_LANG, Z_MOT, Z_SEM, Z_OVERALL,
             COG_SE, LANG_SE, MOT_SE, SEM_SE, OVERALL_SE,
             COG_SE, LANG_SE, MOT_SE, SEM_SE, OVERALL_SE,
             COG_flag, LANG_flag, MOT_flag, OVERALL_flag, NOTES)

  #Sanitize Short Form data
  if(is_sf == TRUE){
    output_scored <- output_scored %>%
      select(c("ID", "OVERALL", "OVERALL_SE", "Z_OVERALL", "NOTES"))
  }

  #Write out the output df
  output_df = merge(x = input_df, y = output_scored, by = "ID") #re-merge with original data.

  # Write out the data
  if(interactive == TRUE){
    out_dlgDir = dlgSave(default = csv_wd, title = "Save scores as", gui = .GUI)
    out_csv = paste(strsplit(out_dlgDir$res,"/")[[1]],collapse = "/")

    if (!endsWith(out_csv,".csv")){out_csv = paste(out_csv, ".csv", sep = "")}
    readr::write_csv(output_df, path = out_csv)

    log[length(log)+1] = paste("\n Scores written to ", out_csv,".", sep = "")

    txt_wd = paste(strsplit(out_csv,"/")[[1]][-length(strsplit(out_csv,"/")[[1]])],collapse = "/")
    out_txt = paste(txt_wd,"/logfile - CREDI scoring.txt", sep = "")
    write_log(log = log, folder = txt_wd, file = out_txt)

    writeLines("\n")
    writeLines( paste("\n Scores written to ", out_csv,".", sep = "") )

  }

  return(list(scores = output_df, log = log))
}
