#' Score CREDI response data
#'
#' This calculates the posterior density function.
#' @param data (data.frame) Defaults to NULL. Response data. If NULL, then user is prompted to identify a .csv file with response data. Defaults to NULL.
#' @param reverse_code (Logical) Defaults to TRUE. If TRUE, then reverse coding is automated to appropriately handle the negatively worded items LF9, LF102, LFMH1, LFMH2, LFMH3, LFMH4, LFMH5, LFMH7, LFMH8, & LFMH9. If FALSE, then no reverse coding is applied.
#' @param interactive (Logical) Defaults to TRUE. If TRUE, the user may be prompted with caution messages regarding whether scoring should be continued, where to save the scores, where to save a logfile, etc. If FALSE, continuation is assumed and scores and the user is not prompted to save scores or a logfile.
#' @keywords CREDI
#' @export
#' @examples
#' score()

#
# reverse_code = FALSE
# save_logfile = TRUE
# interactive = FALSE
# data = input_df

score<-function(data = NULL, reverse_code = TRUE, interactive = TRUE){



    # Identify if dialog specifying .csv file should be bypassed.
    bypass = ifelse(is.null(data), FALSE, TRUE)
    if (bypass == TRUE){
      if (!is.data.frame(data)){
        stop("data argument must be type data.frame")
      }
    }

    # Load required pacakges

    require("stats")
    require("svDialogs")

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
      input_df = read.csv(file = csv_file)
    } else {
      input_df = data
    }

    # Clean the input data
    list_cleaned = clean(input_df = input_df, mest_df = mest_df, reverse_code = reverse_code,
                         interactive = interactive, log = log)
    log = list_cleaned$log
    if(list_cleaned$stop!=0){

      print("*Error: Processing the provided response data resulted in errors. See log for more details.")

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

      return(list(log = log))

    } # End if stop != 0
    cleaned_df = list_cleaned$cleaned_df
    items_noresponse = list_cleaned$items_noresponse

    # Crate data matricies
    X = model.matrix(~1 + I( (AGE-18)/10.39 ) + I( ((AGE-18)/10.39)^2 ) + I( ((AGE-18)/10.39)^3 ), data = cleaned_df)
    X_4 = model.matrix(~1 + I( (AGE-18)/10.39 ) + I( ((AGE-18)/10.39)^2 ) + I( ((AGE-18)/10.39)^3 ) + I( ((AGE-18)/10.39)^4 ), data = cleaned_df)
    Y = as.matrix(cleaned_df[,-match(c("ID","AGE",items_noresponse), names(cleaned_df))]); Y[is.na(Y)] = -9L
    MU_LF = X%*%as.matrix(B) #NxK (matrix)
    MU_SF = X%*%as.numeric(beta) #Nx1

    # Obtain necessary parameter matricies
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
    Z_LF = MAP_LF
    Z_SF = MAP_SF
    SE_LF = MAP_LF
    SE_SF = MAP_SF
    writeLines(paste("\nScoring ", N, " observations:"))
    pb<-txtProgressBar(min = 0, max = N, initial = 0, style = 3)
    for (i in 1:N){

      # Obtain the standardized estimates
      center_i = X_4[i,] %*% as.matrix(normcoef_mean)
      scale_i =  X_4[i,] %*% as.matrix(normcoef_sd)

      # Score the long form
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
        fisherInfo = out_LF$hessian
        SE_LF[i,] = sqrt(diag(solve(fisherInfo,diag(K))))

        Z_LF[i,1:4] = (MAP_LF[i,]+50-center_i[1,1:4])/scale_i[1,1:4]

        # Average of the scores (note that the _SF is misleading b/c it is not on the same scale as short form)
        MAP_SF[i, 1] = mean(out_LF$par)
        SE_SF[i,1] = (1/1)*(1/sqrt(sum(sum(out_LF$hessian))))
        Z_SF[i,1] = weighted.mean(Z_LF[i,], w = (1./SE_LF[i,])^2)
      }


      # # Score the short form
      # out_SF = optim(par = as.vector(THETA0_SF[i,]),
      #                fn = sf_posterior_density,
      #                Yi = as.vector(Y[i,]),
      #                MUi = as.vector(MU_SF[i,]),
      #                SIGMA_SQi = as.numeric(SIGMA_SQ[i]),
      #                DELTA = as.vector(DELTA),
      #                ALPHA = as.vector(ALPHA),
      #                J = as.integer(J),
      #                method = "BFGS",
      #                hessian = TRUE)
      # if(out_SF$convergence == 0){
      #   MAP_SF[i,] = out_SF$par
      #   SE_SF[i,] = sqrt(1.0/out_SF$hessian)
      #   Z_SF[i,1] = (MAP_SF[i,]+50-center_i[1,5])/scale_i[1,5]
      # }


      setTxtProgressBar(pb, i)
    }

    # Clean up the MAP_LF and SE_LF
    MAP_LF = data.frame(round(MAP_LF,3)+50)
    SE_LF = data.frame(round(SE_LF,3)); names(SE_LF) = paste(names(SE_LF),"_SE", sep = "")

    # Clean up the MAP_SF and SE_SF
    MAP_SF = data.frame(OVERALL = round(MAP_SF,3)+50)
    SE_SF = data.frame(OVERALL_SE = round(SE_SF,3))

    #Clean the standardized estimates
    Z_LF = data.frame(round(Z_LF,3))
    names(Z_LF) = paste("z_",names(Z_LF), sep = "")

    Z_SF = data.frame(round(Z_SF, 3))
    names(Z_SF) = "z_OVERALL"

    # Put in the input
    output_df = cbind(data.frame(ID = cleaned_df$ID), Z_LF, Z_SF, MAP_LF, MAP_SF,SE_LF, SE_SF)

    # Write out the data
    if(interactive == TRUE){
        out_dlgDir = dlgSave(default = csv_wd, title = "Save scores as", gui = .GUI)
        out_csv = paste(strsplit(out_dlgDir$res,"/")[[1]],collapse = "/")

        if (!endsWith(out_csv,".csv")){out_csv = paste(out_csv, ".csv", sep = "")}
        write.csv(output_df, file = out_csv, row.names = FALSE)

        log[length(log)+1] = paste("\n Scores written to ", out_csv,".", sep = "")

        txt_wd = paste(strsplit(out_csv,"/")[[1]][-length(strsplit(out_csv,"/")[[1]])],collapse = "/")
        out_txt = paste(txt_wd,"/logfile - CREDI scoring.txt", sep = "")
        write_log(log = log, folder = txt_wd, file = out_txt)

        writeLines("\n")
        writeLines( paste("\n Scores written to ", out_csv,".", sep = "") )


    }



    return(list(scores = output_df, log = log))
}
