write_log<-function(log, folder, file = NULL){

  if (is.null(file)){
    out_dlgDir = dlgSave(default = folder, title = "Save as", gui = .GUI)
    out_txt = paste(strsplit(out_dlgDir$res,"/")[[1]],collapse = "/")
  } else {
    out_txt = file;
  }
  if (!endsWith(out_txt,".txt")){out_txt = paste(out_txt, ".txt", sep = "")}
  sink(file = out_txt)
  for (l in 1:length(log)){
    if (is.character(log[[l]])){
      writeLines(log[[l]])
    } else {
      print(log[[l]])

    }
  }
  sink()

}
