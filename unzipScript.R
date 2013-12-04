allzips <- paste0(getwd(),"/",list.files(path=getwd(),pattern="*.zip"))
sapply(allzips,function(x) {
  unzip(zipfile=x,overwrite=F)
})