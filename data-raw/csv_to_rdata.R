thefiles.upperlevel <- grep("csv", list.files(), value = T)
thedata.upperlevel <- gsub(".csv", "", thefiles.upperlevel)

thefiles.fluor <- grep("csv", list.files("autofluorescence/"), value = T)
thedata.fluor <- gsub(".csv", "", thefiles.fluor)

thefiles.examples <- grep("csv", list.files("examples_chapters_1-4/"), value = T)
thedata.examples <- gsub(".csv", "", thefiles.examples)

thefiles.surv <- grep("csv", list.files("survival/"), value = T)
thedata.surv <- gsub(".csv", "", thefiles.surv)

for(i in 1:length(thefiles.upperlevel)){
  temp <- read.csv(thefiles.upperlevel[i], stringsAsFactors = FALSE)
  assign(thedata.upperlevel[i], temp)
  save(list=thedata.upperlevel[i], file = paste("../data/", thedata.upperlevel[i],".RData", sep = ""))
}

setwd("autofluorescence/")
for(i in 1:length(thefiles.fluor)){
  temp <- read.csv(thefiles.fluor[i], stringsAsFactors = FALSE)
  assign(thedata.fluor[i], temp)
}
save(list=thedata.fluor, file = "../../data/autofluorescence.RData")

setwd("../examples_chapters_1-4/")
for(i in 1:length(thefiles.examples)){
  temp <- read.csv(thefiles.examples[i], stringsAsFactors = FALSE)
  assign(thedata.examples[i], temp)
  save(list=thedata.examples[i], file = paste("../../data/", thedata.examples[i], ".RData", sep = ""))

}


setwd("../survival/")
for(i in 1:length(thefiles.surv)){
  temp <- read.csv(thefiles.surv[i], stringsAsFactors = FALSE, na.strings="?")
  assign(thedata.surv[i], temp)
}
save(list=thedata.surv, file = "../../data/survival.RData")
