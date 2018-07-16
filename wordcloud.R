library(tidyverse)
library(wordcloud)
library(readxl)


################################################################################
############################# reading data #####################################

setwd("C:\\Users\\ba3ef8\\Google Drive\\bilder\\WORDCLOUD_Lenny\\")

data <- read_excel("woerter.xlsx")

# reshape from wide to long
data_new <- gather(data, month, wort, starts_with("Lenny_"))
data_new2 <-gather(data, month, freq, starts_with("Freq_"))
data_new3 <-gather(data, month, deutsch, starts_with("Deutsch"))
data_new4 <-gather(data, month, eng, starts_with("Eng"))

# convert month string to numeric
data_new <- data_new %>% mutate(mon=substr(month,7,8))

# combine data sets
data_NEW <- as.data.frame(c(data_new[,c("mon","wort")],data_new2[,"freq"],data_new3[,"deutsch"],data_new4[,"eng"]))

# exclude missing values
data_NEW <- data_NEW %>% filter(data_NEW$wort!="NA")

# clean workspace
rm(data_new, data_new2, data_new3, data_new4)


################################################################################
############################# plotting #########################################

# seed for replication
set.seed(14091985)

# adding random noise to frequency variables
data_NEW$freq<- data_NEW$freq+rnorm(n=nrow(data_NEW),mean=0,sd=1)


# plot Lenny
for(i in 12:17){
  # define path to save files
  mypath <- file.path("C:","Users//ba3ef8//Google Drive//bilder//WORDCLOUD_Lenny//",paste("WCalt_Lenny_", i, ".jpg", sep = ""))
  # keep data for each month
  data2 <- data_NEW[data_NEW$mon==1*i,]  
  # prepare jpeg plot
  jpeg(file=mypath)
  # plot word cloud
  wordcloud(data2$wort, freq=data2$freq, min.freq=1, random.order=FALSE, rot.per=0, colors=brewer.pal(8, "RdYlBu"))
  # close picture device
  dev.off()
}


# plot deutsch
for(i in 12:17){
  mypath <- file.path("C:","Users//Christoph//Google Drive//bilder//WORDCLOUD_Lenny//",paste("WCalt_Deutsch_", i, ".jpg", sep = ""))
  data2 <- data_NEW[data_NEW$mon==1*i,]    
  jpeg(file=mypath)
  wordcloud(data2$deutsch, freq=data2$freq, min.freq=1, random.order=FALSE, rot.per=0, colors=brewer.pal(8, "RdYlBu"))
  dev.off()
}

# plot englisch
for(i in 12:17){
  mypath <- file.path("C:","Users//Christoph//Google Drive//bilder//WORDCLOUD_Lenny//",paste("WCalt_Englisch_", i, ".jpg", sep = ""))
  data2 <- data_NEW[data_NEW$mon==1*i,]   
  jpeg(file=mypath)
  wordcloud(data2$eng, freq=data2$freq, min.freq=1, random.order=FALSE, rot.per=0, colors=brewer.pal(8, "RdYlBu"))
  dev.off()
}


