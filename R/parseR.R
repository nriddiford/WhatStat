#' parseR
#'
#' The main parsing function for extracting a dataframe from WhatsApp chat log
#' @param in_file Whats App chat log (.txt or .zip)
#' @param user Select a user
#' @import stringi dplyr stringr lubridate tools
#' @export
parseR <- function(in_file, drop="44", user=FALSE){

  if(file_ext(in_file)=='zip'){
    unzipped <- unzip(in_file)
    rawData<-scan(unzipped, what="", sep="\n")
    if (file.exists(unzipped)) file.remove(unzipped)
  }

  if(file_ext(in_file)=='txt'){
    rawData<-scan(in_file, what="", sep="\n")
  }

  joinedData <- rep(NA, length(rawData))

  gr <- 1
  for (i in 1:length(rawData)) {
    # if starting with timestamp, save into out and move on (gr)
    find.startline <- regexpr("^\\[?\\d{1,2}\\/\\d{1,2}\\/\\d{2,4}", rawData[i], perl = TRUE)
    if (find.startline) {
      joinedData[gr] <- rawData[i]
      gr <- gr + 1
    }

    if (!find.startline) {
      # if doesn't start with timestamp, append to previous (ss)
      ss <- gr - 1
      joinedData[ss] <- paste(joinedData[ss], rawData[i])
    }
  }

  joinedData <- joinedData[!is.na(joinedData)]

  joinedData <- as.data.frame(joinedData,row.names = NULL, optional = FALSE )
  colnames(joinedData)<-'V1'


  # get rid of square brackets around datetime
  if(grepl("^\\[", joinedData$V1[5], perl = TRUE)){
    joinedData$V1 <- sub("^\\[.*?", "", joinedData$V1)
    joinedData$V1 <- sub("\\].*?", ":", joinedData$V1)
  }

  phonetype = 'iPhone'
  if(str_split(head(joinedData$V1,1), " ",simplify = TRUE)[,3] == "-"){
    phonetype = 'android'
  }

  if(phonetype == 'android'){
    sepData<-suppressWarnings(tidyr::separate(joinedData, V1, c("datetime", "message"), sep = ": ", extra = "merge"))
    sepData<-suppressWarnings(tidyr::separate(sepData, datetime, c("datetime", "sender"), sep = "- ", extra = "merge"))
  } else {
    sepData<-suppressWarnings(tidyr::separate(joinedData, V1, c("datetime", "sender", "message"), sep = ": ", extra = "merge"))
  }

  sepData$message<- stringi::stri_trans_general(sepData$message, "latin-ascii")

  sepData$message <- trimws(sepData$message)

  filtData <- sepData %>%
    group_by(sender) %>%
    filter(n() >= 2) %>%
    ungroup() %>%
    filter(!str_detect(sender, 'changed|left|added|created')) %>%
    filter(!grepl('\\+', sender)) %>%
    filter(!is.na(message)) %>%
    mutate(sender = as.factor(sender)) %>%
    droplevels()

  if(phonetype == 'android'){
    suppressWarnings(filtData$datetime<-dmy_hm(filtData$datetime))
  } else {
    suppressWarnings(filtData$datetime<-dmy_hms(filtData$datetime))
  }

  cleanData<-tidyr::separate(filtData, datetime, c("date", "time"), sep = " ", remove =TRUE)

  return(cleanData)
}
