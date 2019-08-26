# Date: 9/27/2018
# Updated On: 9/27/2018
# Author: Yiran Jia
# Project Name: Steward Health System Tableau Report Prepare 
#
# This code is helping prepareing the report in Tableau 
# Prerequesties for using this code below:
# name the column name to Region, Account, Modality, and Install
# no blanks in Region
# no blanks in account name 
# no blanks in modality 
# no weird date/special/characters in install, blanks are ok 

library(dplyr)
library(lubridate)

steward <- read.csv("Steward_Data.csv", stringsAsFactors = FALSE, na.strings = "") %>% 
  filter(Modality != "3D Mammo",
         Modality != "ACC",
         Modality != "DEXA",
         Modality != "Injector",
         Modality != "MAM",
         Modality != "Mammo",
         Modality != "NUC",
         Modality != "NM",
         Modality != "DEN",
         Modality != "ICAP",
         Modality != "LAC",
         Modality != "SIM",
         Modality != "UPS",
         Modality != "IR OP/IP  (Fluoro)") # "Those modality are deleted from the list because Philips does no longer manufacte." - Brad Pollock

steward[steward$Modality == "CRM", "Modality"] <- "C-Arm"
steward[steward$Modality == "PORXR", "Modality"] <- "Portable X-ray"
steward[steward$Modality == "UL", "Modality"] <- "Ultrasound"
steward[steward$Modality == "ULP", "Modality"] <- "Ultrasound"
steward[steward$Modality == "US", "Modality"] <- "Ultrasound"
steward[steward$Modality == "USP", "Modality"] <- "Ultrasound"
steward[steward$Modality == "XR", "Modality"] <- "X-ray"
steward[steward$Modality == "DXR", "Modality"] <- "X-ray"
steward[steward$Modality == "CV", "Modality"] <- "IGT"
steward[steward$Modality == "IXR", "Modality"] <- "IGT"
steward[steward$Modality == "PET", "Modality"] <- "PET/CT"


steward$Install <- as.Date(steward$Install, format = "%m/%d/%Y")
steward <- steward %>% mutate(Replace = ymd(Install) + years(10))

steward$Install <- as.integer(format(steward$Install,"%Y"))
steward$Replace <- as.integer(format(steward$Replace, "%Y"))
mean_install <- steward %>% filter(!is.na(Install)) %>% summarise(mean = mean(Install)) %>% as.integer()
steward[is.na(steward$Install), "Install"] <- mean_install
mean_replace <- steward %>% filter(!is.na(Replace)) %>% summarise(mean = mean(Replace)) %>% as.integer()
steward[is.na(steward$Replace), "Replace"] <- mean_replace


# to create report 1

steward1 <- steward %>% mutate(Age = 2018 - Install, Green = NA, Yellow = NA, Red = NA)

mark <- function(x) {
  if (x[, "Age"] <= 5) {
    x[, "Green"] <- 1
  } else if (x[, "Age"] <= 10) {
    x[, "Yellow"]  <- 1
  } else {
    x[, "Red"]  <- 1
  }
  return(x)
}

for (i in 1:nrow(steward1)) {
  steward1[i,] <- mark(steward1[i,])
}
steward1[is.na(steward1)] <- 0

write.csv(steward1, "Steward_Tableau1.csv")



# to create report 2 
steward2 <- steward %>% mutate(Age = 2018 - Install, Status = NA)

mark <- function(x) {
  if (x[, "Age"] <= 5) {
    x[, "Status"] <- "Green"
  } else if (x[, "Age"] <= 10) {
    x[, "Status"]  <- "Yellow"
  } else {
    x[, "Status"]  <- "Red"
  }
  return(x)
}

for (i in 1:nrow(steward2)) {
  steward2[i,] <- mark(steward2[i,])
}
steward2[is.na(steward2)] <- 0

write.csv(steward2, "Steward_Tableau2.csv")

