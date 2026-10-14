
library("dplyr")
library("readr")


dat <- read_csv("X:/My Downloads/voorbeelddata.csv")

dat$leraar_regie <- recode(dat$leraargedrag, '11'='1', '12'='1', '21'='2', '22'='2', '31'='3', '32'='3', '41'='4', '42'='4', '52'='5')
dat$leraar_nabijheid <- recode(dat$leraargedrag, '11'='1', '12'='2', '21'='1', '22'='2', '31'='1', '32'='2', '41'='1', '42'='2', '52'='2')

dataa <- dat[c(1,3,7,8)]
write.csv(dataa,"X:/My Downloads/voorbeelddata.csv", row.names = FALSE)
