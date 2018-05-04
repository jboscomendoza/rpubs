download.file(url = "http://openpsychometrics.org/_rawdata/IQ1.zip",
              destfile = "IQ1.zip")

unzip("IQ1.zip")

iq1 <- read.csv("IQ1/data.csv")

head(iq1)

iq1[c("score", "gender", "age")] <- NULL

iq1 <- data.frame(
  lapply(iq1,function(x) {
    ifelse(x == 10, 1, ifelse(x == 0, NA, 0))
  })
)

head(iq1)

install.packages("psych")

library(psych)

alfa <- alpha(iq1)

alfa

iq1_sin_Q21 <- iq1
iq1_sin_Q21["Q21"] <- NULL
alfa_sin_Q21 <- alpha(iq1_sin_Q21)

