ecg <- read.csv("ecg_sample.csv")
ecg$Time <- ecg$Time - min(ecg$Time)              
summary(ecg)

# Subtract the max negative value to make the dataset non negative
c<-(-874)
ecg[,] <- t(apply(ecg[,], 1, function(x) x-c))
summary(ecg)

plot_timespan <- function(t1, t2, ecg_data){
  with(ecg_data[ecg_data$Time >=t1 & ecg_data$Time < t2,],
       plot(ec ~ Time, type='l')
  )
}

#Plotting the heart rate for 1 minute, 7500th obervation
plot_timespan(0, 2725, ecg)

# Here 7500 stands for 1 minute worth of data (125 * 60 * number of minutes)
i <- 125
newecg <- ecg[1:i,]
object.size(newecg$ec)
summary(newecg)

library(caTools)
encoding1 <- base64encode(newecg$ec, size=NA, endian=.Platform$endian)
nchar(encoding1)
encoding1

pack2values <- function(v1, v2){ # must be non-negative integers in 12-bit range
  mask8 <- 255
  bitvec <- bitwShiftL(v1, 12) + v2 # 24 bit: v1v2
  as.raw(c(bitwAnd(bitwShiftR(bitvec, 16), mask8), bitwAnd(bitwShiftR(bitvec, 8), mask8), bitwAnd(bitvec, mask8)))
  # 24 bits 0111000110010001011001010 -> c("01110001", "10010001", "11001010") # 8 bit values
}



# 125 integer 12 bit -> how many base64 characters can it be encoded to, How short can I make it?
# Subtract 2000 to make a unifrom range, make them all positive 
# Trying to use base64 encoding from the library
# Checksum for ?
# How to take 12 bits from the 64 bit, use bitwise operators in R to bitwshiftl, shiftr, bitwn 

unpack2values <- function(pv){ # 3 8 bit values
  mask4 <- 15
  mask12 <- 2047 
  makeLast40s <- 4080
  pv1 <- as.integer(pv[1])
  pv2 <- as.integer(pv[2])
  pv3 <- as.integer(pv[3])
  
  res <- bitwAnd(bitwShiftL(bitwAnd(pv1, mask12), 4), makeLast40s) + bitwAnd(bitwShiftR(pv2,4), mask4) 
  
  res2 <- bitwShiftL(bitwAnd(pv2, mask4), 8) + pv3
  
  c(res, res2)

}

ans <- pack2values(190,100)
unpack2values(ans)


