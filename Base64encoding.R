ecg <- read.csv("ecg_sample.csv")
ecg$Time <- ecg$Time - min(ecg$Time)              
summary(ecg)

# Subtract the max negative value to make the dataset non negative
# given a vector of 125 values, pack them using the same function and unpack them too.
# Same tests, important tests are near the boundaries.
# put those tests in the vector packing function,
# check which ones pass and which fail 
# random vector-> pack and unpack them and make sure they are still the same.

c<-min(ecg$ec)
ecg$ec <- ecg$ec-c
summary(ecg)
head(ecg)
# 
# plot_timespan <- function(t1, t2, ecg_data){
#   with(ecg_data[ecg_data$Time >=t1 & ecg_data$Time < t2,],
#        plot(ec ~ Time, type='l')
#   )
# }

#Plotting the heart rate for 1 minute, 7500th obervation
# plot_timespan(0, 2725, ecg)

# Here 7500 stands for 1 minute worth of data (125 * 60 * number of minutes)
i <- 125
newecg <- ecg[1:i,]
object.size(newecg$ec)
summary(newecg)

pack2values <- function(v1, v2){ # must be non-negative integers in 12-bit range
  mask8 <- 255
  bitvec <- bitwShiftL(v1, 12) + v2 # 24 bit: v1v2
  as.raw(c(bitwAnd(bitwShiftR(bitvec, 16), mask8), bitwAnd(bitwShiftR(bitvec, 8), mask8), bitwAnd(bitvec, mask8)))
}

newpack <- function(vec){
  count <- 1
  pack <- c()
  answer <- c()
  for (value in vec){
    pack<-append(pack,value)
    if(count%%2==0){
      res<-pack2values(pack[1],pack[2])
      answer <- append(answer,res)
      pack <- c()
    }
    count <- count +1
  }
  return(answer)
}

pack2values(newecg$ec[1], newecg$ec[2])
packedVector <- newpack(newecg$ec)
packedVector

library(caTools)
encodedVector <- base64encode(packedVector, size=NA, endian=.Platform$endian)
nchar(encodedVector)
encodedVector
typeof(packedVector)

typeof(encodedVector)

decodedVector <- base64decode(encodedVector, size=NA, typeof(packedVector))
nchar(decodedVector)
decodedVector

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
  answer <- c(res, res2)
  return (answer)
}

newunpack <- function(vec){
  count <- 1
  unpack<- c()
  result <- c()
  for(value in vec){
    unpack <- append(unpack, value)
    if(count%%3 == 0){
      answer <- unpack2values(unpack)
      result <- append(result, answer)
      print(result)
      unpack <-c()
    }
    count <- count + 1
  }
  return(result)
}

originalVector <- newunpack(decodedVector)
newecg$ec
originalVector

#Test if my vector is same
all.equal(packedVector, decodedVector)
all.equal(newecg$ec, originalVector)

# Write a series of tests for checking the unpacking
testVector <- round(runif(n = 20, min =0 , max = 2800))
testVector
for (i in seq_along(testVector)){
  testPack <-pack2values(testVector[i], testVector[i+1])
  print(testPack)
  testUnpack <-unpack2values(testPack)
  print(testUnpack)
}

# after I get the pack working for length N
all.equal(testVector, testUnpack)
ans <- pack2values(4096,6)
unpack2values(ans)
