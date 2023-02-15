myMean<- function(data){
  mean = 0
  count = 0
  for (i in data){
    count = count + 1
    mean = mean + i
  }
  mean = mean / count
  return <- mean
}

V <- function(data) {
  l = 0
  mean = myMean(data)
  sd = 0
  for (i in data)
  {
    v = v + (i - mean)^2
    l = l + 1
  }
  v = v / l
  return <- v
}



print("Enter the data: ")
data = c(1,2,3,4,5,6)
v = V(data)
print(paste("variance of given data: ", v))




for (number in 1:100) {
  if (number %% 15 == 0){
    cat('FizzBuzz\n')
  }
  else if (number %% 3 == 0){
    cat('Fizz\n')
  }
  else if (number %% 5 == 0){
    cat('Buzz\n')
  }
  
  
}























x<-c(2.1,4.0,6.3,5.4,4.8,3.7,6.1,3.3)
y<-c(4.1,0.6,3.1,2.5,4.0,6.2,1.6,2.2,1.9,5.4)
print("Enter level of significance:")
alpha = 0.05
t<-wilcox.test(x,y,alternative = "two.sided")
print(t)
if (t$p.value > alpha){
  print("Accepted")
}else{
  print("Rejected")
}

