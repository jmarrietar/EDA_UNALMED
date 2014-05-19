rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  data1<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
  
  #as.numeric(num)
  ## Check that state and outcome are valid
  lista<-list(data1$State)
  exist<-state %in% lista [[1]]
  
  if (exist == FALSE){
    stop("invalid state")
  }
  if(outcome != "heart attack" && outcome != "heart failure" && outcome !="pneumonia" ){
    stop("invalid outcome")
  }
  
  if(num=="best"){
    num<-1
  }
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
  if(outcome=="heart attack"){
    
    
    datasubset<-subset(data1,State==state,c("Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"))
    
    
    
    sort.name<-datasubset[order(datasubset$Hospital.Name),]
    
    sort.name[ , 2] <- as.numeric(sort.name[ , 2])
    
    complete<-subset(sort.name,complete.cases(sort.name))
    
    sort.data<-complete[order(complete$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
    if(num=="worst"){
      num<-length(sort.data[[1]])
    }
    
    
  }else if (outcome=="heart failure"){ 
    
    datasubset<-subset(data1,State==state,c("Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"))
    
    
    sort.name<-datasubset[order(datasubset$Hospital.Name),]
    
    sort.name[ , 2] <- as.numeric(sort.name[ , 2])
    
    complete<-subset(sort.name,complete.cases(sort.name))
    
    sort.data<-complete[order(complete$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]
    
    if(num=="worst"){
      num<-length(sort.data[[1]])
    }
    
  }else if(outcome=="pneumonia") {
    
    datasubset<-subset(data1,State==state,c("Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"))
    
    sort.name<-datasubset[order(datasubset$Hospital.Name),]
    
    sort.name[ , 2] <- as.numeric(sort.name[ , 2])
    
    complete<-subset(sort.name,complete.cases(sort.name))
    
    sort.data<-complete[order(complete$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]
    
    if(num=="worst"){
      num<-length(sort.data[[1]])
    }
    
    
  }
  
  hospital<-(as.character(sort.data[num,1]))
  return (hospital)
  
}