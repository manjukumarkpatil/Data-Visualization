data <- EHRdataSample_Sheet0
temp <-""
#fileConn<-file("/Users/suhas/Downloads/output.txt")
count <- rep(0,length(unique(data$PatientID)))
patient_id <- 1
injury_1 <- 'suhas'
injury_2 <-  'array'
e_time <-  ''
df <-data.frame(p = patient_id,i1 = injury_1,i2=injury_2,t = e_time)
for(k in 1:length(unique(data$PatientID))){
  flag <- rep(0,16)
  m_data <- data[data$PatientID == unique(data$PatientID)[k],]
  m1_data <- m_data[,19:34]
  colNames <- names(m1_data)
  colNames[17] <- 'Date'
  m1_data <- cbind(m1_data,m_data$Encounter_date)
  names(m1_data) <- colNames
  m1_data <- m1_data[order(m1_data$Date),]
  sum(m1_data[,1:16])
  
  #-------------
  
  
  string1 <- NA
  string2 <- NA
  for(i in 1:dim(m1_data)[1]){
    for(j in 1:16){
      if(m1_data[i,j] == 1){
        if(is.na(string1)&flag[j]!=1){
          string1 <- colNames[j]
          flag[j]=1
        }else{
          string2 <- colNames[j]
          if(string1 != string2){
            patient_id <- c(patient_id,unique(data$PatientID)[k])
            injury_1 <- c(injury_1,string1)
            injury_2 <- c(injury_2,string2)
            e_time <- c(e_time,m1_data$Date[i])
            df <- rbind(df,c(patient_id,str(injury_1),str(injury_2),str(e_time) ))
            print(paste(temp,unique(data$PatientID)[k],string1,string2,m1_data$Date[i],'\n'))
            string1 = string2
            count[k] <- count[k]+1
          }
        }
      }
    }
  }
}
#writeLines(temp, fileConn)
#close(fileConn)
