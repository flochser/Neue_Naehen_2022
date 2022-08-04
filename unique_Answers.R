## regrouped
## 
## ## produced uniques to regroup

tab <- data.frame(x=rep("o",155))

for (t in free_table_list) {
  name <- paste(t[["Question_ID"]][1],"_an1",sep="")
  u_list <- c()
  for (i in t$Antwort) {
    for (s in strsplit(i,",")) {
      u_list <- append(u_list, s)
    }
  }
  u_list <- unique(u_list)
  na <- rep(NA,155 - length(u_list))
  u_list <- append(u_list,na)
  tab <- cbind(tab,u_list)
  colnames(tab)[ncol(tab)] <- name
} 

for (t in free_table_list) {
  name <- paste(t[["Question_ID"]][1],"_an2",sep="")
  u_list <- c()
  for (i in t$Antwort.2) {
    for (s in strsplit(i,",")) {
      u_list <- append(u_list, s)
    }
  }
  u_list <- unique(u_list)
  na <- rep(NA,155 - length(u_list))
  u_list <- append(u_list,na)
  tab <- cbind(tab,u_list)
  colnames(tab)[ncol(tab)] <- name
} 

write.csv(tab,file= "zusammenfassung.csv")