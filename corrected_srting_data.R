for (i in 1:nrow(test)){
  if (between(test[i,"Person_ID"],60,96)){
    if (test[i,"QUESTION_TYPE"] == "BI"){
      x <- append(x,test[i,"Antwort"])
      if (is.na(test[i,"Antwort"])){
        next
      }
      else if (test[i,"Antwort"] == "1"){
        test[i,"Antwort"] <- "0"
      }
      else if (test[i,"Antwort"] == "2"){
        test[i,"Antwort"] <- "1"
      }
      else if (test[i,"Antwort"] == "1,2"){
        test[i,"Antwort"] <- "1"
      }
      y <- append(y,test[i,"Antwort"])
    }
  }
}
write.csv(test,"corrected_Data_Umfrage_Str.csv")


##NAMEN EINTAUSCHEN

all <- excel_sheets("zusammenfassung.xlsx")
list <- lapply(all, function(x) {          # Read all sheets to list
  as.data.frame(read_excel("zusammenfassung.xlsx", sheet = x)) } )

new_qu_free_answered <- qu_free_answered

for (element in answers){
  if(is.na(element)){next}
  
}

ggplot(plot_table, aes(as.character(split_up), Freq ,width=.95)) +
  geom_col()
