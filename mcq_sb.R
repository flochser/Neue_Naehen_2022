library(ggplot2)
library(dplyr)
library(stringr)
library(extrafont)
library(remotes)
library(readxl)
#remotes::install_version("Rttf2pt1", version = "1.3.8")
extrafont::font_import(path = "/home/ff/WiLaLe/CD", pattern = "Drescher Grotesk BT SemiBold")

setwd("/home/ff/WiLaLe/Superblocks/Umfrage/R_Umfrage/Neue_Naehen_2022/")

#LOAD DATA

qu_multichoice <- read.csv("MC_Questions_Data.csv")
questions <- read.csv("Questions.csv")
ans_strings_multichoice <- read.csv("MC_Questions.csv")
qu_free_answered <- read.csv("corrected_Data_Umfrage_Str.csv")
qu_free_answered <- qu_free_answered[qu_free_answered$QUESTION_TYPE!="MC",]
qu_binary <- qu_free_answered[qu_free_answered$QUESTION_TYPE=="BI",]

ex_sheets <- excel_sheets("zusammenfassung.xlsx")
exchange_list <- lapply(ex_sheets, function(x) {
  as.data.frame(read_excel("zusammenfassung.xlsx", sheet = x)) } )
names(exchange_list) <- ex_sheets

#VARIABLES

participants <- max(qu_multichoice$PERSON_ID)
question_list <- list()
free_table_list <- list()
mc_table_list <-list ()
bi_table_list <- list()

#FUNCTIONS



##SUM UP TABLES FOR PLOTTING

#FILL LIST OF BINARY CHOICE TABLES

for (qu in unique(qu_binary$Question_ID)) {
  table <- qu_binary[qu_binary$Question_ID == qu,]
  na <- sum(is.na(table$Antwort))
  yes <- sum(table$Antwort == 1, na.rm = TRUE)
  no <- sum(table$Antwort == 0, na.rm = TRUE)
  table <- data_frame("qu" = qu, yes,no,na)
  bi_table_list <- append(bi_table_list,list(table))
  question_list <- append(question_list,qu)
}
names(bi_table_list) <- question_list
question_list <- list()

#FILL LIST OF MULTIPLE CHOICE TABLES

for (qu in unique(qu_multichoice$QUESTION_ID)) {
  table <- qu_multichoice[qu_multichoice$QUESTION_ID == qu,]
  sum_table <- aggregate(ANSWER ~ CHOICE_ID, table,sum)
  sum_table$ans_qu <- ans_strings_multichoice[ans_strings_multichoice$MC_F_ID == qu,3]
  sum_table$percentage <- sum_table$ANSWER/participants*100
  sum_table$QUESTION_ID <- rep(qu,nrow(sum_table))
  mc_table_list <- append(mc_table_list,list(sum_table))  
  question_list <- append(question_list,qu)
}
names(mc_table_list) <- question_list
question_list <- list()
mc_table_list <- lapply(mc_table_list,prep_pie_chart)

#FILL LIST OF FREE STRING ANSWER TABLES

for (table_name in ex_sheets){
  exchange_table <- exchange_list[[table_name]]
  where <- str_split(table_name,"_")
  table <- qu_free_answered[qu_free_answered$Question_ID == where[[1]][1],]
  answers <- table[,where[[1]][2]]
  split_up <- c()
  for (element in answers){
    if (is.na(element)){next}
    split_element <- str_split(element,",")
    for (e in split_element){split_up <- append(split_up, e)}
  }
  for (i in 1:length(split_up)){
    for (col in 1:ncol(exchange_table)){
      if (split_up[i] %in% exchange_table[,col]) {
        split_up[i] <- colnames(exchange_table)[col]
      }
    }
  }
  sum_table <- as.data.frame(table(split_up))
  free_table_list <- append(free_table_list,list(sum_table))
}
names(free_table_list) <- ex_sheets

rm("qu","table","yes","no","na")

prep_pie_chart <- function(x) {
  x <- x %>%
    arrange(desc(ANSWER)) %>%
    mutate(ans_qu = str_wrap(ans_qu,40)) %>%
    mutate(ans_qu = factor(ans_qu,ans_qu)) %>%
    mutate(prop = ANSWER / sum(x$ANSWER) *100) %>%
    mutate(ypos = cumsum(prop) - 0.5 *prop)
}

make_barplot_MC <- function(data,abscissa,ordinate) {
  ggplot(data, aes(x=abscissa, y=ordinate, fill=str_wrap(ans_qu,40),width=.95)) +
    geom_bar(stat="identity", width=1, color="darkgrey") +
    ggtitle(questions[questions$question_ID=="F01",2]) +
    theme(text=element_text(family="Drescher Grotesk BT Semibold"), 
          legend.title=element_blank(),
          plot.title = element_text(size = 21, hjust = -1, vjust=.5),
          legend.text = element_text(size = 11),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())+
    ylab("")
}

make_barplot_free <- function(data,abscissa,ordinate,title="default") {
  ggplot(data, aes(x=abscissa, y=ordinate, fill=str_wrap(split_up,40),width=.95)) +
    geom_bar(stat="identity", width=1, color="darkgrey") +
    ggtitle(title)
}


###PLOTS

ggplot(F18_sum_table, aes(x="", y=prop, fill=ans_qu)) +
  geom_bar(stat="identity", width=1, color="darkgrey") +
  coord_polar(theta = "y", start=0, direction = -1) +
  theme_void() + 
  ggtitle(questions[questions$question_ID=="F18",2]) +
  theme(text=element_text(family="Drescher Grotesk BT Semibold"), 
        legend.title=element_blank(),
        plot.title = element_text(size = 21, hjust = 0, vjust= -1),
        legend.text = element_text(size = 11))+
  #labs(title = questions[questions$question_ID=="F01",2]) +
  geom_text(aes(x = 1.2, y = 100 - ypos, label = ANSWER), color = "black", size=4) +
  scale_fill_brewer(palette="Set3")

