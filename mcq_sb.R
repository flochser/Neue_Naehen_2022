library(ggplot2)
library(dplyr)
library(stringr)
library(extrafont)
library(remotes)
remotes::install_version("Rttf2pt1", version = "1.3.8")
extrafont::font_import(path = "/home/ff/WiLaLe/CD", pattern = "Drescher Grotesk BT SemiBold")

setwd("/home/ff/WiLaLe/Superblocks/Umfrage/Neue_Naehen_2022")

#Variables and fundamental tables
mc_data <- read.csv("MC_Questions_Data.csv")
mc_answers <- read.csv("MC_Questions.csv")
questions <- read.csv("Questions.csv")
free_answered <- read.csv("Data_Umfrage_Str.csv")
free_answered <- free_answered[free_answered$QUESTION_TYPE!="MC",]

participants <- max(mc_data$PERSON_ID)
free_table_list <- list()
mc_table_list <-list()
mc_sum_table_list <- list()


#functions
prep_pie_chart <- function(x) {
  x <- x %>%
    arrange(desc(ANSWER)) %>%
    mutate(ans_qu = str_wrap(ans_qu,40)) %>%
    mutate(ans_qu = factor(ans_qu,ans_qu)) %>%
    mutate(prop = ANSWER / sum(x$ANSWER) *100) %>%
    mutate(ypos = cumsum(prop) - 0.5 *prop)
}


#make lists for executions
for (qu in unique(free_answered$Question_ID)) {
  table <- free_answered[free_answered$Question_ID == qu,]
  name <- paste(qu,"_table",sep = "") 
  free_table_list <- append(free_table_list,list(table))
}


for (qu in unique(mc_data$QUESTION_ID)) {
  table <- mc_data[mc_data$QUESTION_ID == qu,]
  mc_table_list <- append(mc_table_list,list(table))
  for (answer in unique(table$CHOICE_ID)) {
    ans_qu <- mc_answers[mc_answers$MC_F_ID == qu,3]
    sums <- aggregate(ANSWER ~ CHOICE_ID, table,sum)
    sum_table <- cbind(ans_qu,sums)
    sum_table$percentage <- sums$ANSWER/participants*100
    mc_sum_table_list <- append(mc_sum_table_list,list(sum_table))
  }
}
rm("qu","name","table","sums","answer","ans_qu","sum_table")

mc_sum_table_list <- lapply(mc_sum_table_list,prep_pie_chart)




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


ggplot(F01_sum_table, aes(x=ans_qu, y=ANSWER, fill=str_wrap(ans_qu,40),width=.95)) +
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



## produced uniques to regroup

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
