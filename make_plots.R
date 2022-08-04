library(ggplot2)


mc_table_list[[1]]

ggplot(mc_table_list[[1]], aes(x=ans_qu, y=ANSWER, fill=str_wrap(ans_qu,40),width=.95)) +
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
