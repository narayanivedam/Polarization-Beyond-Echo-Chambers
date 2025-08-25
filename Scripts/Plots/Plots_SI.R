library(tidyverse)

data<- read_csv("~/Survey_data.csv")
new_data <- data %>% filter(Q2.3!=1 & Q3.3!=1)

# Outlet Classification
Outlets <- c("ABC News", "BBC News", "Business Insider", "Breitbart", "BuzzFeed","CBS News", "CNN", "Daily Caller","Fox News", "The Guardian", "The Hill", "HUFFPOST", "MSNBC", "NewsWeek", "NEWYORKPOST",
"The NewYork Times", "npr", "POLITICO", "TIME", "USA TODAY", "VICE", "VOX",
"WALL STREET JOURNAL", "Washington Examiner", "Washington Post", "NBC NEWS", "PBS")

bias_scores <- c(30/37,10/22,3/5,8,5/9,26/33,24/53,3,60/23,3/8,5/8,3/12,14/33,3/7,5/5,9/31,11/30,
                 5/14,4/10,10/13,2/6,1/8,11/15,4/2,8/26,28/40,11/22)

newsdata<- tibble(Outlets,bias_scores)
ordered_levels <- newsdata %>%
  arrange(bias_scores)
newsdata$Outlets <- factor(newsdata$Outlets, levels = ordered_levels$Outlets)
mid_val <- median(newsdata$bias_scores, na.rm = TRUE)
min_val <- min(newsdata$bias_scores, na.rm = TRUE)
max_val <- max(newsdata$bias_scores, na.rm = TRUE)

# Figure A3 in Appendix
newsdata %>%
  ggplot(aes(x = bias_scores, y = Outlets, color = bias_scores)) +
  geom_point(size = 2) +
  scale_color_gradientn(
    colors = c("#8da0cb", "#a6cee3", "#fc8d62"),
    values = scales::rescale(c(min_val, mid_val, max_val)),
    name = "Partisanship",
    guide = "none"
  ) +
  scale_x_continuous(breaks = seq(0, 10, by = 1)) +
  labs(
    y = "News Outlets",
    x = "Rep:Dem ratio"
  ) +
  theme_minimal()


Outlets <- c("CBS News", "Washington Examiner", "CNN", "Daily Caller",
             "Fox News", "HUFFPOST",  "MSNBC", "NBC NEWS", "NewsWeek", "NEWYORKPOST", "npr", "PBS",
             "POLITICO", "The Guardian", "The Hill", "WALL STREET JOURNAL", "Washington Post",
             "TIME", "USA TODAY", "VICE", "VOX", "The NewYork Times", "ABC News", "BBC News",  "Breitbart",
             "Business Insider", "BuzzFeed")
respondents <- function(x) length(which(is.na(x)==FALSE))
question_recog <-  t(data.frame(lapply(new_data[seq(31,161,by=5)],respondents),row.names = "Recognise"))
question_consume <-  t(data.frame(lapply(new_data[seq(32,162,by=5)],respondents),row.names = "Consume"))
question_trust <-  t(data.frame(lapply(new_data[seq(33,163,by=5)],respondents),row.names = "Trust"))
question_feel <-  t(data.frame(lapply(new_data[seq(34,164,by=5)],respondents),row.names = "Feeling"))
question_share <-  t(data.frame(lapply(new_data[seq(35,165,by=5)],respondents),row.names = "Likelihood"))
question_all <- bind_cols(Outlets,question_recog,question_consume,question_trust,question_feel,question_share)


# Democrats
respondents_dem <- function(x) length(which((!is.na(x) & new_data$Q31.3==1)==TRUE))
question_dem_recog <-  t(data.frame(lapply(new_data[seq(31,161,by=5)],respondents_dem),row.names = "Recognise"))
question_dem_consume <-  t(data.frame(lapply(new_data[seq(32,162,by=5)],respondents_dem),row.names = "Consume"))
question_dem_trust <-  t(data.frame(lapply(new_data[seq(33,163,by=5)],respondents_dem),row.names = "Trust"))
question_dem_feel <-  t(data.frame(lapply(new_data[seq(34,164,by=5)],respondents_dem),row.names = "Feeling"))
question_dem_share <-  t(data.frame(lapply(new_data[seq(35,165,by=5)],respondents_dem),row.names = "Likelihood"))
question_dem_all <- bind_cols(Outlets,question_dem_recog,question_dem_consume,question_dem_trust,question_dem_feel,question_dem_share)
question_dem_all$Partisanship <- rep("Democrat",27)

# Republican
respondents_rep <- function(x) length(which((!is.na(x) & new_data$Q31.3==2)==TRUE))
question_rep_recog <-  t(data.frame(lapply(new_data[seq(31,161,by=5)],respondents_rep),row.names = "Recognise"))
question_rep_consume <-  t(data.frame(lapply(new_data[seq(32,162,by=5)],respondents_rep),row.names = "Consume"))
question_rep_trust <-  t(data.frame(lapply(new_data[seq(33,163,by=5)],respondents_rep),row.names = "Trust"))
question_rep_feel <-  t(data.frame(lapply(new_data[seq(34,164,by=5)],respondents_rep),row.names = "Feeling"))
question_rep_share <-  t(data.frame(lapply(new_data[seq(35,165,by=5)],respondents_rep),row.names = "Likelihood"))
question_rep_all <- bind_cols(Outlets,question_rep_recog,question_rep_consume,question_rep_trust,question_rep_feel,question_rep_share)
question_rep_all$Partisanship <- rep("Republican",27)

# Independent
respondents_ind <- function(x) length(which((!is.na(x) & new_data$Q31.3==3)==TRUE))
question_ind_recog <-  t(data.frame(lapply(new_data[seq(31,161,by=5)],respondents_ind),row.names = "Recognise"))
question_ind_consume <-  t(data.frame(lapply(new_data[seq(32,162,by=5)],respondents_ind),row.names = "Consume"))
question_ind_trust <-  t(data.frame(lapply(new_data[seq(33,163,by=5)],respondents_ind),row.names = "Trust"))
question_ind_feel <-  t(data.frame(lapply(new_data[seq(34,164,by=5)],respondents_ind),row.names = "Feeling"))
question_ind_share <-  t(data.frame(lapply(new_data[seq(35,165,by=5)],respondents_ind),row.names = "Likelihood"))
question_ind_all <- bind_cols(Outlets,question_ind_recog,question_ind_consume,question_ind_trust,question_ind_feel,question_ind_share)
question_ind_all$Partisanship <- rep("Independent/Others",27)

# Figure A1 in Appendix
question_final <- bind_rows(question_dem_all,question_ind_all,question_rep_all)
question_final$...1 <- factor(question_final$...1, levels = ordered_levels$Outlets)
# Bar Plot
names = c("Republican","Independent/Others","Democrat")
question_final$Partisanship <- factor(question_final$Partisanship,names)
question_final %>% rename("Outlets"="...1") %>% pivot_longer(cols=`Recognise`,names_to="Respondents",values_to="Value") %>%
  ggplot(aes(x=Value,y=Outlets,fill=Partisanship))+ geom_bar(position = "stack",stat = "identity")+
  labs(x="Number of Respondents",y="News Outlets",subtitle=NULL) +
  scale_fill_manual(values = c("#fc8d62","#66c2a5","#8da0cb"))+ theme_minimal()

#  Respondents who recognise
respondents_rec <- function(x) length(which(is.na(x)==FALSE & x==1))
question_recog_rec <-  t(data.frame(lapply(new_data[seq(31,161,by=5)],respondents_rec),row.names = "Recognise"))
respondents_dem_rec <- function(x) length(which((!is.na(x) & new_data$Q31.3==1 & x==1)==TRUE))
question_dem_recog_rec <-  t(data.frame(lapply(new_data[seq(31,161,by=5)],respondents_dem_rec),row.names = "Recognise"))
question_dem_recog_rec_all <- bind_cols(Outlets,question_dem_recog_rec,rep("Democrat",27))

respondents_rep_rec <- function(x) length(which((!is.na(x) & new_data$Q31.3==2 & x==1 )==TRUE))
question_rep_recog_rec <-  t(data.frame(lapply(new_data[seq(31,161,by=5)],respondents_rep_rec),row.names = "Recognise"))
question_rep_recog_rec_all <- bind_cols(Outlets,question_rep_recog_rec,rep("Republican",27))

respondents_ind_rec <- function(x) length(which((!is.na(x) & new_data$Q31.3==3 & x==1)==TRUE))
question_ind_recog_rec <-  t(data.frame(lapply(new_data[seq(31,161,by=5)],respondents_ind_rec),row.names = "Recognise"))
question_ind_recog_rec_all <- bind_cols(Outlets,question_ind_recog_rec,rep("Independent/Others",27))

question_all_rec <- bind_rows(question_dem_recog_rec_all,question_ind_recog_rec_all,question_rep_recog_rec_all)

# Figure A2 in Appendix:
names = c("Republican","Independent/Others","Democrat")
question_all_rec$...3 <- factor(question_all_rec$...3,names)
question_all_rec$...1 <- factor(question_all_rec$...1, levels = ordered_levels$Outlets)
question_all_rec %>% rename("Outlets"="...1","Partisanship"="...3") %>% pivot_longer(cols=`Recognise`,names_to="Respondents",values_to="Value")  %>%
  ggplot(aes(x=Value,y=Outlets,fill=Partisanship))+ geom_bar(position = "stack",stat = "identity")+
  labs(x="Number of Respondents",y="News Outlets") +
  scale_fill_manual(values = c("#fc8d62","#66c2a5","#8da0cb"))+ theme_minimal()

# Filter for each question Q4_1:Q4_28/Q6_1:Q6_28 based on democrats and republicans excluding the "Do not know" response
mean_demo <- function(x,y) mean(as.numeric(x[which((x!="NA" & x<6 & new_data$Q31.3==1 & y==1)==TRUE)]))
mean_repub <- function(x,y) mean(as.numeric(x[which((x!="NA" & x<6 & new_data$Q31.3==2 & y==1)==TRUE)]))
mean_ind <- function(x,y) mean(as.numeric(x[which((x!="NA" & x<6 & new_data$Q31.3==3 & y==1)==TRUE)]))

sd_demo <- function(x,y) sd(as.numeric(x[which((x!="NA" & x<6 & new_data$Q31.3==1 & y==1)==TRUE)]))
sd_repub <- function(x,y) sd(as.numeric(x[which((x!="NA" & x<6 & new_data$Q31.3==2 & y==1)==TRUE)]))
sd_ind <- function(x,y) sd(as.numeric(x[which((x!="NA" & x<6 & new_data$Q31.3==3 & y==1)==TRUE)]))

# Mean/SD values of response for Question 4
four_mean_demo <- mapply(mean_demo,new_data[seq(33,163,by=5)],new_data[seq(31,161,by=5)])
four_mean_repub <- mapply(mean_repub,new_data[seq(33,163,by=5)],new_data[seq(31,161,by=5)])
four_mean_ind <- mapply(mean_ind,new_data[seq(33,163,by=5)],new_data[seq(31,161,by=5)])

four_sd_demo <- mapply(sd_demo,new_data[seq(33,163,by=5)],new_data[seq(31,161,by=5)])
four_sd_repub <- mapply(sd_repub,new_data[seq(33,163,by=5)],new_data[seq(31,161,by=5)])
four_sd_ind <- mapply(sd_ind,new_data[seq(33,163,by=5)],new_data[seq(31,161,by=5)])

# Prepare dataframe to plot for Q4:1to28
df_mean_demo <- data.frame(Mean = unlist(four_mean_demo), Question = seq(1,27))
df_mean_repub <- data.frame(Mean = unlist(four_mean_repub), Question = seq(28,54))
df_mean_ind <- data.frame(Mean = unlist(four_mean_ind), Question = seq(55,81))

df_sd_demo <- data.frame(SD = unlist(four_sd_demo),Question = seq(1,27))
df_sd_repub <- data.frame(SD = unlist(four_sd_repub), Question = seq(28,54))
df_sd_ind <- data.frame(SD = unlist(four_sd_ind), Question = seq(55,81))

final_mean_four <- bind_rows(
  df_mean_demo %>% mutate(Party = "Democrat"),
  df_mean_repub %>% mutate(Party = "Republican"),
  df_mean_ind %>% mutate(Party = "Independent/Others"),
  .id=NULL
)

final_sd_four <- bind_rows(
  df_sd_demo %>% mutate(Party = "Democrat"),
  df_sd_repub %>% mutate(Party = "Republican"),
  df_sd_ind %>% mutate(Party = "Independent/Others"),
  .id=NULL
)

final_four_combined <- merge(final_mean_four, final_sd_four, by = "Question")
final_four_combined <-final_four_combined %>%
  mutate(across(everything(), ~ replace(., is.na(.) | is.nan(.), 0))) %>% mutate(Upper=pmin(Mean+SD,5),Lower=pmax(Mean-SD,1))
final_four_combined$NewsOutlets <- rep(Outlets,3)

newsdata <- newsdata %>% arrange(bias_scores)
ordered_outlets <- newsdata$Outlets
final_four_combined$NewsOutlets <- factor(final_four_combined$NewsOutlets, levels = ordered_outlets)

# Plots of mean values for Q4:1-28
final_four_combined %>% filter(Party.x=="Democrat" | Party.x=="Republican")%>%
  ggplot(aes(x=Mean,color=Party.x,group=Party.x)) +
  geom_point(aes(y=NewsOutlets),show.legend=TRUE) +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper,y=NewsOutlets),height=0.2)  +
  labs(y="News Outlets",x="Trust") +  guides(fill=guide_legend(title="Elite Composition"),color=guide_legend(title="Partisanship")) +
  scale_color_manual(labels = c("Democrat", "Republican"), values = c("#8da0cb", "#fc8d62"))+ xlim(1,5) +theme_minimal()

# Mean/SD values of responsefor likelihood of resharing
# Filter for each question Q4_1:Q4_28/Q6_1:Q6_28 based on democrats and republicans excluding the "Do not know" response
mean_demo <- function(x,y) mean(as.numeric(x[which((x!="NA" & x<6 & new_data$Q31.3==1 & y==1)==TRUE)]))
mean_repub <- function(x,y) mean(as.numeric(x[which((x!="NA" & x<6 & new_data$Q31.3==2 & y==1)==TRUE)]))
mean_ind <- function(x,y) mean(as.numeric(x[which((x!="NA" & x<6 & new_data$Q31.3==3 & y==1)==TRUE)]))

sd_demo <- function(x,y) sd(as.numeric(x[which((x!="NA" & x<6 & new_data$Q31.3==1 & y==1)==TRUE)]))
sd_repub <- function(x,y) sd(as.numeric(x[which((x!="NA" & x<6 & new_data$Q31.3==2 & y==1)==TRUE)]))
sd_ind <- function(x,y) sd(as.numeric(x[which((x!="NA" & x<6 & new_data$Q31.3==3 & y==1)==TRUE)]))

# Mean/SD values of response for likelihood of resharing
five_mean_demo <- mapply(mean_demo,new_data[seq(35,165,by=5)],new_data[seq(31,161,by=5)])
five_mean_repub <- mapply(mean_repub,new_data[seq(35,165,by=5)],new_data[seq(31,161,by=5)])
five_mean_ind <- mapply(mean_ind,new_data[seq(35,165,by=5)],new_data[seq(31,161,by=5)])

five_sd_demo <- mapply(sd_demo,new_data[seq(35,165,by=5)],new_data[seq(31,161,by=5)])
five_sd_repub <- mapply(sd_repub,new_data[seq(35,165,by=5)],new_data[seq(31,161,by=5)])
five_sd_ind <- mapply(sd_ind,new_data[seq(35,165,by=5)],new_data[seq(31,161,by=5)])

# Prepare dataframe to plot for Q4:1to28
df_mean_demo <- data.frame(Mean = unlist(five_mean_demo), Question = seq(1,27))
df_mean_repub <- data.frame(Mean = unlist(five_mean_repub), Question = seq(28,54))
df_mean_ind <- data.frame(Mean = unlist(five_mean_ind), Question = seq(55,81))

df_sd_demo <- data.frame(SD = unlist(five_sd_demo),Question = seq(1,27))
df_sd_repub <- data.frame(SD = unlist(five_sd_repub), Question = seq(28,54))
df_sd_ind <- data.frame(SD = unlist(five_sd_ind), Question = seq(55,81))

final_mean_five <- bind_rows(
  df_mean_demo %>% mutate(Party = "Democrat"),
  df_mean_repub %>% mutate(Party = "Republican"),
  df_mean_ind %>% mutate(Party = "Independent/Others"),
  .id=NULL
)

final_sd_five <- bind_rows(
  df_sd_demo %>% mutate(Party = "Democrat"),
  df_sd_repub %>% mutate(Party = "Republican"),
  df_sd_ind %>% mutate(Party = "Independent/Others"),
  .id=NULL
)

final_five_combined <- merge(final_mean_five, final_sd_five, by = "Question")
final_five_combined <-final_five_combined %>%
  mutate(across(everything(), ~ replace(., is.na(.) | is.nan(.), 0))) %>% mutate(Upper=pmin(Mean+SD,5),Lower=pmax(Mean-SD,1))
final_five_combined$NewsOutlets <- rep(Outlets,3)
final_five_combined$NewsOutlets <- factor(final_five_combined$NewsOutlets, levels = ordered_outlets)

# Plots of mean values for Q4:1-28
final_five_combined %>% filter(Party.x=="Democrat" | Party.x=="Republican")%>%
  ggplot(aes(x=Mean,color=Party.x,group=Party.x)) +
  geom_point(aes(y=NewsOutlets),show.legend=TRUE) +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper,y=NewsOutlets),height=0.2)  +
  labs(y="News Outlets",x="Likelihood") +  guides(fill=guide_legend(title="Elite Composition"),color=guide_legend(title="Partisanship")) +
  scale_color_manual(labels = c("Democrat", "Republican"), values = c("#8da0cb", "#fc8d62"))+ xlim(1,5) +theme_minimal()

# Mean values of feelings for Democrats, Republicans and Independents
mean_demo_new <- function(x,y) mean(as.numeric(x[which((x!="NA"  & new_data$Q31.3==1 & y==1)==TRUE)]))
mean_repub_new <- function(x,y) mean(as.numeric(x[which((x!="NA" & new_data$Q31.3==2 & y==1)==TRUE)]))
mean_ind_new <- function(x,y) mean(as.numeric(x[which((x!="NA" & new_data$Q31.3==3 & y==1)==TRUE)]))

sd_demo_new <- function(x,y) sd(as.numeric(x[which((x!="NA" & new_data$Q31.3==1 & y==1)==TRUE)]))
sd_repub_new <- function(x,y) sd(as.numeric(x[which((x!="NA" & new_data$Q31.3==2 & y==1)==TRUE)]))
sd_ind_new <- function(x,y) sd(as.numeric(x[which((x!="NA" & new_data$Q31.3==3 & y==1)==TRUE)]))

# Average/SD values of response
six_mean_demo <- mapply(mean_demo_new,new_data[seq(34,164,by=5)],new_data[seq(31,161,by=5)])
six_mean_repub <- mapply(mean_repub_new,new_data[seq(34,164,by=5)],new_data[seq(31,161,by=5)])
six_mean_ind <- mapply(mean_ind_new,new_data[seq(34,164,by=5)],new_data[seq(31,161,by=5)])

six_sd_demo <- mapply(sd_demo_new,new_data[seq(34,164,by=5)],new_data[seq(31,161,by=5)])
six_sd_repub <- mapply(sd_repub_new,new_data[seq(34,164,by=5)],new_data[seq(31,161,by=5)])
six_sd_ind <- mapply(sd_ind_new,new_data[seq(34,164,by=5)],new_data[seq(31,161,by=5)])

# Prepare dataframe to plot for Q5:1to28
df_mean_demo <- data.frame(Mean = unlist(six_mean_demo), Question = seq(1,27))
df_mean_repub <- data.frame(Mean = unlist(six_mean_repub), Question = seq(28,54))
df_mean_ind <- data.frame(Mean = unlist(six_mean_ind), Question = seq(55,81))

df_sd_demo <- data.frame(SD = unlist(six_sd_demo),Question = seq(1,27))
df_sd_repub <- data.frame(SD = unlist(six_sd_repub), Question = seq(28,54))
df_sd_ind <- data.frame(SD = unlist(six_sd_ind), Question = seq(55,81))

final_mean_six <- bind_rows(
  df_mean_demo %>% mutate(Party = "Democrat"),
  df_mean_repub %>% mutate(Party = "Republican"),
  df_mean_ind %>% mutate(Party = "Independent/Others"),
  .id=NULL
)

final_sd_six <- bind_rows(
  df_sd_demo %>% mutate(Party = "Democrat"),
  df_sd_repub %>% mutate(Party = "Republican"),
  df_sd_ind %>% mutate(Party = "Independent/Others")
)

final_six_combined <- merge(final_mean_six, final_sd_six, by = "Question")
final_six_combined <-final_six_combined %>%
  mutate(across(everything(), ~ replace(., is.na(.) | is.nan(.), 0))) %>%
  mutate(Upper=pmin(Mean+SD,100),Lower=pmax(Mean-SD,0))
final_six_combined$NewsOutlets <- rep(Outlets,3)
final_six_combined$NewsOutlets <- factor(final_six_combined$NewsOutlets, levels = ordered_outlets)

final_six_combined %>% filter(Party.x=="Democrat" | Party.x=="Republican")%>% filter(Mean!=0) %>%
  ggplot(aes(x=Mean,color=Party.x,group=Party.x)) +
  geom_point(aes(y=NewsOutlets),show.legend=TRUE) +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper,y=NewsOutlets),height=0.2)  +
  labs(y="News Outlets",x="Feeling") +  guides(fill=guide_legend(title="Elite Composition"),color=guide_legend(title="Partisanship")) +
  scale_color_manual(labels = c("Democrat", "Republican"), values = c("#8da0cb", "#fc8d62"))+ xlim(0,100) +theme_minimal()

