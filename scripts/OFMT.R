
###########################################################################
#
#  Experiment:  CARIPARO
#  Programmer:  QUETTIER THOMAS
#  Date:        18/05/2022
#  Description: Accuracy for OFMT
#
#  Update:      30/05/2023
###########################################################################

rm(list=ls()) # remove all objects

# Packages ----------------------------------------------------------------

library(tidyverse)
library(anytime)
library(readr)
library(dplyr)
library(stringr)
library(ggplot2)

# Functions ---------------------------------------------------------------

devtools::load_all()


# loading data ----
datasetname<-"dataset_OFMT"
OFMT_concatenation(datasetname)
load(paste0("objects/",datasetname,".RData") )


data<-dataset%>%
  filter(Attempt == 1, Zone.Type== "response_button_text", inclusionCheck == "test")%>%
  mutate(ANSWER = ifelse(pairType=="same","Della stessa persona","Di persone diverse"),
         Correct = ifelse(ANSWER == Response,1,0))%>%
  select(Participant.Public.ID, Reaction.Time, Correct,ANSWER,Response,face1, face2)%>%
  mutate(Correct = as.numeric(as.character(Correct)))%>%
  'colnames<-'(c("subject" ,"rt.correct","correct","ref","resp","face1","face2"))

id<-dataset%>%
  filter(Question.Key == "response-3")%>%
  select(Participant.Public.ID, Response, UTC.Date)%>%
  'colnames<-'(c("subject" ,"id", "date"))

add_pt <- data.frame(subject = "ium1nfma",
                     id = "01_moebius",
                     date = "24/06/2022 16:58:38")
id<-rbind(id,add_pt)  

scores<-data%>%
  group_by(subject)%>%
  summarise(acc = sum(correct)/200) # 200 trials

scores<-left_join(id,scores, by = "subject")%>%
  mutate(num = parse_number(id))%>%
  arrange(num)
  
  


data%>%
  group_by(subject,ref)%>%
  summarise_at("correct",sum)%>%
  # group_by(ref)%>%
  # summarise_at("correct",list(mean,sd))%>%
  data.frame()%>%
  ggplot()+
  geom_bar(aes(y=correct,x=ref, fill= ref),stat='identity')+
  #geom_errorbar( aes(x=ref, ymin=fn1-fn2, ymax=fn1+fn2), width=0.2, size=.5)+
  theme(text=element_text(size=16,  family="Times New Roman"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "none")+
  scale_y_discrete(name="correct (%)")+
  scale_x_discrete(name="")

data%>%
  group_by(subject,ref)%>%
  summarise_at("rt.correct",mean)%>%
  group_by(ref)%>%
  summarise_at("rt.correct",list(mean,sd))%>%
  data.frame()%>%
  ggplot()+
  geom_bar(aes(y=fn1,x=ref, fill= ref),stat='identity')+
  geom_errorbar( aes(x=ref, ymin=fn1-fn2, ymax=fn1+fn2), width=0.2, size=.5)+
  theme(text=element_text(size=16,  family="Times New Roman"),
        panel.background = element_blank(),
        legend.position = "none")+
  scale_y_discrete(name="RT correct (ms)",labels = c(0,3000))+
  scale_x_discrete(name="")

data%>%
  group_by(subject,ref)%>%
  summarise_at("resp.slider",mean)%>%
  group_by(ref)%>%
  summarise_at("resp.slider",list(mean,sd))%>%
  data.frame()%>%
  ggplot()+
  geom_bar(aes(y=fn1,x=ref, fill= ref),stat='identity')+
  geom_errorbar( aes(x=ref, ymin=fn1-fn2, ymax=fn1+fn2), width=0.2, size=.5)

data%>%
  group_by(subject,ref)%>%
  summarise_at("rt.correct",mean)%>%
  group_by(ref)%>%
  summarise_at("rt.correct",list(mean,sd))%>%
  data.frame()%>%
  ggplot()+
  geom_bar(aes(y=fn1,x=ref, fill= ref),stat='identity')+
  geom_errorbar( aes(x=ref, ymin=fn1-fn2, ymax=fn1+fn2), width=0.2, size=.5)





