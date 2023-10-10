#***************************************
#name: pq_email_contacts.R
#purpose: automatically send PQ emails
#written by: Graeme 19/11/19 - updated Scott 20/07/21

#info: this will take the allocated PQ
#add contact info, pool PQs by area,
#generate and send emails

#Note: currently (Sep 2020) RDCOMClient
#is not available for R3.6 - so this is
#run using R3.5
#***************************************

#***************************************
#install packages####
#***************************************
#RDCOMClient won't install on server
#use desktop RStudio to run
install.packages("RDCOMClient", repos = "http://www.omegahat.net/R")
#run both lines below for RDCOMClient if the above fails or loads into wrong version
url <- "http://www.omegahat.net/R/bin/windows/contrib/3.5.1/RDCOMClient_0.93-0.zip"
install.packages(url, repos=NULL, type="binary")
#install.packages("glue")
#install.packages("tidylog")
#install.packages("lubridate")
#install.packages("broom")
#install.packages("dplyr")
#install.packages("readr")
#install.packages("stringr")

#***************************************
#load packages####
#***************************************
library(RDCOMClient)
library(glue)
library(tidylog)
library(lubridate)
library(broom)
library(dplyr)
library(readr)
library(stringr)

#***************************************
#file to use####
#***************************************

#file format usually YYYY_MM_DD_new_pqs_allocated.csv
pq_dated <- format(today(), "%Y_%m_%d") #if running today
#pq_dated <- "2021_08_19" #if running retrospectively

#define path to PQ files - two options, one mapped, one not
file_path_pq <- "//Isdsf00d03/cl-out/Stats_Gov/pq_allocation" #not mapped
#file_path_pq <- "Y:/pq_allocation" #cl-out mapped to Y:/

#read allocated files from network using date above
pq <- read_csv(glue("{file_path_pq}/allocated/",
                    "{pq_dated}_new_pqs_allocated.csv"),
               col_types = "cTTTcccclccc")

#***************************************
#optional! clean up of encoding####
#***************************************

#sometimes there are encoding problems between server/laptops
#server encodes as UTF-8, windows uses Latin1
#e.g. sometimes a problem with £ symbol

#to check encoding for each question:
pq %>% 
  mutate(encd = Encoding(item_text)) %>% 
  select(event_id, encd, item_text, topic_area) %>% 
  filter(encd != "unknown") %>% 
  pull(topic_area)

#if any are UTF8, check question for encoding problems
#you can replace weird characters in the csv file directly
#unicode characters present in R (e.g. <U+00A3>) are encoded ok in emails
#often problems are with " ' £ characters

#***************************************
#get email addresses####
#***************************************
emails <- read_csv(glue("{file_path_pq}/email_addresses.csv"))
stats_gov <- filter(emails, contact == "stats_gov") %>% pull(info)
sg <- filter(emails, contact == "sg") %>% pull(info)
sg_phone <- filter(emails, contact == "sg_phone") %>% pull(info)

#***************************************
# email intro text####
#***************************************

#this will be added to the top of the email
email_intro <- glue(
  "Dear all,<br><br>",
  "Please note the following Parliamentary Questions for the attention of your service area.",
  " Please read the instructions below and, if necessary, get in touch with your policy contact to agree a deadline.<br><br>",
  "<strong>Instructions:</strong><br><br>",
  "<strong>For Information</strong> - This indicates that the question is highlighted as of interest",
  " to your service area. There is no current expectation of formal input to be provided by you to the Scottish",
  " Government's work on their answer to the question. However, you should be prepared in case a SG policy contact",
  " decides to make contact with you to help them with some background information.<br><br>",
  "<strong>For Review</strong> - This indicates that the question might require you to take action if your area holds some or all of the relevant information.",
  " You should contact SG colleagues to confirm any input you will provide to the Scottish Government's work on an answer, and also to agree a deadline if support is required.<br><br>",
  "<strong>Policy contacts at the Scottish Government: </strong>",
  "The Executive Team Programme Hub within the Scottish Government handles PQ allocations.",
  " The number to use to check if support is needed on a Parliamentary Question in the Scottish",
  " Government is {sg_phone} though you can email {sg}<br><br>",
  "<strong>Expected answer dates:</strong> the dates included below show the deadline for the Scottish",
  " Government's answer to appear <em>in Parliament</em>. <strong>Your deadline</strong>, therefore,",
  " for any support will be ahead of that."
)

#***************************************
# load contact info/process####
#***************************************

#read contact info for topic areas
contact <- read_csv(glue("{file_path_pq}/contact_info.csv"),
                    skip_empty_rows = TRUE,
                    trim_ws = TRUE)

#clean up topic area in case entered incorrectly
#make lower case, replace spaces with underscore, remove whitespace
contact <- contact %>% 
  mutate(topic_area = str_to_lower(topic_area),
         topic_area = str_replace_all(topic_area, pattern = " ","_"),
         topic_area = str_squish(topic_area))

#collapse emails and remove blanks
#group by full name to preserve column for using in subject
contact <-  contact %>%
  group_by(topic_area, full_name) %>% 
  summarize(send_to = glue_collapse(contact_email, sep = "; ")) %>% 
  na.omit()

#***************************************
#process questions####
#***************************************

#get rid of questions not allocated (NA)
pq <- pq %>% filter(!is.na(topic_area))

#arrange so 'for review' is first
pq <- pq %>% arrange(topic_area, desc(action))

#clean up topic area in case entered incorrectly
#make lower case, replace spaces with underscore, remove whitespace
#remove question marks for any
pq <- pq %>% 
  mutate(topic_area = str_to_lower(topic_area),
         topic_area = str_replace_all(topic_area, pattern = " ","_"),
         topic_area = str_replace_all(topic_area, pattern = "\\?",""),
         topic_area = str_squish(topic_area))


#produce a table to show the number of questions each topic area will be receiving
count(pq, topic_area)

#***************************************
#update running list of questions####
#***************************************

#keeps list of all allocated questions
#get rid of duplicated rows
all_pq <- read_csv(glue("{file_path_pq}/allocated/all_allocated_pqs.csv"),
                   col_types = "cTTTcccclccc")
all_pq <- bind_rows(all_pq, pq)
all_pq <- distinct(all_pq)
write_csv(all_pq, glue("{file_path_pq}/allocated/all_allocated_pqs.csv"))

#***************************************
#generate message####
#***************************************

#generate message per question
pq <- pq %>%
  mutate(expected_answer_date = format(expected_answer_date, "%d/%m/%y"),
         approved_date = format(approved_date, "%d/%m/%y"),
         meeting_date = format(meeting_date, "%d/%m/%y")) %>%
  mutate(action = glue("<span style='color:blue'>For {action}</span><br>"),
         id = glue("<strong>{event_id}</strong><br>"),
         expct = glue("Answer expected: <strong>{expected_answer_date}</strong><br>"),
         mtg = glue("Meeting date (if applicable): <strong>{meeting_date}</strong><br>"),
         apprvd = glue("Question approved: {approved_date}<br>"),
         askd = glue("Asked by: {name}, {party}, {mp_area}<br>"),
         qstn = glue("<strong>Question: </strong>{item_text}<br>"),
         nts = glue("<strong>Notes:</strong> {notes}<br>")) %>%
  mutate(message = if_else(is.na(notes),
                           glue("{action}{id}{expct}{mtg}{apprvd}{askd}{qstn}<br>"),
                           glue("{action}{id}{expct}{mtg}{apprvd}{askd}{qstn}{nts}<br>")
  ))

#group questions by topic and collapse message
pq <- pq %>% 
  group_by(topic_area) %>%
  select(topic_area, message) %>%
  summarize(pooled = glue_collapse(message))

#***************************************
#join contact info to questions####
#***************************************

pq <- left_join(pq, 
                contact, 
                by = "topic_area")

#***************************************
#generate/send emails####
#***************************************

#get topics that have questions allocated
topics <- na.omit(pq$topic_area)
#loop across topics and send email
for (i in topics) {
  
  #extract topic info
  #i<-"alcohol"
  x <- pq %>% filter(topic_area == i)
  
  #extract info for emails
  send_to <- str_squish(x$send_to)
  subject <- paste0(Sys.Date()," new PQs - ", x$full_name)
  msg_body <- paste0(email_intro, "<br><br>", x$pooled)
  
  #create an email
  OutApp <- COMCreate("Outlook.Application")
  outMail = OutApp$CreateItem(0)
  
  #*************************************
  #send test emails to yourself first 
  #to check that nothing looks weird
  #*************************************
  
  #configure email parameters
  #outMail[["To"]] = "catherine.johnston2@phs.scot" #send to yourself as test
  outMail[["To"]] = "vay.ly@phs.scot" #send to yourself as test
  #outMail[["To"]] = "camilla.somers2@phs.scot" #send to yourself as test
  #outMail[["To"]] = "miranda.mathieson@phs.scot" #send to yourself as test
  #outMail[["To"]] = "sadia.ahmed2@phs.scot" #send to yourself as test
  #outMail[["To"]] = send_to #send to allocated teams
  
  #outMail[["SentOnBehalfOfName"]] = stats_gov #comment this line to send from yourself
  outMail[["subject"]] = subject
  outMail[["htmlbody"]] = msg_body
  
  #send it
  outMail$Send()
}
