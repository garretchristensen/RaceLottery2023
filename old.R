##############################################################
#DRAW THE LOTTERY

#dplyr function sample_n will work with weights, normalize automatically
#syntax:sample_n(tbl, size, replace = FALSE, weight = NULL, .env = NULL, ...)
#Run the separate lotteries
women_winners<-sample_n(women, n_women_pick, replace = FALSE, weight=women$tickets)
men_winners<-sample_n(men, n_men_pick, replace = FALSE, weight=men$tickets)

#I can't figure out how to label tables, so just make the table itself
#look sort of well-labeled
#subset
#dfnew5 <- subset(diamonds, select=c("color", "carat", "price"))
w_output<-subset(women_winners, select=c("fullname", "City", "State"))
m_output<-subset(men_winners, select=c("fullname", "City", "State"))

#Add ID column data$ID <- seq.int(nrow(data))
w_output$Num<-seq.int(nrow(w_output))
m_output$Num<-seq.int(nrow(m_output))
#rearrange columns df2[,c(1,3,2,4)]
#w_output<-w_output[,c(5,1,2,3,4)]
#m_output<-m_output[,c(5,1,2,3,4)]
#rename: names(data)[3]<-"new_name"
names(w_output)[1]<-"Selected_Women"
names(m_output)[1]<-"Selected_Men"

#Send the winners' names to be output
output$women <- renderTable(w_output)
output$men <- renderTable(m_output)


#########################################################
#waitlist
#########################################################

women_waitlist_pool<-anti_join(women, women_winners)
n_women_waitlist_pool<-nrow(women_waitlist_pool)

men_waitlist_pool<-anti_join(men, men_winners)
n_men_waitlist_pool<-nrow(men_waitlist_pool)

#SIMPLER THIS YEAR, JUST ENTER THE NUMBERS FOR THE WL, 8 and 7
n_women_wait_pick<-42
n_men_wait_pick<-75

#PICK THE WAITLISTERS
#WOMEN MIGHT NOT HAVE ANY

women_waiters <- sample_n(women_waitlist_pool, n_women_wait_pick, replace = FALSE, weight=women_waitlist_pool$tickets)
w_output_wait<-subset(women_waiters, select=c("fullname", "City", "State"))
#w_output_wait_priv<-subset(women_waiters, select=c("fullname", "Email_Address"))
w_output_wait$Num<-seq.int(nrow(w_output_wait))
#w_output_wait<-w_output_wait[,c(5,1,2,3,4)]
names(w_output_wait)[1]<-"Waitlisted_Women"
output$women_wait <- renderTable(w_output_wait)

#ASSUME MEN WILL HAVE ENOUGH FOR A FULL WAITLIST
men_waiters <- sample_n(men_waitlist_pool, n_men_wait_pick, replace = FALSE, weight=men_waitlist_pool$tickets)

#I can't figure out how to label tables, so just make the table itself
#look sort of well-labeled
#subset
#dfnew5 <- subset(diamonds, select=c("color", "carat", "price"))
m_output_wait<-subset(men_waiters, select=c("fullname", "City", "State"))
#m_output_wait_priv<-subset(men_waiters, select=c("fullname", "Email_Address"))
#Add ID column data$ID <- seq.int(nrow(data))
m_output_wait$Num<-seq.int(nrow(m_output_wait))
#rearrange columns df2[,c(1,3,2,4)
#m_output_wait<-m_output_wait[,c(5,1,2,3,4)]
#rename: names(data)[3]<-"new_name"
names(m_output_wait)[1]<-"Waitlisted_Men"
#Send the winners' names to be output
output$men_wait <- renderTable(m_output_wait)   


##########################################
#Don't Zipper the waitlists
########################################
#make column names identical so columns line up
names(m_output_wait)[1]<-"Waitlisted_Name"
names(w_output_wait)[1]<-"Waitlisted_Name"
#bind women first for the waitlist for 2021

temp <- bind_rows(w_output_wait, m_output_wait)

#sort and relabel
#temp <- arrange(temp, Num)
#temp$GenderNum <-temp$Num
#temp$Num <-seq.int(nrow(temp))

output$combo<-renderTable(temp)















################################
# MEN ODDS
#the number of men with a given number of tickets
applicants <- pull((men %>% count(tickets))[,2], n)

#those ticket numbers
tickets_per_applicant <- sort(men$tickets[!duplicated(men$tickets)])

#the total tickets from that 'category'
original_tickets <- applicants * tickets_per_applicant
ticket_counts <- original_tickets

for (i in 1:n_men_pick) {
  #odds of picking that category
  prob_of_selecting_category <- ticket_counts / sum(ticket_counts)
  #expected reduction in tickets by picking a person from that category
  exp_ticket_reduction <- prob_of_selecting_category * tickets_per_applicant
  #reduce the tickets remaining
  ticket_counts <- ticket_counts - exp_ticket_reduction
}
#tickets pulled from a category
tickets_taken <- original_tickets - ticket_counts
#odds from that category
odds_of_selection <- tickets_taken / original_tickets
#people from that category
num_people_taken <- odds_of_selection * applicants
m_odds <- cbind(tickets_per_applicant, odds_of_selection, applicants, num_people_taken)
men_odds<-data.frame(m_odds)
output$men_odds <- renderTable(men_odds)
