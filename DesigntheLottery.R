library(dplyr)
library(tibble)
library(readxl)
library(markdown)


#####################################################
#SET EVERYTHING UP BEFORE DOING THE SHINY PART
#######################################################

#I COULD MAKE THIS AN INTERACTIVE PART TOO LATER

temp<-read.csv("./2022HL100_lotteryentrants_final.csv", stringsAsFactors = FALSE) #LOAD THE DATA
df<-as_tibble(temp)


df$fullname<-paste(df$First_Name, df$Last_Name, sep=" ", collapse = NULL)
head(df)

#df$USA<-ifelse(df$Country=="USA",1,0)

#NUMBER OF MEN AND WOMEN APPLICANTS
n_men_app=nrow(men<-df[which(df$Gender=="M"),])
n_women_app=nrow(women<-df[which(df$Gender=="F"),])

input<-data.frame(matrix(ncol=8, nrow=1))
colnames(input)<-c('finishes', 'volunteer', 'trailwork', 'apps', 'Nw', 'Nm', 'exp', 'mult')
input$finishes<-4
input$volunteer<-0
input$trailwork<-0
input$apps<-0
input$Nw<-62
input$Nm<-66
input$exp<-4
input$mult<-4

  
  #############################################################################
 
      #Do THE VARIABLE TRANSFORMATIONS FOR PEOPLE's ENTERED VALUES
      k_sim <- ifelse(input$finishes==0 , 0,
                      ifelse(input$finishes==1,  0.5,
                             ifelse(input$finishes==2, 1, 
                                    ifelse(input$finishes==3, 1.5,
                                           ifelse(input$finishes>=4, 0.5, 0)))))
      
      #Shifts max out at 30 (10 each race)
      v_sim<-pmin(input$volunteer, 30)
      t_sim<-pmin(input$trailwork, 10)
      
      #Tickets=2^(n+k+1)+2ln(v+t+1) where n, k, v, and t are defined as follows:
      input$exp^(k_sim+input$apps+1) + input$mult*log(v_sim+t_sim+1)
 
      ######################################################################
      #FOR 2022 JUST SET HOW MANY TO PICK
      n_women_pick <- input$Nw
      n_men_pick <- input$Nm
      
      ######################################################################
      #DETERMINE TICKETS FROM THE DATA
      
      df$Applications<-df$Previous_Applications
      
      #k is defined according to the following rule:
      # k=0 if finishes==0
      #k=0.5 if finishes==1
      #k=1 if finishes==2
      #k=1.5 if finishes==3
      #k=1 if finishes>=4
      df$k <- ifelse(df$Previous_Finishes==0 , 0,
                     ifelse(df$Previous_Finishes==1,  0.5,
                            ifelse(df$Previous_Finishes==2, 1, 
                                   ifelse(df$Previous_Finishes==3, 1.5,
                                          ifelse(df$Previous_Finishes>=4, 0.5, 0)))))
      
      
      #Shifts max out at 30 (10 each race)
      df$v<-pmin(df$Volunteer_Shifts, 30)
      df$t<-pmin(df$Extra_Trailwork, 10)
      
      #Tickets=2^(n+k+1)+2ln(v+t+1) where n, k, v, and t are defined as follows:
      df$tickets <-input$exp^(df$k+df$Applications+1) + input$mult*log(df$v+df$t+1)
      
      ##########################################################
      
      
      
      #SPLIT THE DATA INTO MENS AND WOMENS
      men<-df[which(df$Gender=="M"),]
      women<-df[which(df$Gender=="F"),]
      
      
      
      #PRINT THE ODDS
      # WOMEN ODDS
      #the number of women with a given number of tickets
      applicants <- pull((women %>% count(tickets))[,2], n)
      
      #those ticket numbers
      tickets_per_applicant <- sort(women$tickets[!duplicated(women$tickets)])
      
      #the total tickets from that 'category'
      original_tickets <- applicants * tickets_per_applicant
      ticket_counts <- original_tickets
      
      for (i in 1:n_women_pick) {
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
      w_odds <- cbind(tickets_per_applicant, odds_of_selection, applicants, num_people_taken)
      w_odds
      #output$women_odds <- renderTable(w_odds)
      ############################
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
      m_odds
      #output$men_odds <- renderTable(m_odds)
 #output
 
 