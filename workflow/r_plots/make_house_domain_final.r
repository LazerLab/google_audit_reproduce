# Script joins domains labelled with keyword matching to hand labelled domain

setwd("~/internal_websearch")

library(tidyverse)

house_domain <- read.csv("data/house_domain_pre_handcode_122024.csv")

house_domain_labelled <- house_domain %>%
filter(matched == 1) %>%
select(-domain_clean)

# this is the manually handcoded file
house_domain_handcoded <- read.csv("data/house_domain_handcoded.csv")%>%
filter(domain %in% house_domain$domain)%>%
                            filter(!(domain %in% house_domain_labelled$domain))

house_domain_final <- house_domain %>% select(domain, counts) %>%
left_join(rbind(house_domain_labelled,house_domain_handcoded ) %>% select(-counts))

write.csv(house_domain_final,
          "data/house_domain_final_122024.csv", 
          row.names = FALSE)