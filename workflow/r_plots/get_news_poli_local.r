'''
This filie generates plots for news, plolitician-controlled, local, and credibility analysis.
'''


library(tidyverse)
library(ggplot2)
library(cowplot)

ncol_facet = 2
newsguard_raw <- read_csv("../../data/newsguard_metadata_Sep21.csv")

newsguard <- newsguard_raw%>%
rename("mini_domain" = Domain, "parent_domain" = `Parent Domain`, "ng_score" = Score)%>%
#filter(Country == "US")%>%
select(mini_domain, parent_domain, Country, ng_score)%>%
pivot_longer(ends_with("domain"),names_to = "domain_type", values_to = "domain")%>%
drop_na()%>%
filter(!(domain == "vice.com" & Country %in% c("IT", "DE","FR", "ALL")))%>%
filter(!(domain == "msn.com" & Country %in% c("IT", "DE","FR", "ALL")))%>%
select(-domain_type, -Country)%>%
distinct() 

type_df<-read_csv("../../data/house_analysis/domain_isnews_islocal_ispolicontrol.csv")%>% 
rename("cmpt_rank" = serp_rank)%>%
left_join(newsguard) %>%
mutate(low_cred = ifelse(ng_score < 60, 1, 0))

type_sum <- type_df %>%
mutate(local_include = ifelse(is.na(classification) ==FALSE, 1,0),
      cmpt_rank = cmpt_rank+1,
       #weighted_sum= cmpt_rank
       total_prop_news = (counts/sum(type_df%>%filter(category == "news") %>%pull(counts)))*100,
      total_prop = (counts/sum(type_df$counts))*100,
      is_news = factor(ifelse(is_news == TRUE, "News\n(29.7% of All Results)", "Not News\n(70.3% of All Results)"),
                      levels = c( "News\n(29.7% of All Results)", "Not News\n(70.3% of All Results)")),
      is_poli_control = factor(ifelse(is_poli_control == TRUE, "Politician Controllled\n(37.5% of All Results)", 
                                      "Not Politician Controllled\n(62.5% of All Results)"),
                      levels = c( "Politician Controllled\n(37.5% of All Results)", 
                                 "Not Politician Controllled\n(62.5% of All Results)")),
      classification_labelled = case_when(
          classification == "national" ~ "National News\n(25.6% of News Results)",
          classification == "local" ~ "Local News\n(62.5% of News Results)",
          TRUE ~ NA),
        low_cred = factor(ifelse(low_cred == 1, "Low Credibility News\n(0.6% of News Results)", 
                                      "High Credibility News\n(76.5% of News Results)"),
                      levels = c( "High Credibility News\n(76.5% of News Results)", 
                                 "Low Credibility News\n(0.6% of News Results)")))

plot1_df <- type_sum %>% 
       select(total_prop, counts, cmpt_rank,is_news)%>%
       group_by(cmpt_rank,is_news)%>%
       summarise(n = sum(total_prop, na.rm = TRUE),
                n_counts = sum(counts, na.rm = TRUE)) %>%
              group_by(is_news)%>%
               mutate(group_counts = sum(n_counts),
                   mean_rank = sum(cmpt_rank*n_counts)/group_counts,
                     mean_label = ifelse(cmpt_rank == ceiling(mean_rank), 
                                         paste0("Mean = ", as.character(round(mean_rank, 1))),NA))
plot1 <- ggplot(plot1_df,
       aes(y = n, x = cmpt_rank, xintercept = mean_rank))+
geom_col(fill = "gray50", color = NA)+
facet_wrap(~is_news, ncol = ncol_facet)+ 
geom_vline(linetype = "dashed", linewidth = 1, aes(xintercept = mean_rank))+
geom_label(aes(label = mean_label, x = mean_rank +4), y = 9, label.size = 0.25, size = 4.5)+
scale_y_continuous(limits = c(0,10))+
scale_x_continuous(limits = c(0,15))+
labs(y = "Percentage of Total Results", x = "Result Rank")+
theme_bw(base_size = 18)+
theme(legend.position="none")

plot2_df <- type_sum %>% 
       select(total_prop, counts, cmpt_rank,is_poli_control)%>%
       group_by(cmpt_rank,is_poli_control)%>%
       summarise(n = sum(total_prop, na.rm = TRUE),
                n_counts = sum(counts, na.rm = TRUE)) %>%
              group_by(is_poli_control)%>%
               mutate(group_counts = sum(n_counts),
                   mean_rank = sum(cmpt_rank*n_counts)/group_counts,
                     mean_label = ifelse(cmpt_rank == ceiling(mean_rank), 
                                         paste0("Mean = ", as.character(round(mean_rank, 1))),NA))

plot2<- ggplot(plot2_df, 
       aes(y = n, x = cmpt_rank))+
geom_col(fill = "gray50")+
       facet_wrap(~is_poli_control, ncol = ncol_facet)+ 
geom_vline(linetype = "dashed", linewidth = 1, aes(xintercept = mean_rank))+
geom_label(aes(label = mean_label, x = mean_rank +4), y = 9, label.size = 0.25, size = 4.5)+
scale_y_continuous(limits = c(0,10))+
scale_x_continuous(limits = c(0,15))+
labs(y = "Percentage of Total Results", x = "Result Rank")+
theme_bw(base_size = 18)+
theme(legend.position="none")

plot3_df <- type_sum %>% select(total_prop_news,cmpt_rank,classification_labelled, classification,counts )%>% 
       filter(classification %in% c("local", "national"))%>%
       group_by(cmpt_rank,classification_labelled)%>%
       summarise(n = sum(total_prop_news, na.rm = TRUE),
                 n_counts = sum(counts, na.rm = TRUE)) %>%
              group_by(classification_labelled)%>%
               mutate(group_counts = sum(n_counts),
                   mean_rank = sum(cmpt_rank*n_counts)/group_counts,
                     mean_label = ifelse(cmpt_rank == ceiling(mean_rank), 
                                         paste0("Mean = ", as.character(round(mean_rank, 1))),NA))

plot3<- ggplot(plot3_df,
       aes(y = n, x = cmpt_rank))+
geom_col(fill = "gray50")+
geom_vline(linetype = "dashed", linewidth = 1, aes(xintercept = mean_rank))+
geom_label(aes(label = mean_label, x = mean_rank +4), y = 14, label.size = 0.25, size = 4.5)+
scale_x_continuous(limits = c(0,15))+
facet_wrap(~classification_labelled, ncol = ncol_facet)+
labs(y = "Percentage of News Results", x = "Result Rank")+
theme_bw(base_size = 18)+
theme(legend.position="none")

plot4_df <- type_sum %>% 
select(total_prop_news,cmpt_rank,low_cred,counts )%>% 
       filter(is.na(low_cred) == FALSE )%>%
       group_by(cmpt_rank,low_cred)%>%
       summarise(n = sum(total_prop_news, na.rm = TRUE),
                 n_counts = sum(counts, na.rm = TRUE)) %>%
              group_by(low_cred)%>%
               mutate(group_counts = sum(n_counts),
                   mean_rank = sum(cmpt_rank*n_counts)/group_counts,
                     mean_label = ifelse(cmpt_rank == ceiling(mean_rank), 
                                         paste0("Mean = ", as.character(round(mean_rank, 1))),NA),
                     y_lab = ifelse(low_cred ==  "Low Credibility News\n(0.6% of News Results)", 
                                    1, 20))

plot4<- ggplot(plot4_df,
       aes(y = n, x = cmpt_rank))+
geom_col(fill = "gray50")+
geom_vline(linetype = "dashed", linewidth = 1, aes(xintercept = mean_rank))+
geom_label(aes(label = mean_label, x = mean_rank +4, y = y_lab), label.size = 0.25, size = 4.5)+
scale_x_continuous(limits = c(0,15))+
facet_wrap(~low_cred, ncol = ncol_facet, scales = "free_y")+
labs(y = "Percentage of News Results", x = "Result Rank")+
theme_bw(base_size = 18)+
theme(legend.position="none")

plot_grid(plot1, plot3, plot2,plot4, ncol = 2,
          labels = c('A.', 'B.','C.', 'D.'), 
          label_size = 18)
ggsave("../../data/plots/news_poli_local.pdf", width = 12, height = 8, units = "in")