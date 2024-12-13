library(tidyverse)
library(ggplot2)
library(cowplot)
library(ggpubr)


get_partisan_plot <- function(varname){
    plot<- ggplot(dem_rep_sum %>% filter(group==varname), 
       aes(x= group_prop*100, y = factor(value), fill = party_label,
          label = paste(round(group_prop*100, digits = 1), "%")))+
geom_col(position = "dodge", width = 0.7)+
geom_text(aes(x = (group_prop*100)+2 ),hjust = 0, color = "gray30", size = 5, 
          position = position_dodge(width = .7))+
scale_x_continuous(limits = c(0,100))+
scale_fill_manual(values = c( "dodgerblue4","red"))+ 
#labs(y = "", x = "Percentage of Results", fill = "")+
    theme_bw(base_size = 18)+
theme(legend.position = "top", 
      panel.grid.minor = element_blank(), 
      panel.grid.major.y = element_blank())
    return(plot)
}

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
left_join(newsguard) %>%
mutate(low_cred = ifelse(ng_score < 60, 1, 0), 
       cmpt_rank = cmpt_rank+1,
       weighted_sum= cmpt_rank*counts)


type_df_domain <- type_df%>% group_by(domain)%>%
summarise(is_local = mean(is_local),
         low_cred = mean(low_cred),
         is_news = mean(is_news),
         is_poli_control = mean(is_poli_control))
type_df_domain2 <- type_df%>% select(domain, category)%>%
distinct()

dem_rep_news <- read_csv("../../data/house_analysis/house_party_domain_no_duplicate.csv")%>%
left_join(type_df_domain)%>%
left_join(type_df_domain2)

dem_rep_sum1 <- dem_rep_news %>%
select(-is_news, -is_poli_control)%>%
filter(category == "news")%>%
mutate(is_local = case_when(
          is_local == 1 ~ "National\nNews",
          is_local == 0 ~ "Local\nNews",
          TRUE ~ "No\nClassification"),
       low_cred = case_when(
          low_cred == 1 ~ "Low\nCredibility",
          low_cred == 0 ~ "High\nCredibility",
          TRUE ~ "No\nClassification")) %>%
pivot_longer(c("is_local", "low_cred"), names_to = "group", values_to = "value")%>%
group_by(party, group, value) %>%
summarise(group_counts = sum(counts))

dem_rep_sum2 <- dem_rep_news %>%
select(-is_local, -low_cred)%>%
mutate(is_news = factor(ifelse(is_news == TRUE, "News", "Not News"),
                      levels = c( "News", "Not News")),
      is_poli_control = factor(ifelse(is_poli_control == TRUE, "Politician\nControllled", 
                                      "Not Politician\nControllled"),
                      levels = c( "Politician\nControllled", 
                                 "Not Politician\nControllled")))%>%
pivot_longer(c("is_poli_control", "is_news"), names_to = "group", values_to = "value")%>%
group_by(party, group, value) %>%
summarise(group_counts = sum(counts))

dem_rep_sum<- rbind(dem_rep_sum1, dem_rep_sum2)%>%
group_by(party,group)%>%
mutate(total_counts = sum(group_counts), 
       group_prop = group_counts/total_counts,
      party_label = ifelse(party == "Democrat", " Search Results for Democratic members", 
                           "Search Results for Republican members"))%>%
filter(party!= "Independent")


plot1<- get_partisan_plot("is_poli_control")+
labs(y = "", x = "Percentage of Results", fill = "")

plot2<- get_partisan_plot("is_news")+
labs(y = "", x = "Percentage of Results", fill = "")+
scale_y_discrete(limits=rev)

plot3<- get_partisan_plot("low_cred")+
labs(y = "", x = "Percentage of News Results", fill = "")+
scale_y_discrete(limits=rev)

plot4<- get_partisan_plot("is_local")+
labs(y = "", x = "Percentage of News Results", fill = "")+
scale_y_discrete(limits=rev)

legend <- get_legend(plot1)

plot_group <- plot_grid(plot1+ theme(legend.position = "none"), 
                        plot2 + theme(legend.position = "none"), 
                        plot3 + theme(legend.position = "none"), 
                        plot4 + theme(legend.position = "none"), 
                        ncol = 2, align="v", 
          labels = c('A.','B.','C.', 'D.'),
          label_size = 18,
          rel_heights = c(2,3))

plot_grid(plot_group, legend, ncol = 1, rel_heights = c(15,1))
ggsave("../../data/plots/si_partisan.pdf", width = 12, height = 8, units = "in")
