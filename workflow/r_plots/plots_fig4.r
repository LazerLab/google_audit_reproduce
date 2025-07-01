# Script produces Figure 4: Partisan-alignment of search results

setwd("~/internal_websearch")

library(tidyverse)
library(ggplot2)
library(scales)
library(ggrepel)
library(cowplot)

pas <-read_csv("data/partisan_audience_score.csv")

house_domain_final <- read_csv("data/house_domain_final_122024.csv") %>%
select(-title, -matched, -categories_flagged)

dem_rep <- read_csv("data/house_party_domain.csv")%>%
    mutate(total_counts = sum(counts))%>%
    group_by(party)%>%
    mutate(total_party_count = sum(counts),
           total_party_prop = total_party_count/total_counts) %>%
    ungroup()%>%
    group_by(domain)%>%
    mutate(total_domain_count = sum(counts),
          total_domain_party_prop = counts/total_domain_count,
           party_share = counts/total_party_count,
          prop_rep =total_domain_party_prop/ total_party_prop,
          ) 

dem_rep_party_prop <- dem_rep %>%
ungroup() %>%
select(party, total_party_prop) %>% distinct() %>%
mutate(prop_string =  paste0(round(total_party_prop*100, digits = 1), "%"))

party_prop_list  <- dem_rep_party_prop$prop_string
names(party_prop_list) <- dem_rep_party_prop$party

x_axis_lab = paste0("Share of Results for Democratic Members\n(",party_prop_list[["Democrat"]]," of All Results)")
y_axis_lab = paste0("Share of Results for Republican Members\n(",party_prop_list[["Republican"]]," of All Results)")

dem_rep_bias <- rbind( dem_rep %>% 
                      left_join(pas)%>% 
filter(party != "Independent")%>% 
                      mutate(cat = "All Results"),
                      dem_rep %>% 
                      filter(party != "Independent")%>% 
left_join(pas)%>% 
left_join(house_domain_final %>% select(domain, news))%>%
filter(news == 1)%>% mutate(cat = "News Results Only") %>% select(-news))%>%
drop_na()%>%
group_by(cat, party)%>%
mutate(prop = counts/total_counts,
       total_bias_counts = sum(counts),
      mean_bias =sum(counts*bias_score, na.rm = TRUE)/total_bias_counts,
      party_label = ifelse(party == "Democrat", "Search Results for Democratic members", 
                           "Search Results for Republican members"),
       mean_label = ifelse(domain == "nytimes.com", 
                                         paste0("Mean = ", as.character(round(mean_bias, 2))),NA),
      mean_label_x = ifelse(party == "Democrat", mean_bias-0.42, mean_bias+0.42),
      mean_label_y = ifelse(cat == "News Results Only", 2, 9.5))

## A. Color plot regular
min_val = 0
dem_rep_sum <- dem_rep %>% 
    select(domain, party, party_share,total_domain_count)%>%
    pivot_wider(names_from = party, values_from = party_share) %>%
    replace_na(list(Republican = min_val, Democrat = min_val, Independent = min_val)) %>%
    left_join(pas %>% select(domain, bias_score))%>%
left_join(house_domain_final %>% select(domain,news))%>%
    mutate(total_share = Republican+Democrat+Independent)%>%
    arrange(desc(total_share))%>%
    ungroup()%>% drop_na() %>%
    mutate(rank = rank(desc(total_share)),
           label = ifelse(domain %in% c("house.gov", "twitter.com", "ballotpedia.org","govtrack.us",
                                        "facebook.com",
                                        "congress.gov", "nytimes.com","gop.gov") , domain, ""),
           label_x = ifelse(domain %in% c("facebook.com", "house.gov", "ballotpedia.org",  "twitter.com"), 1, 0),
           label_nudge = ifelse(domain %in% c("facebook.com", "house.gov", "ballotpedia.org",  "twitter.com"), -0.003, 0.003))%>% drop_na()

plot1 <- ggplot(dem_rep_sum, aes(x = Democrat, y = Republican, color = bias_score,label = label))+
geom_point(size = 1)+
geom_text(aes(hjust = label_x),color = "black", nudge_x = dem_rep_sum$label_nudge,  size = 3.5)+
scale_color_gradientn(colors = c( "dodgerblue4", "cornflowerblue", "gray70","lightcoral","red"),limits = c(-1,1))+
scale_y_continuous(limits = c(0, 0.25))+
scale_x_continuous(limits = c(0, 0.25))+
geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed")+
coord_fixed()+
labs(title = "Regular Scale",
    x = x_axis_lab, 
     y = y_axis_lab,
    color = "Partisan Audience Score")+
theme_bw(base_size = 14)+ theme(legend.position="bottom")

# B. Color plot on log scale
min_val = 0.000000005
dem_rep_sum <- dem_rep %>% 
    select(domain, party, party_share,total_domain_count)%>%
    pivot_wider(names_from = party, values_from = party_share) %>%
    replace_na(list(Republican = min_val, Democrat = min_val, Independent = min_val)) %>%
    left_join(pas %>% select(domain, bias_score))%>%
left_join(house_domain_final %>% select(domain,news))%>%
    mutate(total_share = Republican+Democrat+Independent)%>%
    arrange(desc(total_share))%>%
    ungroup()%>% drop_na() %>%
    mutate(rank = rank(desc(total_share)),
           label = ifelse(domain %in% c("house.gov", "twitter.com", "congress.gov", "nytimes.com",
                                       "thehill.com",  "foxnews.com", "gop.gov",
                                       "wsj.com", "emilyslist.org", "democrats.org") , domain, ""))

plot2 <- ggplot(dem_rep_sum%>% drop_na(), 
       aes(x = Democrat, y = Republican, color = bias_score,
           label = label))+
geom_point(size = 1)+
geom_text_repel(color = "black",box.padding = unit(0.1, "lines"), force = 2,segment.color = NA, size = 3.5)+
scale_color_gradientn(colors =c( "dodgerblue4", "cornflowerblue", "gray70","lightcoral","red"),limits = c(-1,1))+
geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed")+
scale_x_log10(labels = label_log(digits = 1))+ # CHECK
scale_y_log10(labels = label_log(digits = 1))+
coord_fixed()+
labs(title = "Log-Log Scale",
    x = x_axis_lab, 
     y = y_axis_lab,
    color = "Partisan Audience Score   ")+
theme_bw(base_size = 14)+ theme(legend.position="bottom")

# C. Partisan audience score by member party
plot3<- ggplot()+
geom_histogram(data = dem_rep_bias, 
               aes(x = bias_score, weight = (prop)*100, fill = party_label),
               alpha = 0.3, position = "identity", bins = 30)+
geom_vline(data = dem_rep_bias,aes(xintercept = mean_bias, color = party_label), linewidth = 1)+
scale_fill_manual(values = c(  "cornflowerblue","lightcoral"))+
scale_color_manual(values = c(  "dodgerblue4", "red"))+
geom_label(data = dem_rep_bias,
           aes(label = mean_label, x = mean_label_x, y = mean_label_y, color = party_label), 
           label.size = 0.25, size = 4.5, show.legend = FALSE)+
labs(y = "Percentage of Results", x = "Partisan Audience Score", color = "", fill = "" )+
theme_bw(base_size = 18)+
facet_wrap(~cat, ncol = 2, scales = "free_y")

plot12 <- plot_grid(plot1, plot2, ncol = 2,
          labels = c('A.', 'B.'), 
          label_size = 20)

plot_final <- plot_grid(plot12, plot3, ncol = 1,rel_heights = c(1.5, 1),
          labels = c('','C.'), 
          label_size = 20)
ggsave("figures/partisan_plot.pdf",plot_final, width = 13, height = 11, units = "in" )
