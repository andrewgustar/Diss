library(tidyverse)
library(extrafont)
library(scales)
library(beepr)

rm(list = ls())

diss <- readRDS("diss.RDS")
dissWords <- read_csv("dissWords2.csv")

dissWords <- dissWords %>% distinct(word)

diss <- diss %>% 
  mutate(comps=map(Title,~str_extract(.,dissWords$word)))

diss <- diss %>%
  mutate(comps=map(comps,~.[!is.na(.)])) %>% 
  select(Year, comps) %>% 
  unnest()

period <- 5

charts <- diss %>% count(Year,comps) %>% 
  mutate(Year=as.numeric(Year),
         fiveyear=period*floor(Year/period)) %>% 
  filter(fiveyear>1945,
         fiveyear<2015) %>% 
  group_by(fiveyear,comps) %>% 
  summarise(n=sum(n)) %>% 
  ungroup()
  
table(charts$fiveyear)
length(unique(charts$comps))

charts %>% 
  ggplot(aes(x=fiveyear+period/2, y=comps, fill=n)) +
  geom_raster() +
  scale_fill_continuous(name="theses",low="white",high="black") +
  scale_y_discrete(name="composer (196 of them)", labels=NULL) +
  scale_x_continuous(name="five year period", breaks=seq(1950,2010,10)) + 
  geom_vline(xintercept=seq(1950,2010,5),
             alpha=0.3) +
  ggtitle("Composers in Thesis Titles 1950-2014: transience") +
  theme_light() +
  theme(text=element_text(family = "Cambria"),
        panel.grid=element_blank(),
        panel.background = element_rect(fill=NULL))
ggsave("ThesisTransience.png",width = 6,height = 4,units = "in",dpi = 600)

correl <- charts %>% 
  spread(key=fiveyear, value=n, fill=0) %>% 
  select(-comps) %>% 
  as.matrix() %>% 
  cor() %>% 
  as_tibble() %>% 
  mutate(year=1950+period*(row_number()-1)) %>% 
  gather(key=year2, value=correl, -year) %>% 
  mutate(year2=as.numeric(str_extract(year2,"\\d+"))) %>% 
  mutate(yeardiff=year2-year) %>% 
  filter(yeardiff>0) %>% 
  select(-year2)

correl %>% ggplot(aes(x=yeardiff, y=correl)) +
  geom_point(alpha=0.3) +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs"), se=FALSE) +
  scale_x_log10(name="Time Lag (years)", limits=c(5,60), minor_breaks=NULL) +
  scale_y_continuous(name="correlation coefficient", limits=c(0,1)) +
  theme_light(base_family = "Cambria") +
  annotation_logticks(sides="b") +
  ggtitle("Composers in Thesis Titles 1950-2014: Correlation by Time Lag")
ggsave("ThesisCorrel.png",width = 6,height = 4,units = "in",dpi = 600)
