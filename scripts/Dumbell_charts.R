---
title: "Animal_rescue_by_LFB"
author: "Mahmudur Rahman"
date: "03/07/2021"
output:
  html_document:
    toc: true
    # toc_float: true
    # toc_depth: 2
    # number_sections: true
    # code_folding: hide
    theme: readable

---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, message = FALSE)
```

## Load libraries    
```{r}
library(tidyverse)
library(patchwork)
library(hrbrthemes)
library(lubridate)
library(here)
library(ggalt)
```

##Import Data 
```{r}
animal_rescue <- read.csv("input/animal_rescue.csv")
animal_rescue %>% glimpse()
animal_rescue <-  as_tibble(animal_rescue)

```

Make a dumbbell chart
```{r}
#Prepare data

df <- animal_rescue %>% 
  filter(year %in% c(2016,2020)) %>%
  select(borough,year,animal_group_parent) %>% 
  count(year,borough) %>% 
  spread(year,n) %>% 
  mutate(gap = `2016` - `2020`) %>% 
  arrange(gap) %>% 
  head(10)
  # <!-- %>%  -->
  # <!-- pivot_longer(cols = c(`2016`,`2020`), -->
  # <!--              values_to = "total", -->
  # <!--              names_to = "year") -->

#Make plot

blue <- "#0171CE"
red <- "#DE4433"
df %>% 
ggplot(aes(x = `2016`, xend = `2020`, y = reorder(borough, gap), group = borough)) + 
  geom_dumbbell(colour = "#dddddd",
                size = 3,
                colour_x = blue,
                colour_xend = red)+
  geom_text(data=filter(df, borough == "SOUTHWARK"),
          aes(x=`2016`, y=borough, label= 2016),
          color=blue, size=4, vjust=-1.5, fontface="bold")+
  geom_text(data=filter(df, borough == "SOUTHWARK"),
          aes(x=`2020`, y=borough, label= 2020),
          color=red, size=4, vjust=-1.5, fontface="bold")+
  theme_ipsum()
  

```


```{r}
# to selectively add level points
percent_first <- function(x) {
  x <- sprintf("%d%%", round(x*100))
  x[2:length(x)] <- sub("%$", "", x[2:length(x)])
  x
}
# data prep
df_2 <- animal_rescue %>% 
  filter(year %in% c(2016,2020)) %>%
  select(borough,year,animal_group_parent) %>% 
  count(year,borough) %>% 
  spread(year,n) %>% 
  mutate(gap = `2016` - `2020`) %>% 
  arrange(desc(gap)) %>% 
  head(5)


df_2 %>% 
ggplot(aes(x = `2016`, xend = `2020`, y = reorder(borough, gap), group = borough)) + 
  geom_dumbbell(colour = "#dddddd",
                size = 3,
                colour_x = blue,
                colour_xend = red)+
  geom_text(data=filter(df_2, borough == "ISLINGTON"),
          aes(x=`2016`, y=borough, label= 2016),
          color=blue, size=4, vjust=-1.5, fontface="bold")+
  geom_text(data=filter(df_2, borough == "ISLINGTON"),
          aes(x=`2020`, y=borough, label= 2020),
          color=red, size=4, vjust=-1.5, fontface="bold")+
  theme_ipsum()+
  labs(x= NULL,
       y = NULL,
       title = "Boroughs showing the decrease in calls , 2016-2020")
```

