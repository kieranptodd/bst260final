library(readr)
library(tidyverse)
library(data.table)
library(haven)

#read in brfss2010 data ---------------------------------------------------------------
brfss10 <- read_xpt("/Users/kierantodd/Desktop/CDBRFS10.XPT",
                    col_select = c(`IYEAR`,`_STATE`, `STOPSMK2`)) %>%
  as.data.table() %>%
  setnames(old = c("IYEAR","_STATE", "STOPSMK2"), new = c("YEAR","STATE", "STOPSMK"))


#read in brfss2020 data -----------------------------------------------------------------
brfss20 <- read_xpt("/Users/kierantodd/Desktop/LLCP2020.XPT",
                    col_select = c(`IYEAR`,`_STATE`, `STOPSMK2`)) %>%
  as.data.table() %>%
  setnames(old = c("IYEAR","_STATE", "STOPSMK2"), new = c("YEAR","STATE", "STOPSMK"))


#combine brfss 2010 and 2020 smoking data by state FIPS code 

brfss_all <- rbindlist(list(brfss10, brfss20), use.names = T)
brfss_all <- na.omit(brfss_all)

#filter by Nevada and Arizona
library(dslabs)
brfss_all <- brfss_all |> 
  filter(STATE %in% c("5", "32")) |>
  select(STATE, YEAR, STOPSMK) %>%
  mutate(outcome = case_when(
    STOPSMK == 1 ~ 1,
    STOPSMK == 2 ~ 0,
    TRUE ~ NA_real_))
 

#rename States by their FIPS codes to the State Abbreviations
brfss_all$STATE = ifelse(brfss_all$"STATE" == 5,"Arizona","Nevada")

brfss_analysis <- brfss_all %>% filter(YEAR %in% c(2010,2020))

#run a simple 2x2 difference in difference model using linear probablility model 
did_model1 <- lm(outcome ~ factor(STATE)*factor(YEAR), data=brfss_analysis)


#data viz




#read in brfss2010 data ---------------------------------------------------------------
brfss10 <- read_xpt("/Users/kierantodd/Desktop/CDBRFS10.XPT",
                    col_select = c(`IYEAR`,`_STATE`, `MENTHLTH`)) %>%
  as.data.table() %>%
  setnames(old = c("IYEAR","_STATE", "MENTHLTH"), new = c("YEAR","STATE", "MENTHLTH"))


#read in brfss2020 data -----------------------------------------------------------------
brfss20 <- read_xpt("/Users/kierantodd/Desktop/LLCP2020.XPT",
                    col_select = c(`IYEAR`,`_STATE`, `MENTHLTH`)) %>%
  as.data.table() %>%
  setnames(old = c("IYEAR","_STATE", "MENTHLTH"), new = c("YEAR","STATE", "MENTHLTH"))


#combine brfss 2010 and 2020 smoking data by state FIPS code 

brfss_all <- rbindlist(list(brfss10, brfss20), use.names = T)
brfss_all <- na.omit(brfss_all)

#filter by Nevada and Arizona
library(dslabs)
brfss_all <- brfss_all |> 
  filter(STATE %in% c("5", "32")) |>
  select(STATE, YEAR, MENTHLTH) %>%
  mutate(outcome = case_when(
    MENTHLTH == 88 ~ 0,
    MENTHLTH == 77 ~ NA_real_,
    MENTHLTH == 99 ~ NA_real_,
    TRUE ~ MENTHLTH))

na.omit(brfss_all)


#rename States by their FIPS codes to the State Abbreviations
brfss_all$STATE = ifelse(brfss_all$"STATE" == 5,"Arizona","Nevada")

brfss_analysis <- brfss_all %>% filter(YEAR %in% c(2010,2020))
brfss_analysis <- na.omit(brfss_analysis)

#run a simple 2x2 difference in difference model using linear probablility model 
did_model1 <- lm(outcome ~ factor(STATE)*factor(YEAR), data=brfss_analysis)
summary(did_model1)

# Plotting a density plot of mental health bad days in the pre period
ggplot(data=brfss_analysis[brfss_analysis$YEAR==2010], aes(x=outcome, group=as.factor(STATE)))+
  scale_x_continuous(breaks = seq(from = 0, to = 30, by = 5)) +
  geom_density( aes( fill=as.factor(STATE) ), alpha=0.4 ) +
  theme_bw() + xlab("Number of Days") +
  ylab("") + guides(fill=guide_legend(title="State")) +
  ggtitle("Number of Bad Mental Health Days (in a month) in 2010")

# Plotting a density plot of mental health bad days in the post period
ggplot(data=brfss_analysis[brfss_analysis$YEAR==2020], aes(x=outcome, group=as.factor(STATE)))+
  scale_x_continuous(breaks = seq(from = 0, to = 30, by = 5)) +
  geom_density( aes( fill=as.factor(STATE) ), alpha=0.4 ) +
  theme_bw() + xlab("Number of Days") +
  ylab("") + guides(fill=guide_legend(title="State")) +
  ggtitle("Number of Bad Mental Health Days (in a month) in 2020")

# Creating a plot to test for parallel pre-trends
library(dplyr)
library(plyr)
library(ggplot2)

agg <- ddply(brfss_analysis,.(YEAR, STATE),summarize, val = mean(outcome))

ggplot( data=agg, aes(x=as.factor(YEAR), y=val, group=STATE,
                      color=as.factor(STATE))) +
  geom_line(lwd=1.5)+
  theme_bw() + geom_point(size=4) +
  ylab("Bad Mental Health Days") + xlab("Year") +
  guides(color=guide_legend(title="States")) +
  ggtitle("Mental Health Trajectories for Arizona v. Nevada")

