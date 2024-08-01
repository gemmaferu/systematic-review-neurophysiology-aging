#The neurophysiology of healthy and pathological aging: A comprehensive systematic review (2024)

#Gemma Fernández-Rubio (gemmafr@clin.au.dk)
#Center for Music in the Brain, Aarhus University, Aarhus (Denmark)
#05-02-2024

#LIBRARIES, WORKING DIRECTORY AND DATA ====
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(maps)
library(RColorBrewer)
library(writexl)

setwd('Working_Directory')
srdata <- read_excel('data.xlsx')

#PUBLICATIONS PER YEAR (Fig. 2a) ====

#total frequency and percentage
year <- data.frame(year = sort(unique(srdata$year[1:942])),
                   frequency = as.vector(table(srdata$year[1:942])),
                   percentage = as.vector(prop.table(table(srdata$year[1:942]))*100))

#bar plot
ggplot(year, aes(x = year, y = frequency)) + 
  geom_col(color = 'black', fill = '#FBB4AE', width = 1, size = 0.25) +
  scale_x_continuous(expand = c(0,0.5), breaks = seq(1979,2023,2), name = 'Publication year') +
  scale_y_continuous(expand = c(0,0,0.05,0),breaks = seq(0,160,10),name = 'Number of articles') +
  theme_bw() +
  theme(text = element_text(size = 12, family = 'sans'),
        axis.title = element_text(face = 'bold'))

ggsave('Year.pdf')

#PUBLICATIONS PER COUNTRY (Fig. 2b) ====

#total frequency and percentage
country <- data.frame(country = sort(unique(srdata$country[1:942])),
                   frequency = as.vector(table(srdata$country[1:942])),
                   percentage = as.vector(prop.table(table(srdata$country[1:942]))*100))

#world map plot
world_map <- map_data('world') #load world map
country_freq <- data.frame(table(srdata$country[1:942])) #calculate frequency of each country
names(country_freq)[names(country_freq) == 'Var1'] <- 'region' #rename variable
worldSubset <- left_join(world_map, country_freq, by = 'region') #join world map and country data
ggplot(data = worldSubset, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = Freq)) +
  scale_fill_distiller(name = 'Number of articles', 
                       palette = 'Reds',
                       direction = 1, 
                       guide = guide_colorsteps(even.steps = TRUE,
                                                show.limits = TRUE,
                                                barwidth = 10, 
                                                barheight = 1,
                                                frame.colour = 'gray10', 
                                                ticks.colour = 'gray10')) +
  theme(text = element_text(size = 12, family= 'sans'),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = 'white'),
        plot.title = element_text(hjust = 0.5),
        legend.position = 'bottom',
        legend.title.position = 'top')

ggsave('Country.pdf')

#STUDY DESIGN (Fig. 3a, 3b, & 3c) ====

#total frequency and percentage
design <- data.frame(design = unique(srdata$design[1:942]),
                   frequency = as.vector(table(srdata$design[1:942])),
                   percentage = as.vector(prop.table(table(srdata$design[1:942]))*100))

#pie chart
ggplot(design, aes(x = '', y = percentage, fill = design)) +
  geom_bar(stat = 'identity', color = 'black') +
  coord_polar('y', start = 0) +
  geom_text(aes(label = paste0(c(round(percentage, digits = 2)), '%')),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_fill_brewer(palette = 'Pastel1') +
  theme_classic() +
  theme(text = element_text(size = 15, family = 'sans'),
        axis.title = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = 'bottom',
        legend.title = element_blank())

ggsave('Design.pdf')

#bar plot - design & year
dum <- data.frame(table(srdata$year[1:942],srdata$design[1:942])) #frequency of study design per year
dum <- rbind(dum[1:3, ], data.frame(Var1 = '1984', Var2 = 'Cross-sectional', Freq = 0), dum[4:126, ]) #add missing year ('1984')
dum$Var1 <- factor(dum$Var1, levels = unique(dum$Var1)) #reorder year ('1984' between '1983' and '1985')
dum <- dum[complete.cases(dum), ] #remove empty rows

ggplot(dum, aes(x = Var1, y = Freq, fill = Var2)) +
  geom_col(color = 'black', width = 0.75, size = 0.25) +
  scale_x_discrete(expand = c(0,0.5), breaks = seq(1979,2023,2), name = 'Publication year') +
  scale_y_continuous(expand = c(0,0,0.05,0),breaks = seq(0,160,5),name = 'Number of articles') +
  scale_fill_brewer(palette = 'Pastel1') +
  theme_bw() +
  theme(text = element_text(size = 10, family = 'sans'),
        axis.title = element_text(size = 12, face = 'bold'),
        legend.position = c(0.086,0.94),
        legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid', linewidth = 0.25),
        legend.title = element_blank())

ggsave('Design_Year.pdf')

#ratio of study design to publications per year
ratio <- data.frame(year = srdata$year[1:942], var = srdata$design[1:942])
ratio <- ratio %>%
  group_by(year) %>%
  summarise(total_studies = n(), var1 = sum(var == 'Cross-sectional'), var2 = sum(var == 'Longitudinal'),
            ratio_var1 = var1 / total_studies, ratio_var2 = var2 / total_studies)

ratio <- ratio %>%
  pivot_longer(cols = starts_with('var'), names_to = 'var', values_to = 'value') %>%
  mutate(ratio = ifelse(var == "var1", ratio_var1, ratio_var2)) %>%
  select(year, ratio, var)

ggplot(ratio, aes(x = year, y = ratio, group = var)) +
  geom_line(aes(color = var)) +
  geom_point(aes(color = var)) +
  scale_x_continuous(expand = c(0,0.5), breaks = seq(1979,2023,2), name = 'Year') +
  scale_y_continuous(expand = c(0.05,0,0.05,0),breaks = seq(0,1,0.1),name = 'Experimental design ratio') +
  scale_color_brewer(palette = 'Pastel1', labels = c('Cross-sectional', 'Longitudinal')) +
  theme_bw() +
  theme(text = element_text(size = 10, family = 'sans'),
        axis.title = element_text(size = 12, face = 'bold'),
        legend.position = c(0.9,0.5),
        legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid', linewidth = 0.25),
        legend.title = element_blank())

ggsave('Design_Ratio.pdf')

#STUDY DURATION (Fig. 3d) ====

#total frequency and percentage
duration <- data.frame(duration = sort(unique(srdata$duration[1:72])),
                       frequency = as.vector(table(srdata$duration[1:72])),
                       percentage = as.vector(prop.table(table(srdata$duration[1:72]))*100))

#bar plot
ggplot(duration, aes(x = duration, y = frequency)) +
  geom_col(color = 'black', fill = '#FBB4AE', width = 0.75, size = 0.25) +
  scale_x_discrete(name = 'Average duration', limits = c('< 1 year','1 - 2 years','2 - 3 years','3 - 4 years','> 4 years')) +
  scale_y_continuous(expand = c(0,0,0.05,0), breaks = seq(0,20,1),name = 'Number of longitudinal articles') +
  theme_bw() +
  theme(text = element_text(size = 12, family = 'sans'),
        axis.title = element_text(face = 'bold'))

ggsave('Duration.pdf')

#NEUROIMAGING TECHNIQUES (Fig. 3e, 3f, & 3g) ====

#total frequency and percentage
neuro <- data.frame(neuro = sort(unique(srdata$neuroimaging[1:942])),
                       frequency = as.vector(table(srdata$neuroimaging[1:942])),
                       percentage = as.vector(prop.table(table(srdata$neuroimaging[1:942]))*100))

#pie chart
ggplot(neuro, aes(x = '', y = percentage, fill = reorder(neuro, -percentage))) +
  geom_bar(stat = 'identity', color = 'black') +
  coord_polar('y', start = 0) +
  geom_text(aes(label = paste0(c(round(percentage, digits = 2)), '%')),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_fill_manual(values = brewer.pal(9, 'Pastel1')[c(1,2,5)]) +
  theme_classic() +
  theme(text = element_text(size = 15, family = 'sans'),
        axis.title = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = 'bottom',
        legend.title = element_blank())

ggsave('Neuroimaging.pdf')

#bar plot - design & year
dum <- data.frame(table(srdata$year[1:942],srdata$neuroimaging[1:942])) #frequency of neuroimaging technique per year
dum <- rbind(dum[1:3, ], data.frame(Var1 = '1984', Var2 = 'MEG', Freq = 0), dum[4:126, ]) #add missing year ('1984')
dum$Var1 <- factor(dum$Var1, levels = unique(dum$Var1)) #reorder year ('1984' between '1983' and '1985')
dum$Var2 <- factor(dum$Var2, levels = c('EEG','MEG','M/EEG')) #reorder neuroimaging techniques (highest to lowest frequency)

ggplot(dum, aes(x = Var1, y = Freq, fill = Var2)) +
  geom_col(color = 'black', width = 0.75, size = 0.25) +
  scale_x_discrete(expand = c(0,0.5), breaks = seq(1979,2023,2), name = 'Publication year') +
  scale_y_continuous(expand = c(0,0,0.05,0),breaks = seq(0,160,5),name = 'Number of articles') +
  scale_fill_manual(values = brewer.pal(9, 'Pastel1')[c(1,2,5)]) +
  theme_bw() +
  theme(text = element_text(size = 10, family = 'sans'),
        axis.title = element_text(size = 12, face = 'bold'),
        legend.position = c(0.08,0.92),
        legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid', linewidth = 0.25),
        legend.title = element_blank())

ggsave('Neuroimaging_Year.pdf')

#ratio of neuroimaging technique to publications per year
ratio <- data.frame(year = srdata$year[1:942], var = srdata$neuroimaging[1:942])
ratio <- ratio %>%
  group_by(year) %>%
  summarise(total_studies = n(), var1 = sum(var == 'EEG'), var2 = sum(var == 'MEG'), var3 = sum(var == 'M/EEG'),
            ratio_var1 = var1 / total_studies, ratio_var2 = var2 / total_studies, ratio_var3 = var3 / total_studies)

ratio <- ratio %>%
  pivot_longer(cols = starts_with('var'), names_to = 'var', values_to = 'value') %>%
  mutate(ratio = case_when(var == 'var1' ~ ratio_var1, var == 'var2' ~ ratio_var2, var == 'var3' ~ ratio_var3)) %>%
  select(year, ratio, var)

ggplot(ratio, aes(x = year, y = ratio, group = var)) +
  geom_line(aes(color = var)) +
  geom_point(aes(color = var)) +
  scale_x_continuous(expand = c(0,0.5), breaks = seq(1979,2023,2), name = 'Publication year') +
  scale_y_continuous(expand = c(0.05,0,0.05,0),breaks = seq(0,1,0.1),name = 'Neuroimaging technique ratio') +
  scale_color_manual(values = brewer.pal(9, 'Pastel1')[c(1,2,5)], labels = c('EEG','MEG','M/EEG')) +
  theme_bw() +
  theme(text = element_text(size = 10, family = 'sans'),
        axis.title = element_text(size = 12, face = 'bold'),
        legend.position = c(0.94,0.93),
        legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid', linewidth = 0.25),
        legend.title = element_blank())

ggsave('Neuroimaging_Ratio.pdf')

#EXPERIMENTAL PARADIGM (Fig. 3h, 3i, & 3j) ====

#total frequency and percentage
paradigm <- data.frame(paradigm = sort(unique(srdata$paradigm[1:942])),
                    frequency = as.vector(table(srdata$paradigm[1:942])),
                    percentage = as.vector(prop.table(table(srdata$paradigm[1:942]))*100))

#pie chart
ggplot(paradigm, aes(x = '', y = percentage, fill = reorder(paradigm, -percentage))) +
  geom_bar(stat = 'identity', color = 'black') +
  coord_polar('y', start = 0) +
  geom_text(aes(label = paste0(c(round(percentage, digits = 2)), '%')),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_fill_manual(values = brewer.pal(9, 'Pastel1')[c(1,2,5)]) +
  theme_classic() +
  theme(text = element_text(size = 15, family = 'sans'),
        axis.title = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = 'bottom',
        legend.title = element_blank())

ggsave('paradigm.pdf')

#bar plot - rs/task & year
dum <- data.frame(table(srdata$year[1:942],srdata$paradigm[1:942])) #frequency of neuroimaging technique per year
dum <- rbind(dum[1:3, ], data.frame(Var1 = '1984', Var2 = 'RS', Freq = 0), dum[4:126, ]) #add missing year ('1984')
dum$Var1 <- factor(dum$Var1, levels = unique(dum$Var1)) #reorder year ('1984' between '1983' and '1985')
dum$Var2 <- factor(dum$Var2, levels = c('RS','Task','Both')) #reorder neuroimaging techniques (highest to lowest frequency)

ggplot(dum, aes(x = Var1, y = Freq, fill = Var2)) +
  geom_col(color = 'black', width = 0.75, size = 0.25) +
  scale_x_discrete(expand = c(0,0.5), breaks = seq(1979,2023,2), name = 'Publication year') +
  scale_y_continuous(expand = c(0,0,0.05,0),breaks = seq(0,160,5),name = 'Number of articles') +
  scale_fill_manual(values = brewer.pal(9, 'Pastel1')[c(1,2,5)]) +
  theme_bw() +
  theme(text = element_text(size = 10, family = 'sans'),
        axis.title = element_text(size = 12, face = 'bold'),
        legend.position = c(0.08,0.89),
        legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid', linewidth = 0.25),
        legend.title = element_blank())

ggsave('paradigm_Year.pdf')

#ratio of paradigm to publications per year
ratio <- data.frame(year = srdata$year[1:942], var = srdata$paradigm[1:942])
ratio <- ratio %>%
  group_by(year) %>%
  summarise(total_studies = n(), var1 = sum(var == 'RS'), var2 = sum(var == 'Task'), var3 = sum(var == 'Both'),
            ratio_var1 = var1 / total_studies, ratio_var2 = var2 / total_studies, ratio_var3 = var3 / total_studies)

ratio <- ratio %>%
  pivot_longer(cols = starts_with('var'), names_to = 'var', values_to = 'value') %>%
  mutate(ratio = case_when(var == 'var1' ~ ratio_var1, var == 'var2' ~ ratio_var2, var == 'var3' ~ ratio_var3)) %>%
  select(year, ratio, var)

ggplot(ratio, aes(x = year, y = ratio, group = var)) +
  geom_line(aes(color = var)) +
  geom_point(aes(color = var)) +
  scale_x_continuous(expand = c(0,0.5), breaks = seq(1979,2023,2), name = 'Publication year') +
  scale_y_continuous(expand = c(0.05,0,0.05,0),breaks = seq(0,1,0.1),name = 'paradigm ratio') +
  scale_color_manual(values = brewer.pal(9, 'Pastel1')[c(1,2,5)], labels = c('RS','Task','Both')) +
  theme_bw() +
  theme(text = element_text(size = 10, family = 'sans'),
        axis.title = element_text(size = 12, face = 'bold'),
        legend.position = c(0.93,0.89),
        legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid', linewidth = 0.25),
        legend.title = element_blank())

ggsave('paradigm_Ratio.pdf')

#RESTING-STATE (Fig. 3k) ====

#total frequency and percentage
rs <- na.omit(srdata$paradigm_spec) #remove empty rows
rs <- rs[rs %in% c('EO','EC','EO/EC','N/A')] #only keep values related to resting-state
rs <- data.frame(rs = sort(unique(rs)),
                       frequency = as.vector(table(rs)),
                       percentage = as.vector(prop.table(table(rs))*100))

#pie chart
ggplot(rs, aes(x = '', y = percentage, fill = reorder(rs, -percentage))) +
  geom_bar(stat = 'identity', color = 'black') +
  coord_polar('y', start = 0) +
  geom_text(aes(label = paste0(c(round(percentage, digits = 2)), '%')),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_fill_manual(values = brewer.pal(9, 'Pastel1')[c(1,2,5,4)]) +
  theme_classic() +
  theme(text = element_text(size = 15, family = 'sans'),
        axis.title = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = 'bottom',
        legend.title = element_blank())

ggsave('RestingState.pdf')

#EXPERIMENTAL TASK (Supp. Table 3) ====

#total frequency and percentage
task <- na.omit(srdata$paradigm_spec) #remove empty rows
task <- task[!(task %in% c('EO','EC','EO/EC','N/A'))] #remove values related to resting-state
frequency <- table(task) #calculate frequency

#write table with tasks, frequency, and percentage
task <- data.frame(task = as.factor(names(frequency)),
                 frequency = as.vector(frequency),
                 percentage = as.vector(prop.table(frequency)*100))

write_xlsx(task,'Experimental_Task.xlsx')

#POPULATION (Fig. 4a, 4b, & 4c) ====

#re-organize aging, MCI, and dementia columns
aging <- ifelse(is.na(srdata$aging[1:942]), 0, 'Healthy aging') #remove empty rows
mci <- ifelse(is.na(srdata$mci[1:942]), 0, 'MCI') #remove empty rows
dementia <- ifelse(is.na(srdata$dementia[1:942]), 0, 'Dementia') #remove empty rows
pall <- apply(cbind(aging, mci, dementia), 1, function(x) max(x[x != '0']))

#total frequency and percentage
frequency <- table(pall)
all <- data.frame(all = as.factor(names(frequency)), #total frequency and percentage
                   frequency = as.vector(frequency),
                   percentage = as.vector(prop.table(frequency)*100))

#pie chart
ggplot(all, aes(x = '', y = percentage, fill = reorder(all, -percentage))) +
  geom_bar(stat = 'identity', color = 'black') +
  coord_polar('y', start = 0) +
  geom_text(aes(label = paste0(c(round(percentage, digits = 2)), '%')),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_fill_manual(values = brewer.pal(9, 'Pastel1')[c(1,2,5)]) +
  theme_classic() +
  theme(text = element_text(size = 15, family = 'sans'),
        axis.title = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = 'bottom',
        legend.title = element_blank())

ggsave('Population.pdf')

#bar plot - population & year
dum <- data.frame(table(srdata$year[1:942],Var2 = pall)) #frequency per year
dum <- rbind(dum[1:3, ], data.frame(Var1 = '1984', Var2 = 'Dementia', Freq = 0), dum[4:126, ]) #add missing year ('1984')
dum$Var1 <- factor(dum$Var1, levels = unique(dum$Var1)) #reorder year

ggplot(dum, aes(x = Var1, y = Freq, fill = Var2)) +
  geom_col(color = 'black', width = 0.75, size = 0.25) +
  scale_x_discrete(expand = c(0,0.75), breaks = seq(1979,2023,2), name = 'Publication year') +
  scale_y_continuous(expand = c(0,0,0.05,0),breaks = seq(0,160,5),name = 'Number of articles') +
  scale_fill_manual(values = brewer.pal(9, 'Pastel1')[c(1,2,5)]) +
  theme_bw() +
  theme(text = element_text(size = 10, family = 'sans'),
        axis.title = element_text(size = 12, face = 'bold'),
        legend.position = c(0.11,0.89),
        legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid', linewidth = 0.25),
        legend.title = element_blank())

ggsave('Population_Year.pdf')

#ratio of population to publications per year
ratio <- data.frame(year = srdata$year[1:942], var = pall)
ratio <- ratio %>%
  group_by(year) %>%
  summarise(total_studies = n(), var1 = sum(var == 'Dementia'), var2 = sum(var == 'MCI'), var3 = sum(var == 'Healthy aging'),
            ratio_var1 = var1 / total_studies, ratio_var2 = var2 / total_studies, ratio_var3 = var3 / total_studies)

ratio <- ratio %>%
  pivot_longer(cols = starts_with('var'), names_to = 'var', values_to = 'value') %>%
  mutate(ratio = case_when(var == 'var1' ~ ratio_var1, var == 'var2' ~ ratio_var2, var == 'var3' ~ ratio_var3)) %>%
  select(year, ratio, var)

ggplot(ratio, aes(x = year, y = ratio, group = var)) +
  geom_line(aes(color = var)) +
  geom_point(aes(color = var)) +
  scale_x_continuous(expand = c(0,0.5), breaks = seq(1979,2023,2), name = 'Publication year') +
  scale_y_continuous(expand = c(0.05,0,0.05,0),breaks = seq(0,1,0.1),name = 'Population ratio') +
  scale_color_manual(values = brewer.pal(9, 'Pastel1')[c(1,2,5)], labels = c('Dementia', 'MCI','Healthy aging')) +
  theme_bw() +
  theme(text = element_text(size = 10, family = 'sans'),
        axis.title = element_text(size = 12, face = 'bold'),
        legend.position = c(0.89,0.89),
        legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid', linewidth = 0.25),
        legend.title = element_blank())

ggsave('Population_Ratio.pdf')

#DEMENTIA (Fig. 4d & 4e) ====

#total frequency and percentage
dementia <- data.frame(aging = sort(unique(srdata$dementia[1:942])),
                       frequency = as.vector(table(srdata$dementia[1:942])),
                       percentage = as.vector(prop.table(table(srdata$dementia[1:942]))*100))

#bar plot
popu <- data.frame(table(srdata$neuroimaging,srdata$dementia)) #number of EEG + MEG studies
popu <- popu[popu$Freq >= 10, ] #remove single studies
total_frequency <- aggregate(Freq ~ Var2, data = popu, FUN = sum)
ordered_groups <- total_frequency$Var2[order(total_frequency$Freq, decreasing = TRUE)]
popu$Var2 <- factor(popu$Var2, levels = ordered_groups)

ggplot(popu, aes(x = Var2, y = Freq, fill = Var1)) +
  geom_col(stat = 'identity', color = 'black', width = 0.75, size = 0.25) +
  scale_x_discrete(expand = c(0,0.5), name = 'Dementia',
                   labels = c('ADD vs HC', 'ADD vs MCI vs HC', 'ADD', 'ADD vs YHC vs OHC', 'ADD vs aMCI vs HC', 'ADD vs VD vs HC')) +
  scale_y_continuous(expand = c(0,0,0.05,0), breaks = seq(0,200,5), name = 'Number of articles') +
  scale_fill_brewer(palette = 'Pastel1') +
  theme_bw() +
  theme(text = element_text(size = 10, family = 'sans'),
        axis.title = element_text(size = 12, face = 'bold'),
        legend.position = c(0.93,0.94),
        legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid', linewidth = 0.25),
        legend.title = element_blank())

ggsave('Dementia.pdf')

#bar plot - types of dementia
type <- data.frame(group = na.omit(srdata$dementia)) #all groups
type$add <- ifelse(grepl('ADD', type$group), 'ADD', NA) #Alzheimer's disease
type$pdd <- ifelse(grepl('PDD', type$group), 'PDD', NA) #Parkinson's disease
type$ftd <- ifelse(grepl('FTD', type$group), 'FTD', NA) #Frontotemporal dementia
type$dlb <- ifelse(grepl('DLB', type$group), 'DLB', NA) #Dementia with Lewy bodies
type$vd <- ifelse(grepl('VD', type$group), 'VD', NA) #Vascular dementia
type$deu <- ifelse(grepl('DEM', type$group), 'DEM', NA) #Dementia (unknown etiology)
type$other <- ifelse(!grepl('[ADD|PDD|FTD|DLB|VD|DEM]', type$group), 'OTHER', NA) #other types

type_freq <- data.frame(dem = c('ADD','PDD','FTD','DLB','VD','DEU'),
                        frequency = c(table(type$add),table(type$pdd),table(type$ftd),table(type$dlb),table(type$vd),table(type$deu)))

ggplot(type_freq, aes(x = reorder(dem, -frequency), y = frequency, fill = total)) + 
  geom_col(color = 'black', fill = '#FBB4AE', width = 0.75, size = 0.25) +
  scale_x_discrete(expand = c(0,0.5), name = 'Type') +
  scale_y_continuous(expand = c(0,0,0.05,0),breaks = seq(0,500,30),name = 'Number of publications') +
  theme_bw() +
  theme(text = element_text(size = 12, family = 'sans'),
        axis.title = element_text(face = 'bold'))

ggsave('Dementia_Type.pdf')

#HEALTHY AGING (Fig. 4f) ====

#total frequency and percentage
aging <- data.frame(aging = sort(unique(srdata$aging[1:942])),
                    frequency = as.vector(table(srdata$aging[1:942])),
                    percentage = as.vector(prop.table(table(srdata$aging[1:942]))*100))

#bar plot
popu <- data.frame(table(srdata$neuroimaging,srdata$aging)) #number of EEG + MEG studies
popu <- popu[popu$Freq >= 2, ] #remove single studies
total_frequency <- aggregate(Freq ~ Var2, data = popu, FUN = sum)
ordered_groups <- total_frequency$Var2[order(total_frequency$Freq, decreasing = TRUE)]
popu$Var2 <- factor(popu$Var2, levels = ordered_groups)

ggplot(popu, aes(x = Var2, y = Freq, fill = Var1)) +
  geom_bar(stat = 'identity', color = 'black', width = 0.75, size = 0.25) +
  scale_x_discrete(expand = c(0,0.5), name = 'Healthy aging', 
                   labels = c('Y vs O', 'O', 'Lifespan', 'Y vs M vs O', 'M vs O')) +
  scale_y_continuous(expand = c(0,0,0.05,0), breaks = seq(0,160,5), name = 'Number of articles') +
  scale_fill_manual(values = brewer.pal(9, 'Pastel1')[c(1,2,5)], labels = c('EEG', 'MEG', 'M/EEG')) +
  theme_bw() +
  theme(text = element_text(size = 10, family = 'sans'),
        axis.title = element_text(size = 12, face = 'bold'),
        legend.position = c(0.92,0.92),
        legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid', linewidth = 0.25),
        legend.title = element_blank())

ggsave('HealthyAging.pdf')

#MILD COGNITIVE IMPAIRMENT (Fig. 4g) ====

#total frequency and percentage
mci <- data.frame(aging = sort(unique(srdata$mci[1:942])),
                   frequency = as.vector(table(srdata$mci[1:942])),
                   percentage = as.vector(prop.table(table(srdata$mci[1:942]))*100))
#bar plot
popu <- data.frame(table(srdata$neuroimaging,srdata$mci)) #number of EEG + MEG studies
popu <- popu[popu$Freq >= 5, ] #remove single studies
total_frequency <- aggregate(Freq ~ Var2, data = popu, FUN = sum)
ordered_groups <- total_frequency$Var2[order(total_frequency$Freq, decreasing = TRUE)]
popu$Var2 <- factor(popu$Var2, levels = ordered_groups)

ggplot(popu, aes(x = Var2, y = Freq, fill = Var1)) +
  geom_col(stat = 'identity', color = 'black', width = 0.75, size = 0.25) +
  scale_x_discrete(expand = c(0,0.5), name = 'Mild cognitive impairment',
                   labels = c('MCI vs HC', 'aMCI vs HC', 'ADMCI vs HC', 'MCI vs YHC vs OHC','Stable vs Progressive MCI', 'MCI vs SCD vs HC')) +
  scale_y_continuous(expand = c(0,0,0.05,0), breaks = seq(0,200,5), name = 'Number of articles') +
  scale_fill_brewer(palette = 'Pastel1') +
  theme_bw() +
  theme(text = element_text(size = 10, family = 'sans'),
        axis.title = element_text(size = 12, face = 'bold'),
        legend.position = c(0.93,0.94),
        legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid', linewidth = 0.25),
        legend.title = element_blank())

ggsave('MCI.pdf')

#bar plot - types of mci
type <- data.frame(comp = na.omit(srdata$mci))
type$mci <- ifelse(!grepl('[AD|PD|a|STABLE]', type$comp), 'x', NA) #MCI
type$admci <- ifelse(grepl('ADMCI', type$comp), 'x', NA) #ADMCI
type$pdmci <- ifelse(grepl('PDMCI', type$comp), 'x', NA) #PDMCI
type$amci <- ifelse(grepl('aMCI', type$comp), 'x', NA) #aMCI
type$promci <- ifelse(grepl('PROGREMCI', type$comp), 'x', NA) #Progressive MCI

type_freq <- data.frame(mci = c('MCI','ADMCI','PDMCI','aMCI','PROMCI'),
                           frequency = c(table(type$mci),table(type$admci),table(type$pdmci),
                                         table(type$amci),table(type$promci)))

ggplot(type_freq, aes(x = reorder(mci, -frequency), y = frequency)) + 
  geom_col(color = 'black', fill = '#FBB4AE', width = 0.75, size = 0.25) +
  scale_x_discrete(expand = c(0,0.5), name = 'Type') +
  scale_y_continuous(expand = c(0,0,0.05,0),breaks = seq(0,500,5),name = 'Number of publications') +
  theme_bw() +
  theme(text = element_text(size = 12, family = 'sans'),
        axis.title = element_text(face = 'bold'))

ggsave('Dementia_Type.pdf')

#ANALYSIS (Fig. 5a, 5b, & 5c)  ====

#total frequency and percentage
analysis <- data.frame(analysis = sort(unique(srdata$analysis[1:942])),
                      frequency = as.vector(table(srdata$analysis[1:942])),
                      percentage = as.vector(prop.table(table(srdata$analysis[1:942]))*100))

#pie chart
ggplot(analysis, aes(x = '', y = percentage, fill = reorder(analysis, -percentage))) +
  geom_bar(stat = 'identity', color = 'black') +
  coord_polar('y', start = 0) +
  geom_text(aes(label = paste0(c(round(percentage, digits = 2)), '%')),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_fill_manual(values = brewer.pal(9, 'Pastel1')[c(1,2,5,4,7,9)]) +
  theme_classic() +
  theme(text = element_text(size = 15, family = 'sans'),
        axis.title = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = 'bottom',
        legend.title = element_blank())

ggsave('Analysis.pdf')

#bar plot - analysis & year
dum <- data.frame(table(srdata$year[1:942],srdata$analysis[1:942]))
dum <- rbind(dum[1:3, ], data.frame(Var1 = '1984', Var2 = 'Complexity', Freq = 0), dum[4:252, ]) #add missing year ('1984')
dum$Var1 <- factor(dum$Var1, levels = unique(dum$Var1)) #reorder year ('1984' between '1983' and '1985')
dum$Var2 <- factor(dum$Var2, levels = c('Spectral','Event-related','Functional connectivity',
                                        'Mixed','Complexity','Other')) #reorder analysis (highest to lowest frequency)
ggplot(dum, aes(x = Var1, y = Freq, fill = Var2)) +
  geom_col(color = 'black', width = 0.75, size = 0.25) +
  scale_x_discrete(expand = c(0,0.5), breaks = seq(1979,2023,2), name = 'Publication year') +
  scale_y_continuous(expand = c(0,0,0.05,0), breaks = seq(0,800,10), name = 'Number of articles') +
  scale_fill_manual(values = brewer.pal(9, 'Pastel1')[c(1,2,5,4,7,9)]) +
  theme_bw() +
  theme(text = element_text(size = 10, family = 'sans'),
        axis.title = element_text(size = 12, face = 'bold'),
        legend.position = c(0.15,0.81),
        legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid', linewidth = 0.25),
        legend.title = element_blank())

ggsave('Analysis_Year.pdf')

#ratio of analysis to publications per year
ratio <- data.frame(year = srdata$year[1:942], var = srdata$analysis[1:942])
ratio <- ratio %>%
  group_by(year) %>%
  summarise(total_studies = n(), var1 = sum(var == 'Spectral'), var2 = sum(var == 'Event-related'), var3 = sum(var == 'Functional connectivity'), var4 = sum(var == 'Mixed'), var5 = sum(var == 'Complexity'), var6 = sum(var == 'Other'),
            ratio_var1 = var1 / total_studies, ratio_var2 = var2 / total_studies, ratio_var3 = var3 / total_studies, ratio_var4 = var4 / total_studies, ratio_var5 = var5 / total_studies, ratio_var6 = var6 / total_studies)

ratio <- ratio %>%
  pivot_longer(cols = starts_with('var'), names_to = 'var', values_to = 'value') %>%
  mutate(ratio = case_when(var == 'var1' ~ ratio_var1, var == 'var2' ~ ratio_var2, var == 'var3' ~ ratio_var3, var == 'var4' ~ ratio_var4, var == 'var5' ~ ratio_var5, var == 'var6' ~ ratio_var6)) %>%
  select(year, ratio, var)

ggplot(ratio, aes(x = year, y = ratio, group = var)) +
  geom_line(aes(color = var)) +
  geom_point(aes(color = var)) +
  scale_x_continuous(expand = c(0,0.5), breaks = seq(1979,2023,2), name = 'Publication year') +
  scale_y_continuous(expand = c(0.05,0,0.05,0),breaks = seq(0,1,0.1),name = 'Analysis ratio') +
  scale_color_manual(values = brewer.pal(9, 'Pastel1')[c(1,2,5,4,7,9)], labels = c('Spectral','Event-related','Functional connectivity',
                                                                                   'Mixed','Complexity','Other')) +
  theme_bw() +
  theme(text = element_text(size = 10, family = 'sans'),
        axis.title = element_text(size = 12, face = 'bold'),
        legend.position = c(0.85,0.81),
        legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid', linewidth = 0.25),
        legend.title = element_blank())

ggsave('Analysis_Ratio.pdf')

#FEATURES (Fig. 5d & Supp. Table 4) ====

#total frequency and percentage - spectral
spectral <- data.frame(spectral = sort(unique(srdata$spectral)), #total frequency and percentage
                       frequency = as.vector(table(srdata$spectral)),
                       percentage = as.vector(prop.table(table(srdata$spectral))*100))

write_xlsx(spectral,'Spectral.xlsx')

#total frequency and percentage - complexity
complexity <- data.frame(complexity = sort(unique(srdata$complexity)), #total frequency and percentage
                       frequency = as.vector(table(srdata$complexity)),
                       percentage = as.vector(prop.table(table(srdata$complexity))*100))

write_xlsx(complexity,'Complexity.xlsx')

#total frequency and percentage - connectivity
connectivity <- data.frame(connectivity = sort(unique(srdata$connectivity)), #total frequency and percentage
                       frequency = as.vector(table(srdata$connectivity)),
                       percentage = as.vector(prop.table(table(srdata$connectivity))*100))

write_xlsx(connectivity,'Connectivity.xlsx')

#total frequency and percentage - connectivity
event_related <- data.frame(event_related = sort(unique(srdata$event_related)), #total frequency and percentage
                           frequency = as.vector(table(srdata$event_related)),
                           percentage = as.vector(prop.table(table(srdata$event_related))*100))

write_xlsx(event_related,'Event_Related.xlsx')

#total frequency and percentage - other
other <- data.frame(other = sort(unique(srdata$other)), #total frequency and percentage
                       frequency = as.vector(table(srdata$other)),
                       percentage = as.vector(prop.table(table(srdata$other))*100))

write_xlsx(other,'Other.xlsx')

#select 3 most used features
spectral <- spectral[order(spectral$frequency,decreasing = TRUE),]
spectral <- spectral[1:3,]
complexity <- complexity[order(complexity$frequency,decreasing = TRUE),]
complexity <- complexity[1:3,]
connectivity <- connectivity[order(connectivity$frequency,decreasing = TRUE),]
connectivity <- connectivity[1:3,]
event_related <- event_related[order(event_related$frequency,decreasing = TRUE),]
event_related <- event_related[1:3,]
other <- other[order(other$frequency,decreasing = TRUE),]
other <- other[1:3,]

#bar plot
features <- data.frame(feature = c(spectral$spectral,complexity$complexity,connectivity$connectivity,event_related$event_related,other$other),
                       frequency = c(spectral$frequency,complexity$frequency,connectivity$frequency,event_related$frequency,other$frequency),
                       type = c('Spectral','Spectral','Spectral','Complexity','Complexity','Complexity','Functional connectivity','Functional connectivity','Functional connectivity','Event-related','Event-related','Event-related','Other','Other','Other'))
ggplot(features, aes(x = reorder(feature,-frequency), y = frequency, fill = type)) +
  geom_bar(stat = 'identity', color = 'black', width = 0.75, size = 0.25) +
  scale_x_discrete(expand = c(0,0.5), name = 'Feature') +
  scale_y_continuous(expand = c(0,0,0.05,0), breaks = seq(0,300,10), name = 'Number of articles') +
  scale_fill_manual(values = brewer.pal(9, 'Pastel1')[c(1,2,5,4,9)],
                    labels = c('Spectral','Event-related','Functional connectivity','Complexity','Other')) +
  theme_bw() +
  theme(text = element_text(size = 10, family = 'sans'),
        axis.title = element_text(size = 12, face = 'bold'),
        legend.position = c(0.88,0.88),
        legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid', linewidth = 0.25),
        legend.title = element_blank())

ggsave('Features.pdf')

#MACHINE LEARNING (Fig. 5e, 5f, & 5g) ====

#total frequency and percentage
machine <- data.frame(machine = unique(srdata$machine_learning[1:942]),
                      frequency = as.vector(table(srdata$machine_learning)),
                      percentage = as.vector(prop.table(table(srdata$machine_learning))*100))

#pie chart
ggplot(machine, aes(x = '', y = percentage, fill = machine)) +
  geom_bar(stat = 'identity', color = 'black') +
  coord_polar('y', start = 0) +
  geom_text(aes(label = paste0(c(round(percentage, digits = 2)), '%')),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_fill_brewer(palette = 'Pastel1') +
  theme_classic() +
  theme(text = element_text(size = 15, family = 'sans'),
        axis.title = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = 'bottom',
        legend.title = element_blank())

ggsave('MachineLearning.pdf')

#bar plot - machine learning & year
dum <- data.frame(table(srdata$year[1:942],srdata$machine_learning[1:942])) #frequency per year
dum <- rbind(dum[1:3, ], data.frame(Var1 = '1984', Var2 = 'Yes', Freq = 0), dum[4:126, ]) #add missing year ('1984')
dum$Var1 <- factor(dum$Var1, levels = unique(dum$Var1)) #reorder year
dum <- dum[complete.cases(dum), ] #remove empty rows

ggplot(dum, aes(x = Var1, y = Freq, fill = Var2)) +
  geom_col(color = 'black', width = 0.75, size = 0.25) +
  scale_x_discrete(expand = c(0,0.5), breaks = seq(1979,2023,2), name = 'Year') +
  scale_y_continuous(expand = c(0,0,0.05,0), breaks = seq(0,800,10), name = 'Number of publications') +
  scale_fill_brewer(palette = 'Pastel1', direction = 1, labels = c('No ML','ML')) +
  theme_bw() +
  theme(text = element_text(size = 10, family = 'sans'),
        axis.title = element_text(size = 12, face = 'bold'),
        legend.position = c(0.07,0.94),
        legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid', linewidth = 0.25),
        legend.title = element_blank())

ggsave('MachineLearning_Year.pdf')

#ratio of machine learning studies to publications per year
ratio <- data.frame(year = srdata$year[1:942], var = srdata$machine_learning[1:942])
ratio <- ratio %>%
  group_by(year) %>%
  summarise(total_studies = n(), var1 = sum(var == 'No'), var2 = sum(var == 'Yes'), 
            ratio_var1 = var1 / total_studies, ratio_var2 = var2 / total_studies)

ratio <- ratio %>%
  pivot_longer(cols = starts_with('var'), names_to = 'var', values_to = 'value') %>%
  mutate(ratio = ifelse(var == 'var1', ratio_var1, ratio_var2)) %>%
  select(year, ratio, var)

ggplot(ratio, aes(x = year, y = ratio, group = var)) +
  geom_line(aes(color = var)) +
  geom_point(aes(color = var)) +
  scale_x_continuous(expand = c(0,0.5), breaks = seq(1979,2023,2), name = 'Publication year') +
  scale_y_continuous(expand = c(0.05,0,0.05,0),breaks = seq(0,1,0.1),name = 'Machine learning ratio') +
  scale_color_brewer(palette = 'Pastel1', labels = c('No ML', 'ML')) +
  theme_bw() +
  theme(text = element_text(size = 10, family = 'sans'),
        axis.title = element_text(size = 12, face = 'bold'),
        legend.position = c(0.9,0.5),
        legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid', linewidth = 0.25),
        legend.title = element_blank())

ggsave('MachineLearning_Ratio.pdf')

#SOURCE RECONSTRUCTION (Fig. 5h, 5i, & 5j) ====

#total frequency and percentage
source <- data.frame(source = unique(srdata$source_reconstruction[1:942]),
                     frequency = as.vector(table(srdata$source_reconstruction)),
                     percentage = as.vector(prop.table(table(srdata$source_reconstruction))*100))

#pie chart
ggplot(source, aes(x = '', y = percentage, fill = source)) +
  geom_bar(stat = 'identity', color = 'black') +
  coord_polar('y', start = 0) +
  geom_text(aes(label = paste0(c(round(percentage, digits = 2)), '%')),
            position = position_stack(vjust = 0.5), size = 3) +
  scale_fill_brewer(palette = 'Pastel1') +
  theme_classic() +
  theme(text = element_text(size = 15, family = 'sans'),
        axis.title = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = 'bottom',
        legend.title = element_blank())

ggsave('SourceReconstruction.pdf')

#bar plot - neuroimaging & source reconstruction
dum <- data.frame(table(srdata$neuroimaging[1:942],srdata$source_reconstruction[1:942]))
ggplot(dum, aes(x = Var1, y = Freq, fill = Var2)) +
  geom_col(color = 'black', width = 0.75, size = 0.25) +
  scale_x_discrete(limits = c('EEG','MEG','M/EEG'), expand = c(0,0.5), name = 'Neuroimaging technique') +
  scale_y_continuous(expand = c(0,0,0.05,0), breaks = seq(0,800,40), name = 'Number of articles') +
  scale_fill_brewer(palette = 'Pastel1', labels = c('No SR', 'SR')) +
  theme_bw() +
  theme(text = element_text(size = 10, family = 'sans'),
        axis.title = element_text(size = 12, face = 'bold'),
        legend.position = c(0.9,0.9),
        legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid', linewidth = 0.25),
        legend.title = element_blank())

ggsave('SourceReconstruction_Year.pdf')

#ratio of source reconstruction studies to year
ratio <- data.frame(year = srdata$year[1:942], var = srdata$source_reconstruction[1:942])
ratio <- ratio %>%
  group_by(year) %>%
  summarise(total_studies = n(), var1 = sum(var == 'No'), var2 = sum(var == 'Yes'), 
            ratio_var1 = var1 / total_studies, ratio_var2 = var2 / total_studies)

ratio <- ratio %>%
  pivot_longer(cols = starts_with('var'), names_to = 'var', values_to = 'value') %>%
  mutate(ratio = ifelse(var == 'var1', ratio_var1, ratio_var2)) %>%
  select(year, ratio, var)

ggplot(ratio, aes(x = year, y = ratio, group = var)) +
  geom_line(aes(color = var)) +
  geom_point(aes(color = var)) +
  scale_x_continuous(expand = c(0,0.5), breaks = seq(1979,2023,2), name = 'Publication year') +
  scale_y_continuous(expand = c(0.05,0,0.05,0),breaks = seq(0,1,0.1),name = 'Source reconstruction ratio') +
  scale_color_brewer(palette = 'Pastel1', labels = c('No SR', 'SR')) +
  theme_bw() +
  theme(text = element_text(size = 10, family = 'sans'),
        axis.title = element_text(size = 12, face = 'bold'),
        legend.position = c(0.9,0.5),
        legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid', linewidth = 0.25),
        legend.title = element_blank())

ggsave('SourceReconstruction_Ratio.pdf')

