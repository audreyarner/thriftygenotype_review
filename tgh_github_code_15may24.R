
### Thrifty genotype hypothesis publications descriptives. 
## MMW Dec. 6, 2023 (mod by LB) 


# Read data 
tgh <- as.data.frame(readxl::read_xlsx("~/Desktop/vandy/lea lab/thrifty genotype/citations-2023-12-05.XLSX"))
tgh <- tgh[,!colnames(tgh)=="PB"]
colnames(tgh) <- c("type","authors","title","publish_year","abstract","SP","EP","journal","volume","issue","doi")



# Cummulative citiations over time
tmp <- tgh %>% 
  filter(!publish_year=="NA") %>%
  group_by(publish_year) %>% 
  summarise(n=n()) %>% 
  mutate(publish_year=as.numeric(publish_year))

tmp$cumulative_instances <- cumsum(tmp$n)

tmp %>% ggplot(aes(x = publish_year, y = cumulative_instances)) +
  geom_point(size=3, color="#004f75") + 
  geom_line(size=1,color= "#004f75") + 
  scale_x_continuous(breaks = seq(1965, 2020, by = 5), labels = seq(1965, 2020, by = 5)) + 
  scale_y_continuous(breaks = seq(0, 3000, by = 500), labels = seq(0, 3000, by = 500)) + 
  theme_classic(base_size = 30) + 
  theme(axis.text.x = element_text(angle=45, hjust = 1)) +
  labs(x = "Year", y = "Cumulative Citations")


# Number studies by journal - colored by field  
tmp <- tgh %>% 
  filter(type=="JOUR") %>% 
  mutate(journal = gsub("\\s*\\([^\\)]+\\)","",journal), 
         journal = gsub(":.*","",journal),
         journal = stringr::str_to_title(journal),
         journal = gsub("United States Of America", "USA",journal),
         journal = gsub("Of","of",journal),
         journal = gsub(" The "," the ",journal),
         journal = gsub("Plos One", "PLOS ONE", journal)) %>% 
  group_by(journal) %>% 
  summarise(n = n()) %>% 
  filter(!n < 8) 
#only used top 20 for ploting  
#assigned field manually by journal description (see methods) 

tmp %>% 
  ggplot(aes(x = n, y = reorder(journal,n), fill=Field)) + 
  scale_y_discrete() + 
  scale_x_continuous(limits = c(0,45)) + 
  geom_col(color = "black") + 
  theme_classic(base_size = 50) + 
  geom_text(aes(label = n), hjust = -1, size=15) + 
  labs(x = "Count", y = "Journal Title") + 
  scale_fill_manual(values = c("#004f75", "#278100", "#e8c900", "#DE7900"))

# Top words in abstracts 
library(stopwords)
library(stringr)
tmp <- data.frame(do.call(rbind, lapply(1:nrow(tgh), function(x) { 
  df <- as.data.frame(str_split(string = gsub("\\:","", 
                                              gsub("\\/","",
                                                   gsub("\\)","",
                                                        gsub("\\(","",
                                                             gsub("(C)", "", 
                                                                  gsub("-", "", 
                                                                       gsub("\\;", "", 
                                                                            gsub(",", "", 
                                                                                 gsub("\\.", "", 
                                                                                      gsub("\\...","",
                                                                                           tgh$abstract[x])))))))))), 
                                pattern = " "))
  colnames(df) <- "word"
  df$word <- tolower(df$word)
  return(df)
})) %>% 
  count(word, sort = TRUE) %>% 
  mutate(word == tolower(word)) %>% 
  filter(!word %in% stopwords("en")) %>% 
  filter(!word %in% c("can", "may", "however", "found","also","whether",
                      "suggest","show","na","=","2","de")) %>% filter(n > 400))

#condensed words by similarity manually 


tmp %>% ggplot(aes(x = n, y = reorder(word, n))) + 
  geom_col(color = "black", fill = "grey70") +
  scale_x_continuous(limits=c(0,2200)) + 
  labs(x = "Count \n", y = "\n Word ") +
  geom_text(aes(label = n), hjust = -0.25, size=15) + 
  theme_classic(base_size = 50)


