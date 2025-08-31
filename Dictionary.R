# Load libraries
library(readtext) 
library(quanteda) 
library(corpora)
library(ggplot2)
library(ggplotify)
library(car)
library(ggpubr)
library(broom)
require(quanteda)
require(quanteda.textstats)
library(textdata)
library(stargazer)
library(dplyr)
library(tidyverse)
library(modelsummary)
library(boot)
library(date)
library(rstatix)
library(DHARMa)

rm(list=ls())
# Load data
setwd("C:/Users/nehak/OneDrive - University College London/Uni/Dissertation")
data <- read.csv(file = 'Speeches coded.csv', sep = ",", header = TRUE, stringsAsFactors = FALSE)

corpus <-corpus(data$Speech..Translated.)
words1 = ntoken(corpus)


# View columns
str(data)

# metadata
docvars(corpus, "Date") <- data$Date
docvars(corpus, "Speaker") <- data$Speaker
docvars(corpus, "Party") <- data$`Party Affiliation`
docvars(corpus, "Alignment") <- data$Alignment

# Tokenise and clean text
toks <- tokens(corpus,
               remove_punct = TRUE,
               remove_symbols = TRUE,
               remove_numbers = TRUE)

#corpus
txtCorpus <-dfm(toks)

# Dictionary: cybersecurity themes by ideology
cyber_dict <- dictionary(list(
  # Right-leaning themes
  national_security = c("national security*", "sovereignty", "jihadist", "cyberwarfare", "hybrid threat*", 
                        "foreign interference*", "state actor*", "geopolitical", "military", "defense", 
                        "protection", "infrastructure", "national interest*", 
                        "cyber shield*", "territorial integrity*", " radical", 
                        "armed forces*", "autonomy", "intelligence"),
  crime = c("cybercrime", "crime", "criminal", "prosecution", "illegal", "fraud", 
            "law enforcement*", "investigation", "dark web*", "police cooperation*", 
            "cyber fraud*", "child exploitation*", "hacking", "blackmail", "identity theft*", 
            "ransomware", "malware", "extremism", "radicalisation*", "online crime*"),
  threat_lang = c("threat", "attack", "cyberattack", "emergency", "vulnerability", 
                  "resilience", "enemy", "hostile", "warfare", "terrorism", "deterrence", 
                  "security breach*", "penetration", "target", "exploit", 
                  "incident", "early warning*", "breach response*", 
                  "countermeasure", "capability"),
  
  # Left-leaning themes
  privacy_rights = c("privacy", "rights", "freedom", "liberties", "civil liberties*", "data protection*", "elections",
                     "surveillance", "oversight", "GDPR", "anonymity", "encryption", "personal data*", 
                     "tracking", "watchlist", "facial recognition*", "bulk collection*", "transparency", 
                     "data misuse*", "digital dignity*"),
  
  tech_regulation = c("regulation", "tech giants*", "accountability", "responsibility*",
                      "AI governance*", "bias", "fairness", "digital markets*", "misinformation", 
                      "online harms*", "decision making*", "self regulation*", "big tech*", "data monopoly*", 
                      "consumer protection*", "interoperability*”, “disinformation", "whistleblower", 
                      "transparency", "oversight")
  ,
  inclusion_equity = c("inclusion", "digital divide*", "education", "training", "access", 
                       "digital literacy*", "inequality", "equity", "underrepresented", 
                       "diversity", "vulnerable groups*", "ethics", 
                       " access", "manipulation", "democracy", "public interest*", 
                       "bias mitigation*", "ethical design*", "responsible")
))

#Applying dictionary
pot <- dfm_lookup(txtCorpus,cyber_dict, valuetype = "glob")
pop_tokens2 <- as.matrix(pot)
pop_tokens2
datapot = data.frame(pop_tokens2)

# Extract docvars (metadata) from the corpus
metadata <- docvars(corpus)

# Combine dictionary counts with metadata
datapot = cbind(metadata, as.data.frame(pop_tokens2))


write.csv(datapot, "dictionary.csv", row.names = FALSE)
