# Load relevant libraries
library(ggplot2)
library(dplyr)
library(rstatix)   # For Welch's t-test
library(quanteda)
library(gridExtra)

# Clear environment
rm(list=ls())

# Load data
setwd("C:/Users/nehak/OneDrive - University College London/Uni/Dissertation")
data <- read.csv(file = 'Speeches coded.csv', sep = ",", header = TRUE, stringsAsFactors = FALSE)

# Create corpus
corpus <- corpus(data, text_field = "Speech..Translated.")

# Dictionary: cybersecurity themes by ideology
cyber_dict <- dictionary(list(
  # Right-leaning themes
  national_security = c("national security*", "sovereignty", "jihadist", "cyberwarfare", "hybrid threat*", 
                        "foreign interference*", "state actor*", "geopolitical", "military", "defense", 
                        "protection", "infrastructure", "national interest*", 
                        "cyber shield*", "territorial integrity*", "radical", 
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
                      "consumer protection*", "interoperability*", "disinformation", "whistleblower", 
                      "transparency", "oversight"),
  inclusion_equity = c("inclusion", "digital divide*", "education", "training", "access", 
                       "digital literacy*", "inequality", "equity", "underrepresented", 
                       "diversity", "vulnerable groups*", "ethics", 
                       "access", "manipulation", "democracy", "public interest*", 
                       "bias", "ethical", "responsible")))

# Tokenize
toks <- tokens(corpus, remove_punct = TRUE, remove_symbols = TRUE)

# 5. Apply dictionary to count matches
dict_dfm <- dfm(toks) %>%
  dfm_lookup(dictionary = cyber_dict)

# 6. Convert to proportions (/speech length)
total_words <- ntoken(toks)
prop_df    <- convert(dict_dfm, to = "data.frame") %>%
  mutate(total_words = total_words)

# 7. Collapse to threat vs risk proportions
prop_df <- prop_df %>%
  mutate(threat_total = (national_security + crime + threat_lang) / total_words,
         risk_total   = (privacy_rights + tech_regulation + inclusion_equity) / total_words,
         Alignment    = data$Alignment)  # add ideology variable

# 8. t-test Threat logic
t_threat <- t.test(threat_total ~ Alignment, data = prop_df, var.equal = FALSE)
t_risk   <- t.test(risk_total   ~ Alignment, data = prop_df, var.equal = FALSE)

print(t_threat)
print(t_risk)

# 9. means by group
prop_df %>%
  group_by(Alignment) %>%
  summarise(mean_threat = mean(threat_total),
            mean_risk   = mean(risk_total))

# 10. Boxplots
p1 <-ggplot(prop_df, aes(x = Alignment, y = threat_total, fill = Alignment)) +
  geom_boxplot() + labs(title = "")

p2 <- ggplot(prop_df, aes(x = Alignment, y = risk_total, fill = Alignment)) +
  geom_boxplot() + labs(title = "")

p1
p2

# Standard deviations for threat_total by alignment
sd(prop_df$threat_total[prop_df$Alignment == "Left"], na.rm = TRUE)
sd(prop_df$threat_total[prop_df$Alignment == "Right"], na.rm = TRUE)

# Standard deviations for risk_total by alignment
sd(prop_df$risk_total[prop_df$Alignment == "Left"], na.rm = TRUE)
sd(prop_df$risk_total[prop_df$Alignment == "Right"], na.rm = TRUE)

# Means (to confirm your existing results)
mean(prop_df$threat_total[prop_df$Alignment == "Left"], na.rm = TRUE)
mean(prop_df$threat_total[prop_df$Alignment == "Right"], na.rm = TRUE)

mean(prop_df$risk_total[prop_df$Alignment == "Left"], na.rm = TRUE)
mean(prop_df$risk_total[prop_df$Alignment == "Right"], na.rm = TRUE)

# Sample sizes for each alignment
length(prop_df$threat_total[prop_df$Alignment == "Left" & !is.na(prop_df$threat_total)])
length(prop_df$threat_total[prop_df$Alignment == "Right" & !is.na(prop_df$threat_total)])

