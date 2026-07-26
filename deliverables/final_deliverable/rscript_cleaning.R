# ============================================
# LIBRARIES 
# ============================================


library(tidyverse)
library(stringdist)


# ============================================
# DATA SOURCE
# ============================================

"
Data Source:
https://openpsychometrics.org/_rawdata/

DASS Overview:
https://www2.psy.unsw.edu.au/dass/
"

# ============================================
# SET DIR & READ IN DATA
# ============================================


df = read.delim("data/data.csv", header = TRUE)


# ============================================
# CLEAN MAJOR COLUMN
# ============================================

"
Create a list of majors and use an algorithm to replace user entries with 
standard major labels.  Then add a column with major catgegories. This adds
two levels of resolution where one can compare specific majors 
(e.g., psychology vs. sociology) vs. major categories (e.g., STEM vs. Arts).
"

# Master major reference list
master_majors = c(
  # Agriculture, Food & Environment
  "agricultural sciences", "animal science", "fermentation science", "food science", 
  "forestry", "horticulture", "wildlife biology",
  
  # Architecture & Construction
  "architecture", "construction management", "landscape architecture", "urban planning",
  
  # Arts, Media & Design
  "acting", "advertising", "animation", "art", "communications", "digital media", 
  "fashion design", "film studies", "fine arts", "graphic design", "interior design", 
  "journalism", "media studies", "music", "public relations", "theater",
  
  # Business, Finance & Real Estate
  "accounting", "business", "business administration", "economics", "finance", 
  "hospitality", "human resource management", "international business", "logistics", 
  "management", "management information systems", "marketing", "real estate", 
  "supply chain management",
  
  # Computer Science, Math & IT
  "actuarial science", "computer science", "cybersecurity", "data science", 
  "information technology", "instructional technology", "mathematics", "robotics", 
  "software engineering", "statistics",
  
  # Education & Teaching
  "early childhood education", "education", "elementary education", "physical education", 
  "secondary education", "special education", "teaching",
  
  # Engineering
  "aerospace engineering", "biomedical engineering", "chemical engineering", 
  "civil engineering", "computer engineering", "electrical engineering", 
  "environmental engineering", "geological engineering", "industrial engineering", 
  "mechanical engineering", "metallurgical engineering", "spacial engineering",
  
  # Health, Medicine & Wellness
  "biochemistry", "dentistry", "dietetics", "exercise science", "health", 
  "health administration", "kinesiology", "medical technology", "medicine", 
  "nursing", "nutrition", "occupational therapy", "pharmacology", "public health", 
  "radiography", "speech pathology",
  
  # Humanities & Liberal Arts
  "classics", "creative writing", "english", "foreign languages", "history", 
  "liberal arts", "linguistics", "literature", "philosophy", "religious studies", 
  "theology", "writing",
  
  # Law & Public Policy
  "criminal justice", "criminology", "human rights", "law", "prelaw",
  
  # Natural Sciences
  "anthropology", "astronomy", "biology", "chemistry", "cognitive science", 
  "environmental science", "genetics", "geography", "geology", "marine biology", 
  "microbiology", "neuroscience", "physics",
  
  # Social Sciences & Services
  "ethnic studies", "gender studies", "global studies", "human development", 
  "international relations", "librarian", "political science", "psychology", 
  "social work", "sociology",
  
  # Trades & Personal Services
  "culinary", "hairdressing", "surveying", "tourism", "other")

# Clean College Major Section
df_clean = df %>% 
  dplyr::mutate(
    # 1. Clean the text completely first and strip credential noise
    clean_text = major %>% 
      stringr::str_to_lower() %>% 
      stringr::str_remove_all("[:punct:]") %>% 
      stringr::str_remove_all("\\b(masters of|diploma|dip)\\b") %>% 
      stringr::str_squish(),
    
    # 2. Apply your specific mappings, rules, and current batch recommendations
    clean_text = dplyr::case_when(
      clean_text == "na"                                    ~ NA_character_,
      
      # Broad Umbrella Groupings (Regex Wildcards)
      stringr::str_detect(clean_text, "^religi|christian|catholic|islam|muslim|hindu|buddh|juda|jewish|mormon|theology|sikh|tao|shinto") ~ "religion",
      stringr::str_detect(clean_text, "lang|languange|^spani|^frenc|^germ|^ital|^chine|^japan|^arab|^mandarin|portug") ~ "foreign languages",
      stringr::str_detect(clean_text, "^accoun")            ~ "accounting",
      stringr::str_detect(clean_text, "comm.*tion|telecom") ~ "communication",
      stringr::str_detect(clean_text, "health.*scien")      ~ "public health", 
      stringr::str_detect(clean_text, "a[rc]*h.*ect")       ~ "architecture",
      stringr::str_detect(clean_text, ".*planning")         ~ "urban planning",
      stringr::str_detect(clean_text, "polic|syariah")      ~ "political science",
      
      # Computer Science & IT
      clean_text %in% c("cs", "it", "networking administration", "science computer") ~ "computer science",
      clean_text == "programming"                           ~ "software engineering",
      
      # Engineering, Trades & Applied Sciences
      clean_text %in% c("chemical technology", "chemical industry") ~ "chemical engineering",
      clean_text %in% c("electronics", "electronica", "instrumentation technition") ~ "electrical engineering",
      clean_text %in% c("engg", "engineering", "neering")   ~ "engineering",
      clean_text == "renewable energies"                    ~ "environmental engineering",
      clean_text == "dip auto engineering science"          ~ "mechanical engineering",
      clean_text == "mechatronics"                          ~ "robotics",
      clean_text == "quantity surveyor"                     ~ "surveying",
      clean_text == "it engineering"                        ~ "computer engineering",
      
      # Health, Medicine & Wellness
      clean_text %in% c("veterinary medicine", "pre med")   ~ "medicine",
      clean_text %in% c("clinical measurement", "medical technology", "medical laboratory science") ~ "medical technology",
      clean_text %in% c("allied health", "health science")   ~ "health science",
      clean_text == "physiotherapy"                         ~ "kinesiology",
      clean_text == "global health"                         ~ "public health",
      
      # Natural Sciences, Food & Math
      clean_text == "zoology"                               ~ "wildlife biology",
      clean_text == "applied maths"                         ~ "mathematics",
      clean_text == "ecology and evolution"                  ~ "biology",
      clean_text == "biotechnology"                         ~ "biochemistry",
      clean_text == "materials science"                     ~ "chemistry",
      clean_text == "biopsych"                              ~ "psychology",
      clean_text == "maths computer"                        ~ "math",
      clean_text == "science and technology studies"         ~ "liberal arts",
      clean_text == "science sosial"                        ~ "sociology",
      clean_text == "food biotechnology"                    ~ "food science",
      
      # Business, Management & Hospitality
      clean_text %in% c("hotel management", "event management", "bakery pastry hospitality") ~ "hospitality",
      clean_text %in% c("mba", "administrative", "commerce", "corporate admin", "entrepreneurship") ~ "business administration",
      clean_text %in% c("hr mgmt", "hr")                    ~ "human resource management",
      clean_text %in% c("sport management")                 ~ "management",
      clean_text == "maths science management"              ~ "management",
      clean_text == "investment management"                 ~ "finance",
      
      # Social Sciences, Law & Services
      clean_text %in% c("social sciences", "work psychology") ~ "psychology",
      clean_text == "human rights"                          ~ "political science",
      clean_text == "justice system dynamics"               ~ "criminal justice",
      clean_text == "human services"                        ~ "social work",
      clean_text %in% c("paralegal", "law and psychology", "medical science and law") ~ "law",
      
      # Arts, Design & Media
      clean_text %in% c("italianhistory of art", "drawing", "art and design", "aa") ~ "art",
      clean_text %in% c("film and video production", "cinema") ~ "film studies",
      clean_text %in% c("design", "web design")             ~ "graphic design",
      clean_text == "visual communication design animation" ~ "communications",
      clean_text == "audiovisual communicator"              ~ "communications",
      clean_text == "drama"                                 ~ "theater",
      clean_text == "pastry abd culinary"                   ~ "culinary",
      clean_text == "multimedia"                            ~ "digital media",
      clean_text == "publishing"                            ~ "journalism",
      
      # Education & Humanities
      clean_text %in% c("modern history and politics", "history and international relations") ~ "history",
      clean_text == "professional writing"                  ~ "writing",
      clean_text == "pedagogics"                            ~ "education",
      clean_text == "math education"                        ~ "secondary education",
      clean_text == "french lit"                            ~ "foreign languages",
      clean_text == "scandinavian studies"                  ~ "liberal arts",
      
      # Standalone Wildcard Sweepers (Keep near the bottom)
      clean_text == "nature conservation"                   ~ "environmental science",
      clean_text == "veterinarian"                          ~ "animal science",
      clean_text == "development science"                   ~ "human development",
      clean_text %in% c("science", "sciences")              ~ "liberal arts",
      clean_text == "odontology"                            ~ "dentistry",
      
      # Drop out completely non-major noise
      clean_text %in% c("development studies", "hotel and tourism management", "social networking") ~ NA_character_,
      
      .default = clean_text 
    ),
    
    # 3. Automatically find the index of the closest matching master major
    match_index = stringdist::amatch(
      clean_text, 
      master_majors, 
      method = "jw",      
      maxDist = 0.25      
    ),
    
    # 4. Pull the official name from the master list using that index
    standardized_major = master_majors[match_index],
    
    # 5. Conditional Sweeper: If match failed BUT the clean text is not NA, label as "other"
    standardized_major = dplyr::if_else(is.na(match_index) & is.na(clean_text), "other", standardized_major),
    
    # 6. Map individual majors into broad macro categories
    major_category = dplyr::case_when(
      # Business
      standardized_major %in% c("accounting", "business", "business administration", "economics", 
                                "finance", "hospitality", "human resource management", 
                                "international business", "logistics", "management", "marketing", 
                                "real estate", "supply chain management", "construction management", 
                                "investment management") 
      ~ "Business & Management",
      
      # STEM
      standardized_major %in% c("computer science", "data science", "information technology", "statistics", 
                                "mathematics", "actuarial science", "cybersecurity", "software engineering", 
                                "robotics", "math", "computer engineering", "aerospace engineering", 
                                "biomedical engineering", "chemical engineering", "civil engineering", 
                                "electrical engineering", "environmental engineering", "geological engineering", 
                                "industrial engineering", "mechanical engineering", "metallurgical engineering", 
                                "spacial engineering", "engineering", "agricultural sciences", "animal science", 
                                "fermentation science", "food science", "forestry", "horticulture", "wildlife biology", 
                                "astronomy", "biology", "chemistry", "cognitive science", "environmental science", 
                                "genetics", "geology", "marine biology", "microbiology", "neuroscience", "physics", 
                                "biochemistry") 
      ~ "STEM & Natural Sciences",
      
      # Health
      standardized_major %in% c("nursing", "public health", "kinesiology", "exercise science", "health administration", 
                                "nutrition", "dietetics", "speech pathology", "dentistry", "health", "medical technology", 
                                "medicine", "occupational therapy", "pharmacology", "radiography", "health science") 
      ~ "Health & Medicine",
      
      # Social Sciences
      standardized_major %in% c("psychology", "sociology", "political science", "anthropology", "criminology", "criminal justice", 
                                "international relations", "human development", "urban planning", "human rights", "law", "prelaw", 
                                "ethnic studies", "gender studies", "global studies", "social work", "geography") 
      ~ "Social Sciences & Policy",
      
      # Humanities
      standardized_major %in% c("english", "history", "philosophy", "religious studies", "linguistics", "creative writing", 
                                "liberal arts", "classics", "literature", "theology", "writing", "foreign languages", "librarian") 
      ~ "Humanities & Liberal Arts",
      
      # Communication
      standardized_major %in% c("communications", "communication", "journalism", "public relations", "media studies", "digital media", 
                                "advertising") 
      ~ "Communications & Media",
      
      # Arts
      standardized_major %in% c("fine arts", "graphic design", "architecture", "interior design", "music", "theater", "film studies", 
                                "animation", "fashion design", "acting", "landscape architecture", "drama") 
      ~ "Arts & Design",
      
      # Education
      standardized_major %in% c("elementary education", "secondary education", "special education", "early childhood education", 
                                "physical education", "instructional technology", "education", "teaching", "pedagogics", 
                                "math education") 
      ~ "Education",
      
      # Trades
      standardized_major %in% c("culinary", "hairdressing", "surveying", "tourism", "quantity surveyor") 
      ~ "Trades & Services",
      
      # Other
      standardized_major == "other" ~ "Other / Unclassified",
      
      .default = NA_character_
  )) %>%
  select(-clean_text, -match_index) %>%
  rename(major_std = standardized_major,
         major_cat = major_category)

# sort(unique(df_clean$major_std))

# sum(df_clean$major_std == "other", na.rm = TRUE)

df_clean %>% 
  dplyr::count(major_cat, sort = TRUE)


# ============================================
# ADJUST DASSS SCALES BY SUBTRACTING 1
# ============================================

"
Per Lovibond & Lovibond (1995), Likert-scale should be 0-3, not 1-4
"

df_clean = df_clean %>%
    mutate(across(matches("^Q.*A$", ignore.case = FALSE), ~ .x - 1))


# ============================================
# CALCULATE DASS ITEMS
# ============================================

"
Scoring for the 42 item scale: https://www2.psy.unsw.edu.au/dass/over.htm

Scale and subscale: Lovibond and Lovibond (1995, p. 339).  Question order same as p. 339.

Long vs. short measure split: Antony et al. (1998), 21-item (p. 179), and 42-item (p. 178)
"


df_clean = df_clean %>%
  # Order matches Antony et al. (1998) DASS Stress Scale, p. 178
  mutate(stress21 = rowSums(across(matches("^Q(14|18|12|11|8|1|6)A$")), na.rm = TRUE),
         depres21 = rowSums(across(matches("^Q(21|10|3|16|17|13|5)A$")), na.rm = TRUE),
         anxiet21 = rowSums(across(matches("^Q(19|4|7|15|20|9|2)A$")), na.rm = TRUE),
         stress42 = rowSums(across(matches("^Q(1|11|27|39|18|35|6|14|8|29|32|12|22|33)A$")), na.rm = TRUE),
         depres42 = rowSums(across(matches("^Q(37|38|10|34|21|31|17|16|3|26|24|13|42|5)A$")), na.rm = TRUE),
         anxiet42 = rowSums(across(matches("^Q(41|7|15|4|25|28|23|20|36|40|2|9|19|30)A$")), na.rm = TRUE)
         ) %>%
  mutate(
    # --- DASS-21 Depression Subscales ---
    sub21_dysphoria       = rowSums(across(matches("^Q(13)A$")), na.rm = TRUE),
    sub21_hopelessness    = rowSums(across(matches("^Q(10)A$")), na.rm = TRUE),
    sub21_deval_life      = rowSums(across(matches("^Q(21)A$")), na.rm = TRUE),
    sub21_self_deprec     = rowSums(across(matches("^Q(17)A$")), na.rm = TRUE),
    sub21_lack_interest   = rowSums(across(matches("^Q(16)A$")), na.rm = TRUE),
    sub21_anhedonia       = rowSums(across(matches("^Q(3)A$")), na.rm = TRUE),
    sub21_inertia         = rowSums(across(matches("^Q(5)A$")), na.rm = TRUE),
    
    # --- DASS-21 Anxiety Subscales ---
    sub21_auto_arousal    = rowSums(across(matches("^Q(19|4|2)A$")), na.rm = TRUE),
    sub21_skeletal_muscle = rowSums(across(matches("^Q(7)A$")), na.rm = TRUE),
    sub21_sit_anxiety     = rowSums(across(matches("^Q(9)A$")), na.rm = TRUE),
    sub21_anxious_affect  = rowSums(across(matches("^Q(15|20)A$")), na.rm = TRUE),
    
    # --- DASS-21 Stress Subscales ---
    sub21_diff_relaxing   = rowSums(across(matches("^Q(8)A$")), na.rm = TRUE),
    sub21_nervous_arousal = rowSums(across(matches("^Q(12)A$")), na.rm = TRUE),
    sub21_easily_upset    = rowSums(across(matches("^Q(11|1)A$")), na.rm = TRUE),
    sub21_irritable       = rowSums(across(matches("^Q(18|6)A$")), na.rm = TRUE),
    sub21_impatient       = rowSums(across(matches("^Q(14)A$")), na.rm = TRUE),
    
    # --- DASS-42 Depression Subscales ---
    sub42_dysphoria       = rowSums(across(matches("^Q(26|13)A$")), na.rm = TRUE),
    sub42_hopelessness    = rowSums(across(matches("^Q(37|10)A$")), na.rm = TRUE),
    sub42_deval_life      = rowSums(across(matches("^Q(38|21)A$")), na.rm = TRUE),
    sub42_self_deprec     = rowSums(across(matches("^Q(17|34)A$")), na.rm = TRUE),
    sub42_lack_interest   = rowSums(across(matches("^Q(16|31)A$")), na.rm = TRUE),
    sub42_anhedonia       = rowSums(across(matches("^Q(3|24)A$")), na.rm = TRUE),
    sub42_inertia         = rowSums(across(matches("^Q(5|42)A$")), na.rm = TRUE),
    
    # --- DASS-42 Anxiety Subscales ---
    sub42_auto_arousal    = rowSums(across(matches("^Q(25|19|2|4|23)A$")), na.rm = TRUE),
    sub42_skeletal_muscle = rowSums(across(matches("^Q(7|41)A$")), na.rm = TRUE),
    sub42_sit_anxiety     = rowSums(across(matches("^Q(40|9|30)A$")), na.rm = TRUE),
    sub42_anxious_affect  = rowSums(across(matches("^Q(28|36|20|15)A$")), na.rm = TRUE),
    
    # --- DASS-42 Stress Subscales ---
    sub42_diff_relaxing   = rowSums(across(matches("^Q(22|29|8)A$")), na.rm = TRUE),
    sub42_nervous_arousal = rowSums(across(matches("^Q(12|33)A$")), na.rm = TRUE),
    sub42_easily_upset    = rowSums(across(matches("^Q(11|1|39)A$")), na.rm = TRUE),
    sub42_irritable       = rowSums(across(matches("^Q(6|27|18)A$")), na.rm = TRUE),
    sub42_impatient       = rowSums(across(matches("^Q(35|14|32)A$")), na.rm = TRUE)
  )


# ============================================
# DOUBLE DASS-21 VALUES
# ============================================


"
Per Antony et al. (1998, p. 177), DASS-21 scores to be double to achieve parity with
DASS-42 measure
"

# Exlcude cols with Q21 to avoid quadrupling the values
df_clean = df_clean %>% mutate(across(!matches("^Q21") & contains("21"), ~ .x * 2))

# Confirm the doubling worked correctly

# df1 = df_new %>% select(contains("21"))
# df2 = df_clean %>% select(contains("21"))
# 
# # Find column names present in both data frames
# common_cols <- intersect(names(df1), names(df2))
# 
# # Create a new data frame with the subtracted values
# df_diff = df1[, common_cols] - df2[, common_cols]


# ============================================
# CALC THE TIME FOR BOTH 21 AND 42
# ============================================

"
Basically the same code as above with col name changes and suffix (e.g., A$ is now E$)
"

df_clean = df_clean %>%
  mutate(time_stress21 = rowSums(across(matches("^Q(14|18|12|11|8|1|6)E$")), na.rm = TRUE),
         time_depres21 = rowSums(across(matches("^Q(21|10|3|16|17|13|5)E$")), na.rm = TRUE),
         time_anxiet21 = rowSums(across(matches("^Q(19|4|7|15|20|9|2)E$")), na.rm = TRUE),
         time_stress42 = rowSums(across(matches("^Q(1|11|27|39|18|35|6|14|8|29|32|12|22|33)E$")), na.rm = TRUE),
         time_depres42 = rowSums(across(matches("^Q(37|38|10|34|21|31|17|16|3|26|24|13|42|5)E$")), na.rm = TRUE),
         time_anxiet42 = rowSums(across(matches("^Q(41|7|15|4|25|28|23|20|36|40|2|9|19|30)E$")), na.rm = TRUE))


# ============================================
# CALC PERSONLITY ITEMS
# ============================================


# Scoring per Gosling et al. (2003, p. 25)


# Reverse scores

reverse_cols = c("TIPI2", "TIPI4", "TIPI6", "TIPI8", "TIPI10")

for (col in reverse_cols) {
  df_clean[[col]] = 8 - df_clean[[col]]
}


# Add cols for mean score and label as personality construct

df_clean = df_clean %>%
  mutate(extraversion      = rowMeans(across(matches("^TIPI(1|6)")), na.rm = TRUE),
         agreeableness     = rowMeans(across(matches("^TIPI(2|7)")), na.rm = TRUE),
         conscientiousness = rowMeans(across(matches("^TIPI(3|8)")), na.rm = TRUE),
         stability         = rowMeans(across(matches("^TIPI(4|9)")), na.rm = TRUE),
         openness          = rowMeans(across(matches("^TIPI(5|10)")), na.rm = TRUE))


# ============================================
# VALIDITY CHECK
# ============================================

"
Per codebook, VCL 6, 9, 12 contain fake words which can be used as a check.
0 = I do not know the definition, 1 = I know the definition.
To ensure mistakes are not captured, two or more fake words must have been
checked as known.

2 or more checked  = 0 (fail)
1 or fewer checked = 1 (pass)
"

df_clean = df_clean %>%
  mutate(valid = if_else((VCL6 + VCL9 + VCL12) == 3, 0, 1))

df_clean %>% 
  dplyr::count(valid, sort = TRUE)


# ============================================
# ELIM Q##AIE TO REDUCE DF SIZE
# ============================================


df_clean = df_clean %>% 
  select(-matches("^Q.*[AIE]$"),
         -matches("^TIPI"),
         -matches("^VCL"),
         -major,
         -introelapse,
         -testelapse,
         -surveyelapse)


# ============================================
# CREATE A CATEGORICAL DEMO DF
# ============================================


df_cat = df_clean %>%
  mutate(
    education = case_when(
      education == 1 ~ "Less than high school",
      education == 2 ~ "High school",
      education == 3 ~ "University degree",
      education == 4 ~ "Graduate degree",
      TRUE ~ NA_character_
    ),
    
    urban = case_when(
      urban == 1 ~ "Rural (country side)",
      urban == 2 ~ "Suburban",
      urban == 3 ~ "Urban (town, city)",
      TRUE ~ NA_character_
    ),
    
    gender = case_when(
      gender == 1 ~ "Male",
      gender == 2 ~ "Female",
      gender == 3 ~ "Other",
      TRUE ~ NA_character_
    ),
    
    engnat = case_when(
      engnat == 1 ~ "Yes",
      engnat == 2 ~ "No",
      TRUE ~ NA_character_
    ),
    
    hand = case_when(
      hand == 1 ~ "Right",
      hand == 2 ~ "Left",
      hand == 3 ~ "Both",
      TRUE ~ NA_character_
    ),
    
    religion = case_when(
      religion == 1  ~ "Agnostic",
      religion == 2  ~ "Atheist",
      religion == 3  ~ "Buddhist",
      religion == 4  ~ "Christian (Catholic)",
      religion == 5  ~ "Christian (Mormon)",
      religion == 6  ~ "Christian (Protestant)",
      religion == 7  ~ "Christian (Other)",
      religion == 8  ~ "Hindu",
      religion == 9  ~ "Jewish",
      religion == 10 ~ "Muslim",
      religion == 11 ~ "Sikh",
      religion == 12 ~ "Other",
      TRUE ~ NA_character_
    ),
    
    orientation = case_when(
      orientation == 1 ~ "Heterosexual",
      orientation == 2 ~ "Bisexual",
      orientation == 3 ~ "Homosexual",
      orientation == 4 ~ "Asexual",
      orientation == 5 ~ "Other",
      TRUE ~ NA_character_
    ),
    
    race = case_when(
      race == 10 ~ "Asian",
      race == 20 ~ "Arab",
      race == 30 ~ "Black",
      race == 40 ~ "Indigenous Australian",
      race == 50 ~ "Native American",
      race == 60 ~ "White",
      race == 70 ~ "Other",
      TRUE ~ NA_character_
    ),
    
    voted = case_when(
      voted == 1 ~ "Yes",
      voted == 2 ~ "No",
      TRUE ~ NA_character_
    ),
    
    married = case_when(
      married == 1 ~ "Never married",
      married == 2 ~ "Currently married",
      married == 3 ~ "Previously married",
      TRUE ~ NA_character_
    ))


# ============================================
# SPLIT DATA FRAMES FOR EXPORT
# ============================================

"
Created one DF for 21 and 42 item DASS scales, as well as numeric and categorical.  The
idea here is to compare the 21 to the 42 item scale.  Maybe it will be useful to have
the numeric and categorical CSV files for different analyses.
"

write.csv(df_clean %>% select(-matches("21")), "dass42_numeric.csv")
write.csv(df_cat %>% select(-matches("21")),   "dass42_categorical.csv")
write.csv(df_clean %>% select(-matches("42")), "dass21_numeric.csv")
write.csv(df_cat %>% select(-matches("42")),   "dass21_categorical.csv")

dim(df)
dim(df_clean)
