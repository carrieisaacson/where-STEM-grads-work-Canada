## Libraries ####
library(magrittr)
library(dplyr)
## Read Data ####

# Read first two rows - combine into single header
header <- read.csv("../Major Field STEM NOC.csv",
                header = F, 
                nrows = 2,
                stringsAsFactors = F)

header <- apply(header, 1:2, FUN = function(x) gsub("  STEM fields of study", "STEM", x))
header <- apply(header, 1:2, FUN = function(x) gsub("  Other fields of study \\(Non-STEM\\)", "nonSTEM", x))
header <- apply(header, 1:2, FUN = function(x) gsub("  ", "", x)) # Omit muliple spaces
header <- apply(header, 1:2, FUN = function(x) gsub(",", "", x))  # Omit commas
header <- apply(header, 2, function(x) paste(x, collapse="_"))
header <- gsub(" ", "_", header)
header <- gsub("(^_)|(_$)", "", header)

# Read data table
dat <- read.csv("../Major Field STEM NOC.csv",
                header = F, 
                skip = 2,
                stringsAsFactors = F)
names(dat) <- header

## NOC codes ####

# Strip all them extra spaces
dat$Occupation <- gsub("\\s{2}", "", dat$Occupation)

# Split NOC code and NOC description
# TODO: Some are combined (e.g. J226 Painters and coaters, industrial/J227 Plating, 
# metal spraying and related operators) these aren't handled yet
sub(" ", "@", dat$Occupation) %>% strsplit(., "@") %>% do.call(rbind,.) %>% .[,1] -> dat$NOC
sub(" ", "@", dat$Occupation) %>% strsplit(., "@") %>% do.call(rbind,.) %>% .[,2] -> dat$NOCdesc

# These NOC codes are more or less complete categories "non-STEM"
NOC_nonSTEM <- c("A", 
              "B", 
              "D",
              "E01", "E1", "E2",
              "F", 
              "G", 
              "H", 
              "I", 
              "J")
dat_nonSTEM <- subset(dat, dat$NOC %in% NOC_nonSTEM)

# Category E02 - E021 (Psychologists) = 
#       E022:E025 Social Workers, Counsellors, Clergy and Probation Officers == non-STEM NOC
dat_nonSTEM_E02 <- subset(dat, dat$NOC %in% paste("E02", 2:5, sep=""))
dat_nonSTEM_E02 <- data.frame( Occupation = "E02 Social workers, counsellors, clergy and probation officers (less Psychologists)",
                               t(dat_nonSTEM_E02 %>% select(Education_STEM:Other_fields_of_study_nonSTEM) %>% colSums),
                               NOC = "E02 (omit E021)", 
                               NOCdesc = "Social workers, counsellors, clergy and probation officers")

# Category E03 - Policy and Program Officers, Researchers and Consultants
# E033, E035, E036, E037 and E039 are not STEM NOC
dat_nonSTEM_E03 <- subset(dat, dat$NOC %in% paste("E03", c(3, 5, 6, 7, 9), sep=""))
dat_nonSTEM_E03 <- data.frame( Occupation = "E03 non-STEM Policy and program officers, researchers and consultants",
                               t(dat_nonSTEM_E03 %>% select(Education_STEM:Other_fields_of_study_nonSTEM) %>% colSums),
                               NOC = "E033/5/6/7/9", 
                               NOCdesc = "Policy and program officers, researchers and consultants (non-STEM)")

# Combine all non-STEM NOC rows
dat_nonSTEM <- merge(dat_nonSTEM, dat_nonSTEM_E02, all.x = T, all.y = T)
dat_nonSTEM <- merge(dat_nonSTEM, dat_nonSTEM_E03, all.x = T, all.y = T)
dat_nonSTEM$stem <- F

# These NOC codes are more or less complete categories "STEM"
# NOC C and E021 Pyschologists == STEM NOC
NOC_STEM <- c(paste("C0", 1:7, sep=""), "C1", "E021")
dat_STEM <- subset(dat, dat$NOC %in% NOC_STEM)

# Category E03 - Policy and Program Officers, Researchers and Consultants
# E031, E032, E034, and E038 are STEM NOC

dat_STEM_E03 <- subset(dat, dat$NOC %in% paste("E03", c(1, 2, 4, 8), sep=""))
dat_STEM_E03 <- data.frame( Occupation = "E03 STEM Policy and program officers, researchers and consultants",
                               t(dat_STEM_E03 %>% select(Education_STEM:Other_fields_of_study_nonSTEM) %>% colSums),
                               NOC = "E031/2/4/8", 
                               NOCdesc = "Policy and program officers, researchers and consultants (STEM)")

# Combine all STEM NOC rows
dat_STEM <- merge(dat_STEM, dat_STEM_E03, all.x = T, all.y = T)
dat_STEM$stem <- T

dat <- merge(dat_nonSTEM, dat_STEM, all.x = T, all.y = T)

write.csv(dat, "../Table - Major Field STEM vs NOC STEM.csv")
