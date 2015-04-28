## Libraries ####
library(magrittr)

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

head(dat)

dat$NOC[grep("C", dat$NOC)]

sum(dat[dat$NOC=="C1",][2:25])

NOCkeep <- c("A", "B", paste("C0", 1:7, sep=""), "C1", "D", "F", "G", "H", "I", "J", )

# These categories are fine as simply non-STEM: "E01", "E1", "E2"
# but E02 and EO3 NOC codes do not subdivide neatly into STEM / non-STEM
# Going to have to do a little figuring out
NOC_E
          
             