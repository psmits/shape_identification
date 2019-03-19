##  peter d smits
##  psmits@uchicago.edu
###############################################################################

# packages
library(pacman)

p_load(here, shapes, geomorph, plyr, caret, tidyverse)

# source files
source(here::here('R', 'helper02_array2df.r'))
source(here::here('R', 'helper04_df2array.r'))
source(here::here('R', 'helper07_support_functions.r'))

# landmarks
n.land <- 26
n.dim <- 2
rawturt <- read.table(here::here('data', 'raw_marmorata.txt'))
land <- read.table(here::here('data', 'marmorata_land.txt'), sep = ' ')
land <- df2array(land, n.land, n.dim)
#centroid <- rawturt[, n.land + 1]  # size of each observation

# meta data
#meta <- read.csv('../data/marmorata_meta2.csv')
meta <- read.csv(here::here('data', 'marm_upbins.csv'), 
                 skip = 1, header = TRUE)
#names(meta) <- c('ind', 'spec', 'lat', 'long', 'year',
#                 'sh1', 'sh2', 'sh3', 'sh4', 'sh5', 'spinks.pre', 'spinks',
#                 'p.sex', 'b.sex', 'sex.pre', 'notes')
names(meta) <- c('ind', 'spec', 'lat', 'long', 'year',
                 'sp10.1', 'sp10.2', 'sp10.3', 'sp14.1', 'sp14.2', 'morph', 
                 'bad1', 'bad2', 'bad3',
                 'p.sex', 'b.sex', 'sex.pre', 'notes')
cl <- c('sp10.1', 'sp10.2', 'sp10.3', 'sp14.1', 'sp14.2', 'morph')
meta[, cl] <- apply(meta[, cl], 2, function(x) as.character(x))
meta[, c('lat', 'long')] <- apply(meta[, c('lat', 'long')], 
                                  2, function(x) as.numeric(as.character(x)))
meta[meta == '?' | meta == '??'] <- NA

# get rid of everything that doesn't have lat-long
#meta$long <- meta$long
land <- land[, , !is.na(meta$lat)]
rawturt<- rawturt[!is.na(meta$lat), ]
meta <- meta[!is.na(meta$lat), ]

# get rid of everything that doesn't have assigns
# clean off the missing important values

imp <- c('lat', 'long', 'sp10.1', 'sp10.2', 'sp10.3', 'sp14.1', 'sp14.2')
#rms <- Reduce('|', alply(meta[, imp], 2, is.na))
#land <- land[, , !rms]
#meta <- meta[!rms, ]
#rawturt <- rawturt[!rms, ]


# clean out the juveniles
jv <- grepl(pattern = '[jJh]', meta$p.sex)
land.adult <- land[, , !jv]
meta.adult <- meta[!jv, ]
rawturt <- rawturt[!jv, ]


# there are three dead columns that give problems with tibble-s
meta.adult <- 
  meta.adult[, -((ncol(meta.adult) - 2):ncol(meta.adult))] %>%
  as_tibble(.) %>%
  mutate(spec = as.character(spec))


# now analyze data
# ken scute data
scute_df <- read_csv(here::here('data', 'clean_meta_marm_complete.csv')) %>%
  dplyr::select(spec, inguinal_scute)


# combine adult with new scute information
meta.adult <- left_join(meta.adult, scute_df, by = 'spec')

# need to drop scute NAs from fit$rotated, which isn't obvious
trm <- is.na(meta.adult$inguinal_scute)

meta.adult <- meta.adult %>% drop_na(inguinal_scute)
land.adult <- land.adult[, , !trm]
rawturt <- rawturt[!trm, ]



# final clean
rms <- is.na(meta.adult$sp10.1) | meta.adult$sp10.1 == ''
land.adult <- land.adult[, , !rms]
meta.adult <- meta.adult[!rms, ]
rawturt <- rawturt[!rms, ]
centroid <- rawturt[, n.land + 1]  # size of each observation

# don't think this is needed anymore
#geo <- cbind(lat = meta.adult$lat, long = meta.adult$long)

fit <- procGPA(land.adult)
#adult.dist <- riem.matrix(land.adult)

centroid <- scale(centroid)
adult <- cbind(data.frame(size = centroid, 
                          inter = centroid * fit$stdscores[, 1],
                          inter2 = centroid * fit$stdscores[, 2],
                          scute = meta.adult$inguinal_scute,
                          fit$stdscores),
               meta.adult)



schemes <- c('sp10.1', 'sp10.2', 'sp10.3', 'sp14.1', 'sp14.2', 'morph')

nspec <- as_tibble(adult) %>%
  dplyr::select(!!!schemes) %>%
  gather(key = key, value = value) %>%
  filter(value != '') %>%
  split(., .$key) %>%
  map(., ~ table(.x$value))

nsp_table <- reshape2::melt(nspec)
names(nsp_table) <- c('class', 'count', 'scheme')
write_csv(nsp_table,
          path = here::here('doc', 'class_counts.csv'))


tsex <- as.character(adult$p.sex)  # lots of regex to get this sexy
fe <- grep(pattern = '[fF]', tsex, perl = TRUE)
ma <- grep(pattern = '[mM]', tsex, perl = TRUE)
na <- grep(pattern = '[nN]', tsex, perl = TRUE)
tsex[fe] <- 'F'
tsex[ma] <- 'M'
tsex[na] <- NA
tsex[tsex == ''] <- NA
adult$new_sex <- tsex


cliped <- adult[, c('spec', 'lat', 'long', 'scute', 
                    'p.sex', 'new_sex',
                    'sp10.1', 'sp10.2', 'sp10.3', 
                    'sp14.1', 'sp14.2', 
                    'morph')]
write.csv(cliped, file = here::here('data', 'clean_meta_marm.csv'), 
          fileEncoding = 'UTF-16LE')

# write landmarks
writeland.tps(A = land.adult,
              file = here::here('data', 'mamorota_clean_land.tps'))

# full data print outs
write_rds(adult, path = here::here('data', 'turtle_plastron_data_clean.rds'))
write_rds(fit,
          path = here::here('data', 'turtle_plastron_gpa.rds'))
