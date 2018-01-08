#'######################################### Status ################################################
#'# Version of July 30 of 2008 -> 2018
#'# Works with inputs for Marxan v2.0.2 and Inedit reader
#'# Tested with R v2.7.1
#'# Any comment or improvement please contact Paulo Cardoso pecardoso@netcabo.pt
#'# TRANSMAP:HABITATS AND CHARISMATIC SPECIES

#'###################################### Requirements #############################################
#'# For GIS outputs, it recquires Lattice, Maptools and associated libraries.
#'# Input.dat must be produced with Inedit or be formated accordingly. CHECK specifically for Input and Output folders!
#'# R will call marxan from Input directory
#'# Create a Folders for each scenario and subfolders arranged as follows:
#'# /gis, /input, /output, /tables and /vectorial. This is mandatory.
#'# All lines with setwd() clause must be renamed accondingly to work any other stored scenario

#'################################# Pre processing fase ############################################
#'# Bound.dat created with Arcview 3.2 "Make Marxan Bound.dat" extension.
#'# For Arcview 3.x Tabulate areas, it is MANDATORY to use Conservation Features NAMES)
#'# Export a txt file table of each tabulate done and  name each one as [tabulate...]
#'# Create a txt file table with atributes of ALL Conservation Features to be considered. Name it as polyhabitat.txt
#'# Create, when appropriate, a txt file with PU's to be locked. Name it lock_pu.txt


#'################################# Number of PU's ################################################
bp <-
  read.table("bound.dat",
             sep = ",",
             dec = ".",
             header = T)
max(bp) ## Obtaining number of PU's from pre existing Bound file and confirmation
pus = data.frame(seq(1:max(bp[, 1])))## R internal Number of PU's obtained from Bound.dat file
names(pus) <- "pu" ## names of PU's columns

#'################## Manipulate abundance data produced by Arcview3.2 ##############################
#'#Combining tables from Tabulate areas with overlayed Cfeatures
setwd(paste(wd, "/tables/", sep = "")) ## Set working directory
tabulate.file <-
  dir(pattern = "tab") ## List of tabulate files created with Spatial Analyst
tabs <-
  read.table(
    tabulate.file[1],
    header = T,
    dec = ".",
    sep = ",",
    quote = "\""
  ) ## Tabulate of habitats considering NO Empty PU's
ifelse(
  nrow(tabs) == nrow(pus),
  tabs <-
    tabs,
  tabs <-
    merge(
      pus,
      tabs,
      by.x = "pu",
      by.y = names(tabs[1]),
      all.x = T
    )
) ## adjust tabulate when empty PU's exists

#'# Extending tabs when more Cfeatures are to be considered (other than habitats)
ifelse(length(tabulate.file) == 1, tabs,
       (for (i in 2:length(tabulate.file))
       {
         ti <- read.table(tabulate.file[i],
                          header = T,
                          dec = ".",
                          sep = ",")
         ifelse(nrow(ti) == nrow(pus),
                ti,
                ti <-
                  merge(
                    pus,
                    ti,
                    by.x = "pu",
                    by.y = paste(names(ti[1])),
                    all.x = T
                  )) ## adjust tabulate when empty PU's exists
         tabs[, (ncol(tabs) + 1):(ncol(tabs) + (ncol(ti) - 1))] <-
           ti[2:ncol(ti)]
       }))

tabs[, 2:ncol(tabs)][is.na(tabs[, 2:ncol(tabs)])] <- 0
#names(tabs)[2:ncol(tabs)]<-paste("amount",c(1:(ncol(tabs)-1)), sep = "") ## Names that will be associated to each column, allowing "merging"

names(tabs)[1] <- "pu" ## name for the 1st Column
names(tabs)[2:ncol(tabs)] <-
  gsub(".", " ", c(names(tabs)[2:ncol(tabs)]), fixed = T)
tabs[2:ncol(tabs)] <- tabs[2:ncol(tabs)] / 10000 ## Area in hectars
tcol <- ncol(tabs) ## A Maximum of nine CFeatures in the North Area

setwd(paste(wd, "/tables/", sep = ""))## Set working directory
write.table(
  tabs,
  "abundances_by_ha_all.txt",
  sep = "\t",
  dec = ".",
  row.names = F
) ## export table for the specified folder in setwd()

#'######################################## Targets #################################################
#'# Habitats AND Charismatic Species
#'# Making Target.dat
setwd(paste(wd, "/tables/", sep = "")) ## Set working directory

polyh <- read.table("polyhabitats.txt",
                    sep = ",",
                    dec = ".",
                    header = T)
#polyh<-polyh[order(polyh$Value),]
target <-
  data.frame(polyh$Value[polyh$Habitats %in% names(tabs[2:ncol(tabs)])],
             as.character(polyh$Habitats[polyh$Habitats %in% names(tabs[2:ncol(tabs)])]))
#target[,1]<-as.numeric(target[,1])
target$target <- 1
target$spf <- 3
target$type <- 0
names(target) <- c("id", "name", "target", "spf", "type")
pritarget <-
  ## Setting Priority targets by it's names
  target$id[target$name %in% c("Coral reef", "Seagrass and Algal beds", "Mangroves")] 
target

setwd(paste(wd, "/input/", sep = "")) ## Set working directory
write.table(
  target,
  file = "target.dat",
  row.names = F,
  quote = F,
  sep = ",",
  dec = "."
) ## Write target.dat file
#'##################################################################################################

#'########### Creating abundancebypu as required by Marxan v2.0.2 from tabs data.frame #############
tabs.v <- tabs
names(tabs.v)[2:ncol(tabs)] <- paste("v", target$id, sep = "")
abx <-
  data.frame(reshape(
    tabs.v,
    direction = "long",
    varying = 2:ncol(tabs.v),
    sep = ""
  )) ## change "matrix" structure that allows create abundance.dat
abx <- abx[-1]
abx.m2 <- abx

abx.m2 <-
  data.frame(abx.m2$time, abx.m2$id, abx.m2$v) ## abundance by pu in Marxan v2.0.2 format
names(abx.m2) <-
  c("species", "pu", "amount") ## names of each column
#abx.m2[,1]<-abs(abx.m2[,1]) ## may or may not be used - round values of abundance by PU
abx202 <-
  abx.m2[order(abx.m2$pu),] ## R internal file for abundance by pu in Marxan v2.0.2 format

setwd(paste(wd, "/input/", sep = "")) # Set working directory
write.table(
  abx202,
  file = "abundancebypu.dat",
  row.names = F,
  quote = F,
  sep = "\t",
  dec = "."
) ## export to a speficied folder the abundance.dat file formated for Marxan v2.0.2

#'########################### Total available Conservation Features ################################
agg.abx202 <-
  aggregate(abx202$amount,
            by = list(id = abx202$species),
            FUN = "sum") ## Summary of total available area of each CF
agg.abx202$species <-
  target$name[agg.abx202$id] ## Summary of available area of each CF with ID and names obtainied from target.dat
agg.abx202$id <- target$id
setwd(paste(wd, "/tables/", sep = "")) # Set working directory
write.table(
  agg.abx202,
  file = "Area of all Cf available.txt",
  row.names = F,
  quote = F,
  sep = "\t",
  dec = "."
) ## export to a speficied folder the Summary table
abundbypu <-
  abx202 ## R internal abundance file formated for Marxan v2.0.2

#'##################################################################################################
#'################################ MARXAN Parameters ###############################################
#'##################################################################################################

#'#################### Parameters to be changed in input.dat file ##################################

#'################################ BOUND File ######################################################
setwd(paste(wd, "/input/", sep = ""))
boundsq <-
  read.table("bound.dat",
             header = T,
             sep = ",",
             dec = ".") ## internal R file with Boundary layer (bound.dat)

#'#################################### UNIT File ###################################################
#'# Read txt file with locked PU's
#'# Analise hist and statistical summary of Habitat areas

uselock <- "n" ## Change this accondingly ("y" or "n")
lock <-
  tabs$pu[tabs$Mangroves >= 100] ##retrieve PU's with contain the specified interval of Habitat'areas
length(lock)

#'# Create unit with id and cost and status
unit <-
  data.frame(seq(1:nrow(pus)), rep(1, nrow(pus)), rep(1, nrow(pus))) ## sequence of PU's from 1 to
names(unit) <-
  c("id", "cost", "status") ## names to be associated to unit columns, as required by Marxan v2.0.2
ifelse(uselock == "y", unit$status[unit$id %in% lock] <-
         2, unit) ## Will match the unit PU status with lock PUs selected and set status = 2
setwd(paste(wd, "/input/", sep = "")) ## Set working directory
write.table(
  unit,
  file = "unit.dat",
  sep = "\t",
  dec = ".",
  row.names = F,
  quote = F
) ## export unit file to the speficied folder to be used by Marxan v2.0.2

#'#################################### IMPUT file ##################################################
setwd(paste(wd, "/input/", sep = "")) # Set working directory
## FIRST THING TO DO IS SET INPUT AND OUTPUT FOLDERS WITHIN WINEDIT !!
input <-
  readLines("input.dat") ## Read and obtain Input.dat parameters
iter = c("100000000") ## Number of iterations to be used (must be between " " )
costs <- c("1") ## Costs of each PU (must be between " " )
perct <-
  c(0.3) ## Vector with priority target's percentages to iterate
perci = 0.1 ## Target % to be associated to Non-priority Targets
sptt <- c("4") ## Vector of spf to be used (must be between " ")
blm <-
  c("1") ## Vector of Blm values to be used (must be between " " )
nreps = c(100) ## Number of reps to be used

#'######################################## RUNNING #################################################
#'######################################## MARXAN ##################################################

setwd(paste(wd, "/input/", sep = "")) ## Set working directory
for (z in 1:length(iter))
  ## R iterations with iterations to be used by Marxan
{
  input[17] <-
    paste("NUMITNS", as.character(iter[z]), sep = " ") ## write the iter parameter in R internal input file
  for (cc in 1:length(costs))
    ## R iterations with costs to be used by Marxan
  {
    unit$cost <-
      as.numeric(costs[cc]) ## write the cost parameter into the R internal unit file
    write.table(
      unit,
      "unit.dat",
      sep = "\t",
      row.names = F,
      quote = F,
      dec = "."
    ) ## write the unit.dat file to be used in each R loop
    for (tt in 1:length(perct))
      ## R iterations with CF target percentages to be used by Marxan
    {
      target[, 3][pritarget] <-
        round(agg.abx202[, 2][pritarget] * perct[tt]) ## All Priority targets with a rounded perct % of the total available area
      target[, 3][-pritarget] <-
        round(agg.abx202[, 2][-pritarget] * perci) ## Non Priority targets with a rounded perci % of the total available area
      target[which(target$name == "Terrestrial habitats"), 3] <-
        0 ## Terrestrial habitats with 0% of total available in all cenarios
      for (ii in 1:length(sptt))
        ## R iterations with Penalty to be used by MarxanTarget spf parameter
      {
        target[, 4] <-
          rep(as.numeric(sptt[ii]), nrow(target)) ## write the SPF parameter into the R internal target file
        write.table(
          target,
          "target.dat",
          sep = ",",
          row.names = F,
          quote = FALSE
        ) ## write the target.dat file to be used in each R loop
        for (i in 1:length(blm))
          ## R iterations with BLM factor to be used by Marxan
        {
          input[10] <-
            paste("BLM", blm[i]) ## write the BLM factor into the R internal input file
          for (r in 1:length(nreps))
          {
            input[14] <-
              paste("NUMREPS", nreps[r]) ## write the Number of Runs into the R internal input file
            temp <-
              date()
            temp <- gsub(':', '', temp)
            temp <- gsub(' ', '', temp)
            input[35] <-
              paste(
                "SCENNAME",
                " ",
                ## write the Scenario Name into the R internal input file with all parameters
                paste(substring(temp, 7, 14), sep = ""),
                "-",
                # internal clock time at the beggining of the process
                "It",
                as.character(iter[z]),
                "-",
                # Number of iteractions
                "Co",
                costs[cc],
                "-",
                # Cost used
                "Tp",
                perct[tt],
                "-",
                # % Targetsfor priority Habitats
                "Tn",
                perci,
                "-",
                # % Targets for non priority Habitats
                "Sp",
                sptt[ii],
                "-",
                # SPF used
                "Bl",
                blm[i],
                "-",
                # BLM used
                "Re",
                nreps[r],
                # Nreps used
                #"all",
                sep = ""
              )
            
            write(input, "input.dat")# Re-write the final input.dat file to be used at each marxan  run with the corresponding changed parameters
            system("Marxan.exe",
                   wait = T,
                   invisible = T) # Calling Marxan v 2.0.2 to execute each scen?rio
          }
        }
      }
    }
  }
}

#'# End of Process when R presents:

#             The End
#Press return to exit.

#'# all combinations of parameters Iter, Cost, Targ, Spf, Blm, Reps are given by the string bellow. The total time may be quite long depending of iter values
paste(
  "A total number of parameters combinations:",
  length(iter) * length(costs) * length(perct) * length(sptt) * length(blm) *
    length(nreps)
) # Total Combinations of parameters
paste(
  "A total of",
  (
    length(iter) * length(costs) * length(perct) * length(sptt) * length(blm) *
      length(nreps)
  ) * 6,
  "files will be writted in output folder"
)

## Create a table to be imported within GIS
setwd(paste(wd, "/output/", sep = "")) ## Set working directory
sol.gis.file <-
  dir(pattern = "ssoln.txt") ## List of _ssoln.txt files
gisfile <-
  read.table(sol.gis.file[length(sol.gis.file)],
             header = T,
             dec = ".",
             sep = ",") ## read the last ssoln.txt file only
names(gisfile)[2] <- "Nptgt_all"
setwd(paste(wd, "/gis/", sep = ""))
write.table(
  gisfile,
  "pnruvuma_all_ptgt.txt",
  sep = ",",
  dec = ".",
  row.names = F
)
