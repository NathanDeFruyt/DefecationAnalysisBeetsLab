## This script analyses defecation data. This script was written
## in R version 4.0.4 "Lost Library Book" with warmth and love for the dearest 
## of my colleagues Signe Kenis (signe.kenis@kuleuven.be). 
##
## More info on the format your data should be in, can be requested with 
## Signe, with Prof. Dr. Isabel Beets (isabel.beets@kuleuven.be), or at my adress
## (nathan.defruyt@kuleuven.be).
## Please refer to the above contacts for any suggestions or question you would have.

library(ggplot2)
library(ggthemes)
library(lme4)
library(car)
library(magrittr)
library(doBy)
library(emmeans)
library(doBy)
library(effects)
library(tcltk)
library(shiny)

source("https://raw.githubusercontent.com/NathanDeFruyt/DefecationAnalysisBeetsLab/main/R/20220201-defecation_all_functions_v4a.R")

## load files,  convert the strain code to strain + add a filename
folderinfolder <- tcltk::tk_messageBox('yesno', message = 'Do you want to analyse experiments of different days at once? Every day will be handled separately, so code files will be considered accordingly for each experiment.',
                                       caption = 'Analyse multiple experiments at once?')

if (folderinfolder == 'yes') {
  tk_messageBox(type = 'ok', message = 'Now a folder selection window will appear. Please click through to the folder that contains separate folders, each labeled with the date of the experiment for which it contains data. Each folder should contain at least one data file in .tsv format, and a code file that explains how you called each strain in your blinded experiments. We do not consider experiments that were not blinded. You can analyse those yourself.')
  path <- tcltk::tk_choose.dir(default = "", caption = 'Point to you stinky directory')
  
  ## a function to read in all files in one folder: 
  folders <- list.files(path = path, full.names = T)
  for (folder in folders) {
    print(folder)
    d <- read.stinky.folder(folder)
    data <- d$data
    pBocs <- d$pBocs
    if (folder == folders[1]) { cdata <- data; cpBocs <- pBocs } else { cdata <- rbind(cdata, data); cpBocs <- rbind(cpBocs, pBocs)}
  }
} else {
  tk_messageBox(type = 'ok', message = 'Now a folder selection window will appear. Please click through to the folder that contains data for ONLY 1 experiment, labeled with the date of the experiment for which it contains data. This folder should contain at least one data file in .tsv format, and a code file that explains how you called each strain in your blinded experiments. We do not consider experiments that were not blinded. You can analyse those yourself.')
  path <- tcltk::tk_choose.dir(default = "", caption = 'Point to you stinky directory')
  d <- read.stinky.folder(path)
  cdata <- d$data
  cpBocs <- d$pBocs
}

shinyApp(ui, server)
