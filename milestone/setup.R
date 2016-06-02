## File: Setup.R
## Description: To set up project folders and downloading the source data. 

## Setting up the project environment.
dir.create('DS_Capstone')
setwd('DS_Capstone')
new_wd <- getwd()

## Downloading the source files
src_url <- 'https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip'
download.file(src_url,'train.zip')
unzip('train.zip')

## Creating the data directory
dir.create('data')
dir.create('data/raw') ## For placing raw data
dir.create('./data/sample') ## For placing Sample data

## We chose to use the en_US datasets. Placing the EN files to data directory.
file.copy(list.files('./final/en_US/', full.names = T),'data/raw')

##we can delete the rest of the data.
unlink('./final', recursive = T, force = T)
unlink('train.zip')
