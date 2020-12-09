# PURPOSE: Load and transform data
# AUTHOR:
# DATE: 2020-09-22
# NOTES:

#1. Load the required packages
library(stringr)
library(dplyr)
library(tidyverse)
library(readxl)
library(here)
library(ICPIutilities)
library(glamr)
library(purrr)
library(here)
#here("Datasets")


# GLOBALS -----------------------------------------------------------------

  data_in  <- "Data"
  data_out <- "Dataout"
  
  # Check for files in 
  here(data_in) %>% dir()
  

# FILES IN --------------------------------------------------------------

  files<-list.files(data_in, pattern="COPMatrixExport_", full.names = TRUE)
  

# FUNCTIONALIZE PROCESS ---------------------------------------------------

#2. Run the Read_SCM function
  
  df <- read_xlsx(files)
  
  df_cln <- df %>% 
    rename(mech_id = MechanismID) %>% 
    mutate(mech_id = as.character(mech_id),
           appropriation_year = as.character(`Appropriation Year`),
           data_stream = "Standard COP Matrix") %>% 
    select(-c(`Prime Partner DUNS`, `Award Number`)) %>% 
    mutate_at(vars(`GAP`:`SE`),~replace_na(., 0)) 
  
  
  df_budget <- df_cln %>% 
    select(-c(ASP:SE)) %>% 
    pivot_longer(
      cols = `CIRC GAP`: `WCF GAP`,
      names_to="Budget Code",
      values_to="Amount")
  
  df_pa_amounts <- df_cln %>% 
    select(-(`CIRC GAP`: `WCF GAP`)) %>% 
    pivot_longer(
      cols=`ASP` : `SE`,
      names_to="Program Area",
      values_to="Funding Amount"
    )
  
  
  
  

  Read_SCM<-function(df)
  {
    #reads in the Excel version of the SCM
    df <- readxl::read_xlsx(df)%>% 
      #rename and change to characters, add data stream label
      SCM<-SCM%>%dplyr::rename(`Mechanism ID` =MechanismID)%>%
      dplyr::mutate(`Mechanism ID`=as.character(`Mechanism ID`))%>%
      dplyr::mutate(`Appropriation Year`=as.character(`Appropriation Year`))%>%
      dplyr::mutate(`Data Stream` = "Standard COP Matrix")%>%
      dplyr::select( - c('Prime Partner DUNS',	'Award Number'))%>%
      
      #replace NAs in budget numbers to 0s
       mutate_at(vars(`GAP`:`SE`),~replace_na(.,0))
      
      
    df_budget <- df %>% 
      select(-c(ASP:SE)) %>% 
      pivot_longer(
          cols = `CIRC GAP`: `WCF GAP`,
          names_to="Budget Code",
          values_to="Amount")
      
      #Use pivot_long to put PA amounts in one column. currently have it as funding amount as it throws up an error for just "Amount"
      pivot_longer(
          cols=`ASP` : `SE`,
          names_to="Program Area",
         values_to="Funding Amount"
        )
        
    #  What I was using instead of pivot_long to do this previously
      #SCM<-SCM%>%gather(`Budget Code`,`Funding Amount`,`CIRC GAP`:`WCF GAP`,na.rm=TRUE)
        
    
  #replace duplicates with 0s. Having an issue with some budget codes/Program Areas though
  df<-df%>%
    mutate_at(vars(`GAP`:`Total Planned Funding`, `Amount`,`Funding Amount`), funs(replace(., duplicated (.), 0)))
      
      return(df)
  }

#3. Read in the files and run the function

SCM<-purrr::map_dfr(.x=files,
                    .f=~Read_SCM(.x))

#These items below were used for testing. Please ignore
#SCM<-readxl::read_xlsx("Datasets/COPMatrixExport_All Operating Units.xlsx")  
#write_csv(SCM, "TestSCM.csv")
