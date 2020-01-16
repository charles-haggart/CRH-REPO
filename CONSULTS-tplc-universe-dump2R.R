# trace(utils:::unpackPkgZip, edit = TRUE) # change line 142 from " Sys.sleep(0.5)" to " Sys.sleep(2)", and click save
# trace("unpackPkgZip", where=asNamespace("utils"), edit=TRUE) # change line 142 from " Sys.sleep(0.5)" to " Sys.sleep(2)", and click save
install.packages(c("data.table","readxl","rlang","pillar","tidyverse"))
library(data.table)
library(readxl)
library(rlang)
library(pillar)
library(tidyverse)

rm(d.cnslt,d1)
d.cnslt <- read_excel(paste0("//sharepoint.fda.gov/orgs/CDRH-QM/Performance/CARS/",
                           "consults-tplc-universe-dump2r.xlsx"), "Raw Data")
d.cnslt[,c(2:4)] <- lapply(d.cnslt[,c(2:4)], as.Date, format='%Y-%m-%d')

# rm(d.prmkt,d2)
# d.prmkt <- read_excel(paste0("//sharepoint.fda.gov/orgs/CDRH-QM/Performance/CARS/",
#                              "submissions-tplc-universe-dump2r.xlsx"), "Premarket")
# d.prmkt[,c(4:11)] <- lapply(d.prmkt[,c(4:11)], as.Date, format='%Y-%m-%d')
# 
# rm(d.rcl,d3)
# d.rcl <- read_excel(paste0("//sharepoint.fda.gov/orgs/CDRH-QM/Performance/CARS/",
#                              "submissions-tplc-universe-dump2r.xlsx"), "Recall")
# d.rcl[,c(2:4)] <- lapply(d.rcl[,c(2:4)], as.Date, format='%Y-%m-%d')

# tidy data ---------------------------------------------------------------
rm(d1)
d1 <- d.cnslt %>% rename(consult_num="Doc Num CTS Hyperlink", date_request="Consult Date Requested", date_complete="Consult Date Completed",
                         date_request_recd="Consult Date Request Rec'd", parent_num="Consult Parent Doc Number", request_home="Consult Requestor Home",
                         consult_home="Consultant Home", consult_type="Consult Type Code", deliverable="Consult Deliverable", 
                         consult_wf_state="Consult Workflow State", consult_wf_type="Consult Workflow Type As Detail", 
                         request_name="Consult Requestor", consult_name="Consultant Name", instructions="Consult Instructions",
                         cy_request="Year (CY) Consult Requested", parent_type="Consult Parent Submission Type and Document Type (Where Applicable)") %>%
  arrange(consult_num) %>% 
  select(1,15,2:16) %>%
  mutate(parent_type = case_when(
    str_detect(parent_num,"COR") ~ "COR",
    str_detect(parent_num,"CPT") ~ "CPT",
    str_detect(parent_num,"CR") ~ "CR",
    str_detect(parent_num,"CW") ~ "CW",
    str_detect(parent_num,"EC") ~ "EC",
    str_detect(parent_num,"DEN") ~ "DEN",
    str_detect(parent_num,"ICC") ~ "ICC",
    str_detect(parent_num,"RCL") ~ "RCL",
    str_detect(parent_num,"PEUA") ~ "PEUA",
    str_detect(parent_num,"EUA") ~ "EUA",
    str_detect(parent_num,"GEN") ~ "GEN",
    str_detect(parent_num,"GUD") ~ "GUD",
    str_detect(parent_num,"RH") ~ "RH",
    str_detect(parent_num,"SPJ") ~ "SPJ",
    str_detect(parent_num,"PS") ~ "PS",
    str_detect(parent_num,"RUL") ~ "RUL",
    str_detect(parent_num,"RWL") ~ "RWL",
    str_detect(parent_num,"C") ~ "C",
    str_detect(parent_num,"G") ~ "G",
    str_detect(parent_num,"H") ~ "H",
    str_detect(parent_num,"K") ~ "K",
    str_detect(parent_num,"M") ~ "M",
    str_detect(parent_num,"N") ~ "N",
    str_detect(parent_num,"P") ~ "P",
    str_detect(parent_num,"Q") ~ "Q",
    is.na(parent_num) ~ "SA")) %>%
  mutate(parent_length = as.double(str_length(parent_num))) %>%
  mutate(root_length_slash = as.double(str_locate(parent_num, '/')[,1]-1)) %>%
  mutate(root_length_dash = as.double(str_locate(parent_num, '-')[,1]-1)) %>%
  mutate(root_length = as.double(if_else(!is.na(root_length_slash), root_length_slash, 
                               if_else(!is.na(root_length_dash), root_length_dash, 
                                       if_else(!is.na(parent_length), parent_length, NULL))))) %>%
  mutate(root_num = if_else(!is.na(root_length), str_sub(parent_num, c(rep(1,nrow(d.cnslt))), root_length), NULL, NULL)) %>%
  mutate(sep_1 = if_else(!is.na(root_length_slash),"/",if_else(!is.na(root_length_dash),"-",NULL))) %>%
  select(1:17,20:22) %>%
  mutate(post_root_length_all = as.double(if_else(!(parent_length == root_length), parent_length - root_length - 1, NULL))) %>%
  mutate(post_root_num_all = if_else(!is.na(post_root_length_all), str_sub(parent_num, root_length+2, parent_length), NULL, NULL)) %>%
  mutate(post_root_length_slash = as.double(str_locate(post_root_num_all, '/')[,1]-1)) %>%
  mutate(post_root_length_dash = as.double(str_locate(post_root_num_all, '-')[,1]-1)) %>%
  mutate(post_root_length_1 = as.double(if_else(!is.na(post_root_length_slash), post_root_length_slash, 
                               if_else(!is.na(post_root_length_dash), post_root_length_dash, 
                                       if_else(!is.na(post_root_length_all), post_root_length_all, NULL))))) %>%
  mutate(post_root_num_1 = if_else(!is.na(post_root_length_1), str_sub(post_root_num_all, c(rep(1,nrow(d.cnslt))), post_root_length_1), NULL, NULL)) %>%
  mutate(post_root_num_2 = if_else(!is.na(post_root_length_all) & post_root_length_all > post_root_length_1, 
                                   str_sub(post_root_num_all, post_root_length_1+2, post_root_length_all), NULL, NULL)) %>%
  mutate(parent_num_short = if_else(!is.na(post_root_num_1), str_sub(parent_num, c(rep(1,nrow(d.cnslt))), root_length+post_root_length_1+1), root_num)) %>%
  mutate(sep_2 = if_else(!is.na(post_root_length_slash),"/",if_else(!is.na(post_root_length_dash),"-",NULL))) %>%
  mutate(post_root_length_2 = as.double(if_else(!is.na(post_root_num_2), str_length(post_root_num_2), NULL))) %>%
  mutate(post_root_type_1 = if_else(!is.na(post_root_num_1), str_sub(post_root_num_1, c(rep(1,nrow(d.cnslt))), post_root_length_1-3), NULL)) %>%
  mutate(post_root_type_2 = if_else(!is.na(post_root_num_2), str_sub(post_root_num_2, c(rep(1,nrow(d.cnslt))), post_root_length_2-3), NULL)) %>%
  select(1:6,28,7:16,20,31,29,32,19,26:27) %>%
  mutate(o_num = is.na(post_root_num_1)) %>%
  mutate(a_num = if_else(!is.na(post_root_num_1),str_detect(post_root_num_1,"A"),FALSE)) %>%
  mutate(s_num = if_else(!is.na(post_root_num_1),str_detect(post_root_num_1,"S"),FALSE)) %>%
  mutate(r_num = if_else(!is.na(post_root_num_1),str_detect(post_root_num_1,"R"),FALSE)) %>%
  mutate(m_num = if_else(!is.na(post_root_num_1),str_detect(post_root_num_1,"M"),FALSE)) %>%
  mutate(e_num = if_else(!is.na(post_root_num_1),str_detect(post_root_num_1,"E"),FALSE)) %>%
  mutate(dsh_num = if_else(!is.na(sep_1),str_detect(sep_1,"-"),FALSE)) %>%
  mutate(parent_num_alt = if_else(parent_type=="K" | parent_type=="DEN",root_num,
                                  if_else( m_num | e_num | dsh_num | ((s_num | r_num) & (parent_type=="G" | parent_type=="H" | parent_type=="P" |
                                                                          parent_type=="N" | parent_type=="Q")), parent_num_short,
                                              if_else(o_num | a_num | s_num | r_num, root_num, NULL)))) %>%
  mutate(parent_type_category = if_else( parent_type=="C" | parent_type=="CR" | parent_type=="CW" | parent_type=="DEN" | parent_type=="EUA" |
                                        parent_type=="G" | parent_type=="H" | parent_type=="K" | parent_type=="M" | parent_type=="N" |
                                        parent_type=="P" | parent_type=="PEUA" | parent_type=="Q", "Premarket",
                                      if_else( parent_type=="CPT" | parent_type=="EC" | parent_type=="RCL" | parent_type=="RUL" | parent_type=="RWL", "Compliance",
                                               if_else( parent_type=="PS" | parent_type=="SPJ", "Surveillance",
                                                        if_else( parent_type=="COR" | parent_type=="GEN" | parent_type=="GUD" | parent_type=="ICC" | 
                                                                   parent_type=="RH" | parent_type=="SA","Other", NULL))))) %>%
  mutate(parent_sub_type = if_else(parent_type=="K" | parent_type=="DEN",parent_type,
                                   if_else( m_num | e_num | dsh_num | ((s_num | r_num) & (parent_type=="G" | parent_type=="H" | parent_type=="P" |
                                                                           parent_type=="N" | parent_type=="Q")),
                                  str_c(parent_type,sep_1,post_root_type_1,collapse=NULL),
                                    if_else(o_num | a_num | s_num | r_num, parent_type, NULL)))) %>%
  select(1:5,33,6,32,8:15,17,19,21,34,22,18,23,20,24,16) %>%
  mutate(elapsed_days = date_request - min(date_request)) %>%
  filter(consult_name != "BYS") %>%  #removing all consults assigned to "BYS" which is duplicate to "Berkman Sahiner"
  filter( !(consult_num=="CON164868" & parent_num=="K142953") & !(consult_num=="CON1810417" & parent_num=="G180072") & 
            !(consult_num=="CON181069" & parent_num=="COR18000012") & !(consult_num=="CON1816772" & parent_num=="P150030/R007/A001") & 
            !(consult_num=="CON1820143" & parent_num=="G180071/S001/A001") & !(consult_num=="CON189178" & parent_num=="G180072") & 
            !(consult_num=="CON197419" & parent_num=="RCL190248")) %>% #removing duplicate or erroneous CON#-Parent# combinations
  group_by(parent_num_alt, consult_type, consult_name) %>%
  mutate(cnslt_seq_type_name = rank(elapsed_days, na.last="keep", ties.method="first")) %>%
  ungroup() %>%
  group_by(parent_num_alt, consult_type) %>%
  mutate(cnslt_seq_type = rank(elapsed_days, na.last="keep", ties.method="first")) %>%
  ungroup() %>%
  group_by(parent_num_alt, consult_name) %>%
  mutate(cnslt_seq_name = rank(elapsed_days, na.last="keep", ties.method="first")) %>%
  ungroup() %>%
  arrange(parent_num_alt,date_request)

# rm(d2)
# d2 <- d.prmkt %>% rename(sub_num="PreMkt Subm Document Number", sub_type="PreMkt Subm Type Code", rvw_track="PreMkt Review Track",
#                          date_recd="PreMkt Date Subm Received", date_date="PreMkt Date FDA Due", date_start="PreMkt Date FDA Initial Clock Start",
#                          date_final="PreMkt Date Final Decision", rta_date="Date RTA Latest Decision", rtf_date="Date Latest RTF",
#                          si_date="Date Latest SI", mdufa_date="Date MDUFA III Decision", legacy_of="PreMkt Office Code (Lead)",
#                          legacy_dv="PreMkt Division Code (Lead)", legacy_br="PreMkt Branch Code (Lead)", tplc_of="TPLC Office Code",
#                          tplc_dv="TPLC Division Code", tplc_tm="TPLC Team Code",  rta_dec="RTA Latest Decision",
#                          rtf_dec="RTF Latest Decision",  si_dec="SI Decision", mdufa_dec="MDUFA III Decision") %>%
#   arrange(sub_num)


# START HERE - FIGURE OUT HOW TO SCAN PREMARKET MATRIX TO IDENTIFY SUBMISSION-LEVEL INFO FOR EACH PARENT_NUM_ALT VALUE
# rm(ix,n)
# ix <- as.double(c())
# for(n in 1:nrow(d1)) {
#   ix[n] <- if(any(which(d2$sub_num == d1$parent_num_alt[n]))) as.double(which(d2$sub_num == d1$parent_num_alt[n])) else as.double(0)
# }

# combine & rename cols & export CSV for Tableau ---------------------------------------
d1 <- select(d1, 1:26,28:30) %>%
  rename("Doc Num CTS Hyperlink"="consult_num", "Year (CY) Consult Requested"="cy_request", "Consult Date Requested"="date_request", 
         "Consult Date Completed"="date_complete", "Consult Date Request Rec'd"="date_request_recd", "Parent Submission Category"="parent_type_category", 
         "Consult Parent Doc Number"="parent_num", "Parent Submission Number"="parent_num_alt", "Consult Requestor Home"="request_home", 
         "Consultant Home"="consult_home", "Consult Type Code"="consult_type", "Consult Deliverable"="deliverable", 
         "Consult Workflow State"="consult_wf_state", "Consult Workflow Type As Detail"="consult_wf_type", "Consult Requestor"="request_name",
         "Consultant Name"="consult_name", "Parent Submission Type"="parent_type","Separator #1"="sep_1", "Parent Submission Sub-Type"="post_root_type_1", 
         "Separator #2"="sep_2", "Parent Submission Sub-Sub-Type"="post_root_type_2", "Parent Root Number"="root_num", "Parent Sub-Type Number"="post_root_num_1",
         "Parent Sub-Sub-Type Number"="post_root_num_2", "Consult Instructions"="instructions", "Consult Sequence (by Type & Consultant)"="cnslt_seq_type_name",
         "Consult Sequence (by Type)"="cnslt_seq_type", "Consult Sequence (by Consultant)"="cnslt_seq_name", "Parent Submission Type/Sub-Type"="parent_sub_type")
write_csv(d1, "//sharepoint.fda.gov/orgs/CDRH-QM/Performance/CARS/consults-tplc-dashboard-tidyr.csv",na = "")
rm(d1)
