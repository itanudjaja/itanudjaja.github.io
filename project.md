---
title: "Using R packages to populate IR"
output:
  html_document:
    highlight: pygments
    keep_md: yes
    theme: cerulean
    number_sections: yes
    toc: yes
    toc_float: yes
---
# Using R packages to populate IR

Many institutions have reported that **participation rates of article deposit in their IR are low** regardless of their various efforts in outreach and engagement. Even when the deposit is mandated, the participation rate can still be quite low. 

Once this hurdle was overcome, there is another challenge faced by the IR administrators, ensuring that the version submitted by the researcher is the appropriate version. If it is not, IR administrators would need to take additional steps to correspond with the researcher to obtain the appropriate version. Thus, **increasing their administrative work load**.

Therefore, some institutions had taken the pro-active initiative to complete the deposit on behalf of their researchers. This certainly is not a small undertaking. However, there are openly available R packages (https://ropensci.org/) that can be used to automate some of the processes. In this page, I will summarize the steps to do that.  

The following packages are required, please install them beforehand.





```r
# install.packages("fulltext")

library(kableExtra)
library(fulltext)
```

```
## Warning: package 'fulltext' was built under R version 3.6.2
```

```
## Registered S3 method overwritten by 'hoardr':
##   method           from
##   print.cache_info httr
```

```
## 
## Attaching package: 'fulltext'
```

```
## The following object is masked from 'package:dplyr':
## 
##     collect
```

```r
library(tidyverse)
library(plyr)
```

```
## ------------------------------------------------------------------
```

```
## You have loaded plyr after dplyr - this is likely to cause problems.
## If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
## library(plyr); library(dplyr)
```

```
## ------------------------------------------------------------------
```

```
## 
## Attaching package: 'plyr'
```

```
## The following objects are masked from 'package:plotly':
## 
##     arrange, mutate, rename, summarise
```

```
## The following objects are masked from 'package:dplyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename,
##     summarise, summarize
```

```
## The following object is masked from 'package:purrr':
## 
##     compact
```

```r
library(roadoi)
library(purrr)
library(rromeo)
library(rcrossref)
library(tidyr)
library(dplyr)
```

# Step 1: Data Loading 

First thing, get a list of the DOIs of your institution's works that you would like to deposit. This list might be very long, so it can be split into several CSV files. This is so that we do not hit the limit when querying Unpaywall API. The CSV file looks like the following.
![](/Users/eneri/itanudjaja.github.io/images/2019-09-06_100004.png)



```r
list.filenames<-list.files(pattern=".csv$") 
```

# Step 2: Query Unpaywall API

Unpaywall (https://unpaywall.org/) is a non profit organization that aims to make scholarly works more open. They maintain a database of links to full-text articles from open-access sources all over the world. The content is harvested from legal sources including repositories run by universities, governments, and scholarly societies, as well as open content hosted by publishers themselves. 

Unpaywall requests users to keep the API requests to below 100k per day and to include their email to the URL of requests. Please include your own email below.


```r
unpaywall<- tibble()

# Creating loop so that we can query Unpaywall API based on dois in several CSV files

for (i in 1:length(list.filenames))
{
  #reading dois from csv file
  
  dois <- read.csv(list.filenames[i], header=TRUE)
  vec_doi <- as_tibble(dois)
  
  ##querying unpaywall API & to 
  #catch error when the API does not return valid JSON or is not available
  
  df_data <- purrr::map(vec_doi, .f = safely(function(x) oadoi_fetch(x, email="clbti@nus.edu.sg")))
  df <- purrr::map_df(df_data, "result")
  
  
  ##getting values from best_oa_location
  best_oa_evidence <- df %>%
    dplyr::mutate(evidences = purrr::map(best_oa_location, "evidence") %>% 
             purrr::map_if(purrr::is_empty, ~ NA_character_) %>% 
             purrr::flatten_chr())%>%
    .$evidences 
  
   best_oa_host <- df %>%
    dplyr::mutate(hosts = purrr::map(best_oa_location, "host_type") %>% 
             purrr::map_if(purrr::is_empty, ~ NA_character_) %>% 
             purrr::flatten_chr())%>%
    .$hosts 
  
  best_oa_license <- df %>%
    dplyr::mutate(licenses = purrr::map(best_oa_location, "license") %>% 
             purrr::map_if(purrr::is_empty, ~ NA_character_) %>% 
             purrr::flatten_chr())%>%
    .$licenses
  
  best_oa_url <- df %>%
    dplyr::mutate(urls = purrr::map(best_oa_location, "url") %>% 
             purrr::map_if(purrr::is_empty, ~ NA_character_) %>% 
             purrr::flatten_chr())%>%
    .$urls 
  
  best_oa_url_for_pdf <- df %>%
    dplyr::mutate(pdfs = purrr::map(best_oa_location, "url_for_pdf") %>% 
             purrr::map_if(purrr::is_empty, ~ NA_character_) %>% 
             purrr::flatten_chr())%>%
    .$pdfs 
  
  best_oa_version <- df %>%
    dplyr::mutate(versions = purrr::map(best_oa_location, "version") %>% 
             purrr::map_if(purrr::is_empty, ~ NA_character_) %>% 
             purrr::flatten_chr())%>%
    .$versions 
 
  #selecting specific columns from the results
  df_selection <- df %>% 
   select(-c(oa_locations,updated,authors)) 
  
  ##merging the columns together
  df_final <- add_column(df_selection, best_oa_evidence, best_oa_host, best_oa_license,best_oa_url,best_oa_url_for_pdf,best_oa_version)
  
  #to combine all the entries
  unpaywall <- bind_rows(unpaywall, df_final)
  
  Sys.sleep(60)  
  
}

# changing year from factor to numeric, and limit to journal article only

unpaywall <- unpaywall %>% 
  filter(genre == "journal-article") %>% 
  select(-c(best_oa_location,journal_issn_l))

kable(unpaywall) %>% 
  kable_styling(bootstrap_options = c("striped", "hover")) %>% 
  scroll_box(width = "100%", height = "400px")
```

<div style="border: 1px solid #ddd; padding: 0px; overflow-y: scroll; height:400px; overflow-x: scroll; width:100%; "><table class="table table-striped table-hover" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> doi </th>
   <th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;"> data_standard </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> is_oa </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> genre </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> oa_status </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> has_repository_copy </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> journal_is_oa </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> journal_is_in_doaj </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> journal_issns </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> journal_name </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> publisher </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> title </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> year </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> best_oa_evidence </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> best_oa_host </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> best_oa_license </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> best_oa_url </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> best_oa_url_for_pdf </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> best_oa_version </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 10.1145/903893.903896 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> journal-article </td>
   <td style="text-align:left;"> closed </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> 0001-0782 </td>
   <td style="text-align:left;"> Communications of the ACM </td>
   <td style="text-align:left;"> Association for Computing Machinery (ACM) </td>
   <td style="text-align:left;"> The role of IT in successful knowledge management initiatives </td>
   <td style="text-align:left;"> 2003 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10.1145/1314215.1314231 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> journal-article </td>
   <td style="text-align:left;"> green </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> 0001-0782,1557-7317 </td>
   <td style="text-align:left;"> Communications of the ACM </td>
   <td style="text-align:left;"> Association for Computing Machinery (ACM) </td>
   <td style="text-align:left;"> Record matching in digital library metadata </td>
   <td style="text-align:left;"> 2008 </td>
   <td style="text-align:left;"> oa repository (via OAI-PMH doi match) </td>
   <td style="text-align:left;"> repository </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> http://www.comp.nus.edu.sg/~kanmy/papers/2008-cacm.pdf </td>
   <td style="text-align:left;"> http://www.comp.nus.edu.sg/~kanmy/papers/2008-cacm.pdf </td>
   <td style="text-align:left;"> submittedVersion </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10.1145/3166068 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> journal-article </td>
   <td style="text-align:left;"> closed </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> 0001-0782,1557-7317 </td>
   <td style="text-align:left;"> Communications of the ACM </td>
   <td style="text-align:left;"> Association for Computing Machinery (ACM) </td>
   <td style="text-align:left;"> Which is the fairest (rent division) of them all? </td>
   <td style="text-align:left;"> 2018 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10.2514/1.j055575 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> journal-article </td>
   <td style="text-align:left;"> green </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> 0001-1452,1533-385X </td>
   <td style="text-align:left;"> AIAA Journal </td>
   <td style="text-align:left;"> American Institute of Aeronautics and Astronautics (AIAA) </td>
   <td style="text-align:left;"> Establishment Times of Hypersonic Shock-Wave/Boundary-Layer Interactions in Intermittent Facilities </td>
   <td style="text-align:left;"> 2017 </td>
   <td style="text-align:left;"> oa repository (via OAI-PMH title and first author match) </td>
   <td style="text-align:left;"> repository </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> https://eprints.soton.ac.uk/406505/1/Establishment_Times_of_Hypersonic_Shock_Wave.pdf </td>
   <td style="text-align:left;"> https://eprints.soton.ac.uk/406505/1/Establishment_Times_of_Hypersonic_Shock_Wave.pdf </td>
   <td style="text-align:left;"> acceptedVersion </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10.2514/3.12162 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> journal-article </td>
   <td style="text-align:left;"> green </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> 0001-1452,1533-385X </td>
   <td style="text-align:left;"> AIAA Journal </td>
   <td style="text-align:left;"> American Institute of Aeronautics and Astronautics (AIAA) </td>
   <td style="text-align:left;"> Interlaminar stresses in composite laminates under out-of-plane shear/bending </td>
   <td style="text-align:left;"> 1994 </td>
   <td style="text-align:left;"> oa repository (semantic scholar lookup) </td>
   <td style="text-align:left;"> repository </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> http://pdfs.semanticscholar.org/d33a/c33e4eff6fd5ece6c203e42d14a09f2e99bc.pdf </td>
   <td style="text-align:left;"> http://pdfs.semanticscholar.org/d33a/c33e4eff6fd5ece6c203e42d14a09f2e99bc.pdf </td>
   <td style="text-align:left;"> submittedVersion </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10.2514/1.14873 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> journal-article </td>
   <td style="text-align:left;"> green </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> 0001-1452,1533-385X </td>
   <td style="text-align:left;"> AIAA Journal </td>
   <td style="text-align:left;"> American Institute of Aeronautics and Astronautics (AIAA) </td>
   <td style="text-align:left;"> Experimental Study of Linear Closed-Loop Control of Subsonic Cavity Flow </td>
   <td style="text-align:left;"> 2006 </td>
   <td style="text-align:left;"> oa repository (via OAI-PMH doi match) </td>
   <td style="text-align:left;"> repository </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> http://repository.bilkent.edu.tr/bitstream/11693/23809/1/Experimental%20study%20of%20linear%20closed-loop%20control%20of%20subsonic%20cavity%20flow.pdf </td>
   <td style="text-align:left;"> http://repository.bilkent.edu.tr/bitstream/11693/23809/1/Experimental%20study%20of%20linear%20closed-loop%20control%20of%20subsonic%20cavity%20flow.pdf </td>
   <td style="text-align:left;"> submittedVersion </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10.2514/2.445 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> journal-article </td>
   <td style="text-align:left;"> green </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> 0001-1452,1533-385X </td>
   <td style="text-align:left;"> AIAA Journal </td>
   <td style="text-align:left;"> American Institute of Aeronautics and Astronautics (AIAA) </td>
   <td style="text-align:left;"> Residual Strength of Aging Aircraft with Multiple Site Damage/Multiple Element Damage </td>
   <td style="text-align:left;"> 1998 </td>
   <td style="text-align:left;"> oa repository (semantic scholar lookup) </td>
   <td style="text-align:left;"> repository </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> http://pdfs.semanticscholar.org/0707/c925d99c6275d0b722784ff8e1489fb8c512.pdf </td>
   <td style="text-align:left;"> http://pdfs.semanticscholar.org/0707/c925d99c6275d0b722784ff8e1489fb8c512.pdf </td>
   <td style="text-align:left;"> submittedVersion </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10.2514/1.j053565 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> journal-article </td>
   <td style="text-align:left;"> green </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> 0001-1452,1533-385X </td>
   <td style="text-align:left;"> AIAA Journal </td>
   <td style="text-align:left;"> American Institute of Aeronautics and Astronautics (AIAA) </td>
   <td style="text-align:left;"> Prediction of Transonic Limit-Cycle Oscillations Using an Aeroelastic Harmonic Balance Method </td>
   <td style="text-align:left;"> 2015 </td>
   <td style="text-align:left;"> oa repository (via OAI-PMH doi match) </td>
   <td style="text-align:left;"> repository </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> https://pureadmin.qub.ac.uk/ws/files/15386385/aiaa_a_hb.pdf </td>
   <td style="text-align:left;"> https://pureadmin.qub.ac.uk/ws/files/15386385/aiaa_a_hb.pdf </td>
   <td style="text-align:left;"> publishedVersion </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10.2514/1.2159 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> journal-article </td>
   <td style="text-align:left;"> green </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> 0001-1452,1533-385X </td>
   <td style="text-align:left;"> AIAA Journal </td>
   <td style="text-align:left;"> American Institute of Aeronautics and Astronautics (AIAA) </td>
   <td style="text-align:left;"> Aerodynamic Data Reconstruction and Inverse Design Using Proper Orthogonal Decomposition </td>
   <td style="text-align:left;"> 2004 </td>
   <td style="text-align:left;"> oa repository (semantic scholar lookup) </td>
   <td style="text-align:left;"> repository </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> http://pdfs.semanticscholar.org/2e93/a6d420b050d948c269fed2be8cbac5bc882d.pdf </td>
   <td style="text-align:left;"> http://pdfs.semanticscholar.org/2e93/a6d420b050d948c269fed2be8cbac5bc882d.pdf </td>
   <td style="text-align:left;"> submittedVersion </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10.2514/1.4799 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> journal-article </td>
   <td style="text-align:left;"> green </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> 0001-1452,1533-385X </td>
   <td style="text-align:left;"> AIAA Journal </td>
   <td style="text-align:left;"> American Institute of Aeronautics and Astronautics (AIAA) </td>
   <td style="text-align:left;"> Logic-Based Active Control of Subsonic Cavity Flow Resonance </td>
   <td style="text-align:left;"> 2004 </td>
   <td style="text-align:left;"> oa repository (semantic scholar lookup) </td>
   <td style="text-align:left;"> repository </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> http://pdfs.semanticscholar.org/bfdd/8ad6d04229eef6d012828589bbfd6e91adb1.pdf </td>
   <td style="text-align:left;"> http://pdfs.semanticscholar.org/bfdd/8ad6d04229eef6d012828589bbfd6e91adb1.pdf </td>
   <td style="text-align:left;"> submittedVersion </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10.2514/1.j055143 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> journal-article </td>
   <td style="text-align:left;"> green </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> 0001-1452,1533-385X </td>
   <td style="text-align:left;"> AIAA Journal </td>
   <td style="text-align:left;"> American Institute of Aeronautics and Astronautics (AIAA) </td>
   <td style="text-align:left;"> Nonlinear Aerodynamic and Aeroelastic Model Reduction Using a Discrete Empirical Interpolation Method </td>
   <td style="text-align:left;"> 2017 </td>
   <td style="text-align:left;"> oa repository (via OAI-PMH doi match) </td>
   <td style="text-align:left;"> repository </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> https://pureadmin.qub.ac.uk/ws/files/123540593/aiaa_journal_DEIM_submission.pdf </td>
   <td style="text-align:left;"> https://pureadmin.qub.ac.uk/ws/files/123540593/aiaa_journal_DEIM_submission.pdf </td>
   <td style="text-align:left;"> publishedVersion </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10.1002/aic.12373 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> journal-article </td>
   <td style="text-align:left;"> green </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> 0001-1541 </td>
   <td style="text-align:left;"> AIChE Journal </td>
   <td style="text-align:left;"> Wiley </td>
   <td style="text-align:left;"> Acid-sensitive magnetic nanoparticles as potential drug depots </td>
   <td style="text-align:left;"> 2010 </td>
   <td style="text-align:left;"> oa repository (via OAI-PMH doi match) </td>
   <td style="text-align:left;"> repository </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> http://europepmc.org/articles/pmc3134249?pdf=render </td>
   <td style="text-align:left;"> http://europepmc.org/articles/pmc3134249?pdf=render </td>
   <td style="text-align:left;"> acceptedVersion </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10.1111/j.1399-0039.2011.01796.x </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> journal-article </td>
   <td style="text-align:left;"> green </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> 0001-2815 </td>
   <td style="text-align:left;"> Tissue Antigens </td>
   <td style="text-align:left;"> Wiley </td>
   <td style="text-align:left;"> Natural killer cell engineering for cellular therapy of cancer </td>
   <td style="text-align:left;"> 2011 </td>
   <td style="text-align:left;"> oa repository (via OAI-PMH doi match) </td>
   <td style="text-align:left;"> repository </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> http://europepmc.org/articles/pmc3218564?pdf=render </td>
   <td style="text-align:left;"> http://europepmc.org/articles/pmc3218564?pdf=render </td>
   <td style="text-align:left;"> acceptedVersion </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10.1111/abac.12091 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> journal-article </td>
   <td style="text-align:left;"> green </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> 0001-3072 </td>
   <td style="text-align:left;"> Abacus </td>
   <td style="text-align:left;"> Wiley </td>
   <td style="text-align:left;"> Comments on Shan and Walter: ‘Towards a Set of Design Principles for Executive Compensation Contracts’ </td>
   <td style="text-align:left;"> 2016 </td>
   <td style="text-align:left;"> oa repository (via OAI-PMH doi match) </td>
   <td style="text-align:left;"> repository </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> https://strathprints.strath.ac.uk/55436/8/Hillier_etal_Abacus_2015_CEO_compensation_that_benefits.pdf </td>
   <td style="text-align:left;"> https://strathprints.strath.ac.uk/55436/8/Hillier_etal_Abacus_2015_CEO_compensation_that_benefits.pdf </td>
   <td style="text-align:left;"> acceptedVersion </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10.2307/30040617 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> journal-article </td>
   <td style="text-align:left;"> green </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> 0001-4273,1948-0989 </td>
   <td style="text-align:left;"> Academy of Management Journal </td>
   <td style="text-align:left;"> Academy of Management </td>
   <td style="text-align:left;"> OWNERSHIP STRUCTURE, EXPROPRIATION, AND PERFORMANCE OF GROUP-AFFILIATED COMPANIES IN KOREA. </td>
   <td style="text-align:left;"> 2003 </td>
   <td style="text-align:left;"> oa repository (via OAI-PMH title match) </td>
   <td style="text-align:left;"> repository </td>
   <td style="text-align:left;"> cc-by-nc-nd </td>
   <td style="text-align:left;"> https://www.krm.or.kr/krmts/sdata/frbr/bizmap/2001/2001_041.pdf </td>
   <td style="text-align:left;"> https://www.krm.or.kr/krmts/sdata/frbr/bizmap/2001/2001_041.pdf </td>
   <td style="text-align:left;"> submittedVersion </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10.5465/amj.2011.0727 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> journal-article </td>
   <td style="text-align:left;"> green </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> 0001-4273,1948-0989 </td>
   <td style="text-align:left;"> Academy of Management Journal </td>
   <td style="text-align:left;"> Academy of Management </td>
   <td style="text-align:left;"> "I Put in Effort, Therefore I Am Passionate": Investigating the Path from Effort to Passion in Entrepreneurship </td>
   <td style="text-align:left;"> 2015 </td>
   <td style="text-align:left;"> oa repository (via OAI-PMH doi match) </td>
   <td style="text-align:left;"> repository </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> http://fox.leuphana.de/portal/files/7377532/Gielnik_et_al._2015_Entrepreneurial_passion.pdf </td>
   <td style="text-align:left;"> http://fox.leuphana.de/portal/files/7377532/Gielnik_et_al._2015_Entrepreneurial_passion.pdf </td>
   <td style="text-align:left;"> publishedVersion </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10.5465/amj.2009.47084665 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> journal-article </td>
   <td style="text-align:left;"> green </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> 0001-4273,1948-0989 </td>
   <td style="text-align:left;"> Academy of Management Journal </td>
   <td style="text-align:left;"> Academy of Management </td>
   <td style="text-align:left;"> Does Patent Strategy Shape the Long-Run Supply of Public Knowledge? Evidence from Human Genetics </td>
   <td style="text-align:left;"> 2009 </td>
   <td style="text-align:left;"> oa repository (via OAI-PMH doi match) </td>
   <td style="text-align:left;"> repository </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> http://fmurray.scripts.mit.edu/docs/Huang.Murray_AMJ_09.16.2008_FINAL.pdf </td>
   <td style="text-align:left;"> http://fmurray.scripts.mit.edu/docs/Huang.Murray_AMJ_09.16.2008_FINAL.pdf </td>
   <td style="text-align:left;"> submittedVersion </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10.5465/amj.2013.1082 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> journal-article </td>
   <td style="text-align:left;"> green </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> 0001-4273,1948-0989 </td>
   <td style="text-align:left;"> Academy of Management Journal </td>
   <td style="text-align:left;"> Academy of Management </td>
   <td style="text-align:left;"> Why and When Leaders’ Affective States Influence Employee Upward Voice </td>
   <td style="text-align:left;"> 2017 </td>
   <td style="text-align:left;"> oa repository (via OAI-PMH title and first author match) </td>
   <td style="text-align:left;"> repository </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> http://ira.lib.polyu.edu.hk/bitstream/10397/67339/2/Liu_et_al._2017_AMJ.pdf </td>
   <td style="text-align:left;"> http://ira.lib.polyu.edu.hk/bitstream/10397/67339/2/Liu_et_al._2017_AMJ.pdf </td>
   <td style="text-align:left;"> submittedVersion </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10.1134/s0001434610090324 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> journal-article </td>
   <td style="text-align:left;"> green </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> 0001-4346,1573-8876 </td>
   <td style="text-align:left;"> Mathematical Notes </td>
   <td style="text-align:left;"> Pleiades Publishing Ltd </td>
   <td style="text-align:left;"> A supercongruence motivated by the Legendre family of elliptic curves </td>
   <td style="text-align:left;"> 2010 </td>
   <td style="text-align:left;"> oa repository (via OAI-PMH doi match) </td>
   <td style="text-align:left;"> repository </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> http://wain.mi.ras.ru/PS/supercong-MN2010.pdf </td>
   <td style="text-align:left;"> http://wain.mi.ras.ru/PS/supercong-MN2010.pdf </td>
   <td style="text-align:left;"> submittedVersion </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10.1021/ar900178k </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> journal-article </td>
   <td style="text-align:left;"> green </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> 0001-4842,1520-4898 </td>
   <td style="text-align:left;"> Accounts of Chemical Research </td>
   <td style="text-align:left;"> American Chemical Society (ACS) </td>
   <td style="text-align:left;"> Cofabrication: A Strategy for Building Multicomponent Microsystems </td>
   <td style="text-align:left;"> 2010 </td>
   <td style="text-align:left;"> oa repository (via OAI-PMH doi match) </td>
   <td style="text-align:left;"> repository </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> http://europepmc.org/articles/pmc2857577?pdf=render </td>
   <td style="text-align:left;"> http://europepmc.org/articles/pmc2857577?pdf=render </td>
   <td style="text-align:left;"> acceptedVersion </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10.1021/ar500303m </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> journal-article </td>
   <td style="text-align:left;"> green </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> 0001-4842,1520-4898 </td>
   <td style="text-align:left;"> Accounts of Chemical Research </td>
   <td style="text-align:left;"> American Chemical Society (ACS) </td>
   <td style="text-align:left;"> Electronic Structure and Optical Signatures of Semiconducting Transition Metal Dichalcogenide Nanosheets </td>
   <td style="text-align:left;"> 2014 </td>
   <td style="text-align:left;"> oa repository (via OAI-PMH doi match) </td>
   <td style="text-align:left;"> repository </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> http://repositorium.sdum.uminho.pt/bitstream/1822/39757/1/Optical%20signagures%20of%20SC%20TMD%20NSs.pdf </td>
   <td style="text-align:left;"> http://repositorium.sdum.uminho.pt/bitstream/1822/39757/1/Optical%20signagures%20of%20SC%20TMD%20NSs.pdf </td>
   <td style="text-align:left;"> publishedVersion </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10.1021/ar900183k </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> journal-article </td>
   <td style="text-align:left;"> bronze </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> 0001-4842,1520-4898 </td>
   <td style="text-align:left;"> Accounts of Chemical Research </td>
   <td style="text-align:left;"> American Chemical Society (ACS) </td>
   <td style="text-align:left;"> Attachment Chemistry of Organic Molecules on Si(111)-7 × 7 </td>
   <td style="text-align:left;"> 2009 </td>
   <td style="text-align:left;"> open (via free pdf) </td>
   <td style="text-align:left;"> publisher </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> https://pubs.acs.org/doi/pdf/10.1021/ar900183k </td>
   <td style="text-align:left;"> https://pubs.acs.org/doi/pdf/10.1021/ar900183k </td>
   <td style="text-align:left;"> publishedVersion </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10.1121/1.4996860 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> journal-article </td>
   <td style="text-align:left;"> green </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> 0001-4966 </td>
   <td style="text-align:left;"> The Journal of the Acoustical Society of America </td>
   <td style="text-align:left;"> Acoustical Society of America (ASA) </td>
   <td style="text-align:left;"> Space-time domain solutions of the wave equation by a non-singular boundary integral method and Fourier transform </td>
   <td style="text-align:left;"> 2017 </td>
   <td style="text-align:left;"> oa repository (via OAI-PMH doi match) </td>
   <td style="text-align:left;"> repository </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> http://arxiv.org/pdf/1706.07919 </td>
   <td style="text-align:left;"> http://arxiv.org/pdf/1706.07919 </td>
   <td style="text-align:left;"> submittedVersion </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10.1121/1.3625257 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> journal-article </td>
   <td style="text-align:left;"> green </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> 0001-4966 </td>
   <td style="text-align:left;"> The Journal of the Acoustical Society of America </td>
   <td style="text-align:left;"> Acoustical Society of America (ASA) </td>
   <td style="text-align:left;"> Passive acoustic survey of Yangtze finless porpoises using a cargo ship as a moving platform </td>
   <td style="text-align:left;"> 2011 </td>
   <td style="text-align:left;"> oa repository (semantic scholar lookup) </td>
   <td style="text-align:left;"> repository </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> http://pdfs.semanticscholar.org/d504/f8ad0fd51d7f0cecd85fc6d2c07c77038fe9.pdf </td>
   <td style="text-align:left;"> http://pdfs.semanticscholar.org/d504/f8ad0fd51d7f0cecd85fc6d2c07c77038fe9.pdf </td>
   <td style="text-align:left;"> submittedVersion </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10.1121/1.2721658 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> journal-article </td>
   <td style="text-align:left;"> closed </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> 0001-4966 </td>
   <td style="text-align:left;"> The Journal of the Acoustical Society of America </td>
   <td style="text-align:left;"> Acoustical Society of America (ASA) </td>
   <td style="text-align:left;"> Echolocation click sounds from wild inshore finless porpoise (Neophocaena phocaenoides sunameri) with comparisons to the sonar of riverine N. p. asiaeorientalis </td>
   <td style="text-align:left;"> 2007 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10.1121/1.4929492 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> journal-article </td>
   <td style="text-align:left;"> green </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> 0001-4966 </td>
   <td style="text-align:left;"> The Journal of the Acoustical Society of America </td>
   <td style="text-align:left;"> Acoustical Society of America (ASA) </td>
   <td style="text-align:left;"> Echolocation signals of free-ranging Indo-Pacific humpback dolphins (Sousa chinensis) in Sanniang Bay, China </td>
   <td style="text-align:left;"> 2015 </td>
   <td style="text-align:left;"> oa repository (semantic scholar lookup) </td>
   <td style="text-align:left;"> repository </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> http://pdfs.semanticscholar.org/447a/9f5e8997ca658e6499c5130c57e8c8bb72d9.pdf </td>
   <td style="text-align:left;"> http://pdfs.semanticscholar.org/447a/9f5e8997ca658e6499c5130c57e8c8bb72d9.pdf </td>
   <td style="text-align:left;"> submittedVersion </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10.1121/1.3021302 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> journal-article </td>
   <td style="text-align:left;"> closed </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> 0001-4966 </td>
   <td style="text-align:left;"> The Journal of the Acoustical Society of America </td>
   <td style="text-align:left;"> Acoustical Society of America (ASA) </td>
   <td style="text-align:left;"> Comparison of stationary acoustic monitoring and visual observation of finless porpoises </td>
   <td style="text-align:left;"> 2009 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10.1121/1.3442574 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> journal-article </td>
   <td style="text-align:left;"> green </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> 0001-4966 </td>
   <td style="text-align:left;"> The Journal of the Acoustical Society of America </td>
   <td style="text-align:left;"> Acoustical Society of America (ASA) </td>
   <td style="text-align:left;"> Density estimation of Yangtze finless porpoises using passive acoustic sensors and automated click train detection </td>
   <td style="text-align:left;"> 2010 </td>
   <td style="text-align:left;"> oa repository (semantic scholar lookup) </td>
   <td style="text-align:left;"> repository </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> http://pdfs.semanticscholar.org/f06e/dc9e6ae7464ae20408f4b925d7eb184f63b3.pdf </td>
   <td style="text-align:left;"> http://pdfs.semanticscholar.org/f06e/dc9e6ae7464ae20408f4b925d7eb184f63b3.pdf </td>
   <td style="text-align:left;"> submittedVersion </td>
  </tr>
</tbody>
</table></div>


# Step 3: Compliment with Sherpa/Romeo information

SHERPA RoMEO is an online resource that aggregates and analyses publisher open access policies from around the world and provides summaries of self-archiving permissions and conditions of rights given to authors on a journal-by-journal basis.

To apply for API key, please refer to this page - http://www.sherpa.ac.uk/romeo/api.html


```r
#separate the 2 ISSNs
unpaywall2 <- unpaywall %>% 
  separate(journal_issns, c("issn1","issn2"), sep = "\\,", remove = TRUE)

#querying romeo API using the issn1
#your_own_key : please use your own API key

romeo <- map(unpaywall2$issn1, .f = safely(function(x) rr_journal_issn(x, your_own_key)))

# pluck the data, get rid of duplicate ISSNs and the extra title column
romeo_df <- map_df(romeo, "result") %>% 
  filter(!duplicated(issn)) %>%
  select(-title)

# join by ISSN
unpaywall_romeo1 <- unpaywall2 %>%
  left_join(romeo_df, by = c("issn1" = "issn"))

# Using issn2 for those resulted in NA using issn

# add rownames as identifiers so we can bind together
unpaywall_romeo1 <- unpaywall_romeo1 %>%
  rownames_to_column()

# create table of items not found by issn1 (where romeocolour is NA)
unp.na <- unpaywall_romeo1 %>%
  filter(is.na(romeocolour))

# create vector of NA rownames
na_rownames <- as.integer(unp.na$rowname)

# query sherpa-romeo with issn2
romeo2 <- map(unp.na$issn2, .f = safely(function(x) rr_journal_issn(x, key=your_own_key)))

# pluck data, remove duplicates and extra title column
romeo2_df <-  map_df(romeo2, "result") %>%
  filter(!duplicated(issn)) %>%
  select(-title)

# delete columsn with NA create new table with the values we just retrieved
unp.na2 <- unp.na %>%
  select(-romeocolour, -preprint, -postprint, -pdf, -pre_embargo, -post_embargo, -pdf_embargo) %>%
  left_join(romeo2_df, by = c("issn2" = "issn"))

# join NA data back
# remove NA data from unpaywall_romeo1 and replace with new data, rearrange by rowname
unpaywall_romeo2 <- unpaywall_romeo1 %>%
  slice(-na_rownames) %>%
  bind_rows(unp.na2) %>%
  mutate(rowname = as.integer(rowname)) %>%
  arrange(rowname)
```

Create a summary table.

```r
# create new column to give information about publisher permissions
unpaywall_romeo2 <- read.csv("~/Documents/nBox/R/full_text/sherpa.csv")

# create new column to give information about publisher permissions

`%notin%` <- negate(`%in%`)

unpaywall_romeo_final <- unpaywall_romeo2 %>%
  mutate(status = case_when(pdf == "can" & pdf_embargo == "NA" ~ "final_immediate",
                            pdf == "restricted" ~ "final_restricted",
                            postprint == "can" & post_embargo == "NA" &
                              pdf %notin% c("can", "restricted") ~                          
                              "postprint_immediate",
                            postprint == "restricted" & pdf %notin% c("can", "restricted") ~
                              "postprint_restricted",
                            preprint == "can"  & pre_embargo == "NA" &
                              postprint %notin% c("can", "restricted") & 
                              pdf %notin% c("can", "restricted") ~ "preprint_immediate",
                            preprint == "restricted" & postprint %notin% c("can", "restricted") &
                              pdf %notin% c("can", "restricted") ~ "preprint_restricted",
                            preprint == "cannot" & postprint == "cannot" & pdf == "cannot" ~
                              "fully_restricted"))
```


```r
# create a summary table

unpaywall_romeo_summary <- unpaywall_romeo_final %>%
  group_by(status) %>%
  tally()

kable(unpaywall_romeo_summary) %>% 
  kable_styling(bootstrap_options = c("striped", "hover")) 
```

<table class="table table-striped table-hover" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> status </th>
   <th style="text-align:right;"> n </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> final_restricted </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fully_restricted </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> postprint_restricted </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 22 </td>
  </tr>
</tbody>
</table>

# Step 4: Identify the works that can be deposited

For example, let's say we only want to deposit full text that the publisher allows PDF version to be deposited immediately.


```r
to_dl <- sample %>% 
  filter(status == "final_immediate" & best_oa_license == "cc-by")
```

# Step 5: Download the full text

There are 2 methods to download the full text :

- Using another R package called fulltext 

- Using direct URL download

## 5.1. Using fulltext package

The fulltext package made it easy to download the full text of a publication based on a DOI. They have several data sources, for e.g. PLOS, arXiv. 


```r
#change the class of DOI from factor to character
df <- to_dl %>% 
  mutate(doi = as.character(doi))

#splitting the data into a smaller dataframe for example 100 dois per dataframe

#get the number of dataframes to be created, in this case, it is variable s 
s <- (nrow(df)-1) %/% 100

dt <- split(df, (seq(nrow(df))-1) %/% 100)

#so we will create s dataframe that is named as doi1, doi2, ... till doi(s)
for(i in 1:s){
  nam <- paste("doi", i, sep = "")
  assign(nam, dt [[i]])
}

#creating loop to download full text 
#note: once you are done with the first dataframe, doi, change it to doi2 and so on

for(j in 1:length(doi1)){
  cat(".")
  res <- purrr::map(doi1, .f = safely(function(x) ft_get(x,type="pdf")))
}

#note:
#the location of the full text downloaded can be found at /Users/xxx/Library/Caches/R/fulltext
```

There will be warnings or errors displayed if the full text is not downloaded.
![](/Users/eneri/itanudjaja.github.io/images/error.png)

## 5.2. Using direct url

For full text that cannot be downloaded using the fulltext package, we can use the direct URL approach as Unpaywall has provided the direct URL to get the full text of the publication. Similarly, there will be a warning if the full text cannot be downloaded from the URL.


```r
# convert the url as charcter, as by default it is assigned as factor
urls <- to_dl %>% 
  select(doi,best_oa_url_for_pdf) %>% 
  mutate(doi=as.character(doi), url=as.character(best_oa_url_for_pdf))

#set location to download the files
setwd("~/Desktop/ft/files/")

#create empty error file (error1.txt) if the url is not working
for(s in 1:nrow(urls)){
  name=paste("doi",s,".pdf",sep="") #create a name for the file to be downloaded
  url_n=urls$url[[s]]
  tryCatch(download.file(url_n,
                         destfile=name, method='auto'),
           error = function(e) {
             file.create(paste("error", s, ".txt",sep="")) 
             
           }) #if the url error, create an empty error file and skip to next url
}
```

# Step 6 (Optional): Create the metadata file

Use Crossref as a source to create the metadata file.


```r
#get the metadata based on a list of doi from dataframe called doi1
works_list <- purrr::map(unpaywall_romeo_final$doi,
                         function(x) {
                           my_works <- cr_works(doi = x) %>%
                             purrr::pluck("data")
                         })

#unnest the list
works_df <- works_list %>%
  purrr::map_dfr(., function(x) {
    x[, ] 
  })

#unnest the authors field to combine them
authors <- works_df %>% 
  filter(!map_lgl(author, is.null)) %>% 
  unnest(author, .drop = TRUE) %>% 
  unite(author, c(family, given), sep = " ", remove = FALSE) %>% 
  dplyr::group_by(doi) %>% 
  filter(!is.na(author)) %>% 
  dplyr::summarise(all_authors=paste(author, collapse=" ; "))

#combine back with metadata

works_df1 <- works_df %>% 
  select(c(title,container.title, doi,issn,issue,volume,page,publisher, published.print))

combined <- left_join(x=works_df1, y=authors, by="doi")

cls <- sapply(combined, class)

newCombined <- combined %>% select(which(cls=="character"))

kable(newCombined) %>% 
  kable_styling(bootstrap_options = c("striped", "hover")) %>% 
  scroll_box(width = "100%", height = "400px")
```

<div style="border: 1px solid #ddd; padding: 0px; overflow-y: scroll; height:400px; overflow-x: scroll; width:100%; "><table class="table table-striped table-hover" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> title </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> container.title </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> doi </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> issn </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> issue </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> volume </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> page </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> publisher </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> published.print </th>
   <th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;"> all_authors </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> The role of IT in successful knowledge management initiatives </td>
   <td style="text-align:left;"> Communications of the ACM </td>
   <td style="text-align:left;"> 10.1145/903893.903896 </td>
   <td style="text-align:left;"> 0001-0782 </td>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:left;"> 46 </td>
   <td style="text-align:left;"> 69-73 </td>
   <td style="text-align:left;"> Association for Computing Machinery (ACM) </td>
   <td style="text-align:left;"> 2003-09-01 </td>
   <td style="text-align:left;"> Kankanhalli Atreyi ; Tanudidjaja Fransiska ; Sutanto Juliana ; Tan Bernard C. Y. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Record matching in digital library metadata </td>
   <td style="text-align:left;"> Communications of the ACM </td>
   <td style="text-align:left;"> 10.1145/1314215.1314231 </td>
   <td style="text-align:left;"> 0001-0782,1557-7317 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 51 </td>
   <td style="text-align:left;"> 91-94 </td>
   <td style="text-align:left;"> Association for Computing Machinery (ACM) </td>
   <td style="text-align:left;"> 2008-02 </td>
   <td style="text-align:left;"> Kan Min-Yen ; Tan Yee Fan </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Which is the fairest (rent division) of them all? </td>
   <td style="text-align:left;"> Communications of the ACM </td>
   <td style="text-align:left;"> 10.1145/3166068 </td>
   <td style="text-align:left;"> 0001-0782,1557-7317 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 61 </td>
   <td style="text-align:left;"> 93-100 </td>
   <td style="text-align:left;"> Association for Computing Machinery (ACM) </td>
   <td style="text-align:left;"> 2018-01-23 </td>
   <td style="text-align:left;"> Gal Kobi ; Procaccia Ariel D. ; Mash Moshe ; Zick Yair </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Establishment Times of Hypersonic Shock-Wave/Boundary-Layer Interactions in Intermittent Facilities </td>
   <td style="text-align:left;"> AIAA Journal </td>
   <td style="text-align:left;"> 10.2514/1.j055575 </td>
   <td style="text-align:left;"> 0001-1452,1533-385X </td>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:left;"> 55 </td>
   <td style="text-align:left;"> 2875-2887 </td>
   <td style="text-align:left;"> American Institute of Aeronautics and Astronautics (AIAA) </td>
   <td style="text-align:left;"> 2017-09 </td>
   <td style="text-align:left;"> Vanstone Leon ; Estruch-Samper David ; Ganapathisubramani Bharathram </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Interlaminar stresses in composite laminates under out-of-plane shear/bending </td>
   <td style="text-align:left;"> AIAA Journal </td>
   <td style="text-align:left;"> 10.2514/3.12162 </td>
   <td style="text-align:left;"> 0001-1452,1533-385X </td>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:left;"> 32 </td>
   <td style="text-align:left;"> 1700-1708 </td>
   <td style="text-align:left;"> American Institute of Aeronautics and Astronautics (AIAA) </td>
   <td style="text-align:left;"> 1994-08 </td>
   <td style="text-align:left;"> Kim Taehyoun ; Atluri Satya N. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Experimental Study of Linear Closed-Loop Control of Subsonic Cavity Flow </td>
   <td style="text-align:left;"> AIAA Journal </td>
   <td style="text-align:left;"> 10.2514/1.14873 </td>
   <td style="text-align:left;"> 0001-1452,1533-385X </td>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> 44 </td>
   <td style="text-align:left;"> 929-938 </td>
   <td style="text-align:left;"> American Institute of Aeronautics and Astronautics (AIAA) </td>
   <td style="text-align:left;"> 2006-05 </td>
   <td style="text-align:left;"> Yan P. ; Debiasi M. ; Yuan X. ; Little J. ; Ozbay H. ; Samimy M. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Residual Strength of Aging Aircraft with Multiple Site Damage/Multiple Element Damage </td>
   <td style="text-align:left;"> AIAA Journal </td>
   <td style="text-align:left;"> 10.2514/2.445 </td>
   <td style="text-align:left;"> 0001-1452,1533-385X </td>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> 36 </td>
   <td style="text-align:left;"> 840-847 </td>
   <td style="text-align:left;"> American Institute of Aeronautics and Astronautics (AIAA) </td>
   <td style="text-align:left;"> 1998-05 </td>
   <td style="text-align:left;"> Wang L. ; Chow W. T. ; Kawai H. ; Atluri S. N. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Prediction of Transonic Limit-Cycle Oscillations Using an Aeroelastic Harmonic Balance Method </td>
   <td style="text-align:left;"> AIAA Journal </td>
   <td style="text-align:left;"> 10.2514/1.j053565 </td>
   <td style="text-align:left;"> 0001-1452,1533-385X </td>
   <td style="text-align:left;"> 7 </td>
   <td style="text-align:left;"> 53 </td>
   <td style="text-align:left;"> 2040-2051 </td>
   <td style="text-align:left;"> American Institute of Aeronautics and Astronautics (AIAA) </td>
   <td style="text-align:left;"> 2015-07 </td>
   <td style="text-align:left;"> Yao W. ; Marques S. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Aerodynamic Data Reconstruction and Inverse Design Using Proper Orthogonal Decomposition </td>
   <td style="text-align:left;"> AIAA Journal </td>
   <td style="text-align:left;"> 10.2514/1.2159 </td>
   <td style="text-align:left;"> 0001-1452,1533-385X </td>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:left;"> 42 </td>
   <td style="text-align:left;"> 1505-1516 </td>
   <td style="text-align:left;"> American Institute of Aeronautics and Astronautics (AIAA) </td>
   <td style="text-align:left;"> 2004-08 </td>
   <td style="text-align:left;"> Bui-Thanh T. ; Damodaran M. ; Willcox K. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Logic-Based Active Control of Subsonic Cavity Flow Resonance </td>
   <td style="text-align:left;"> AIAA Journal </td>
   <td style="text-align:left;"> 10.2514/1.4799 </td>
   <td style="text-align:left;"> 0001-1452,1533-385X </td>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:left;"> 42 </td>
   <td style="text-align:left;"> 1901-1909 </td>
   <td style="text-align:left;"> American Institute of Aeronautics and Astronautics (AIAA) </td>
   <td style="text-align:left;"> 2004-09 </td>
   <td style="text-align:left;"> Debiasi M. ; Samimy M. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Nonlinear Aerodynamic and Aeroelastic Model Reduction Using a Discrete Empirical Interpolation Method </td>
   <td style="text-align:left;"> AIAA Journal </td>
   <td style="text-align:left;"> 10.2514/1.j055143 </td>
   <td style="text-align:left;"> 0001-1452,1533-385X </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 55 </td>
   <td style="text-align:left;"> 624-637 </td>
   <td style="text-align:left;"> American Institute of Aeronautics and Astronautics (AIAA) </td>
   <td style="text-align:left;"> 2017-02 </td>
   <td style="text-align:left;"> Yao W. ; Marques S. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Acid-sensitive magnetic nanoparticles as potential drug depots </td>
   <td style="text-align:left;"> AIChE Journal </td>
   <td style="text-align:left;"> 10.1002/aic.12373 </td>
   <td style="text-align:left;"> 0001-1541 </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> 57 </td>
   <td style="text-align:left;"> 1638-1645 </td>
   <td style="text-align:left;"> Wiley </td>
   <td style="text-align:left;"> 2011-06 </td>
   <td style="text-align:left;"> Wuang Shy Chyi ; Neoh Koon Gee ; Kang En-Tang ; Leckband Deborah E. ; Pack Daniel W. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Natural killer cell engineering for cellular therapy of cancer </td>
   <td style="text-align:left;"> Tissue Antigens </td>
   <td style="text-align:left;"> 10.1111/j.1399-0039.2011.01796.x </td>
   <td style="text-align:left;"> 0001-2815 </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> 78 </td>
   <td style="text-align:left;"> 409-415 </td>
   <td style="text-align:left;"> Wiley </td>
   <td style="text-align:left;"> 2011-12 </td>
   <td style="text-align:left;"> Shook D. R. ; Campana D. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Comments on Shan and Walter: ‘Towards a Set of Design Principles for Executive Compensation Contracts’ </td>
   <td style="text-align:left;"> Abacus </td>
   <td style="text-align:left;"> 10.1111/abac.12091 </td>
   <td style="text-align:left;"> 0001-3072 </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> 52 </td>
   <td style="text-align:left;"> 685-771 </td>
   <td style="text-align:left;"> Wiley </td>
   <td style="text-align:left;"> 2016-12 </td>
   <td style="text-align:left;"> Beaumont Stacey ; Ratiu Raluca ; Reeb David ; Boyle Glenn ; Brown Philip ; Szimayer Alexander ; da Silva Rosa Raymond ; Hillier David ; McColgan Patrick ; Tsekeris Athanasios ; Howieson Bryan ; Matolcsy Zoltan ; Spiropoulos Helen ; Roberts John ; Smith Tom ; Zhou Qing ; Swan Peter L. ; Taylor Stephen ; Wright Sue ; Yermack David </td>
  </tr>
  <tr>
   <td style="text-align:left;"> OWNERSHIP STRUCTURE, EXPROPRIATION, AND PERFORMANCE OF GROUP-AFFILIATED COMPANIES IN KOREA. </td>
   <td style="text-align:left;"> Academy of Management Journal </td>
   <td style="text-align:left;"> 10.2307/30040617 </td>
   <td style="text-align:left;"> 0001-4273,1948-0989 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 46 </td>
   <td style="text-align:left;"> 238-253 </td>
   <td style="text-align:left;"> Academy of Management </td>
   <td style="text-align:left;"> 2003-04-01 </td>
   <td style="text-align:left;"> Chang S. J. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> “I Put in Effort, Therefore I Am Passionate”: Investigating the Path from Effort to Passion in Entrepreneurship </td>
   <td style="text-align:left;"> Academy of Management Journal </td>
   <td style="text-align:left;"> 10.5465/amj.2011.0727 </td>
   <td style="text-align:left;"> 0001-4273,1948-0989 </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> 58 </td>
   <td style="text-align:left;"> 1012-1031 </td>
   <td style="text-align:left;"> Academy of Management </td>
   <td style="text-align:left;"> 2015-08 </td>
   <td style="text-align:left;"> Gielnik Michael M. ; Spitzmuller Matthias ; Schmitt Antje ; Klemann D. Katharina ; Frese Michael </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Does Patent Strategy Shape the Long-Run Supply of Public Knowledge? Evidence from Human Genetics </td>
   <td style="text-align:left;"> Academy of Management Journal </td>
   <td style="text-align:left;"> 10.5465/amj.2009.47084665 </td>
   <td style="text-align:left;"> 0001-4273,1948-0989 </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> 52 </td>
   <td style="text-align:left;"> 1193-1221 </td>
   <td style="text-align:left;"> Academy of Management </td>
   <td style="text-align:left;"> 2009-12 </td>
   <td style="text-align:left;"> Huang Kenneth G. ; Murray Fiona E. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Why and When Leaders’ Affective States Influence Employee Upward Voice </td>
   <td style="text-align:left;"> Academy of Management Journal </td>
   <td style="text-align:left;"> 10.5465/amj.2013.1082 </td>
   <td style="text-align:left;"> 0001-4273,1948-0989 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 60 </td>
   <td style="text-align:left;"> 238-263 </td>
   <td style="text-align:left;"> Academy of Management </td>
   <td style="text-align:left;"> 2017-02 </td>
   <td style="text-align:left;"> Liu Wu ; Song Zhaoli ; Li Xian ; Liao Zhenyu </td>
  </tr>
  <tr>
   <td style="text-align:left;"> A supercongruence motivated by the Legendre family of elliptic curves </td>
   <td style="text-align:left;"> Mathematical Notes </td>
   <td style="text-align:left;"> 10.1134/s0001434610090324 </td>
   <td style="text-align:left;"> 0001-4346,1573-8876 </td>
   <td style="text-align:left;"> 3-4 </td>
   <td style="text-align:left;"> 88 </td>
   <td style="text-align:left;"> 599-602 </td>
   <td style="text-align:left;"> Pleiades Publishing Ltd </td>
   <td style="text-align:left;"> 2010-10 </td>
   <td style="text-align:left;"> Chan Heng Huat ; Long Ling ; Zudilin V. V. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Do Supplementary Sales Forecasts Increase the Credibility of Financial Analysts’ Earnings Forecasts? </td>
   <td style="text-align:left;"> The Accounting Review </td>
   <td style="text-align:left;"> 10.2308/accr.2010.85.6.2047 </td>
   <td style="text-align:left;"> 0001-4826,1558-7967 </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> 85 </td>
   <td style="text-align:left;"> 2047-2074 </td>
   <td style="text-align:left;"> American Accounting Association </td>
   <td style="text-align:left;"> 2010-11-01 </td>
   <td style="text-align:left;"> Keung Edmund C. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 811 Charnley hips followed for 3–17 years </td>
   <td style="text-align:left;"> Acta Orthopaedica Scandinavica </td>
   <td style="text-align:left;"> 10.3109/17453679308993619 </td>
   <td style="text-align:left;"> 0001-6470 </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> 64 </td>
   <td style="text-align:left;"> 252-256 </td>
   <td style="text-align:left;"> Informa UK Limited </td>
   <td style="text-align:left;"> 1993-01 </td>
   <td style="text-align:left;"> Dall Desmond M ; Learmonth Ian D ; Solomon Michael ; Davenport J Michael </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Detection and monitoring of progressive degeneration of osteoarthritic cartilage by MRI </td>
   <td style="text-align:left;"> Acta Orthopaedica Scandinavica </td>
   <td style="text-align:left;"> 10.3109/17453679509157668 </td>
   <td style="text-align:left;"> 0001-6470 </td>
   <td style="text-align:left;"> sup266 </td>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> 130-138 </td>
   <td style="text-align:left;"> Informa UK Limited </td>
   <td style="text-align:left;"> 1995-01 </td>
   <td style="text-align:left;"> Tyler Jenny A ; Watson Paul J ; Koh Hwee-Ling ; Herrod Nicholas J ; Robson Matthew ; Hall Laurance D </td>
  </tr>
  <tr>
   <td style="text-align:left;"> A Comparison of In-Room and Video Ratings of Team Behaviors of Students in Interprofesional Teams </td>
   <td style="text-align:left;"> American Journal of Pharmaceutical Education </td>
   <td style="text-align:left;"> 10.5688/ajpe6487 </td>
   <td style="text-align:left;"> 0002-9459,1553-6467 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> ajpe6487 </td>
   <td style="text-align:left;"> American Journal of Pharmaceutical Education </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Lie Désirée ; Richter-Lagha Regina ; (Sarah) Ma Sae Byul </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Comparison of Attitudes, Beliefs, and Resource-seeking Behavior for CAM Among First- and Third-Year Czech Pharmacy Students </td>
   <td style="text-align:left;"> American Journal of Pharmaceutical Education </td>
   <td style="text-align:left;"> 10.5688/aj720224 </td>
   <td style="text-align:left;"> 0002-9459,1553-6467 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 72 </td>
   <td style="text-align:left;"> 24 </td>
   <td style="text-align:left;"> American Journal of Pharmaceutical Education </td>
   <td style="text-align:left;"> 2008-09 </td>
   <td style="text-align:left;"> Pokladnikova Jitka ; Lie Desiree </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Using a Human Patient Simulation Mannequin to Teach Interdisciplinary Team Skills to Pharmacy Students </td>
   <td style="text-align:left;"> American Journal of Pharmaceutical Education </td>
   <td style="text-align:left;"> 10.5688/aj710351 </td>
   <td style="text-align:left;"> 0002-9459,1553-6467 </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> 71 </td>
   <td style="text-align:left;"> 51 </td>
   <td style="text-align:left;"> American Journal of Pharmaceutical Education </td>
   <td style="text-align:left;"> 2007-09 </td>
   <td style="text-align:left;"> Fernandez Rosemarie ; Parker Dennis ; Kalus James S. ; Miller Douglas ; Compton Scott </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Biology of the Bee Hoplitis (Hoplitis) monstrabilis Tkalců and Descriptions of Its Egg and Larva (Megachilidae: Megachilinae: Osmiini) </td>
   <td style="text-align:left;"> American Museum Novitates </td>
   <td style="text-align:left;"> 10.1206/646.1 </td>
   <td style="text-align:left;"> 0003-0082,1937-352X </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> 3645 </td>
   <td style="text-align:left;"> 1-12 </td>
   <td style="text-align:left;"> American Museum of Natural History (BioOne sponsored) </td>
   <td style="text-align:left;"> 2009-07-25 </td>
   <td style="text-align:left;"> Rozen Jerome G. ; Özbek Hikmet ; Ascher John S. ; Rightmyer Molly G. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Nests, Petal Usage, Floral Preferences, and Immatures of Osmia (Ozbekosmia) avosetta (Megachilidae: Megachilinae: Osmiini), Including Biological Comparisons with Other Osmiine Bees </td>
   <td style="text-align:left;"> American Museum Novitates </td>
   <td style="text-align:left;"> 10.1206/701.1 </td>
   <td style="text-align:left;"> 0003-0082 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> 3680 </td>
   <td style="text-align:left;"> 1-22 </td>
   <td style="text-align:left;"> American Museum of Natural History (BioOne sponsored) </td>
   <td style="text-align:left;"> 2010-03-04 </td>
   <td style="text-align:left;"> Rozen Jerome G. ; Özbek Hikmet ; Ascher John S. ; Sedivy Claudio ; Praz Christophe ; Monfared Alireza ; Müller Andreas </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Larval Diversity in the Bee GenusMegachile(Hymenoptera: Apoidea: Megachilidae) </td>
   <td style="text-align:left;"> American Museum Novitates </td>
   <td style="text-align:left;"> 10.1206/3863.1 </td>
   <td style="text-align:left;"> 0003-0082,1937-352X </td>
   <td style="text-align:left;"> 3863 </td>
   <td style="text-align:left;"> 3863 </td>
   <td style="text-align:left;"> 1-16 </td>
   <td style="text-align:left;"> American Museum of Natural History (BioOne sponsored) </td>
   <td style="text-align:left;"> 2016-09-23 </td>
   <td style="text-align:left;"> Rozen Jerome G. ; Ascher John S. ; Kamel Soliman M. ; Mohamed Kariman M. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Influence of orthodontic adhesives and clean-up procedures on the stain susceptibility of enamel after debonding </td>
   <td style="text-align:left;"> The Angle Orthodontist </td>
   <td style="text-align:left;"> 10.2319/062610-350.1 </td>
   <td style="text-align:left;"> 0003-3219,1945-7103 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 81 </td>
   <td style="text-align:left;"> 334-340 </td>
   <td style="text-align:left;"> The Angle Orthodontist (EH Angle Education &amp; Research Foundation) </td>
   <td style="text-align:left;"> 2011-03 </td>
   <td style="text-align:left;"> Joo Hyun-Jin ; Lee Yong-Keun ; Lee Dong-Yul ; Kim Yae-Jin ; Lim Yong-Kyu </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Test-retest reliability of smile tasks using three-dimensional facial topography </td>
   <td style="text-align:left;"> The Angle Orthodontist </td>
   <td style="text-align:left;"> 10.2319/062617-425.1 </td>
   <td style="text-align:left;"> 0003-3219,1945-7103 </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> 88 </td>
   <td style="text-align:left;"> 319-328 </td>
   <td style="text-align:left;"> The Angle Orthodontist (EH Angle Education &amp; Research Foundation) </td>
   <td style="text-align:left;"> 2018-05-01 </td>
   <td style="text-align:left;"> Tanikawa Chihiro ; Takada Kenji </td>
  </tr>
</tbody>
</table></div>

#Note
Various resources and lots of Googling were involved in developing the above script, please feel free to contact me if you have suggestions to further improve it.

- https://ropensci.org/

- https://ciakovx.github.io/rcrossref.html
