install.packages("tidyverse")
install.packages("rvest")
install.packages("httr")
library(utils)
library(tidyverse)
library(rvest)
library(httr)




results <- read_html('https://bararanonline.com/')





# Links for all the letters except for "Ր" and "և" (not used in the beginning of words)
link1 <- 'https://bararanonline.com/letter/%D5%A1' # For letter "Ա"
link2 <- 'https://bararanonline.com/letter/%D5%A2' # For letter "Բ"
link3 <- 'https://bararanonline.com/letter/%D5%A3' # For letter "Գ"
link4 <- 'https://bararanonline.com/letter/%D5%A4' # For letter "Դ"
link5 <- 'https://bararanonline.com/letter/%D5%A5' # For letter "Ե"
link6 <- 'https://bararanonline.com/letter/%D5%A6' # For letter "Զ"
link7 <- 'https://bararanonline.com/letter/%D5%A7' # For letter "Է"
link8 <- 'https://bararanonline.com/letter/%D5%A8' # For letter "Ը"
link9 <- 'https://bararanonline.com/letter/%D5%A9' # For letter "Թ"
link10 <- 'https://bararanonline.com/letter/%D5%AA' # For letter "Ժ"
link11 <- 'https://bararanonline.com/letter/%D5%AB' # For letter "Ի"
link12 <- 'https://bararanonline.com/letter/%D5%AC' # For letter "Լ"
link13 <- 'https://bararanonline.com/letter/%D5%AD' # For letter "Խ"
link14 <- 'https://bararanonline.com/letter/%D5%AE' # For letter "Ծ"
link15 <- 'https://bararanonline.com/letter/%D5%AF' # For letter "Կ"
link16 <- 'https://bararanonline.com/letter/%D5%B0' # For letter "Հ"
link17 <- 'https://bararanonline.com/letter/%D5%B1' # For letter "Ձ"
link18 <- 'https://bararanonline.com/letter/%D5%B2' # For letter "Ղ"
link19 <- 'https://bararanonline.com/letter/%D5%B3' # For letter "Ճ"
link20 <- 'https://bararanonline.com/letter/%D5%B4' # For letter "Մ"
link21 <- 'https://bararanonline.com/letter/%D5%B5' # For letter "Յ"
link22 <- 'https://bararanonline.com/letter/%D5%B6' # For letter "Ն"
link23 <- 'https://bararanonline.com/letter/%D5%B7' # For letter "Շ"
link24 <- 'https://bararanonline.com/letter/%D5%B8' # For letter "Ո"
link25 <- 'https://bararanonline.com/letter/%D5%B9' # For letter "Չ"
link26 <- 'https://bararanonline.com/letter/%D5%BA' # For letter "Պ"
link27 <- 'https://bararanonline.com/letter/%D5%BB' # For letter "Ջ"
link28 <- 'https://bararanonline.com/letter/%D5%BC' # For letter "Ռ"
link29 <- 'https://bararanonline.com/letter/%D5%BD' # For letter "Ս"
link30 <- 'https://bararanonline.com/letter/%D5%BE' # For letter "Վ"
link31 <- 'https://bararanonline.com/letter/%D5%BF' # For letter "Տ"
link32 <- 'https://bararanonline.com/letter/%D6%81' # For letter "Ց"
link33 <- 'https://bararanonline.com/letter/%D5%B8%D6%82' # For letter "ՈՒ"
link34 <- 'https://bararanonline.com/letter/%D6%83' # For letter "Փ"
link35 <- 'https://bararanonline.com/letter/%D6%84' # For letter "Ք"
link36 <- 'https://bararanonline.com/letter/%D6%85' # For letter "Օ"
link37 <- 'https://bararanonline.com/letter/%D6%86' # For letter "Ֆ"




links <- c(link1, link2, link3, link4, link5, link6, link7, link8, link9, link10,
           link11, link12, link13, link14, link15, link16, link17, link18, link19, link20,
           link21, link22, link23, link24, link25, link26, link27, link28, link29, link30,
           link31, link32, link33, link34, link35, link36, link37)

# Create an empty tibble
df <- tibble()


# For each letter (link) get number of pages
# It usually shows 2,3,4,5,6,7,8 ... N (can be different for each letter)
# As we have 37 letters, we would expect ~400 observations (~10 for each)
# Take a quick look at the tail of df, to make sure the links for the last letter are scrapped as well
# If not all the links are in the df, that can mean that the website blocked your dot
# If that's the case, you can use Sys.sleep(2) within for loop to 
for (i in links){
  
  results <- read_html(i)
  
  navigation <- results %>%
    html_nodes(".page-link") %>%
    html_attr("href")
  
  df <- rbind(df, tibble(navigation))
  
}



navigation_df <- df %>%
  filter(navigation != "#") %>% # Remove cases when we have # instead of links
  mutate(page_no = as.numeric(str_extract(navigation, "\\d+$"))) %>% # Separate page numbers from links as new variable
  mutate(navigation = str_extract(navigation, ".*(?=\\=\\d+$)")) %>% # Remove all the signs after word "page" for each link
  mutate(page_no = as.integer(str_replace(page_no, "^2$", "1"))) # Convert 2 to 1 so that page numbers start from 1 not 2



# Create expand function to run separately for for all the letters
expand_function <- function(i) {
  
  expand(i, navigation, page_no = full_seq(page_no, 1))
  
}

# First I split the data into small tibbles for each letter
# The reason is that some letters have fewer pages (e.g., 3), but others have more pages(e.g., 100)
# I want to create links by the number of pages that exist for each letter
navigation_df_cont <- navigation_df %>% 
  split(.$navigation) %>% # Separate by groups
  map(~expand_function(.)) %>% # Use expand function for each tibble
  bind_rows() %>% # Combining the tibbles together into 1 large tibble
  unite(url, navigation, page_no, sep = '=', remove = FALSE) # Create final URLs

# We need to have 2047 diverse links
dim(navigation_df_cont)


# Each page contains 75 words and last pages are not always full
# Assuming that last pages are always not full
# We expect to have at least (2047-37) * 75 = 150,750 words
nav_results_list <- tibble(
  html_results = map(navigation_df_cont$url,
                     ~{
                       
                       # Sys.sleep(2)
                       
                       .x %>% read_html()
                     }),
  summary_url = navigation_df_cont$url
)


results_by_page <- tibble(summary_url = nav_results_list$summary_url,
                          
                          name = 
                            map(nav_results_list$html_results,
                                ~ .x %>%
                                  html_nodes('.col-sm-4') %>%
                                  html_text))




df_all_upd <- results_by_page %>%
  mutate(name = str_squish(name))

# Store the scrapped file
write.csv(df_all_upd, "Output_All_UPD1.csv", row.names=TRUE)


