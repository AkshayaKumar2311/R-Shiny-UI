# Student ID : 31021301
# Student Name : Akshaya Kumar Chandrasekaran
# Please Note : Cleaned data set is around 150+MB. 19 COulmns and 850,000+ Rows

#loading the required libraries

library(glue)
library(dplyr)
library(magrittr)
library(quantreg)
library(tidyverse)
library(naniar)
library(hrbrthemes)
library(wordcloud)
library(RColorBrewer)
library(tm)
library(ggplot2)
library(readr)
library(scales)
library(plyr)
library(tidyr)
library(plotrix)
library(wordcloud2)
library(choroplethr)
library(choroplethrMaps)
library(viridis)
library(patchwork)
library(hrbrthemes)
library(circlize)
library(networkD3)
library(tidyverse)
library(treemap)
library(sunburstR)
library(shiny)
library(plotly)

#Loading State Data Set

#Loading the clean data set
combined_dataSet <- read.csv("dataSetProject.csv")
#removing the unwanted column and data preparation
combined_dataSet <- combined_dataSet[-1]
combined_dataSet$emp_length <- as.character(combined_dataSet$emp_length)
combined_dataSet$default_ind <- as.factor(combined_dataSet$default_ind)

###Data Prep for graphs
###########################FOR BOX PLOT###################################

forboxandbar <- subset(combined_dataSet,combined_dataSet$emp_title=='Registered Nurse')
forboxandbar <- rbind(forboxandbar,subset(combined_dataSet,combined_dataSet$emp_title=='Manager'))
forboxandbar <- rbind(forboxandbar,subset(combined_dataSet,combined_dataSet$emp_title=='Teacher'))
forboxandbar <- rbind(forboxandbar,subset(combined_dataSet,combined_dataSet$emp_title=='Owner'))
forboxandbar <- rbind(forboxandbar,subset(combined_dataSet,combined_dataSet$emp_title=='Supervisor'))
forboxandbar <- rbind(forboxandbar,subset(combined_dataSet,combined_dataSet$emp_title=='Sales'))
forboxandbar <- rbind(forboxandbar,subset(combined_dataSet,combined_dataSet$emp_title=='Project Manager'))
forboxandbar <- rbind(forboxandbar,subset(combined_dataSet,combined_dataSet$emp_title=='Driver'))
forboxandbar <- rbind(forboxandbar,subset(combined_dataSet,combined_dataSet$emp_title=='Director'))
forboxandbar <- rbind(forboxandbar,subset(combined_dataSet,combined_dataSet$emp_title=='Engineer'))
forboxandbar <- rbind(forboxandbar,subset(combined_dataSet,combined_dataSet$emp_title=='President'))
forboxandbar <- rbind(forboxandbar,subset(combined_dataSet,combined_dataSet$emp_title=='Vice President'))
forboxandbar <- rbind(forboxandbar,subset(combined_dataSet,combined_dataSet$emp_title=='Attorney'))
forboxandbar <- rbind(forboxandbar,subset(combined_dataSet,combined_dataSet$emp_title=='Programmer Analyst'))
forboxandbar <- rbind(forboxandbar,subset(combined_dataSet,combined_dataSet$emp_title=='Machine operator'))
forboxandbar$issue_d <- as.factor(forboxandbar$issue_d)

######################################CHOROPLETH MAP##################################################

map <- combined_dataSet %>% dplyr::select("name","default_ind")
# prepare the data
map$region <- tolower(map$name)
map$value <- map$default_ind

only0 <- subset(map,map$default_ind==0)
only1 <- subset(map,map$default_ind==1)

data(continental_us_states)

for0onlycount <- plyr::count(only0$region)
for1onlycount <- plyr::count(only1$region)


for0onlycount$region <- for0onlycount$x
for0onlycount$value <- for0onlycount$freq
for1onlycount$region <- for1onlycount$x
for1onlycount$value <- for1onlycount$freq

#######################SCATTER PLOT###################################

forScatter <- subset(combined_dataSet,combined_dataSet$annual_inc<250000)

#########################FOR DONUT CHART############################################

fordonutPrep <- subset(combined_dataSet,combined_dataSet$emp_title=='Registered Nurse')
fordonutPrep <- rbind(fordonutPrep,subset(combined_dataSet,combined_dataSet
                                                $emp_title=='Manager'))
fordonutPrep <- rbind(fordonutPrep,subset(combined_dataSet,combined_dataSet
                                                $emp_title=='Teacher'))
fordonutPrep <- rbind(fordonutPrep,subset(combined_dataSet,combined_dataSet
                                                $emp_title=='Owner'))
fordonutPrep <- rbind(fordonutPrep,subset(combined_dataSet,combined_dataSet
                                                $emp_title=='Supervisor'))
fordonutPrep <- rbind(fordonutPrep,subset(combined_dataSet,combined_dataSet
                                                $emp_title=='Sales'))
fordonutPrep <- rbind(fordonutPrep,subset(combined_dataSet,combined_dataSet$emp_title=='Project 
                                                Manager'))
fordonutPrep <- rbind(fordonutPrep,subset(combined_dataSet,combined_dataSet
                                                $emp_title=='Driver'))
fordonutPrep <- rbind(fordonutPrep,subset(combined_dataSet,combined_dataSet
                                                $emp_title=='Director'))
fordonutPrep <- rbind(fordonutPrep,subset(combined_dataSet,combined_dataSet
                                                $emp_title=='Engineer'))
fordonutPrep <- rbind(fordonutPrep,subset(combined_dataSet,combined_dataSet
                                                $emp_title=='President'))
fordonutPrep <- rbind(fordonutPrep,subset(combined_dataSet,combined_dataSet$emp_title=='Vice 
                                                President'))
fordonutPrep <- rbind(fordonutPrep,subset(combined_dataSet,combined_dataSet
                                                $emp_title=='Attorney'))
fordonutPrep <- rbind(fordonutPrep,subset(combined_dataSet,combined_dataSet
                                                $emp_title=='Programmer Analyst'))
fordonutPrep <- rbind(fordonutPrep,subset(combined_dataSet,combined_dataSet$emp_title=='Machine 
                                                operator'))


fordonut_emp_title <- count(fordonutPrep$emp_title)


# Compute percentages
fordonut_emp_title$fraction = fordonut_emp_title$freq / sum(fordonut_emp_title$freq)
# Compute the cumulative percentages (top of each rectangle)
fordonut_emp_title$ymax = cumsum(fordonut_emp_title$fraction)
# Compute the bottom of each rectangle
fordonut_emp_title$ymin = c(0, head(fordonut_emp_title$ymax, n=-1))
# Compute label position
fordonut_emp_title$labelPosition <- (fordonut_emp_title$ymax + fordonut_emp_title$ymin) / 2
# Compute a good label
fordonut_emp_title$label <- paste0(fordonut_emp_title$x, "\n value: ", fordonut_emp_title$freq)



#Grade
fordonut_grade <- count(fordonutPrep$grade)


# Compute percentages
fordonut_grade$fraction = fordonut_grade$freq / sum(fordonut_grade$freq)
# Compute the cumulative percentages (top of each rectangle)
fordonut_grade$ymax = cumsum(fordonut_grade$fraction)
# Compute the bottom of each rectangle
fordonut_grade$ymin = c(0, head(fordonut_grade$ymax, n=-1))
# Compute label position
fordonut_grade$labelPosition <- (fordonut_grade$ymax + fordonut_grade$ymin) / 2
# Compute a good label
fordonut_grade$label <- paste0(fordonut_grade$x, "\n value: ", fordonut_grade$freq)


########################SANKEY##########################################
forSankey <- count(combined_dataSet$emp_title)

forSankey_1 <- subset(forSankey,forSankey$freq>2000)
forSankey_1$x <- as.character(forSankey_1$x )
forSankey_1$x  <- ifelse(forSankey_1$x=="","Not Willing",forSankey_1$x)
unique(forSankey_1$x)
forSankey_1$x  <- as.factor(forSankey_1$x )

forsankeysubset <- subset(combined_dataSet,combined_dataSet$emp_title=='Registered Nurse')
forsankeysubset <- rbind(forsankeysubset,subset(combined_dataSet,combined_dataSet
                                                $emp_title=='Manager'))
forsankeysubset <- rbind(forsankeysubset,subset(combined_dataSet,combined_dataSet
                                                $emp_title=='Teacher'))
forsankeysubset <- rbind(forsankeysubset,subset(combined_dataSet,combined_dataSet
                                                $emp_title=='Owner'))
forsankeysubset <- rbind(forsankeysubset,subset(combined_dataSet,combined_dataSet
                                                $emp_title=='Supervisor'))
forsankeysubset <- rbind(forsankeysubset,subset(combined_dataSet,combined_dataSet
                                                $emp_title=='Sales'))
forsankeysubset <- rbind(forsankeysubset,subset(combined_dataSet,combined_dataSet$emp_title=='Project 
                                                Manager'))
forsankeysubset <- rbind(forsankeysubset,subset(combined_dataSet,combined_dataSet
                                                $emp_title=='Driver'))
forsankeysubset <- rbind(forsankeysubset,subset(combined_dataSet,combined_dataSet
                                                $emp_title=='Director'))
forsankeysubset <- rbind(forsankeysubset,subset(combined_dataSet,combined_dataSet
                                                $emp_title=='Engineer'))
forsankeysubset <- rbind(forsankeysubset,subset(combined_dataSet,combined_dataSet
                                                $emp_title=='President'))
forsankeysubset <- rbind(forsankeysubset,subset(combined_dataSet,combined_dataSet$emp_title=='Vice 
                                                President'))
forsankeysubset <- rbind(forsankeysubset,subset(combined_dataSet,combined_dataSet
                                                $emp_title=='Attorney'))
forsankeysubset <- rbind(forsankeysubset,subset(combined_dataSet,combined_dataSet
                                                $emp_title=='Programmer Analyst'))
forsankeysubset <- rbind(forsankeysubset,subset(combined_dataSet,combined_dataSet$emp_title=='Machine 
                                                operator'))

sankeyColumns <- forsankeysubset %>% select(emp_title,grade,default_ind)
sankeyColumns$emp_title<- as.character(sankeyColumns$emp_title)
sankeyColumns$grade<- as.character(sankeyColumns$grade)


filteredColumnsByFreq <-  sankeyColumns %>% group_by(emp_title,grade,default_ind) %>% 
  dplyr::summarize(Freq = n())

links <- data.frame(source = c((filteredColumnsByFreq$emp_title)),
                    target = c((filteredColumnsByFreq$default_ind)),
                    value = c((filteredColumnsByFreq$Freq)))


nodes <- data.frame(name=c(as.character(links$source),as.character(links$target)) %>% unique()) 

links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1
my_color <- 'd3.scaleOrdinal() .domain(["group_A", "group_B","group_C", "group_D", "group_E", 
"group_F", "group_G", "group_H"]) .range(["blue", "blue" , "blue", "red", "red", "yellow", "purple", 
"purple"])'

emp_def <- sankeyNetwork(Links = links, Nodes = nodes, Source = "IDsource", Target = "IDtarget", 
                   Value = "value", NodeID = "name", colourScale=my_color)

##Emp title to Grade

links0 <- data.frame(source = c((filteredColumnsByFreq$emp_title)),
                    target = c((filteredColumnsByFreq$grade)),
                    value = c((filteredColumnsByFreq$Freq)))


nodes0 <- data.frame(name=c(as.character(links0$source),as.character(links0$target)) %>% unique()) 

links0$IDsource <- match(links0$source, nodes0$name)-1 
links0$IDtarget <- match(links0$target, nodes0$name)-1
my_color <- 'd3.scaleOrdinal() .domain(["group_A", "group_B","group_C", "group_D", "group_E", 
"group_F", "group_G", "group_H"]) .range(["blue", "blue" , "blue", "red", "red", "yellow", "purple", 
"purple"])'

emp_grade <- sankeyNetwork(Links = links0, Nodes = nodes0, Source = "IDsource", Target = "IDtarget", 
                         Value = "value", NodeID = "name", colourScale=my_color)

##Grade to defaulters

links1 <- data.frame(source = c((filteredColumnsByFreq$grade)),
                    target = c((filteredColumnsByFreq$default_ind)),
                    value = c((filteredColumnsByFreq$Freq)))


nodes1 <- data.frame(name=c(as.character(links1$source),as.character(links1$target)) %>% unique()) 

links1$IDsource <- match(links1$source, nodes1$name)-1 
links1$IDtarget <- match(links1$target, nodes1$name)-1
my_color <- 'd3.scaleOrdinal() .domain(["group_A", "group_B","group_C", "group_D", "group_E", 
"group_F", "group_G", "group_H"]) .range(["blue", "blue" , "blue", "red", "red", "yellow", "purple", 
"purple"])'

grade_def <- sankeyNetwork(Links = links1, Nodes = nodes1, Source = "IDsource", Target = "IDtarget", 
                         Value = "value", NodeID = "name", colourScale=my_color)

##########################SUN BURST#########################################
forsunburst <- combined_dataSet %>% select('issue_d','name','purpose','default_ind')

sunburstfreq <-  forsunburst %>% group_by(issue_d,purpose,default_ind) %>% dplyr::summarize(Freq = n())

data_defaulters1 <- forsunburst %>%
  mutate(path = paste(issue_d,purpose,default_ind,name,sep="-")) %>%
  dplyr::select(path,default_ind)

#q1_ds <- subset(data_defaulters1,data_defaulters1$default_ind==1)
#q0_ds <- subset(data_defaulters1,data_defaulters1$default_ind==0)

q1 <- sunburst(data_defaulters1, legend=FALSE)
#q0 <- sunburst(q0_ds, legend=FALSE)

#q0

##################################PIE CHART##################################
forpieChart <- plyr::count(combined_dataSet$purpose)


#purpose
forpieChart <- forpieChart %>% 
  arrange(desc(x)) %>%
  mutate(prop = freq / sum(forpieChart$freq) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

treeMap <- (subset(combined_dataSet,(combined_dataSet$purpose!="debt_consolidation"))) #or finalData$purpose!="credit_card"))
treeMap <- subset(treeMap,treeMap$purpose!="credit_card")
treeMap$purpose <- factor(treeMap$purpose,levels = (unique(treeMap$purpose)))

treeMapCount <- count(treeMap$purpose)

subset_pie <- treeMapCount %>% 
  arrange(desc(x)) %>%
  mutate(prop = freq / sum(treeMapCount$freq) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )


##############################WORD CLOUD#######################################

forWordCloud2 <- plyr::count(combined_dataSet$emp_title)
forWordCloud_2 <- subset(forWordCloud2,forWordCloud2$freq>300)
forWordCloud_2$x <- as.character(forWordCloud_2$x )
unique(forWordCloud_2$x)
forWordCloud_2$x  <- as.factor(forWordCloud_2$x )


forWordCloud_purpose <- plyr::count(combined_dataSet$purpose)

forWordCloud_purpose <- (subset(forWordCloud_purpose,(forWordCloud_purpose$x!="debt_consolidation"))) #or finalData$purpose!="credit_card"))
forWordCloud_purpose <- subset(forWordCloud_purpose,forWordCloud_purpose$x!="credit_card")

#################################EMPLOYMENT LENGTH###########################################

theTable <- within(combined_dataSet, 
                   Position <- factor(combined_dataSet$emp_length, 
                                      levels=names(sort(table(combined_dataSet$emp_length), 
                                                        decreasing=TRUE))))
theTable$emp_length <- factor(theTable$emp_length,levels = c("< 1 year","1 year","2 years","3 years","4 years","5 years","6 years","7 years","8 years","9 years","10+ years"),ordered = T)


###################################DATA PREP FOR CHARTS ENDS#########################################


##########################################BUILD UI###############################################
#navbarpage - for creating a page
#tags -> for using html tags
#br() -> break (creates empty line)
#hr() -> makes a horizontal line
#p() -> Paragraph in UI
#Title panel -> in UI for creating a heading
#Tab Panel -> For creatig a new tab 


ui <-
  navbarPage(
    "ANALYSIS ON LOAN APPLICANTS",
    inverse = TRUE,
    collapsible = TRUE,
    #ui side functions
    tabPanel(
      #################FIRST PAGE WELCOME PAGE############################
      "Welcome Page",
      titlePanel(h4("Visualization Project on Loan Applicants")),
      hr(),
      p(tags$li(strong("Introduction:")),br(),
        "Almost all of us apply for loan for some of the essential needs and requirements
    in an intention to repay to the loan within the stipulated date and time. 
    However, few of them are unfortunately not able to repay the loan as planned and  
      end up being a defaulter.",align = 'justify'),
      p("So what loan providers generally do is collect required basic information about 
        the person to assess the application and then decide to proceed with the application or not.",align = 'justify'),
      br(),
      p(tags$li(
        strong("Problem Summary:")),br(), "The more the number of defaulters, the more the loss is for the loan providers. 
        Hence before processing and granting any type loan for the applicants, 
        the loan providers do some ground works and analyse if they can proceed. 
        Analysis and explorations are done to identify any such factors which will 
        influence the granting on loan.",align = 'justify'
        ),
      br(),
      p(tags$li(
        strong("Data Set:")),br(), "Tabular data consisting of 855696 records and 74 variables. (All kinds of variables such as 
      location, continuous, categorical and simple text with punctuations and 
      numbers are present in the dataset)",br(),
        (tags$a(href = "https://www.kaggle.com/sonujha090/xyzcorp-lendingdata", "Link to Data set")
      )),
      br(),
      tags$li(p("Subset of the data is taken for analysis purpose. Data from the source was unclean. 
        Data was cleaned as part of data wrangling, data cleaning and then sunset of the data is done.")
      ),
      p(tags$li(
        strong("Intended Audience:")), "People with little knowledge on banking process , data analyst/scientist aspirants et.al.,"
      )),
    tabPanel(
      #################SECOND PAGE INTRODUCTION PAGE############################
      "Introduction",
      titlePanel(h4("Introduction")),
      p(
        "Applying" ,strong("loan for immediate/essential financial needs"), 
        "is almost done by everyone. The only factor that varies is the reason and the amount required.
        There are many factors that are involved in determining if the applicant is going repay the loan or not.
        Through this analysis process, I have tried to find if there are any correlations which has impact on the loan defaulters.",
        align = "justify"),
      tags$i(
        "From the below map, it can be seen that we are analysing applicants from",strong("United States of America.")
      ),
      p(
        "Below", strong("choropleth map"), "describes the number of applicants from the country.",
        hr(),
        tags$i(" Note: "),
        "850000+ applicant's record were analysed for the report.",
        align = "justify"
      ),
      fluidRow(
        column(2,radioButtons(
          "defornotradio",
          label = "Status",
          c("Non - Defaulters" = 0,
            "Defaulters" = 1),selected = 0
        )),
        column(8, plotOutput("choroplethmap")),
        column(
          2,
          p(
            "There are almost", strong("35% of applicants from the states New York, Texas, California and Florida"), "as the sum of applicants 
            from these states alone were", strong("more than approximately 300,000.."),
            align = "justify"
          ),
          p(
            "An interesting fact that applicants from the 
            mentioned states", strong("North Dakota(ND) and Maine(ME)"), "have not defaulted a loan."
          )
          )
        )),
    navbarMenu(
      #################THIRD PAGE ANALYSIS PAGE############################
      "Analysis of Applicants",
      tabPanel(
        #################THIRD PAGE FIRST TAB############################
        "Annual Income of the Applicant",
        titlePanel(h4("Analysis on Annual Income of the applicants")),
        hr(),
        fluidRow(column(2,radioButtons(
          "annualincom",
          label = "Income",
          c("Full-Data Set" = 0,
            "< 1000K" = 1,
            "< 250K" = 2),selected = 2
        )),
        column(
          5,
          fluidRow(
            p(
              "Annual Income is one of the most important criteria looked upon for loan applicants.
              If the applicant's annual income is more, the chance that the applicant is not going to 
              be a defaulter is more and vice versa. Here in this graph, the applicant's annual income
              with respect to their home owner ship is displayed.",
              align = "justify"
            ),
            tags$i(
              "Explore the applicant's annual income --> " , actionButton("inc_diag", "Annual Income Insight") 
            ),
            tags$li(
              "Annual income based on the home ownership"
            )
          ),
          hr(),
          fluidRow(plotOutput("boxplotsal"))
          ),
        column(5,fluidRow(
        hr(),
        fluidRow(plotOutput("bartop")),
        p(
          "There are more than 150+ applicant's employment title. Top 12 has been chosen and the number of
          count for each title is displayed in bar chart.",
          align = "justify"
        )
        ))
        )
        ),
      tabPanel(
        #################THIRD PAGE SECOND TAB############################
        "Employment Title & Defaulters",
        titlePanel(h4("Analysis based on Employee title and Grade of the applicants")),
        hr(),
        p(
          "For each of the applicant's employee title based on the annual income, they have been assigned a 
          grade. Top 15 Applicant's Employee title have been taken and analysed whether the applicant has 
          got a chance of defaulting a loan or not.",
          align = "justify"
        ),
        tags$i("Click here to know more about the choosen relationship -->", actionButton("sankey_diag", "Emp Title Insight")),
        hr(),
        fluidRow(column(2, radioButtons(
          "sankeyselect",
          label = "Relation between",
          c("Applicant's Employee Title & Defaulters " = 0,
            "Applicant's Employee Title & Grade" = 1,
            "Grade & Defaulters" = 2),selected = 2
        )),
                 column(5, plotOutput("donutsemp")),
                 column(5, sankeyNetworkOutput("sankeyempgrade")))
      ),
      tabPanel(
        #################THIRD PAGE THIRD TAB############################
        "Year-wise Purpose of Loan",
      titlePanel(
        h4("Analysis based on Employee title of applicant year-wise")
      ),
      hr(),
      p(
        "As the economy and the cost of living is increasing every year, need for financial aid also 
        gets increased. In this page, the number of applicant's employee title based on each year is
        analysed. How their behaviour is and whether they have defaulted the loan or not.",
        align = "justify"
      ),
      tags$li(
        "Explore the trend by hovering on each year, the emp title to get the % of defaulters"
      ),
      tags$li(
        "Explore the purpose by clicking here --> ",actionButton("pie_diag", "Purpose Insight")
      ),
      hr(),
      fluidRow(column(2, radioButtons(
        "halforpie",
        label = "Display Option",
        c("Full Data set " = 0,
          "Subset debt and Credit" = 1),selected = 0
      )),
               column(5, plotOutput("pieorhalf")),
               column(5, sunburstOutput("sundefault",height = "550",width = "80%")))
      ),
      tabPanel(
        #################THIRD PAGE FOURTH TAB############################
        "Top Employee Title of Applicants",
        titlePanel(
          h4("Analysis based on Employee title")
        ),
        hr(),
        p(
          "The applicant's employee title also plays a major role in deciding if the person is going
          to default or not. In this page, we can see the word cloud of applicant's employee title.
          The bigger the word, the more number of applicants with the same employee title. 
          We can also change the frequency and the repetition of words.",
          align = "justify"
        ),
        tags$i(
          "Explore the trend by changing the number of words frequency of the emp title."
        ),
        hr(),
        fluidRow(column(2, selectInput("wordchoose", "Category: ",
                                       c("Employee Title" = 0,
                                         "Purpose of Loan" = 1),selected = 0),
                        sliderInput("freq","Minimum Frequency : ",min=2000,max=5000,value=2000)),
                 column(8, wordcloud2Output("wordcloudemp")),
                 column(2, textOutput("wordtext"))
      )),
      tabPanel(
        #################THIRD PAGE FIFTH TAB############################
        "Home Ownership and Employment Length",
        titlePanel(
          h4("Analysis based on Employee length and Home ownership")
        ),
        hr(),
        p(
          "The more than applicant's employment length, the more will be the applicant's annual income
          and less will be the chance that the applicant will default the loan. In this page, we are trying
          to find whether there are any correlations between the employment length and defaulters and 
          home ownership and defaulters.",
          align = "justify"
        ),
        tags$i(
          "Explore the trend by clicking here -->",actionButton("scatter_diag", "Home Ownership Insight")
        ),
        hr(),
        fluidRow(column(2, radioButtons(
          "hmeown",
          label = "Type Of Home ownership",
          c("Rent" = "RENT",
            "Mortgage" = "MORTGAGE",
            "Others"= "OTHER",
            "Own" = "OWN"),selected = "MORTGAGE"
        )),
                 column(5, plotOutput("scatterPlothome")),
                 column(5, plotOutput("barchartlength")))
    )
    ),
    tabPanel(
      #################FOURTH PAGE CONCLUSION PAGE############################
      "Conclusion",
      titlePanel(h4("Analysis Summary")),
      hr(),
      p(
        "On analysing various attributes which could affect the applicant to be a defaulter or not, 
        below main attributes has their own significance and influence:",align = 'justify'),
      br(),
      tags$li(
        strong("Annual Income:"), "Applicants with" ,strong("annual income less than 250k are most likely to 
        be a defaulter."), "Also, we cannot access the applicant's application only 
        with their annual income. Other attributes should also be taken into considerations.",align = 'justify'
      ),
      br(),
      tags$li(
        strong("Employment length:"), "The attribute employment length has", strong("no greater influence in deciding"),
      "if the person is going to be a defaulter or not. But one quite interesting fact found is, 
      applicants with more than 10 years of experience are applying for loan. 
      This makes sense because the applicant may have planned their future 
      based on their financial status to make progress in their life",align="justify"
      ),
      br(),
      tags$li(
        strong("Home Ownership:"), "Applicant who are residing in a", strong("mortgaged house or 
        in a rented house are likely to default their loan provided their annual salary is 
        less than 250k."), "Hence only this particular attribute has some influence on 
        determining if the applicant is going to default or not and has greater influence 
        when other attributes are analysed alongside.",align = "justify"
      ),
      br(),
      p(
        "Thank you for providing me with an oppurtunity to learn various tools and languages, enhance 
        the knowledge on how significant visualisation is not only in the field of Data science but also
        in general."
      ),
      h3(tags$i("References:")),
      hr(),
        tags$li(tags$a(href = "http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know", "Word Cloud in R")),
        tags$li(tags$a(href = "https://www.displayr.com/sankey-diagrams-r/", "Sankey Diagram")),
        tags$li(tags$a(href = "https://www.data-to-viz.com/graph/sunburst.html", "Sunburst Diagram")),
        tags$li(tags$a(href = "https://rstudio.com/wp-content/uploads/2016/11/ggplot2-cheatsheet-2.1.pdf", "About GGPLOT 2 graphs")),
        tags$li(tags$a(href = "https://shiny.rstudio.com/images/shiny-cheatsheet.pdf", "About Shiny UI")),
        tags$li(tags$a(href = "https://en.wikipedia.org/wiki/Default_(finance)", "Domain Knowledge")),
        tags$li(tags$a(href = "https://shiny.rstudio.com/reference/shiny/latest/modalDialog.html", "Pop Up Window"))
      
    )
  )
#####################################   UI DESIGN ENDS HERE      #############################


#####################################    SERVER SIDE IMPLEMENTATION   ####################
server <- function(input, output) {
  ####OberseEvent--> Diaglog box action for box plot
  observeEvent(input$inc_diag, {
    if(input$annualincom==0){
      showModal(modalDialog(
        title = "Insights about Annual Income- Full Data set",
        "Applicants with less than 1000K as their annual income are subject to be defaulters.",
        "Since the box-plot was not clearly visible and the stats of a box plot could not be interpreted,
        please choose other two options to know in detail."
      ))
    }else if(input$annualincom==1){
      showModal(modalDialog(
        title = "Insights about Annual Income (<1000k)",
        "Applicants who are salaried less than 750k and who are either living in a rented house or
        has mortgaged their house are likely to be defaulters. 
        Also, people who own the house but are getting less than 500k are also likely to be defaulters."
      ))
    }else{
      showModal(modalDialog(
        title = "Insights about Annual Income (<250k)",
        "It can be seen that the annual salary's 25 percentile (Q1), 
        50 percentile/median(Q2),75 percentile (Q3) were almost the same 
        for both defaulters and non-defaulters"
      ))
    }
  })
  ####OberseEvent--> Diaglog box action for SANKEY DIAGRAM
  observeEvent(input$sankey_diag, {
    if(input$sankeyselect==0){
      showModal(modalDialog(
        title = "Insights about Top 12 Employee Title",
        "It can be seen that in top 12 applicant's employee title, most of the applicants are 
        either teacher or manager.",
        "On the defaulter side, 1 symbolises the non-defaulters and 2 symbolises the defaulters.",
        "Irrespective of the applicant's employee title, only a few have defaulted the loan."
      ))
    }else if(input$sankeyselect==1){
      showModal(modalDialog(
        title = "Applicant's Employee title and Grade",
        "When applying for loan, loan providers assign a grade to the applicants.",
        "It can be seen that most of the applicants have been assigned A,B,C and D.",
        "Few of the applicants have been assigned E,F and G."
      ))
    }else{
      showModal(modalDialog(
        title = "Grade and Defaultment",
        "It can be seen that most of the applicants who are graded in E,F and G have
        defaulted the loan more when compared to applicants who was given A,B,C and D.",
        "Thus can be concluded that if the provider grades a person in E,F and G, they are
        more likely to be a defaulters."
      ))
    }
    })
  ####OberseEvent--> Diaglog box action for PIE CHART
  observeEvent(input$pie_diag, {
    if(input$halforpie==0){
      showModal(modalDialog(
        title = "Insights about Purpose",
        "It can be seen that almost 60% of the applicants have applied loan for the purpose of 
        debt consolidations (debt consolidation is process of getting a loan to close of other loans and manage financially).",
        "Next reason for applying loan is to repay their credit card debts."
      ))
    }else{
      showModal(modalDialog(
        title = "Insights about Purpose removing Debt consolidations and Credit Card",
        "It can be seen that the next major purpose to get the loan was for house improvement,major purchase and other"
      ))
    }
    })
  ####OberseEvent--> Diaglog box action for SCATTER PLOT
  observeEvent(input$scatter_diag, {
    if(input$hmeown=="RENT"){
      showModal(modalDialog(
        title = "Home Ownership : Rent",
        "Applicants whose annual income is less than 250k has their house ownership as 'rented' are
        likely to be defaulters. But their is more than that of applicant's who own the house."
      ))
    }else if(input$hmeown=="MORTGAGE"){
      showModal(modalDialog(
        title = "Home Ownership : Mortgage",
        "Applicants whose annual income is less than 250k has their house ownership as 'mortgaged' are
        likely to be defaulters. But their is more than that of applicant's who own the house."
      ))
    }else if(input$hmeown=="OWN"){
      showModal(modalDialog(
        title = "Home Ownership : Own",
        "Applicants who own their house are also likely to be defaulters but their
        annual income is less when compared to applicants whose house is either 'mortgaged' or 'rented'."
      ))
    }else{
      showModal(modalDialog(
        title = "Home Ownership : Other",
        "Very few applicants have given this option. Proper analysis could not be made since the
        number of defaulters and non-defaulters are same in this case."
      ))
    }
  })
  
  
  
####PAGE TWO#######
  output$choroplethmap <- renderPlot({
    if (input$defornotradio == 0){
    state_choropleth(for0onlycount, 
                     num_colors=9,
                     zoom = continental_us_states) +
      scale_fill_brewer(palette="YlOrBr") +
      labs(title = " Bank Defaulters count in States",
           subtitle = "NON-DEFAULTERS",
           caption = "MAP SHOWING NON-DEFAULTERS COUNT ACROSS THE COUNTRY STATEWISE",
           fill = "Frequency")}
    else {
      state_choropleth(for1onlycount, 
                       num_colors=9,
                       zoom = continental_us_states) +
        scale_fill_brewer(palette="YlOrBr") +
        labs(title = " Bank Defaulters count in States",
             subtitle = "DEFAULTERS",
             caption = "MAP SHOWING DEFAULTERS COUNT ACROSS THE COUNTRY STATEWISE",
             fill = "Frequency")
    }})

  #####PAGE THREE TAB ONE###############
  output$boxplotsal <- renderPlot(
    if (input$annualincom==0){
    ggplot((combined_dataSet),aes(y=annual_inc)) + 
      geom_boxplot(aes(colour=home_ownership)) +
      facet_grid(default_ind~home_ownership) + 
      ggtitle("Annual Income & Defaulters")+
      theme(plot.title = element_text(hjust = 0.5)) + labs(y="Annual Income")+
     scale_x_continuous(breaks = 0)
    }else if(input$annualincom==1){
      ggplot(subset(combined_dataSet,combined_dataSet$annual_inc<1000000),aes(y=annual_inc)) + 
        geom_boxplot(aes(colour=home_ownership)) +
        facet_grid(default_ind~home_ownership) + 
        ggtitle("Annual Income & Defaulters")+
        theme(plot.title = element_text(hjust = 0.5)) + labs(y="Annual Income")+
       scale_x_continuous(breaks = 0)
    }else{
      ggplot(subset(combined_dataSet,combined_dataSet$annual_inc<250000),aes(y=annual_inc)) + 
        geom_boxplot(aes(colour=home_ownership)) +
        facet_grid(default_ind~home_ownership) + 
        ggtitle("Annual Income & Defaulters")+
        theme(plot.title = element_text(hjust = 0.5)) + labs(y="Annual Income")+
       scale_x_continuous(breaks = 0)
    }
  )

  
  output$bartop <- renderPlot(
    if (input$annualincom==0){
  ggplot(forboxandbar,aes(x=emp_title)) + 
    geom_bar(aes(colour=emp_title))  + 
    ggtitle("Top Applicant's Employee title")+
    theme(plot.title = element_text(hjust = 0)) + labs(y="Count") +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x=element_text(size=10, angle=90))
    }else if(input$annualincom==1){
      ggplot(subset(forboxandbar,forboxandbar$annual_inc<300000),aes(x=emp_title)) + 
        geom_bar(aes(colour=emp_title))  + 
        ggtitle("Top Applicant's Employee title")+
        theme(plot.title = element_text(hjust = 0)) + labs(y="Count") +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(axis.text.x=element_text(size=10, angle=90))
    }else{
      ggplot(subset(forboxandbar,forboxandbar$annual_inc<250000),aes(x=emp_title)) + 
        geom_bar(aes(colour=emp_title))  + 
        ggtitle("Top Applicant's Employee title")+
        theme(plot.title = element_text(hjust = 0)) + labs(y="Count") +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(axis.text.x=element_text(size=10, angle=90))
    }
  )
  
  #########PAGE THREE TAB FOUR######################
  output$wordcloudemp <- renderWordcloud2(
    if(input$wordchoose==0){
      wordcloud2(subset(forWordCloud_2,forWordCloud_2$freq>mean(input$freq)),size = 2)
    }else{
      wordcloud2(subset(forWordCloud_purpose,forWordCloud_purpose$freq>mean(input$freq)),size = 2)
    }
  )
    output$wordtext <- renderText(
      if(input$wordchoose==0){
        paste("It can be seen that most of the applicants
          are one among Teacher, Project Managerial roles, Owner, 
                                  Sales person, Engineer, accountant and supervisors.")
      }else{
        paste0("Almost 60% of the applicants have applied loan for the purpose of 
          debt consolidations. 
        (debt consolidation is process of getting a loan to close of 
        other loans and manage financially).
        Next reason for applying loan is to repay their credit card debts. Thus those two reasons
        has been removed  and subsetted and the remaining purpose is analysed.")
      }
    )  
  
    #########PAGE THREE TAB TWO######################
    output$donutsemp <- renderPlot(
    if(input$sankeyselect == 2){
      ggplot(fordonut_grade, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=x)) +
        geom_rect() +
        geom_label( x=3.5, aes(y=labelPosition, label=label), size=3) + #For lablelling
        # scale_fill_brewer(palette=4) + (If required uncomment it)
        scale_color_brewer(palette=4) +
        coord_polar(theta="y") +
        xlim(c(2, 4)) +
        theme_void() +
        theme(legend.position = "none") +
        ggtitle("Top 15 Employee Title")+theme(plot.title = element_text(hjust = 0.5))
    }else{
    ggplot(fordonut_emp_title, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=x)) +
      geom_rect() +
      geom_label( x=3.5, aes(y=labelPosition, label=label), size=3) + #For lablelling
      # scale_fill_brewer(palette=4) + (If required uncomment it)
      scale_color_brewer(palette=4) +
      coord_polar(theta="y") +
      xlim(c(2, 4)) +
      theme_void() +
      theme(legend.position = "none") +
      ggtitle("Top 15 Employee Title")+theme(plot.title = element_text(hjust = 0.5))
    }
  )
  output$sankeyempgrade <-renderSankeyNetwork(
    if(input$sankeyselect == 0){
      emp_def
    }else if(input$sankeyselect == 1){
      emp_grade
    }else{
      grade_def
    }
  )
  #########PAGE THREE TAB THREE######################
  output$sundefault <- renderSunburst(
    q1
  )
  output$sunnondefault <- renderSunburst(
    q0
  )
  output$pieorhalf <- renderPlot(
    if(input$halforpie==0){
      ggplot(forpieChart, aes(x="", y=prop, fill=x)) +
        geom_bar(stat="identity", width=1, color="white") +
        coord_polar("y", start=0) +
        theme_void() +ggtitle("Pie chart of Purpose")+theme(plot.title = element_text(hjust = 0.5))
      
    }else{
      ggplot(subset_pie, aes(x="", y=prop, fill=x)) +
        geom_bar(stat="identity", width=1, color="white") +
        coord_polar("y", start=0) +
        theme_void() +
        ggtitle("Pie chart of Purpose")+theme(plot.title = element_text(hjust = 0.5))
      
    }
  )
  
  #########PAGE THREE TAB FIVE######################
  output$barchartlength <- renderPlot(
    ggplot(subset(theTable,theTable$home_ownership==input$hmeown),aes(emp_length)) + geom_bar(aes(colour=home_ownership))+ 
      geom_text(stat='count', aes(label=..count..), vjust=0) + 
      facet_grid(home_ownership~default_ind)+ 
      theme(axis.text.x=element_text(size=15, angle=90)) +
      ggtitle("Employee Length & Defaulters")+theme(plot.title = element_text(hjust = 0.5))
  )
  output$scatterPlothome <- renderPlot(
    ggplot(subset(forScatter,forScatter$home_ownership==input$hmeown),aes(loan_amnt,annual_inc)) +
      geom_point(aes(colour=home_ownership)) + 
      facet_grid(default_ind~home_ownership) + 
      ggtitle("Home Ownership & Defaulters")+theme(plot.title = element_text(hjust = 0.5))+ 
      theme(axis.text.x=element_text(size=8, angle=90))
    
  )
  }

# Create Shiny app ----
shinyApp(ui = ui, server = server)