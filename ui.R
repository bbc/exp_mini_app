library("tidyverse")
library("readxl")
library("writexl")
library("shiny")
library("ggridges")
library("pwr")
library("scales")
library("shinyWidgets")
library("shinycustomloader")
library("shinycssloaders")
library("shinyjs")
library("dplyr")
library("shinyBS")
library("ggplot2")

### Navigation panel - summary page ####

navbarPage(
    "Experimentation Mini-App | (V1 Beta)",
  tabPanel(
    "Summary",
    fluidPage(
      img(src='BBC_logo.png', align = "left")
      ,br(),br(),
      setBackgroundColor(
      color = c("#F7FBFF", "Purple"),
      gradient = "linear",
      direction = "top",
    )),
    headerPanel("Mini-app Summary"),
      wellPanel(p("This mini-app has been developed by the Experimentation and Optimisation Team at the BBC
                  in order to assist products across the organisation throughout the experimentation pipeline:"),br(),
                
                
                
                img(src='Pipeline.jpg', align = "middle"),br(),br(),    
                
                
      p("Firstly, prior to conducting an experiment it is important that we calculate some pre-determined sample size numbers;
        which helps us ascertain when an experiment should be concluded and how much traffic we need to allocate in Optimizely.
        The sample size calculator you use will be determined by the metric you want to assess. If you are wanting to determine
        the required sample size for a conversion rate (proportional metric), please use the Sample Size Calculator (Conversion Rates) tool.
        If you are wanting to determine the required sample size for a per browser (total) metric, then please use the 
        Sample Size Calculator (per Browser Metrics) tool."),br(),
      
      
      p("Similarly to the calculation of sample sizes, tracking statistical significance during post-hoc testing requires the user to know
        the type of metric they are attempting to analyse. For conversion rate (proportional metrics) please use the Significance Calculator (Conversion Rates)
        tool. For per browser (total) metrics please use the Significance Calculator (per Browser Metrics"),
      
                fluidRow(),
        column(width = 6))),

### Sample size calcualtor page (conversion rates)###

tabPanel(
  "Sample Size Calculator (Conversion Rates)",
  fluidPage(),
  headerPanel("Sample Size Calculator (Conversion Rates)"),
  wellPanel(
    helpText("This calculator will help you detect your rquired sample size if you are analysing a conversion rate (proportional metric). All you have to do
             is fill in some baseline/required values and the tool will do the rest of the work for you.")),
    column(width = 12,
           wellPanel(
             helpText("First of all fill in the current conversion rate of your control condition and the desired conversion 
              rate of your experimental condition (variant):")),
             column(width = 12,
                    wellPanel(
                    numericInput('control','Control', value = 8, min = 0.01, max = 99999, width = 125),"%",
                    ## numericInput('variant','Variant', value = 8.1, min = 0.01, max = 99999, width = 125),"%",
                    numericInput('uplift','Uplift', value = 10, min = 0.01, max = 99999, width = 125),"%",
                    )),
                    column(width = 12,
                    wellPanel(
                    helpText("The number below is our computed effect size, we will need this below:"),
                    htmlOutput("h")),
                    column(width = 12,
                    wellPanel(
                    helpText("Please input the above effect size into the 'Effect Size (Baseline)' input. Leave the other inputs
                    as they are, unless your experimental design differs from your conventional testing:"),
                    numericInput('h','Effect Size (Baseline)', value = -0.003675599, min = 0.01, max = 0.99999),
                    numericInput('siglevela','Sig Level', value = 0.05, min = 0.01, max = 0.99),
                    numericInput('powera','Power', value = 0.8, min = 0.01, max = 0.99),
                    ))),
                    column(width = 12,
                    wellPanel(
                    helpText("The following is the output from our sample size calculations. Please note,
                             this is the required sample size for each variant, so in theory you will need x2 of the below value:"),
                    htmlOutput("nrequired"))
                    ))),

### Sample size calcualtor page (per browser metrics)###

tabPanel(
  "Sample Size Calculator (per Browser Metrics)",
  fluidPage(),
  headerPanel("Sample Size Calculator (per Browser Metrics)"),
  wellPanel(
    helpText("This calculator will help you detect your required sample size for per browser (total) metrics. All you have to do
             is fill in some baseline/required values and the tool will do the rest of the work for you.")),
  column(width = 12,
         wellPanel(
           helpText("First of all, fill in the current performance of your control condition and the desired performance of your experimental condition (variant):")),
         column(width = 12,
                wellPanel(
                  numericInput('control_pb','Control', value = 1.456, min = 0.01, max = 99999),
                  numericInput('uplift_pb','Uplift', value = 0.5, min = 0.01, max = 99999),"%"
                  ## numericInput('variant_pb','Variant', value = 1.463, min = 0.01, max = 99999),
                )),
         column(width = 12,
                wellPanel(
                  helpText("The number below is the magnitude of the difference desired between
                           variants:"),
                  htmlOutput("delta_units")),
                column(width = 12,
                       wellPanel(
                         helpText("Please input the above magnitude of difference into the 'Delta (Magnitude of difference)' input. Leave the other inputs
                    as they are, unless your experimental design differs from your conventional testing:"),
                         numericInput('delta_input','Delta (Magnitude of difference)', value = 0.004, min = 0.001, max = 0.99999),
                         numericInput('sd_input','Standard Deviation', value = 0.5, min = 0.001, max = 0.99999),
                         numericInput('sig_level_pb','Sig Level', value = 0.05, min = 0.001, max = 0.99999),
                         numericInput('power_pb','Power', value = 0.8, min = 0.001, max = 0.99999),
                       ))),
         column(width = 12,
                wellPanel(
                  helpText("The following is the output from our sample size calculations. Please note,
                             this is the required sample size for each variant, so in theory you will need x2 of the below value:"),
                  htmlOutput("nrequired_pb")))
         )),
  
### Conversion rate significance calculator ###

tabPanel(
  "Significance Calculator (Conversion Rates)",
  fluidPage(),
  headerPanel("Significance Calculator (Conversion Rates)"),
  wellPanel(p("PLEASE NOTE: only check for statistical significance after the recommended time for running the experiment has passed*.
Checking for significance at any point before this time can lead to the wrong conclusion and therefore the wrong business decision"),br(),
p("*You can calculate the recommended run time using the sample size calculators within this app"),br(),
            
            
  p("This calculator will help you detect statistical significance for conversion rates (proportional metrics).
  Please note as this is for conversion rate metrics your values will always be < 1 for your control and variant as it represents 
  the proportion of your users that fired an event.")),br(),
  
column(width = 12,
       wellPanel(
         helpText("Please fill out the volume of users in each bucket and the number of times the event of interest was fired:"),br(),br(),
         numericInput('control_uv','Control Visitors', value = 100000, min = 0.01, max = 999999999999),
         numericInput('variant_uv','Variant Visitors', value = 100000, min = 0.01, max = 999999999999),
         numericInput('control_events','Control Events', value = 5000, min = 0.01, max = 999999999999),
         numericInput('variant_events','Variant Events', value = 7500, min = 0.01, max = 999999999999),
       )),
        column(width = 12,
        wellPanel(
        helpText("Below is the result of your experiment:"),
        htmlOutput("conversionr_output"))),br(),br(),
),

### per Browser data page - where data is uploaded and concatentated and processed ###


tabPanel(
  "Significance Calculator (per Browser Metrics)",
  fluidPage(),
  headerPanel("Significance Calculator (per Browser Metrics)"),
  wellPanel(p("PLEASE NOTE: only check for statistical significance after the recommended time for running the experiment has passed*.
Checking for significance at any point before this time can lead to the wrong conclusion and therefore the wrong business decision."),br(),
            p("*You can calculate the recommended run time using the sample size calculators within this app"),br(),
            
  
  
  p("When an experiment is concluded there may come a time when you want to see how the experiment performed for a certain segment, 
  time period and/or demographic. Similarly, it may be pertinent to the success of you experiment to see how a particular metric 
  performed that wasn’t tracked in Optimizely. What’s important for both aforementioned instances is we determine these changes to statistical 
  significance so we can determine whether increases/decreases are due to the independent nature of our experimental conditions. 
  For this reasons the Experimentation Team at the BBC have purpose built tools that use computational methods to detect statistical significance.  
  The purpose of this tool is to determine the statistical difference between your experimental conditions for per browser metrics; 
  this is the same as a “total” count in Optimizely, so you can ensure these are formatted the same for a point of reference. Examples of such metrics are:"),br(),br(),


p("- Page views per browser"),br(),
p("- Content items per browser"),br(),
p("- Signed-in page views per browser"),br(),
p("- Signed-out page views per browser"),br(),br(),
  
            p("First of all you are going to want to download the Excel template and fill this out with your own data, it is essential
              that you follow the formatting of this document in order for the tool to work. When you press the button below you will
              be asked to save the Excel file; save this in any given directory:"),br(),
            fluidRow(),
            column(
                  downloadButton("download_data_template",
                                 "Download template"),
                  width = 2
                ),br(),br(),br(),

p("If you are looking to fill this template with data from ATI, you can simply copy and paste your Data Query export into the correct columns. 
In ATI the variant is located in the publisher universe as 'Variant (Publisher)'. You will then want to obtain your metric of interest and copy
this into the 'Metric' column. In the example below 'Page Views' are used. With the corresponding experimental variant we can now work out the
average per bucket:"),br(),


            img(src='ATI_DQ.jpg', align = "middle"),br(),br(), 

p("If you are looking to fill this template with data from Redshift or Athena, you can simply copy and paste the export from your query into
  the correct columns. The variant parameter is located as 'user_experience' in the publisher universe. You can then select any metric you would like
  in your query. Below I have used 'publisher_impressions' from the same experiment as the ATI example above:"),br(),


img(src='Redshift_DQ.jpg', align = "middle"),br(),br(),   



            p("You can now upload your filled out data back into the app and prep it for analysis. 
              Locate where you stored the filled out data:"),br(),
            fluidRow(),
            column(
              fileInput("upload_unfiltered_data",
                             "Upload Data"),
              width = 12
              ),br(),
            p("You can now run your statistical analysis by clicking the 'Calculate Significance' widget below. You can use the 'p value' of the statatistical
            output to determine the result of your test (using the annotations provided). Please note that this action automatically removes statistical outliers 
            from your data (any data +/- 3 standard deviations
              from the mean):"),br(),
            fluidRow()
            ),

            wellPanel(
              bsButton("box_plot",
                       "Calculate Significance",
                       width = "100%",
                       size = "large",
                       type = "toggle",
                       style = "default"),
              withSpinner(plotOutput("stats_sig_summary"), type = 6, color = "#4C0099"),
              bsTooltip(id = "box_plot", title = "Remove outliers and calculate statistical significance", placement = "centre", trigger = "hover"),br(),
              p("* p > 0.05 = non sig, p < 0.05 = 95% sig, p < 0.01 = 99% sig, p < 0.001 = 99.99% sig")),
column(width = 12,
       wellPanel(
         bsButton("raw_data", "Show Group Averages", width ="100%"),br(),br(),
         tableOutput("data_means"),
         bsButton("raw_data_output", "Statistical Output", width = "100%"),br(),
         tableOutput("raw_data_output"),br(),
         p("* ns = < 95% sig, * = 95% sig, ** = 99% sig, *** = 99.99% sig"),br()))


))
