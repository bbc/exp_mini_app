options(shiny.maxRequestSize=3000000000*1024^2) 

library("readxl")
library("writexl")
library("shiny")
library("ggridges")
library("pwr")
library("scales")
library("shinyWidgets")
library("shinycustomloader")
library("dplyr")
library("shinyBS")
library("ggplot2")
library("ggpubr")
library("ggthemes")


function(input, output, session) {
  
### Make uploader reactive (Uploader) ###
  
  
upload_unfiltered_data <- eventReactive(c(input$upload_unfiltered_data),
                                        {
                                          data_download_1 <- read_excel(input$upload_unfiltered_data$datapath)
                                          
                                          
                                        })
  
### Make uploader reactive (Output) ###
  
  
upload_data_template <- eventReactive(c(input$upload_data_template),
                                        {
                                          data_download_2 <- read_excel(input$upload_data_template$datapath)
                                          
                                          data_download_2
                                        })
  
### Sample size (conversion rates) server-side ###
  
  
output$h <- renderPrint({
ES.h(p1 = input$control / 100, 
     p2 = (input$uplift / 100)*(input$control / 100) + input$control / 100)
})
     
     
output$nrequired <- renderPrint({
p <- pwr.p.test(h = input$h, sig.level = input$siglevela, power = input$powera)
p$n})

### Sample size (per browser) server-side ###

## renderPrint({(input$uplift_pb /100)*(input$control / 100) + input$control / 100})

output$delta_units <- renderPrint({(input$uplift_pb /100 * input$control_pb) + input$control_pb / input$control_pb -1})

output$nrequired_pb <- renderPrint({
p_pb <- power.t.test(delta = input$delta_input, sd = input$sd_input, sig.level = input$sig_level_pb, power = input$power_pb)
p_pb$n})
      
### Conversion rate significance calculator server-side ###
      
output$conversionr_output <- renderPrint({
p_val <- prop.test(c(input$control_events, input$variant_events), c(input$control_uv, input$variant_uv))
p_val_sig <- p_val$p.value
prob_success <- 1-p_val_sig
conversion_rate_control <- input$control_events/input$control_uv
conversion_rate_variant <- input$variant_events/input$variant_uv
uplift <- (conversion_rate_variant/conversion_rate_control)-1
uplift <- percent(uplift)


percent_if <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

if (p_val_sig <0.05 && uplift > 0){
  sprintf("There is a statistically significant difference between the control and the variant. During the test, the variant performed better than the default by %s. The mean of variant is %s and the mean of the control is %s. The probability that the difference observed is due to a true difference in means is: %s", uplift, percent_if(conversion_rate_variant), percent_if(conversion_rate_control), percent_if(prob_success)) 
}else if (p_val_sig < 0.05 && uplift < 0){
  sprintf("There is a statistically significant difference between the control and the variant. During the test, the variant performed worse than the default by %s.The mean of variant is %s and the mean of the control is %s. The probability that the difference observed is due to a true difference in means is: %s", uplift, percent_if(conversion_rate_variant), percent_if(conversion_rate_control), percent_if(prob_success))  
}else{sprintf("No significant difference detected between the control and the variant.The mean of variant is %s and the mean of the control is %s", percent(conversion_rate_variant), percent(conversion_rate_control))}
}

)


### Download template in uploader tab ###

  output$download_data_template <- downloadHandler(
    filename = function(){
      paste0(Sys.Date(), "_test_data.xlsx")
    },
    content = function(file){
      file.copy("data/test_data.xlsx", file)
    }
  )
  

  ### Ggstatsplot output (Output) ###

  
  output$stats_sig_summary <- renderPlot({
    
    input$box_plot
    
    upload_unfiltered_data <- isolate(upload_unfiltered_data())
    
    remove_outliers <- function(df){
      
      y <- df[1][df[1] > 0] #remove any zero values
      print(paste0("mean = ", mean(y)))
      print(paste0("3sd = ",3*sd(y)))
      print(paste0("3sd + mean = ", 3*sd(y) + mean(y)))
      dfNoOutliers<- df %>% filter(df[1]< 3*sd(y) + mean(y)) #remove any outlisers
      valsremaining <- length(dfNoOutliers)/length(df)
      valsremaining
      
      if (valsremaining < 0.95){
        stop ("This function will remove more than 5% percent of your data. You need to remove outliers manually.")}
      
      else if (valsremaining < 0.99){
        warning("This calculation has removed between 1% and 5% of your data.") 
        print(paste0(valsremaining*100, "% has been removed"))
      }
      else{
        print("Less than 1% of data has been removed")
      }
      return(dfNoOutliers)
    }
      
    data_frame <- as.data.frame(upload_unfiltered_data)
    
    filtered_data <- remove_outliers(data_frame)
    
    means <- aggregate(Metric ~  Variant, filtered_data, mean)
    
    filtered_data %>%
      ggplot(aes(x=Variant, y=Metric, fill = Variant), annotations=p.adj) + 
      geom_boxplot()+
      stat_compare_means(method = "anova")+
      ## geom_label(data = means, aes(label = Metric, y = Metric, size = 5), show.legend = FALSE)+
      theme_hc()+
      scale_fill_brewer(palette="Purples")
    }  
)
    
  ### Raw Data Table (Means) ###
  
  output$data_means <- renderTable({
    
    input$raw_data
    
    upload_unfiltered_data <- isolate(upload_unfiltered_data()) 
    
    remove_outliers <- function(df){
      
      y <- df[1][df[1] > 0] #remove any zero values
      dfNoOutliers<- df %>% filter(df[1]< 3*sd(y) + mean(y)) #remove any outlisers
      valsremaining <- length(dfNoOutliers)/length(df)
      valsremaining
      
      if (valsremaining < 0.95){
        stop ("This function will remove more than 5% percent of your data. You need to remove outliers manually.")}
      
      else if (valsremaining < 0.99){
        warning("This calculation has removed between 1% and 5% of your data.") 
        print(paste0(valsremaining*100, "% has been removed"))
      }
      else{
        print("Less than 1% of data has been removed")
      }
      return(dfNoOutliers)
      
    }
    
    data_frame <- as.data.frame(upload_unfiltered_data)
    
    filtered_data <- remove_outliers(data_frame)
    
    means <- aggregate(Metric ~  Variant, filtered_data, mean)
    
    means
    
    
  })
  
  
  ### Raw Data Table (Comps) ###
  
  
  output$raw_data_output <- renderTable({
    
    input$raw_data_output
    
    upload_unfiltered_data <- isolate(upload_unfiltered_data()) 
    
    remove_outliers <- function(df){
      
      y <- df[1][df[1] > 0] #remove any zero values
      dfNoOutliers<- df %>% filter(df[1]< 3*sd(y) + mean(y)) #remove any outlisers
      valsremaining <- length(dfNoOutliers)/length(df)
      valsremaining
      
      if (valsremaining < 0.95){
        stop ("This function will remove more than 5% percent of your data. You need to remove outliers manually.")}
      
      else if (valsremaining < 0.99){
        warning("This calculation has removed between 1% and 5% of your data.") 
        print(paste0(valsremaining*100, "% has been removed"))
      }
      else{
        print("Less than 1% of data has been removed")
      }
      return(dfNoOutliers)
      
    }
    
    data_frame <- as.data.frame(upload_unfiltered_data)
    
    filtered_data <- remove_outliers(data_frame)
    
    means <- aggregate(Metric ~  Variant, filtered_data, mean)
    
    data_comps <- compare_means(Metric ~ Variant, data = filtered_data, method = "t.test")
    
    filtered_comps <- dplyr::select(data_comps, method , group1, group2, p, p.signif)
    
    filtered_comps
    
  }
  )
}
