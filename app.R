# Load necessary libraries
library(shiny)
library(SurvdigitizeR)
library(here)
library(magrittr)  # for pipe operator
library(ggplot2)   # for plot generation
library(DT)
library(plotly)
library(shinydashboard)
library(tippy)


source("module_demo.R")


# Add Logo image
header <- dashboardHeader(title = "Survival Digitize App")
header$children[[3]] <- tags$a(href='https://www.sickkids.ca/',
                               tags$img(src="https://www.sickkids.ca/contentassets/232e33f4208c495eb192e035ee7ba6e5/sickkids-logo-desktop-140x42.png"),
                               style = "padding-top:10px; padding-right:10px; float:right;")

# Define UI for application
ui <- dashboardPage(
  header,
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "about", icon = icon("glyphicon glyphicon-info-sign", lib = "glyphicon")),
      menuItem("Example", tabName = "example", icon = icon("glyphicon glyphicon-asterisk", lib = "glyphicon", badgeLabel = "Read before start", badgeColor = "white")),
      menuItem("Analysis", icon = icon("glyphicon glyphicon-hand-right", lib = "glyphicon"), tabName = "analysis", badgeLabel = "Start", badgeColor = "green")
    )
  ),
  skin = "purple",
  dashboardBody(
    shiny::includeScript("util.js"),
    tabItems(
      tabItem(tabName = "about",
              h1("About the Survival Digitize App"),
              p("In the realm of economic evaluations and meta-analyses, Kaplan-Meier (KM) survival curves serve as fundamental tools for visualizing and interpreting survival data. Nevertheless, these curves typically only exist in the form of published images, which poses a challenge in retrieving and digitizing the invaluable data they hold. In response to this, we've crafted a user-friendly, web-based Shiny application. This tool, built on the robust foundation of the SurvdigitizeR package, offers an intuitive and user-accessible interface, enabling researchers and clinicians to efficiently digitize KM curves from image files, thereby enriching their data analysis process. Remarkably, our application empowers users with little to no R programming experience to leverage the capabilities of SurvdigitizeR without the necessity of software installation, hence providing a handy platform for survival data analysis.", style = "font-size:120%"),
              h2("How does Digitization Shiny App work?"),
              p("Our Shiny app offers a user-friendly interface to digitize Kaplan-Meier survival curves. Users upload an image of the curve and set certain parameters, such as the number of curves, axis scales, and the presence of line censoring markers. The app, using the SurvdigitizeR package's survival_digitize function, then processes the image, extracts the survival curve data, and presents an interactive plot. The resulting data can be conveniently downloaded from the app.", style = "font-size:120%"),
              h2("Here's a step-by-step guide on how to use our Shiny app:"),
              tags$ul(
                tags$li("Upload an image file of the KM survival curve using the file upload button."),
                tags$li("Input the number of survival curves present in the image."),
                tags$li("Specify the start and end points and increments of the x (time) and y (survival probability) axes."),
                tags$li("Indicate whether the y-axis text is vertical and whether there is a line censoring marker in the plot."),
                tags$li("Click the 'Submit' button. The app will then process the image and display the digitized survival curve."),
                tags$li("Review the digitized plot, and if satisfied with the results, download the digitized data and/or the plot image using the respective download buttons."), style = "font-size:120%"
              ),
              h2("Acknowledgements"),
              p("The Shiny app was developed through a collaborative effort, and we wish to express our deepest appreciation for the contributions made by the authors of the algorithm: Jasper Zhongyuan Zhang, Juan David Rios, Tilemanchos Pechlivanoglou, Alan Yang, Qiyue Zhang, Dimitrios Deris, Ian Cromwell, and Petros Pechlivanoglou.
We are also delighted to mention that the poster for this project was awarded", tags$b("first prize"), "in the virtual poster presentation competition at the 2023 Statistical Society of Canada Annual Meeting in May 2023 during the student conference (CSSC 2023) in Ottawa. This recognition further reinforces the relevance and impact of our work.", style = "font-size:120%"),
              h2("Funding"),
              p("The study of SurvdigitizeR was financially supported by an unrestricted grant from the Canadian Agency for Drugs and Technologies in Health (CADTH).", style = "font-size:120%"),
              h2(""),
              h4("Developed by The Pechlivanoglou Lab at The Hospital for Sick Children (SickKids) Research Institute , July 2023 - https://lab.research.sickkids.ca/pechlivanoglou/", style = "font-size:120%"),
              h4(""),
              h2("SurvdigitizeR model development"),
              p("Jasper Zhongyuan Zhang, Juan David Rios, Tilemanchos Pechlivanoglou, Alan Yang, Qiyue Zhang, Dimitrios Deris, Ian Cromwell, and Petros Pechlivanoglou.", style = "font-size:120%"),
              h2("R & Shiny implementation"),
              p("Qiyue Zhang, Jasper Zhongyuan Zhang, Petros Pechlivanoglou", style = "font-size:120%")
      ),
      tabItem(tabName = "example",
              
              demo_ui("demo")
      ),
      
      tabItem(tabName = "analysis",
              sidebarLayout(
                sidebarPanel(
                  radioButtons("image_type", HTML("Please select the image source: <span id = 'image_type_tip' ><i class='fa-solid fa-circle-info' style='color: blue;'></i></span>"),
                               c("File", "URL"), inline = TRUE),
                  
                  # HTML('<script>tippy("mage_type_tip", {content: "asdfasd fasdjlfja lksdjflka jsldfjals d", placement: "bottom-start",  arrow : true,    trigger : "hover",animation : "scale",theme : "translucent", size : "large"})</script>'),
                  HTML('<script>set_tip("#image_type_tip", "Upload an image file from your local computer or enter a URL link")</script>'),
                  
                  
                  
                  conditionalPanel(
                    condition = "input.image_type == 'File'",
                    fileInput("img_path", "Upload Image"),
                  ),
                  
                  conditionalPanel(
                    condition = "input.image_type == 'URL'",
                    textInput("img_url", "Image URL:", ""),
                  ),
                  
                  
                  numericInput("num_curves", HTML("Number of Curves: <span id = 'num_curves_tip' ><i class='fa-solid fa-circle-info' style='color: blue;'></i></span>"), "", min = 1, max = 10),
                  
                  HTML('<script>set_tip("#num_curves_tip", "Field to input the number of survival curves present in the uploaded image.")</script>'),
                  
                  
                  
                  
                  numericInput("x_start", HTML("X start: <span id = 'x_start_tip' ><i class='fa-solid fa-circle-info' style='color: blue;'></i></span>"), ""),
                  HTML('<script>set_tip("#x_start_tip", "Specify the start point of the x axes")</script>'),
                  
                  
                  
                  numericInput("x_end", HTML("X end: <span id = 'x_end_tip' ><i class='fa-solid fa-circle-info' style='color: blue;'></i></span>"), ""),
                  HTML('<script>set_tip("#x_end_tip", "Specify the end point of the x axes")</script>'),
                  
                  
                  numericInput("x_increment", HTML("X increment: <span id = 'x_increment_tip' ><i class='fa-solid fa-circle-info' style='color: blue;'></i></span>"), ""),
                  HTML('<script>set_tip("#x_increment_tip", "Specify the increments of the x axes")</script>'),
                  
                  numericInput("y_start", HTML("Y start: <span id = 'y_start_tip' ><i class='fa-solid fa-circle-info' style='color: blue;'></i></span>"), ""),
                  HTML('<script>set_tip("#y_start_tip", "Specify the start point of the y axes")</script>'),
                  
                  
                  
                  numericInput("y_end", HTML("Y end: <span id = 'y_end_tip' ><i class='fa-solid fa-circle-info' style='color: blue;'></i></span>"), ""),
                  HTML('<script>set_tip("#y_end_tip", "Specify the end point of the y axes")</script>'),
                  
                  numericInput("y_increment", HTML("Y increment: <span id = 'y_increment_tip' ><i class='fa-solid fa-circle-info' style='color: blue;'></i></span>"), ""),
                  HTML('<script>set_tip("#y_increment_tip", "Specify the increments of the y axes")</script>'),
                  
                  checkboxInput("y_text_vertical", HTML("Y Text Vertical <span id = 'y_text_vertical_tip' ><i class='fa-solid fa-circle-info' style='color: blue;'></i></span>"), TRUE),
                  HTML('<script>set_tip("#y_text_vertical_tip", "Checkbox to indicate if the text labels on the y-axis are vertical.")</script>'),
                  
                  
                  checkboxInput("line_censoring", HTML("Line Censoring Marker <span id = 'line_censoring_tip' ><i class='fa-solid fa-circle-info' style='color: blue;'></i></span>"), FALSE),
                  HTML('<script>set_tip("#line_censoring_tip", "Checkbox to indicate the presence of line censoring markers in the survival curve plot.")</script>'),
                  
                  actionButton("submit", "Submit"),
                  tippy_this("submit", "Button to initiate the digitization process. Click this after filling out all required parameters.",
                             placement =  "right", arrow = TRUE, theme = "translucent", size = "large"),
                  
                  
                  
                  tags$br(),
                  downloadButton("download_plot", "Download Plot"),
                  tags$br(),
                  downloadButton("download_data", "Download Data")
                ),
                mainPanel(
                  
                  uiOutput("fig1h"),  
                  uiOutput("fig1"),
                  imageOutput("loaded_img", height = "100%"),
                  
                  
                  uiOutput("fig2h"),  
                  uiOutput("fig2"),
                  plotlyOutput("digitized_plot"),
                  
                  
                  
                  uiOutput("table1h"),
                  uiOutput("table1"),
                  DTOutput("table_head"),
                  
                )
              )
      )
    )
  )
)


# Define server logic
server <- function(input, output, session) {
  
  demo_server("demo")
  
  data_out <- reactiveValues(df = NULL, imagePath = NULL)
  
  output$fig1h = renderUI({
    tagList(
      h1("Figure 1"),
    )
    
    
  }) %>% bindEvent(input$img_path)
  output$fig1 = renderUI({
    tagList(
      p("Example of Uploaded Kaplan-Meier Survival Curve Image. "),
      p("Instructions: Upload the file from your local computer or paste the URL link.")
    )
    
    
  }) %>% bindEvent(input$img_path)
  
  output$loaded_img <- renderImage({
    # If file uploaded, use it
    
    imginfo = NULL
    
    if(input$image_type == "File") {
      if (isTruthy(input$img_path)) {
        
        data_out$imagePath = input$img_path$datapath
        
        imginfo = list(src = input$img_path$datapath,
                       alt = "Your Image",
                       width = 400,
                       height = 300
        )
      }
    } else {
      
      if (isTruthy(input$img_url)) {
        # Else, if URL provided, use it
        tempFile <- tempfile(fileext = ".png")
        download.file(input$img_url, destfile = tempFile, mode = "wb", method = "curl")
        
        data_out$imagePath = tempFile
        
        print(tempFile)
        
        
        imginfo = list(src = tempFile,
                       alt = "Your Image",
                       width = 400,
                       height = 300
        )
      }
    }
    
    req(imginfo)
    
    imginfo
    
  }, deleteFile = FALSE)
  
  observeEvent(input$submit, {
    req(data_out$imagePath ) # Ensure that the file or URL is provided
    img_path <- data_out$imagePath
    
    data_out$df <- survival_digitize(
      img_path = img_path,
      num_curves = input$num_curves,
      x_start = input$x_start,
      x_end = input$x_end,
      x_increment = input$x_increment,
      y_start = input$y_start,
      y_end = input$y_end,
      y_increment = input$y_increment,
      y_text_vertical = input$y_text_vertical,
      censoring = input$line_censoring
    )
    
    
  })
  
  output$digitized_plot <- renderPlotly({
    req(data_out$df)
    g = data_out$df %>%
      ggplot(aes(x = time, y= St, color = as.factor(curve), group = curve)) +
      geom_step() + theme_bw() + labs(color='Curves') +
      scale_color_manual(values = c("1" = "red", "2" = "blue")) +
      scale_x_continuous(limits = c(input$x_start, input$x_end),
                         breaks = seq(input$x_start, input$x_end, by = input$x_increment))+
      scale_y_continuous(limits = c(input$y_start, input$y_end),
                         breaks = seq(input$y_start, input$y_end, by = input$y_increment))+
      ggtitle(paste0(input$img_path$name,"_Digitized"))
    
    plotly::ggplotly(g)
  }) %>% bindEvent(input$submit)
  
  
  output$fig2h = renderUI({
    tagList(
      h1("Figure 2")
    )
    
    
  }) %>% bindEvent(input$submit)
  
  output$fig2 = renderUI({
    tagList(
      p("Example Output: Digitized Kaplan-Meier Survival Curve."),
      p("Note: This is the digitized version of the original Kaplan-Meier curve displayed in Figure 1.")
    )
    
    
  }) %>% bindEvent(input$submit)
  
  output$table1h = renderUI({
    tagList(
      h1("Table 1"),
    )
    
    
  }) %>% bindEvent(input$submit)    
  
  output$table1 = renderUI({
    tagList(
      p("Extracted Survival Data: Corresponding Table from Digitized Kaplan-Meier Curve."),
      p("Description: This table contains the survival data that has been extracted from the digitized Kaplan-Meier survival curve shown in Figure 2.")
    )
    
    
  }) %>% bindEvent(input$submit)    
  
  output$table_head <- renderDT({
    req(data_out$df)
    data_out$df
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      req(data_out$df)
      write.csv(data_out$df, file, row.names = FALSE)
    }
  )
  
  output$download_plot <- downloadHandler(
    filename = function() {
      paste(input$img_path$name,"_Auto_Digitized.png", sep="")
    },
    content = function(file) {
      req(data_out$df)
      img <- data_out$df %>%
        ggplot(aes(x = time, y= St, color = as.factor(curve), group = curve)) +
        geom_step() + theme_bw() + labs(color='Curves') +
        scale_color_manual(values = c("1" = "red", "2" = "blue")) +
        scale_x_continuous(limits = c(input$x_start, input$x_end),
                           breaks = seq(input$x_start, input$x_end, by = input$x_increment))+
        scale_y_continuous(limits = c(input$y_start, input$y_end),
                           breaks = seq(input$y_start, input$y_end, by = input$y_increment))+
        ggtitle(paste0(input$img_path$name,"_Digitized"))
      ggsave(file, img, width = 10, height = 8)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
