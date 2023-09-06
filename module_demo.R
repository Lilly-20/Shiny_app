
# module ui
demo_ui <- function(id) {
    ns <- NS(id)
    
    
    tagList(
        sidebarLayout(
            sidebarPanel(
                radioButtons(ns("image_type"),  HTML("Please select the image source: <span id = 'eximage_type_tip' ><i class='fa-solid fa-circle-info' style='color: blue;'></i></span>"),
                             c("File", "URL"), inline = TRUE),
                
                HTML('<script>set_tip("#eximage_type_tip", "Upload an image file from your local computer or enter a URL link")</script>'),

                conditionalPanel(ns = ns,
                                 condition = "input.image_type  == 'File'",
                                 fileInput(ns("img_path"), "Upload Image"),
                ),

                conditionalPanel(ns = ns,
                                 condition = "input.image_type == 'URL'",
                                 textInput(ns("img_url"), "Image URL:", ""),
                ),
                
                
                numericInput(ns("num_curves"), HTML("Number of Curves: <span id = 'exnum_curves_tip' ><i class='fa-solid fa-circle-info' style='color: blue;'></i></span>"), 2, min = 1, max = 10),
                HTML('<script>set_tip("#exnum_curves_tip", "Field to input the number of survival curves present in the uploaded image.")</script>'),
                
                
                
                numericInput(ns("x_start"), HTML("X start: <span id = 'exx_start_tip' ><i class='fa-solid fa-circle-info' style='color: blue;'></i></span>"), 0),
                HTML('<script>set_tip("#exx_start_tip", "Specify the start point of the x axes")</script>'),
                
                
                
                numericInput(ns("x_end"), HTML("X end: <span id = 'exx_end_tip' ><i class='fa-solid fa-circle-info' style='color: blue;'></i></span>"), 10),
                HTML('<script>set_tip("#exx_end_tip", "Specify the end point of the x axes")</script>'),
                
                
                
                
                numericInput(ns("x_increment"),  HTML("X increment: <span id = 'exx_increment_tip' ><i class='fa-solid fa-circle-info' style='color: blue;'></i></span>"), 1),
                HTML('<script>set_tip("#exx_increment_tip", "Specify the increments of the x axes")</script>'),
                
                
                
                numericInput(ns("y_start"), HTML("Y start: <span id = 'exy_start_tip' ><i class='fa-solid fa-circle-info' style='color: blue;'></i></span>"), 0),
                HTML('<script>set_tip("#exy_start_tip", "Specify the start point of the y axes")</script>'),
                
                
                numericInput(ns("y_end"), HTML("Y end: <span id = 'exy_end_tip' ><i class='fa-solid fa-circle-info' style='color: blue;'></i></span>"), 1),
                HTML('<script>set_tip("#exy_end_tip", "Specify the end point of the y axes")</script>'),
                
                
                
                
                numericInput(ns("y_increment"), HTML("Y increment: <span id = 'exy_increment_tip' ><i class='fa-solid fa-circle-info' style='color: blue;'></i></span>"), 0.25),
                HTML('<script>set_tip("#exy_increment_tip", "Specify the increments of the y axes")</script>'),
                
                
                
                
                checkboxInput(ns("y_text_vertical"), HTML("Y Text Vertical <span id = 'exy_text_vertical_tip' ><i class='fa-solid fa-circle-info' style='color: blue;'></i></span>"), TRUE),
                HTML('<script>set_tip("#exy_text_vertical_tip", "Checkbox to indicate if the text labels on the y-axis are vertical.")</script>'),
                
                
                checkboxInput(ns("line_censoring"), HTML("Line Censoring Marker <span id = 'exline_censoring_tip' ><i class='fa-solid fa-circle-info' style='color: blue;'></i></span>"), FALSE),
                HTML('<script>set_tip("#exline_censoring_tip", "Checkbox to indicate the presence of line censoring markers in the survival curve plot.")</script>'),
                
                
                
                actionButton(ns("exsubmit"), "Submit"),
                tippy_this(ns("exsubmit"), "Button to initiate the digitization process. Click this after filling out all required parameters.",
                           placement =  "right", arrow = TRUE, theme = "translucent", size = "large"),
                
                
                
                
                tags$br(),
                downloadButton(ns("download_plot"), "Download Plot"),
                tags$br(),
                downloadButton(ns("download_data"), "Download Data")
            ),
            mainPanel(
                uiOutput(ns("fig1h")), 
                uiOutput(ns("fig1")),
                imageOutput(ns("loaded_img"), height = "100%"),

                
                uiOutput(ns("fig2h")),   
                uiOutput(ns("fig2")),
                plotlyOutput(ns("digitized_plot")),

                
                uiOutput(ns("table1h")),  
                uiOutput(ns("table1")),
                DTOutput(ns("table_head")),

            )
        )
        
        
        
    )
}

################################################################
# module server
demo_server <- function(id) {
    moduleServer(
        id,
        function(input, output, session) {
            
            
            
            output$fig1h = renderUI({
                tagList(
                    h1("Figure 1"),
                )
                
                
            })
            output$fig1 = renderUI({
                tagList(
                    p("Example of Uploaded Kaplan-Meier Survival Curve Image. "),
                    p("Instructions: Upload the file from your local computer or paste the URL link.")
                )
                
                
            })
            
            output$fig2h = renderUI({
                tagList(
                    h1("Figure 2")
                )
                
                
            })
            
            output$fig2 = renderUI({
                tagList(
                    p("Example Output: Digitized Kaplan-Meier Survival Curve."),
                    p("Note: This is the digitized version of the original Kaplan-Meier curve displayed in Figure 1.")
                )
                
                
            })
            
            
            output$table1h = renderUI({
                tagList(
                    h1("Table 1"),
                )
                
                
            })
            
            output$table1 = renderUI({
                tagList(
                    p("Extracted Survival Data: Corresponding Table from Digitized Kaplan-Meier Curve."),
                    p("Description: This table contains the survival data that has been extracted from the digitized Kaplan-Meier survival curve shown in Figure 2.")
                )
                
                
            })   

            

            output$loaded_img <- renderImage({
                # If file uploaded, use it
                
                list(src = "KMCurve.png",
                     alt = "Your Image",
                     width = 400,
                     height = 300)
            }, deleteFile = FALSE)
            
            mydf = reactive({
                img_path <- "KMCurve.png"
                
                mydf <- survival_digitize(
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
                
                mydf
                
                
            })
            
            output$digitized_plot <- renderPlotly({
                
                g = mydf() %>%
                    ggplot(aes(x = time, y= St, color = as.factor(curve), group = curve)) +
                    geom_step() + theme_bw() + labs(color='Curves') +
                    scale_color_manual(values = c("1" = "red", "2" = "blue")) +
                    scale_x_continuous(limits = c(input$x_start, input$x_end),
                                       breaks = seq(input$x_start, input$x_end, by = input$x_increment))+
                    scale_y_continuous(limits = c(input$y_start, input$y_end),
                                       breaks = seq(input$y_start, input$y_end, by = input$y_increment))+
                    ggtitle(paste0(input$img_path$name,"_Digitized"))
                
                plotly::ggplotly(g)
            })
            
            output$table_head <- renderDT({
                mydf()
                
            })
            
            output$download_data <- downloadHandler(
                filename = function() {
                    paste("data-", Sys.Date(), ".csv", sep="")
                },
                content = function(file) {
                    
                    write.csv(mydf(), file, row.names = FALSE)
                }
            )
            
            output$download_plot <- downloadHandler(
                filename = function() {
                    paste(input$img_path$name,"_Auto_Digitized.png", sep="")
                },
                content = function(file) {
                    df = mydf()
                    img <- df %>%
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
    )
}
