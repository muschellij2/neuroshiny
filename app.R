rm(list = ls())
library(ggplot2)
library(ggneuro) # from muschellij2/ggneuro
library(shiny)
library(neurobase)

img = readnii("mean.nii.gz")
mat = readRDS("fmri.rds")

# get some coordinates and colors
curr_xyz =  ceiling(dim(img) / 2)
gray_gradient = scale_fill_gradient(low = gray(0),
                                    high = gray(1),
                                    na.value = "black")


##########################################
# Start the SHINY!!!!!!!!!!!!!!!
##########################################
ui <- basicPage(
  plotOutput("plot1", click = "plot_click"),
  plotOutput("plot2"),
  verbatimTextOutput("info")
)

server <- function(input, output, session) {
  fd = neurobase::img_colour_df(img)
  tfd = t(fd[, paste0("dim", 1:3)])
  # fd
  
  ## CHANGE HERE
  ## Set up buffer, to keep the click.
  click_saved <- reactiveValues(plot_click = NULL)
  
  ## CHANGE HERE
  ## Save the click, once it occurs.
  observeEvent(eventExpr = input$plot_click, handlerExpr = {
    click_saved$plot_click <- input$plot_click
  })
  
  # getting the xyz coordinates
  get_data = reactive({
    
    print("before click data")
    print(curr_xyz)
    pc = click_saved$plot_click
    plane = pc$panelvar1
    print(is.null(plane))
    
    if (!is.null(plane)) {
      # curr_xyz = ##### SOMETHING
      curr_xyz = switch(
        plane,
        axial = c(pc$x, pc$y, curr_xyz[3]),
        coronal = c(pc$x, curr_xyz[2], pc$y),
        sagittal = c(curr_xyz[1], pc$x, pc$y)
      )
      curr_xyz = floor(curr_xyz)
    }
    out = ggortho_img_df(fd, xyz = curr_xyz)
    curr_xyz <<- curr_xyz
    print("\n\n")
    L = list(plot = out, xyz = curr_xyz)
    L
  })
  
  output$plot1 <- renderPlot({
    out = get_data()
    out = out$plot
    out
  })
  
  output$plot2 <- renderPlot({
    out = get_data()
    xyz = out$xyz
    ind = which(colAlls(tfd == xyz))
    tc = mat[ind,]
    plot(tc, type = "l")
    lines(lowess(tc), col = "red")
  })
  
  
  
}

shinyApp(ui, server)
