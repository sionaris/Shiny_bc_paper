# Load global functions and variables
source("global.R")

# Load UI and server definitions
source("ui.R")
source("server.R")

# Run the app
shinyApp(ui = ui, server = server)
