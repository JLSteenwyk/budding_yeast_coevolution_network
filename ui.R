library(shiny)
library(shinythemes)
# library(shiny) ; runApp("Desktop/GITHUB/budding_yeast_coevolution_network/.")

# create line break function
linebreaks <- function(n){HTML(strrep(br(), n))}

# Define UI for application
shinyUI(fluidPage(theme = shinytheme("spacelab"),
    # display logo
    titlePanel(
        title="Budding Yeast Coevolutionary Network",
        windowTitle="Budding Yeast Coevolutionary Network"
    ), 
    hr(),
    # Create a spot for the choices
    sidebarLayout(
    sidebarPanel(
        h4("Genes of interest"), 
        fileInput("gene_list_file", ""),
        h4(linebreaks(.5)), 
        h4("Distance"), 
        sliderInput("order", "", 
            min = 0, max = 10,
            value = 0, step = 1
        ),
        h4(linebreaks(1)), 
        h4("Layout style"), 
        radioButtons("layout_style", "", 
            c(
                "MDS" = "layout_with_mds",
                "FR" = "layout_with_fr",
                "Circle" = "layout_in_circle", 
                "Grid" = "layout_on_grid",
                "DRL" = "layout_with_drl"
            ), inline = TRUE
        ),
        # include labels or no
        column(5,
            h4(linebreaks(1), "Node labels"),
            radioButtons("node_labels", "",
                c("Yes" = TRUE, "No" = FALSE), selected = FALSE, inline = TRUE
            )
        ),
        column(7,
            h4(linebreaks(1), "Node opacity"),
            sliderInput("node_alpha", "",
                min = 0, max = 1,
                value = .5, step = .1
            )
        ),
        h4(linebreaks(2)), 
        actionButton("userGO", "Submit"),
        hr(),
        helpText("Budding Yeast Coevolutionary Network is developed and maintained by ", a("Jacob L. Steenwyk",href="https://jlsteenwyk.github.io/")),
        # user instructions
        h4("Function explanations"),
        p(strong("Genes of interest:"), 
            "Upload a single column file of genes IDs from", 
            em("Saccharomyces cerevisiae, Candida albicans,"),
            "or orthogroup gene identifier."),
        p(strong("Distance:"), 
            "Get additional nodes", em("N"), "distance from nodes of interest.",
            "For example, setting distance to 1 will get the subnetwork of the nodes",
            "of interest and all nodes they are immediately connected to whereas setting",
            "distance to 2 will get all nodes that two degrees removed from all nodes of interest."
        ),
        p(strong("Layout style:"), 
            "Various algorithms that determine coordinates for each node in a network."
        ),
        p(strong("Node labels:"), 
            "Yes or no to include node labels in the plot. Note, identifier labels for nodes",
            "of interest will always be plotted."
        ),
        p(strong("Node opacity:"), 
            "Determined by a number between 1 (opaque) and 0 (transparent)."
        ),
        helpText(strong("Warning:"), 
            "Not all uploaded genes may be present in the network. Below the download buttons,",
            "there is a table that shows which inputted genes are not present in the network.",
            "Genes that are present in the network are shown in the table on left."
        )
    ),
    
    # plot phylogeny in main panel
    mainPanel(
    plotOutput("GlobalPlot", height = "700px"),
    # download buttons
    downloadButton(outputId = "NetworkPlot", label = "Download Plot"),
    downloadButton(outputId = "NetworkDataFrame", label = "Download Data Frame"),
    downloadButton(outputId = "NodeInformation", label = "Download Node information"),
    hr(),
    column(7,
        tableOutput('matches')
    ),
    column(5,
        tableOutput('no_matches')
    ),
    # close mainPanel
    )
    # close sidebarLayout
    )
# close shinyUI and fliudPage  
))
