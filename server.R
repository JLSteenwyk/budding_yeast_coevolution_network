#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

# Write function to get information about network including:
# edge-to-edge info, coevo. coeff., 
# Write function to get more complex plots that include
# degree histogram, and possibly network properties such
# as transitivity, mean_distance, etc.

## Load necessary packages
if (!require("igraph")) {
   install.packages("igraph", dependencies = TRUE)
   library(igraph)
   }

## read data
edges<-read.table("Data/2408OGs_gene-gene_correlation.sig-only.edges.txt", sep='\t', header=T)
cols<-read.table("Data/2408OGs_gene-gene_correlation.cfg_community_cols.txt", header=T, comment.char="*")
genes<-read.table("./Data/Scerevisiae_and_Calbicans_gene_per_og.txt", sep='\t', fill=T, header=T)
net <- graph_from_data_frame(d=edges, vertices=cols, directed=F)


# Define server logic
shinyServer(function(input, output, session) {
    ## display table of taxa names
    FUNGItipNames <- eventReactive(input$FUNGIgo, {
        a<-data.frame(FUNGItree()$tip.label)
        # replace column name to "full list of taxa"
        colnames(a)[1]<-"full list of taxa for possible subtree"
        # return to df_subset
        return(a)
        })
    output$FUNGItaxaTable <- renderTable(FUNGItipNames())

    ### "Static" part: no click on actionButton yet
    output$GlobalPlot <- renderImage({
        list(src = "www/community_network.png",
            contentType = 'image/png')
    }, deleteFile = FALSE)

    # reactively read in data
    userData <- reactive({
            inFile <- input$gene_list_file
            og_list<-read.table(inFile$datapath)
            return(og_list)
        })

    ### Click on actionButton
    observeEvent(input$userGO, {
        output$GlobalPlot <- renderUI({
        plotOutput("GlobalPlot")
        })

        output$GlobalPlot <- renderPlot({
            par(fig=c(0,1,.275,1))
            # obtain subnetwork
            nodes_of_interest <- genes[
                apply(genes, 1, function(x) any(grepl(paste(userData()$V1,collapse="|"), x))),
                ]$Orthologous_group
            selnodes <- V(net)[name %in% nodes_of_interest]
            selegoV <- ego(net, order=input$order, nodes = selnodes, mode = "all", mindist = 0)
            selegoG <- induced_subgraph(net,unlist(selegoV))
            # true or false, add vertex labels
            vertex_label <- if (input$node_labels) V(selegoG)$name else 
                ifelse(V(selegoG)$name %in% nodes_of_interest, V(selegoG)$name, NA)
            #l2 <- layout_with_mds(selegoG)

            set.seed(178245)

            if (input$layout_style == "layout_with_mds") { 
                l2 <- layout_with_mds(selegoG)
                } else if (input$layout_style == "layout_with_fr") {
                l2 <- layout_with_fr(selegoG)
                } else if  (input$layout_style == "layout_in_circle") {
                l2 <- layout_in_circle(selegoG)
                } else if  (input$layout_style == "layout_on_grid") {
                l2 <- layout_on_grid(selegoG)
                } else if  (input$layout_style == "layout_with_drl") {
                l2 <- layout_with_drl(selegoG)
                } else {
                l2 <- layout_with_mds(selegoG)
            }

            plot(selegoG, vertex.label.color="black", vertex.label=vertex_label,
                vertex.frame.color=NA, vertex.size=5.75, asp = 0, layout = l2, 
                vertex.color = adjustcolor(V(selegoG)$color, alpha.f = input$node_alpha),
                edge.color = adjustcolor("darkgrey", alpha.f = .75), main="Coevolutionary Network")
            
            degs<-data.frame(degree(selegoG, mode="all"))
            deg.dist <- degree_distribution(selegoG, cumulative=T, mode="all")

            par(fig=c(0,.5,0,.4), new=TRUE)
            nodes_of_interest_degs<-degs[grepl(paste(nodes_of_interest, collapse="|"), rownames(degs)), ]
            data<-data.frame(nodes_of_interest, nodes_of_interest_degs)
            data2 <- data[order(data[,2],decreasing=TRUE),]
            barplot(data2[,2],names.arg=data2[,1], main="Node degree", ylab="Degree", las=2)

            par(fig=c(.5,1,0,.4), new=TRUE)
            plot( x=0:max(degs), y=1-deg.dist, pch=19, cex=1.2, col="orange", 
                xlab="Degree", ylab="Cumulative Frequency", main="Cumulative freq. of node degree")
            
        })
    })
  
    no_matches <- eventReactive(input$userGO, {
        a<-data.frame(userData()$V1)
        tf<-apply(a, 1, function(x) !any(x %in% rbind(genes$Orthologous_group, genes$Scerevisiae, genes$Calbicans)))
        not_found<-data.frame(userData()[tf,])
        colnames(not_found)<-"Not present in network"
        # return to df_subset
        return(not_found)
    })

    matches <- eventReactive(input$userGO, {
        # a<-data.frame(userData()$V1)
        # tf<-apply(a, 1, function(x) any(x %in% rbind(genes$Orthologous_group, genes$Scerevisiae, genes$Calbicans)))
        # found<-data.frame(userData()[tf,])
        # colnames(found)<-"Present in network"
        nodes_of_interest <- genes[
            apply(genes, 1, function(x) any(grepl(paste(userData()$V1,collapse="|"), x))),
            ]$Orthologous_group
        found<-genes[apply(genes, 1, function(x) any(grepl(paste(nodes_of_interest,collapse="|"), x))), ]
        colnames(found) <- c("Identifier", "S. cerevisiae", "C. albicans")
        # return to df_subset
        return(found)
    })

    output$matches <- renderTable(matches())
    output$no_matches <- renderTable(no_matches())

    ## save pdf
    output$NetworkPlot<- downloadHandler(
        #Specify The File Name 
        filename = function() {
        paste("Networkplot-",Sys.Date(),".pdf",sep= "")},
        content = function(file){
        # open the pdf
        pdf(file)

        par(fig=c(0,1,.275,1))
        # obtain subnetwork
        nodes_of_interest <- genes[
            apply(genes, 1, function(x) any(grepl(paste(userData()$V1,collapse="|"), x))),
            ]$Orthologous_group
        selnodes <- V(net)[name %in% nodes_of_interest]
        selegoV <- ego(net, order=input$order, nodes = selnodes, mode = "all", mindist = 0)
        selegoG <- induced_subgraph(net,unlist(selegoV))
        # true or false, add vertex labels
        vertex_label <- if (input$node_labels) V(selegoG)$name else 
            ifelse(V(selegoG)$name %in% nodes_of_interest, V(selegoG)$name, NA)
            #l2 <- layout_with_mds(selegoG)

            set.seed(178245)

            if (input$layout_style == "layout_with_mds") { 
                l2 <- layout_with_mds(selegoG)
                } else if (input$layout_style == "layout_with_fr") {
                l2 <- layout_with_fr(selegoG)
                } else if  (input$layout_style == "layout_nicely") {
                l2 <- layout_nicely(selegoG)
                } else if  (input$layout_style == "layout_on_grid") {
                l2 <- layout_on_grid(selegoG)
                } else if  (input$layout_style == "layout_with_drl") {
                l2 <- layout_with_drl(selegoG)
                } else {
                l2 <- layout_with_mds(selegoG)
            }

            plot(selegoG, vertex.label.color="black", vertex.label=vertex_label,
                vertex.frame.color=NA, vertex.size=5.75, asp = 0, layout = l2, 
                vertex.color = adjustcolor(V(selegoG)$color, alpha.f = input$node_alpha),
                edge.color = adjustcolor("darkgrey", alpha.f = .75), main="Coevolutionary Network")
            
            degs<-data.frame(degree(selegoG, mode="all"))
            deg.dist <- degree_distribution(selegoG, cumulative=T, mode="all")

            par(fig=c(0,.5,0,.4), new=TRUE)
            nodes_of_interest_degs<-degs[grepl(paste(nodes_of_interest, collapse="|"), rownames(degs)), ]
            data<-data.frame(nodes_of_interest, nodes_of_interest_degs)
            data2 <- data[order(data[,2],decreasing=TRUE),]
            barplot(data2[,2],names.arg=data2[,1], main="Node degree", ylab="Degree", las=2)

            par(fig=c(.5,1,0,.4), new=TRUE)
            plot( x=0:max(degs), y=1-deg.dist, pch=19, cex=1.2, col="orange", 
                xlab="Degree", ylab="Cumulative Frequency", main="Cumulative freq. of node degree")

        dev.off()
        }
    )
    
    ## Download data frame 
    output$NetworkDataFrame<- downloadHandler(
        # Specify the file name
        filename=function() {
        paste("Network_data_frame-",Sys.Date(),".txt",sep= "")},
        content=function(file){
            nodes_of_interest <- genes[
                apply(genes, 1, function(x) any(grepl(paste(userData()$V1,collapse="|"), x))),
                ]$Orthologous_group
            selnodes <- V(net)[name %in% nodes_of_interest]
            selegoV <- ego(net, order=input$order, nodes = selnodes, mode = "all", mindist = 0)
            selegoG <- induced_subgraph(net,unlist(selegoV))
            df<-as_data_frame(selegoG)
            df<-data.frame(cbind(df$from, df$to, df$weight))
            colnames(df) <- c("Identifier_1", "Identifier_2", "Coevolution_Coefficient")
            write.table(df, file, row.names=FALSE,sep="\t", quote = FALSE)
        }
    )

    ## Download community and colors 
    output$NodeInformation<- downloadHandler(
        # Specify the file name
        filename=function() {
        paste("Community_info-",Sys.Date(),".txt",sep= "")},
        content=function(file){
            nodes_of_interest <- genes[
                apply(genes, 1, function(x) any(grepl(paste(userData()$V1,collapse="|"), x))),
                ]$Orthologous_group
            selnodes <- V(net)[name %in% nodes_of_interest]
            selegoV <- ego(net, order=input$order, nodes = selnodes, mode = "all", mindist = 0)
            selegoG <- induced_subgraph(net,unlist(selegoV))
            degs<-data.frame(degree(selegoG, mode="all"))
            degs$names <- rownames(degs)
            colnames(degs) <- c("Degree", "Name")
            community_and_col <- cbind(
                cols[
                    apply(cols, 1, function(x) any(grepl(paste(V(selegoG)$name,collapse="|"), x))),
                ],
                genes[
                    apply(genes, 1, function(x) any(grepl(paste(V(selegoG)$name,collapse="|"), x))),
                ],
                degs[
                    apply(degs, 1, function(x) any(grepl(paste(V(selegoG)$name,collapse="|"), x))),
                ]
            )
            community_and_col <- data.frame(cbind(
                community_and_col$Orthologous_group,
                community_and_col$Scerevisiae,
                community_and_col$Calbicans,
                community_and_col$Degree,
                community_and_col$community,
                community_and_col$color
            ))

            colnames(community_and_col) <- c(
                "Orthologous group", "S. cerevisiae", "C. albicans",
                "Degree", "Community identifier", "Community color"
            )

            write.table(community_and_col, file, row.names=FALSE,sep="\t", quote = FALSE)
        }
    )

})