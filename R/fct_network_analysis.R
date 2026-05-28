
#-------------------------------------------------------------#
#-------------------------------------------------------------#
### Functions built by Paul Bzonek to summarize and plot network analysis data
### Functions were made by modifying code from:
        #Author:Kim Whoriskey    |   Whoriskey et al. 2019
        #https://doi.org/10.1111/2041-210X.13188
              #code used to get network movements between receivers

        #Author:Christopher Chizinski
        #https://chrischizinski.github.io/rstats/igraph-ggplotll/
              #code used to plot networks with a ggplot base
#-------------------------------------------------------------#
#-------------------------------------------------------------#




#-------------------------------------------------------------#
### Function to summarize network data for analysis and plotting
#-------------------------------------------------------------#
#' @title Summarize Network Plot Data
#' @name network_summary
#'
#' @description Summarizes acoustic telemetry detection data to produce movement network
#' statistics and data structures for plotting. Tracks movements between receiver stations
#' and accounts for receivers that may have been relocated to multiple positions.
#'
#' @param data A dataframe containing acoustic telemetry detection data with fish IDs,
#' receiver IDs, and geographic coordinates. Alternatively, a positionRtools
#' \code{atel} object — in which case \code{FishID}, \code{ReceiverID},
#' \code{lat}, and \code{long} are extracted automatically from the detections
#' component and do not need to be supplied.
#' @param FishID A vector or column from data describing fish identity (will be
#' coerced to integer). Not required when \code{data} is an \code{atel} object.
#' @param ReceiverID A vector or column from data describing acoustic receiver
#' identity. Not required when \code{data} is an \code{atel} object.
#' @param lat A numeric vector or column from data with latitude coordinates.
#' Not required when \code{data} is an \code{atel} object.
#' @param long A numeric vector or column from data with longitude coordinates.
#' Not required when \code{data} is an \code{atel} object.
#' @param ... Additional arguments (currently unused).
#'
#' @returns A list containing four elements:
#' \describe{
#'   \item{receiver.locations}{Dataframe of unique receiver locations with detection frequencies}
#'   \item{moves.matrix}{Matrix of pairwise movement counts between receivers}
#'   \item{individual.moves}{Dataframe of individual fish movements (from, to, fish)}
#'   \item{plot.data}{Dataframe formatted for network plotting with spatial coordinates}
#' }
#'
#' @export
#'
network_summary <- function(data, FishID = NULL, ReceiverID = NULL, lat = NULL, long = NULL, ...){

  # Extract columns from atel input, or validate that vectors were supplied
  #----------------------------#
  if (inherits(data, "atel")) {
    dets       <- as.data.frame(data$detections)
    FishID     <- dets$animal_id
    ReceiverID <- dets$receiver_sn
    lat        <- dets$deploy_lat
    long       <- dets$deploy_lon
    data       <- dets
  } else {
    if (is.null(FishID) || is.null(ReceiverID) || is.null(lat) || is.null(long)) {
      stop(
        "FishID, ReceiverID, lat, and long are required when data is not an atel object.",
        call. = FALSE
      )
    }
  }

  data$FishID<- as.integer(FishID) #Show function where to find data
  data$ReceiverNames <- ReceiverID #Show function where to find data
  data$ReceiverID<-as.integer(as.factor(ReceiverID)) #Show function where to find data
  data$lat <- as.numeric(lat) #Show function where to find data
  data$long <- as.numeric(long) #Show function where to find data

  #Look for unique reciever-location pairs. This is important if one reciever has been moved to multiple locations
  data$UniqueID <- as.numeric(as.factor(interaction(data$ReceiverID, data$lat, data$long))) #Combine lat and long values to find unique combinations. Convert text to numbers
  data$UniqueID <- as.integer(as.numeric(as.factor(data$UniqueID))) #Make UniqueID numbers sequential

  fishunique <- unique(data$FishID) #build vector of unique fish in dataset
  from <- numeric() #the UniqueID the fish moved FROM
  to <- numeric() #the UniqueID the fish moved TO
  fish <- numeric() #the fish

    for(i in 1:length(fishunique)){ #Build loop to track the receiver movements per fish#
      fishsub <- data[data$FishID==fishunique[i],] ##subset data$FishID to the run of detections for one fish
      from <- append(from, rle(fishsub$UniqueID)$values[-length(rle(fishsub$UniqueID)$values)]) #build a list of UniqueIDs, and show all but last, making a 'from' vector
      to <- append(to, rle(fishsub$UniqueID)$values[-1]) #build a list of UniqueIDs, and show all but first, making a 'to' vector
      fish <- append(fish, rep(unique(fishsub$FishID), length(rle(fishsub$UniqueID)$values)-1)) #record the fish id
      } #end loop tracking fish movements

  #record the data
  individual.moves <- data.frame(from, to, fish) #movements between UniqueIDs
  moves.matrix <- table(individual.moves[,1:2]) #gives the pairwise counts of all fish moving from one UniqueID to another, in matrix form
  moves <- data.frame(moves.matrix) #summarizes the pairwise counts
  moves <- moves[moves$Freq!=0,] #summarizes the pairwise counts

  Receiver.locations <- data.frame(table(data[,c('ReceiverNames','UniqueID','lat','long')])) #Summarize frequency of pings at unique Receiver locations
  Receiver.locations <- Receiver.locations[Receiver.locations$Freq!=0,] #Remove 0s made by non-existenent receiver-lat/long combinations
  Receiver.locations$lat <- as.numeric(as.character(Receiver.locations$lat)) #add latitude to dataframe
  Receiver.locations$long <- as.numeric(as.character(Receiver.locations$long)) #add longitude to dataframe


  #Build the dataframe that will be used for plotting
  plot.data<-as.data.frame(moves) #New dataframe
  plot.data$from.x <- Receiver.locations$long[match(plot.data$from, Receiver.locations$UniqueID)] #match the matrix data with the 'reciver.locations' data
  plot.data$from.y <- Receiver.locations$lat[match(plot.data$from, Receiver.locations$UniqueID)] #match the matrix data with the 'reciver.locations' data
  plot.data$to.x <- Receiver.locations$long[match(plot.data$to, Receiver.locations$UniqueID)] #match the matrix data with the 'reciver.locations' data
  plot.data$to.y <- Receiver.locations$lat[match(plot.data$to, Receiver.locations$UniqueID)] #match the matrix data with the 'reciver.locations' data

  ###
  return(list(receiver.locations=Receiver.locations, moves.matrix=moves.matrix, individual.moves=individual.moves, plot.data=plot.data )) #Store function data in useful structure
  ###
} #end function






#-------------------------------------------------------------#
### Function to summarize network data for analysis and plotting
#-------------------------------------------------------------#
#' @title Plot Network Data
#' @name network_plot
#'
#' @description Creates a ggplot2-based visualization of fish movement networks between
#' acoustic receiver stations. Displays movement frequencies as weighted edges and receiver
#' activity as colored nodes, with optional shapefile overlay for geographic context.
#'
#' @param data The list object created by the \code{network_summary()} function. Must
#' contain \code{plot.data} and \code{receiver.locations} dataframes.
#' @param Min.traffic Numeric. Minimum number of movements required to display an edge
#' between receivers. Default is 1.
#' @param shapefile An sf spatial object to overlay on the plot for geographic context.
#' Default is NA (no shapefile).
#' @param xlim Numeric vector. Optional x-axis limits (longitude). Default is NULL (auto-scaled).
#' @param ylim Numeric vector. Optional y-axis limits (latitude). Default is NULL (auto-scaled).
#' @param line.min Numeric. Minimum line width for movement edges. Default is 0.5.
#' @param line.max Numeric. Maximum line width for movement edges. Default is 2.5.
#' @param plot.title Character. Optional plot title. Default is NULL.
#' @param y.axis Character. Label for the y-axis. Default is "latitude".
#' @param x.axis Character. Label for the x-axis. Default is "longitude".
#' @param labels Logical. If TRUE, adds receiver name labels to the plot. Default is FALSE.
#' @param label.size Numeric. Text size for receiver labels. Default is 2.
#' @param label.transparency Numeric. Alpha transparency for labels (0-1). Default is 0.6.
#' @param label.nudge Numeric. Horizontal nudge distance for labels. Default is 0.
#' @param ... Additional arguments (currently unused).
#'
#' @return A ggplot object displaying the movement network. Nodes represent receivers
#' (colored by detection frequency using viridis scale), and edges represent movements
#' between receivers (width scaled by movement frequency).
#'
#' @import ggplot2
#' @import sf
#'
#' @export
#'
network_plot <- function(data, #specify previously created network matrix
                         Min.traffic=1, #specify if you want ignore any lines below traffic threshold
                         shapefile=NA, xlim=NULL, ylim=NULL, #specify shapefile, and potential domain limits
                         #colour.gradient.low="blue", colour.gradient.high="red", #pick your own colour scale
                         line.min=0.5, line.max=2.5, #pick your own line weights
                         #receiver.shape=21, receiver.size=4, receiver.stroke=2, #customize the look of your nodes
                         plot.title=NULL, y.axis="latitude", x.axis="longitude", #speficy optional figure title (useful if looping across fish)
                         labels=FALSE, label.size=2, label.transparency=0.6, label.nudge=0, #add labels, and specify details
                         ...){ #Start function

  plot.data<-subset(data$plot.data, data$plot.data$Freq>Min.traffic-1) #Filter out network lines/edges below specified traffic



  #Plot network
  a<- ggplot()+ #Use ggplot base
    #geom_polygon(data = shapefile, aes(x = long, y = lat, group = group), colour = "dark grey", fill = NA)+ #plot provided shapefile
    geom_sf(data=shapefile, colour = "black", fill=NA, inherit.aes = FALSE)+
    geom_segment(data=plot.data, aes(x=from.x, xend = to.x, y=from.y, yend = to.y, size=Freq), colour='burlywood4', alpha=0.6) +  #Plot movement lines
    scale_size("line", range = c(line.min, line.max))+  #Set line-size range
    geom_point(data=data$receiver.locations, aes(x=long, y=lat, fill=Freq), size=3, shape=21, stroke=1.3)+  #Add reciever circles, colour by matrix frequency
    viridis::scale_fill_viridis_c()+
    #coord_fixed(xlim=xlim, ylim=ylim)+  #Lock aspect ratio. Specify limits if shapefile is too big
    ylab(y.axis)+ #y axis label
    xlab(x.axis)+ # x axis label
    ggtitle(as.character(plot.title))+ #add optional title
    theme_classic() #simple plot theme


  #If else loop to look for optional labels.
  if(isFALSE(labels)){c <- NULL} else{ #Ignore the lines below if labels are not wanted
    c<-geom_label(data=data$receiver.locations, #add labels for receiver names
          aes(x=long, y=lat, label=(data$receiver.locations$ReceiverNames)), #add labels for receiver names
          hjust = 0, size=label.size, alpha=label.transparency, nudge_x=label.nudge) #add labels for receiver names
    } #end if else loop
  p <- a + c
  print(p)
  invisible(p)
} #end function
