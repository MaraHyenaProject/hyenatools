#' Get and/or plot hyena association networks
#'
#' Creates, plots, and returns association networks for specified clans
#'
#' @param clan One (or more) clan names, supplied as a character vector.
#' @param start Start date for desired networks
#' @param end End date for desired networks
#' @param network_duration Duration of data for each network. If NULL, all data between start and end are lumped into one network.
#' @param return_nets Should networks be output?
#' @param plot_nets Should networks be plotted?
#' @param If `plot_nets` is TRUE, path and filename of pdf where plots are output
#'
#' @return Returns weighted, undirected networks for each time period
#'
#' @examples
#' get_association_network(clan = c('serena.s', 'serena.n', 'happy.zebra'), start = '2009-01-01', end = '2025-01-01', network_duration = '12 months', return_nets = F, plot_nets = T, output_pdf_pathname = '~/Desktop/serena_nets.pdf')
#'
#' @export
#'


get_association_network <- function(clan, start, end,
                                    network_duration = '6 months',
                                    return_nets = F, plot_nets = F,
                                    output_pdf_pathname = NULL){

  suppressWarnings(data(tblSessions, tblHyenasPerSession, overwrite = F))
  library(igraph)
  tbls <- tblSessions
  tblhps <- tblHyenasPerSession
  if(is.null(tblhps$clan)){
    tblhps$clan <- dplyr::left_join(tblhps,
                             tbls,
                             by = 'session')$clan
  }

  clan_to_plot <- clan

  ###### Create sequence of dates #####
  dates <- seq.Date(from = as.Date(start), to = as.Date(end), by = network_duration)

  colors <- data.frame(clan = c('talek', 'talek.w', 'talek.e', 'pond', 'kcm', 'fig.tree', 'mara.river', 'cool.beans', 'happy.zebra', 'serena.s', 'serena.n'),
                       color = c('dodgerblue', 'dodgerblue4', 'magenta', 'firebrick', 'violet', 'darkgoldenrod2', 'darkgreen', 'limegreen', 'aquamarine3', 'salmon', 'skyblue'),
                       row.names = c('talek', 'talek.w', 'talek.e', 'pond', 'kcm', 'fig.tree', 'mara.river', 'cool.beans', 'happy.zebra', 'serena.s', 'serena.n'))

  net_list <- list()
  if(plot_nets)
    pdf(output_pdf_pathname, width = 9, height = 9)
  for(ds in dates){
    edges <- dplyr::filter(tblhps,
                    date >= ds,
                    date < (ds + 30.4375),
                    clan %in% clan_to_plot,
    )

    if(!nrow(edges)){
      next
    }

    gbi <- asnipe::get_group_by_individual(association_data = edges[c('id', 'session')], data_format = 'individuals')
    mat <- asnipe::get_network(gbi)


    ### make network in igraph
    net <- igraph::graph_from_adjacency_matrix(mat, weighted = T, mode = 'undirected')

    membership <- get_clan_status(V(net)$name, dates = as.Date(ds, format = '%Y-%m-%d', origin = '1970-01-01'))
    membership <- dplyr::left_join(membership, colors, by = 'clan')

    net <- set_vertex_attr(net, name = 'color', value = membership$color)
    net <- set_vertex_attr(net, name = 'label.color', value = membership$color)


    if(plot_nets){
      plot.igraph(net, vertex.size = 0.001,
                  edge.width = E(net)$weight*3,
                  edge.curved = TRUE, margin = c(0,0,0,0))
      text(x = seq(from = -0.8, to = 0.8, length.out = length(unique(membership$clan))),
           y = rep(-1.2, length(unique(membership$clan))),
           label = unique(membership$clan),
           col = unique(membership$color))
      text(x = -0.8, y = 1.2, label =
             paste0(dates[which(dates == ds)], ' - ', dates[which(dates == ds)+1]-1))
    }

    net_list[[length(net_list) + 1]] <- net
  }
  if(plot_nets)
    dev.off()

  if(return_nets)
    return(net_list)
}
