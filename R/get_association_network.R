#' Get and/or plot hyena association networks
#'
#' Creates, plots, and returns association networks for specified clans. Time
#' windows containing fewer than two sessions are skipped, since associations
#' cannot be computed from a single observation (a warning is issued for
#' single-session windows).
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

  ##############################################################################
  ### Error checking ###
  # Make sure tables and package exist
  if(!'hyenadata' %in% names(sessionInfo()$otherPkgs))
    warning('hyenadata package not loaded. This function may not work as expected.')

  if(!exists('tblSessions')){
    data("tblSessions")
    warning('tblSessions not in environment. Loading tblSessions from hyenadata package')
  }
  if(!exists('tblHyenasPerSession')){
    data("tblHyenasPerSession")
    warning('tblHyenasPerSession not in environment. Loading tblHyenasPerSession from hyenadata package')
  }
  ##############################################################################

  tbls <- tblSessions
  tblhps <- tblHyenasPerSession
  if(is.null(tblhps$clan)){
    tblhps$clan <- dplyr::left_join(tblhps,
                             tbls,
                             by = 'session')$clan
  }

  clan_to_plot <- clan

  ###### Create sequence of dates #####
  ## network_duration = NULL lumps all data between start and end into one network
  if(is.null(network_duration)){
    dates <- as.Date(start)
  }else{
    dates <- seq.Date(from = as.Date(start), to = as.Date(end), by = network_duration)
  }

  colors <- data.frame(clan = c('talek', 'talek.w', 'talek.e', 'pond', 'kcm', 'fig.tree', 'mara.river', 'cool.beans', 'happy.zebra', 'serena.s', 'serena.n'),
                       color = c('dodgerblue', 'dodgerblue4', 'magenta', 'firebrick', 'violet', 'darkgoldenrod2', 'darkgreen', 'limegreen', 'aquamarine3', 'salmon', 'skyblue'),
                       row.names = c('talek', 'talek.w', 'talek.e', 'pond', 'kcm', 'fig.tree', 'mara.river', 'cool.beans', 'happy.zebra', 'serena.s', 'serena.n'))

  net_list <- list()
  if(plot_nets)
    pdf(output_pdf_pathname, width = 9, height = 9)
  for(ds in dates){
    ds_date <- as.Date(ds, origin = '1970-01-01')
    ## Each network uses one full network_duration of data (window = ds to ds + duration);
    ## when network_duration is NULL, a single network spans all of start..end
    if(is.null(network_duration)){
      window_end <- as.Date(end) + 1
    }else{
      window_end <- seq.Date(ds_date, by = network_duration, length.out = 2)[2]
    }
    edges <- dplyr::filter(tblhps,
                    date >= ds_date,
                    date < window_end,
                    clan %in% clan_to_plot)

    ## A network needs at least two sessions; associations can't be computed
    ## from a single observation (and asnipe errors on a one-group matrix).
    n_sessions <- length(unique(edges$session))
    if(n_sessions < 2){
      if(n_sessions == 1)
        warning('Skipping network for window starting ', ds_date,
                ': only 1 session in this window (need at least 2).')
      next
    }

    gbi <- asnipe::get_group_by_individual(association_data = edges[c('id', 'session')], data_format = 'individuals')
    mat <- asnipe::get_network(gbi)


    ### make network in igraph
    net <- igraph::graph_from_adjacency_matrix(mat, weighted = T, mode = 'undirected')

    membership <- get_clan_status(igraph::V(net)$name, dates = as.Date(ds, format = '%Y-%m-%d', origin = '1970-01-01'))
    membership <- dplyr::left_join(membership, colors, by = 'clan')

    net <- igraph::set_vertex_attr(net, name = 'color', value = membership$color)
    net <- igraph::set_vertex_attr(net, name = 'label.color', value = membership$color)


    if(plot_nets){
      igraph::plot.igraph(net, vertex.size = 0.001,
                  edge.width = igraph::E(net)$weight*3,
                  edge.curved = TRUE, margin = c(0,0,0,0))
      text(x = seq(from = -0.8, to = 0.8, length.out = length(unique(membership$clan))),
           y = rep(-1.2, length(unique(membership$clan))),
           label = unique(membership$clan),
           col = unique(membership$color))
      text(x = -0.8, y = 1.2, label =
             paste0(ds_date, ' - ', window_end - 1))
    }

    net_list[[length(net_list) + 1]] <- net
  }
  if(plot_nets)
    dev.off()

  if(return_nets)
    return(net_list)
}
