# get_association_network() needs asnipe + igraph. Data comes from the network
# fixture (see helper-fixtures.R); edges are co-occurrence in sessions. The
# function warns when hyenadata isn't attached and asnipe prints progress, so we
# suppress warnings and captured output via run_net().

setup_net <- function() {
  skip_if_not_installed("asnipe")
  skip_if_not_installed("igraph")
  use_network_fixtures()
}

# Run the function while silencing asnipe's progress prints and the incidental
# "hyenadata not loaded" warnings.
run_net <- function(...) {
  nets <- NULL
  suppressWarnings(invisible(capture.output(
    nets <- get_association_network(...)
  )))
  nets
}

# Undirected edges as sorted "x-y" strings, for order-independent comparison.
edge_set <- function(net) {
  el <- igraph::as_edgelist(net)
  if (nrow(el) == 0) return(character(0))
  apply(el, 1, function(r) paste(sort(r), collapse = "-"))
}

vertex_set <- function(net) sort(igraph::V(net)$name)

test_that("windows the data by network_duration (one network per non-empty period)", {
  setup_net()
  on.exit(clear_fixtures(), add = TRUE)

  nets <- run_net(clan = "talek", start = "2010-01-01", end = "2010-03-31",
                  network_duration = "1 months", return_nets = TRUE, plot_nets = FALSE)

  # Jan and Mar have data; Feb is empty and skipped
  expect_length(nets, 2)

  sizes <- vapply(nets, igraph::vcount, numeric(1))
  jan <- nets[[which(sizes == 3)]]
  mar <- nets[[which(sizes == 2)]]

  expect_equal(vertex_set(jan), c("a", "b", "c"))
  expect_setequal(edge_set(jan), c("a-b", "b-c"))   # a and c never co-occur in Jan

  expect_equal(vertex_set(mar), c("a", "c"))
  expect_setequal(edge_set(mar), "a-c")
})

test_that("network_duration = NULL lumps all data into a single network", {
  setup_net()
  on.exit(clear_fixtures(), add = TRUE)

  nets <- run_net(clan = "talek", start = "2010-01-01", end = "2010-03-31",
                  network_duration = NULL, return_nets = TRUE, plot_nets = FALSE)

  expect_length(nets, 1)
  expect_equal(vertex_set(nets[[1]]), c("a", "b", "c"))
  # Across all talek sessions: a-b (s1), b-c (s2), a-c (s4)
  expect_setequal(edge_set(nets[[1]]), c("a-b", "b-c", "a-c"))
})

test_that("only sessions of the focal clan are included", {
  setup_net()
  on.exit(clear_fixtures(), add = TRUE)

  # serena.n individuals (d, e) must never appear in a talek network
  talek <- run_net(clan = "talek", start = "2010-01-01", end = "2010-03-31",
                   network_duration = NULL, return_nets = TRUE, plot_nets = FALSE)
  expect_false(any(c("d", "e") %in% vertex_set(talek[[1]])))

  # and a serena.n run yields the d-e network
  serena <- run_net(clan = "serena.n", start = "2010-01-01", end = "2010-03-31",
                    network_duration = NULL, return_nets = TRUE, plot_nets = FALSE)
  expect_equal(vertex_set(serena[[1]]), c("d", "e"))
  expect_setequal(edge_set(serena[[1]]), "d-e")
})

test_that("a window with a single session is skipped with a warning", {
  setup_net()
  on.exit(clear_fixtures(), add = TRUE)

  # serena.n is seen once in Jan (s3) and once in Feb (s6): each one-month window
  # has a single session, so both are skipped (and warn).
  nets <- NULL
  w <- capture_warnings(
    nets <- get_association_network(clan = "serena.n", start = "2010-01-01",
              end = "2010-02-28", network_duration = "1 months",
              return_nets = TRUE, plot_nets = FALSE)
  )

  expect_true(any(grepl("only 1 session", w)))
  expect_length(nets, 0)
})

test_that("a date range with no sessions returns an empty list", {
  setup_net()
  on.exit(clear_fixtures(), add = TRUE)

  nets <- run_net(clan = "talek", start = "2015-01-01", end = "2015-12-31",
                  network_duration = "1 months", return_nets = TRUE, plot_nets = FALSE)

  expect_length(nets, 0)
})

test_that("return_nets = FALSE returns NULL", {
  setup_net()
  on.exit(clear_fixtures(), add = TRUE)

  res <- run_net(clan = "talek", start = "2010-01-01", end = "2010-03-31",
                 network_duration = "1 months", return_nets = FALSE, plot_nets = FALSE)

  expect_null(res)
})
