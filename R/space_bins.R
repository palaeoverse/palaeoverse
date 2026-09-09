#' @export
space_bins <- function(spacing) {
  # This also ensures that there is no partial matching of arg names
  ensure_args_are_named(exceptions = "spacing")

  rlang::check_number_decimal(spacing, min = 0)

  # Generate equal area hexagonal grid
  # Which resolution should be used based on input distance/spacing?
  # Use the h3_info_table to calculate resolution
  grid <- h3_info_table[
    which.min(abs(h3_info_table$avg_cendist_km - spacing)),
  ]
  # Add column grid specification
  grid$grid <- c("primary")

  all_cells <- h3o::h3_from_strings(h3_all_cells)
  # Get children at desired resolution
  children <- h3o::get_children(
    all_cells,
    resolution = grid$h3_resolution
  )
  out <- do.call("c", lapply(children, function(x) sf::st_as_sfc(x)))

  class(out) <- c("palaeo_space_bins", class(out))
  attr(out, "spacing") <- spacing
  attr(out, "h3_resolution") <- grid$h3_resolution
  attr(out, "avg_cendist_km") <- grid$avg_cendist_km

  out
}

# Extracted from h3jsr::h3_info_table
# fmt: skip
h3_info_table <- data.frame(
  h3_resolution = 0:15,
  avg_area_sqm = c(
    4357449416078.39, 609788441794.1339, 86801780398.99731, 12393434655.08818,
    1770347654.491309, 252903858.1819452, 36129062.1644125, 5161293.359717198,
    737327.5975944188, 105332.5134272069, 15047.50190766437, 2149.643129451882,
    307.0918756316063, 43.87026794728301, 6.267181135324322, 0.8953115907605802
  ),
  avg_area_sqkm = c(
    4357449.416078383, 609788.4417941332, 86801.7803989972, 12393.43465508816,
    1770.347654491307, 252.9038581819449, 36.12906216441245, 5.161293359717191,
    0.7373275975944177, 0.1053325134272067, 0.01504750190766435,
    0.002149643129451879, 0.000307091875631606, 4.387026794728296e-05,
    6.267181135324313e-06, 8.95311590760579e-07
  ),
  avg_edge_m = c(
    1107712.591, 418676.0055, 158244.6558, 59810.85794, 22606.3794, 8544.408276,
    3229.482772, 1220.629759, 461.3546837, 174.3756681, 65.90780749, 24.9105614,
    9.415526211, 3.559893033, 1.348574562, 0.509713273
  ),
  avg_edge_km = c(
    1107.712591, 418.6760055, 158.2446558, 59.81085794, 22.6063794, 8.544408276,
    3.229482772, 1.220629759, 0.461354684, 0.174375668, 0.065907807, 0.024910561,
    0.009415526, 0.003559893, 0.001348575, 0.000509713
  ),
  avg_cendist_m = c(
    1918614.4877957636, 725168.1134359868, 274087.78387184907, 103595.4447963644,
    39155.397695978434, 14799.349254644, 5593.6282432723765, 2114.192759818554,
    799.089752478269, 302.0275167529676, 114.1556711881486, 43.1463579898641,
    16.30816977744848, 6.16591560266647, 2.3357996591789454, 0.8828492861282258
  ),
  avg_cendist_km = c(
    1918.6144877957636, 725.1681134359867, 274.08778387184907, 103.5954447963644,
    39.155397695978436, 14.799349254644, 5.593628243272376, 2.114192759818554,
    0.7990897529978843, 0.30202751657976246, 0.11415567033944371,
    0.04314635729704378, 0.016308169411985762, 0.006165915545508794,
    0.002335800417817199, 0.0008828488132783551
  ),
  total_unique_indexes = c(
    122, 842, 5882, 41162, 288122, 2016842, 14117882, 98825162, 691776122,
    4842432842, 33897029882, 237279209162, 1660954464122, 11626681248842,
    81386768741882, 569707381193162
  )
)

# Extracted from h3jsr::get_res0()
# fmt: skip
h3_all_cells <- c(
  "8001fffffffffff", "8003fffffffffff", "8005fffffffffff", "8007fffffffffff",
  "8009fffffffffff", "800bfffffffffff", "800dfffffffffff", "800ffffffffffff",
  "8011fffffffffff", "8013fffffffffff", "8015fffffffffff", "8017fffffffffff",
  "8019fffffffffff", "801bfffffffffff", "801dfffffffffff", "801ffffffffffff",
  "8021fffffffffff", "8023fffffffffff", "8025fffffffffff", "8027fffffffffff",
  "8029fffffffffff", "802bfffffffffff", "802dfffffffffff", "802ffffffffffff",
  "8031fffffffffff", "8033fffffffffff", "8035fffffffffff", "8037fffffffffff",
  "8039fffffffffff", "803bfffffffffff", "803dfffffffffff", "803ffffffffffff",
  "8041fffffffffff", "8043fffffffffff", "8045fffffffffff", "8047fffffffffff",
  "8049fffffffffff", "804bfffffffffff", "804dfffffffffff", "804ffffffffffff",
  "8051fffffffffff", "8053fffffffffff", "8055fffffffffff", "8057fffffffffff",
  "8059fffffffffff", "805bfffffffffff", "805dfffffffffff", "805ffffffffffff",
  "8061fffffffffff", "8063fffffffffff", "8065fffffffffff", "8067fffffffffff",
  "8069fffffffffff", "806bfffffffffff", "806dfffffffffff", "806ffffffffffff",
  "8071fffffffffff", "8073fffffffffff", "8075fffffffffff", "8077fffffffffff",
  "8079fffffffffff", "807bfffffffffff", "807dfffffffffff", "807ffffffffffff",
  "8081fffffffffff", "8083fffffffffff", "8085fffffffffff", "8087fffffffffff",
  "8089fffffffffff", "808bfffffffffff", "808dfffffffffff", "808ffffffffffff",
  "8091fffffffffff", "8093fffffffffff", "8095fffffffffff", "8097fffffffffff",
  "8099fffffffffff", "809bfffffffffff", "809dfffffffffff", "809ffffffffffff",
  "80a1fffffffffff", "80a3fffffffffff", "80a5fffffffffff", "80a7fffffffffff",
  "80a9fffffffffff", "80abfffffffffff", "80adfffffffffff", "80affffffffffff",
  "80b1fffffffffff", "80b3fffffffffff", "80b5fffffffffff", "80b7fffffffffff",
  "80b9fffffffffff", "80bbfffffffffff", "80bdfffffffffff", "80bffffffffffff",
  "80c1fffffffffff", "80c3fffffffffff", "80c5fffffffffff", "80c7fffffffffff",
  "80c9fffffffffff", "80cbfffffffffff", "80cdfffffffffff", "80cffffffffffff",
  "80d1fffffffffff", "80d3fffffffffff", "80d5fffffffffff", "80d7fffffffffff",
  "80d9fffffffffff", "80dbfffffffffff", "80ddfffffffffff", "80dffffffffffff",
  "80e1fffffffffff", "80e3fffffffffff", "80e5fffffffffff", "80e7fffffffffff",
  "80e9fffffffffff", "80ebfffffffffff", "80edfffffffffff", "80effffffffffff",
  "80f1fffffffffff", "80f3fffffffffff"
)
