site_altitude_densities <- site_altitude %$%
  MEAN %>%
  density() %>%
  broom::tidy() %>%
  tibble::as_tibble() %>%
  dplyr::mutate(y = y * 54) %>%
  dplyr::rename(Elevation = x,
                Frequency = y)



random_altitude_densities <- random_360_cut_altitude %$%
  MEAN %>%
  density() %>%
  broom::tidy() %>%
  tibble::as_tibble() %>%
  dplyr::mutate(y = y * 360) %>%
  dplyr::rename(Elevation = x,
                Frequency = y)


random_360_cut_densities <- random_360_cut_altitude %>%
  dplyr::select(MEAN)

d <- as.integer(random_360_cut_densities$MEAN)

random_altitude_densities <- foreach::foreach(n = 1:99, .combine = rbind) %do% {
  d %>%
    sample(nrow(site_altitude),
           replace = TRUE) %>%
    density() %>%
    broom::tidy() %>%
    tibble::as_tibble() %>%
    dplyr::mutate(y = y * 54)
} %>%
  dplyr::group_by(x) %>%
  purrrlyr::by_slice(function(x){
    quantile(x$y, probs = c(0.025, 0.5, 0.975)) %>%
      t() %>%
      broom::tidy()
  }, .collate = "rows") %>%
  magrittr::set_names(c("Elevation", "Lower CI", "Frequency", "Upper CI"))



g <- ggplot() +
  geom_line(data = random_altitude_densities,
            mapping = aes(x = Elevation,
                          y = Frequency)) +
  geom_line(data = site_altitude_densities,
            mapping = aes(x = Elevation,
                          y = Frequency),
            color = "red")
