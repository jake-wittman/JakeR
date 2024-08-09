#' Combine dashboard data with dictionary
#'
#' @param consol_dat Dashboard database
#' @param data_dict The data dictionary
#'
#' @return A data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' joinDataWithDictionary(consol_dat, data_dict)
#' }
joinDataWithDictionary <- function(consol_dat, data_dict) {
  consol_dat |>
    dplyr::mutate(dplyr::across(tidyselect::ends_with('ID'), ~as.character(.x))) |>
    # Get actual names for the IndicatorID
    dplyr::inner_join(data_dict |>
                 dplyr::filter(VariableTypeID == 1) |>
                 dplyr::select(Name, vid),
               by = c('IndicatorID' = 'vid')) |>
    dplyr::rename(IndicatorName = Name) |>
    dplyr::left_join(data_dict |>  # Suppress ID join
                dplyr::filter(VariableTypeID == 4) |>
                dplyr::select(Name, vid),
              by = c('SuppressID' = 'vid')) |>
    dplyr::rename(SuppressName = Name) |>
    dplyr::left_join(data_dict |>  # Race ID join
                dplyr::filter(VariableTypeID == 5) |>
                dplyr::select(Name, vid),
              by = c('RaceID' = 'vid')) |>
    dplyr::rename(RaceName = Name) |>
    dplyr::left_join(data_dict |>  # Gender ID join
                dplyr::filter(VariableTypeID == 6) |>
                dplyr::select(Name, vid),
              by = c('GenderID' = 'vid')) |>
    dplyr::rename(GenderName = Name) |>
    dplyr::left_join(data_dict |>  # Age ID join
                dplyr::filter(VariableTypeID == 7) |>
                dplyr::select(Name, vid),
              by = c('AgeID' = 'vid')) |>
    dplyr::rename(AgeName = Name) |>
    dplyr::left_join(data_dict |>  # Data source ID join
                dplyr::filter(VariableTypeID == 8)  |>
                dplyr::select(Name, vid) |>
                dplyr::distinct(Name, vid),
              by = c('DataSourceID' = 'vid'),
              relationship = 'many-to-one') |>
    dplyr::rename(DatasourceName = Name) |>
    dplyr::left_join(data_dict |>  # Estimate ID join
                dplyr::filter(VariableTypeID == 9) |>
                dplyr::select(Name, vid),
              by = c('EstimateID' = 'vid')) |>
    dplyr::rename(EstimateName = Name) |>
    dplyr::left_join(data_dict |>  # EducationID join
                dplyr::filter(VariableTypeID == 10) |>
                dplyr::select(Name, vid),
              by = c('EducationID' = 'vid')) |>
    dplyr::rename(EducationName = Name) |>
    dplyr::left_join(data_dict |>  # MiscID join
                dplyr::filter(VariableTypeID == 11) |>
                dplyr::select(Name, vid),
              by = c('MiscID' = 'vid')) |>
    dplyr::rename(MiscName = Name) |>
    dplyr::left_join(data_dict |>  # Dataset ID join
                dplyr::filter(VariableTypeID == 20) |>
                dplyr::select(Name, vid),
              by = c('DatasetID' = 'vid')) |>
    dplyr::rename(DatasetName = Name)
}
