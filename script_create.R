# Copyright 2021-2024 Louis Héraut (louis.heraut@inrae.fr)*1,
#                     Éric Sauquet (eric.sauquet@inrae.fr)*1
#
# *1   INRAE, France
#
# This file is part of AEAG_toolbox R toolbox.
#
# AEAG_toolbox R toolbox is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# AEAG_toolbox R toolbox is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with AEAG_toolbox R toolbox.
# If not, see <https://www.gnu.org/licenses/>.


# Script that manages the call to the right process in order to
# realise extraction of data.


if ('create_data' %in% to_do) {

    data = dplyr::tibble()
    data_tmp = dplyr::tibble()
    meta = dplyr::tibble()
    meta_tmp = dplyr::tibble()
    
    for (i in 1:length(data_to_use)) {
        type = names(data_to_use)[i]
        dirs = data_to_use[[i]]
        for (dir in dirs) {
            print(paste0("from ", dir))
            Paths = list.files(file.path(computer_data_path, dir),
                               full.names=TRUE)

            Code = gsub("[_].*", "", basename(Paths))
            if (codes_to_use != "all") {
                Paths = Paths[grepl(pattern_codes_to_use, Code)]
            }
            if (length(Paths) == 0) {
                next
            }

            if (all(grepl(obs_hydro_format, basename(Paths),
                          fixed=TRUE))) {
                data_tmp = create_data_HYDRO(computer_data_path,
                                             dir,
                                             basename(Paths),
                                             variable_to_load="Qm3s",
                                             verbose=verbose)

                for (i in 1:nrow(flag)) {
                    data$Q[data$code == flag$code[i] &
                           data$date == flag$date[i]] = flag$Q[i]
                }
                
                meta_tmp = create_meta_HYDRO(computer_data_path,
                                             dir,
                                             basename(Paths),
                                             hydrological_region_level=2,
                                             verbose=verbose)
                
            } else {
                Paths = Paths[!grepl("meta", Paths)]
                data_tmp = dplyr::tibble()
                for (path in Paths) {
                    data_tmp = bind_rows(bind_cols(
                        code=gsub("[_].*", "",
                                  basename(path)),
                        read_tibble(path)),
                        data_tmp)
                }
                data_tmp$date = as.Date(data_tmp$date)
                data_tmp = dplyr::rename(data_tmp, Q=Qm3s)
                meta_tmp = read_tibble(file.path(computer_data_path,
                                                 dir, "meta.csv"))
            }
            
            data_tmp = rename(data_tmp, !!paste0("Q_", type):=Q)
            meta_tmp$type = type
            meta_tmp$origin = dir

            if (nrow(data) == 0) {
                data = data_tmp
                meta = meta_tmp
            } else {
                if (!(paste0("Q_", type) %in% names(data))) {
                    data = dplyr::full_join(data, data_tmp,
                                            by=c("code", "date"))
                } else {
                    data = dplyr::bind_rows(data, data_tmp)
                }
                meta = bind_rows(meta, meta_tmp)
            }
        }
    }

    data = arrange(data, code, date)
    meta = arrange(meta, code, type)
    meta = distinct(meta, code, type, .keep_all=TRUE)
    Code = levels(factor(data$code))

    meta$is_long[is.na(meta$is_long)] = FALSE
    
    meta = get_lacune(dplyr::select(data, code, date, Q=Q_inf), meta)
    meta = dplyr::rename(meta,
                         meanLac_inf=meanLac,
                         tLac_pct_inf=tLac_pct)
    meta = get_lacune(dplyr::select(data, code, date, Q=Q_nat), meta)
    meta = dplyr::rename(meta,
                         meanLac_nat=meanLac,
                         tLac_pct_nat=tLac_pct)
    
    write_tibble(data,
                 filedir=tmppath,
                 filename=paste0("data.fst"))
    write_tibble(meta,
                 filedir=tmppath,
                 filename=paste0("meta.fst"))
}
