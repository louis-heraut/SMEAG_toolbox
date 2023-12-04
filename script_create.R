# Copyright 2021-2023 Louis Héraut (louis.heraut@inrae.fr)*1,
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
        dir = data_to_use[i]
        Paths = list.files(file.path(computer_data_path, dir),
                           full.names=TRUE)

        if (all(grepl(obs_hydro_format, basename(Paths), fixed=TRUE))) {
            data_tmp = create_data_HYDRO(computer_data_path,
                                         dir,
                                         basename(Paths),
                                         verbose=verbose)
            meta_tmp = create_meta_HYDRO(computer_data_path,
                                         dir,
                                         basename(Paths),
                                         verbose=verbose)
        } else {
            Paths = Paths[!grepl("meta", Paths)]
            data_tmp2 = dplyr::tibble()
            for (path in Paths) {
                data_tmp2 = bind_rows(bind_cols(Code=gsub("[_].*", "",
                                                         basename(path)),
                                               read_tibble(path)),
                                     data_tmp2)
            }
            data_tmp2$Date = as.Date(data_tmp2$Date)
            data_tmp2$Q = data_tmp2$Qls*1E-3
            data_tmp2 = select(data_tmp2, -Qls)
            data_tmp = data_tmp2
            meta_tmp = read_tibble(file.path(computer_data_path,
                                             dir, "meta.csv"))
            
        }
        
        data_tmp = rename(data_tmp, !!paste0("Q_", type):=Q)

        if (nrow(data) == 0) {
            data = data_tmp
            meta = meta_tmp
        } else {
            data = dplyr::full_join(data, data_tmp, by=c("Code", "Date"))
            meta = bind_rows(meta, meta_tmp)
        }
    }

    data = arrange(data, Code)
    meta = arrange(meta, Code)
    meta = distinct(meta, Code, .keep_all=TRUE)
    Code = levels(factor(data$Code))
    
    if (codes_to_use != "all") {
        Code = Code[apply(sapply(codes_to_use, grepl, x=Code), 1, any)]
        data = data[data$Code %in% Code,]
    }
        
    # meta = get_lacune(dplyr::rename(data, Q=Q_inf), meta)

    data = dplyr::mutate(dplyr::group_by(data, Code),
                         nYear=as.numeric((max(Date) -
                                           min(Date))/365.25))
    data = dplyr::filter(data, nYear >= 30)
    data = dplyr::select(data, -nYear)

    Code_to_rm = meta$Code[meta$XL93_m >
                           quantile(meta$XL93_m, 0.999)]
    data = data[!(data$Code %in% Code_to_rm),]
    meta = meta[!(meta$Code %in% Code_to_rm),]
    
    write_tibble(data,
                 filedir=tmppath,
                 filename=paste0("data.fst"))
    write_tibble(meta,
                 filedir=tmppath,
                 filename=paste0("meta.fst"))
}
