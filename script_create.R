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
    
    for (i in 1:length(data_to_use)) {
        type = names(data_to_use)[i]
        file = data_to_use[i]
        
        data_tmp = read_tibble(file.path(local_data_path, file))
        data_tmp = select(data_tmp, -where(is.logical))
        data_tmp = tidyr::pivot_longer(data_tmp,
                                       cols=where(is.numeric),
                                       names_to="Code", values_to="Q")
        data_tmp = rename(data_tmp, Date=X)
        data_tmp = arrange(data_tmp, Code)

        data_tmp = rename(data_tmp, !!paste0("Q_", type):=Q)

        if (nrow(data) == 0) {
            data = data_tmp
        } else {
            data = dplyr::full_join(data, data_tmp, by=c("Code", "Date"))
        }
    }

    Code = levels(factor(data$Code))
    
    if (codes_to_use != "all") {
        Code = Code[apply(sapply(codes_to_use, grepl, x=Code), 1, any)]
        data = data[data$Code %in% Code,]
    }
    data$Date = as.Date(data$Date)
        
    meta = create_meta_HYDRO(computer_data_path,
                             banque_hydro_dir,
                             paste0(Code, obs_hydro_format),
                             verbose=verbose)
    meta = get_lacune(dplyr::rename(data, Q=Q_obs), meta)
    
    write_tibble(data,
                 filedir=tmppath,
                 filename=paste0("data.fst"))
    write_tibble(meta,
                 filedir=tmppath,
                 filename=paste0("meta.fst"))
}
