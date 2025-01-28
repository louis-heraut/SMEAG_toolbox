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


if ('extract_data' %in% to_do) {
    
    data = read_tibble(filedir=tmppath,
                       filename=paste0("data.fst"))
    
    meta = read_tibble(filedir=tmppath,
                       filename=paste0("meta.fst"))

    Code = levels(factor(meta$code))
    ok = meta$is_long
    ok[is.na(ok)] = FALSE
    Code_long = meta$code[ok]
    Code_long = Code_long[!duplicated(Code_long)]
    Code_AEAG = meta$code[grepl("AEAG", meta$origin)]
    Code_MK = sort(c(Code_long, Code_AEAG))
    Code_Sen = meta$code[!grepl("AEAG", meta$origin)]
    Code_Sen = Code_Sen[!duplicated(Code_Sen)]

    for (i in 1:length(extract_data)) {
        extract = extract_data[[i]]

        CARD_management(CARD=CARD_path,
                        tmp=tmppath,
                        layout=c(extract$name, "[",
                                 extract$variables, "]"),
                        overwrite=TRUE)

        if (extract$type == "criteria") {
            simplify = TRUE
            expand = FALSE
        } else if (extract$type == "serie") {
            simplify = FALSE
            expand = TRUE
        }

        if ("code_selection" %in% names(extract)) {
            data_tmp =
                dplyr::filter(data,
                              code %in% get(extract$code_selection))
        } else {
            data_tmp = data
        }

        res = CARD_extraction(data_tmp,
                              CARD_path=CARD_path,
                              CARD_dir=extract$name,
                              CARD_tmp=tmppath,
                              period=extract$period,
                              simplify=simplify,
                              suffix=extract$suffix,
                              expand_overwrite=expand,
                              sampling_period_overwrite=
                                  extract$sampling_period,
                              cancel_lim=extract$cancel_lim,
                              rm_duplicates=TRUE,
                              dev=FALSE,
                              verbose=verbose)
        dataEX = res$dataEX
        metaEX = res$metaEX

        if (!is.null(names(extract$variables))) {
            ok = nchar(names(extract$variables)) == 0
            names(extract$variables)[ok] = extract$variables[ok]
            metaEX$variable =
                names(extract$variables)[match(metaEX$variable_en,
                                               extract$variables)]
        }

        if (extract$do_trend) {
            trendEX = dplyr::tibble()
            
            for (i in 1:length(dataEX)) {
                trendEX_var = process_trend(
                    dataEX=dataEX[[i]],
                    metaEX=metaEX,
                    MK_level=level,
                    time_dependency_option="AR1",
                    suffix=extract$suffix,
                    period_trend=extract$period,
                    # period_change=period_change,
                    extreme_take_not_signif_into_account=TRUE,
                    extreme_take_only_id=NULL,
                    extreme_by_suffix=FALSE,
                    extreme_prob=prob_of_quantile_for_palette,
                    verbose=verbose)

                if (nrow(trendEX) == 0) {
                    trendEX = trendEX_var
                } else {
                    trendEX = dplyr::bind_rows(trendEX,
                                               trendEX_var)
                }
            }
        }

        
        
        write_tibble(dataEX,
                     filedir=tmppath,
                     filename=paste0("dataEX_",
                                     extract$name,
                                     ".fst"))
        write_tibble(metaEX,
                     filedir=tmppath,
                     filename=paste0("metaEX_",
                                     extract$name,
                                     ".fst"))

        if (extract$do_trend) {
            trendEX$period_trend =
                sapply(trendEX$period_trend,
                       paste0, collapse=" ")
            
            write_tibble(trendEX,
                         filedir=tmppath,
                         filename=paste0("trendEX_",
                                         extract$name,
                                         ".fst"))
        }
    }
}
