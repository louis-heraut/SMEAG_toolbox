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
# realise plottings of data analyses.


logo_path = load_logo(resources_path, logo_dir, logo_to_show)
icon_path = file.path(resources_path, icon_dir)

if (!exists("Shapefiles")) {
    print("### Loading shapefiles")
    Shapefiles = load_shapefile(
        computer_shp_path, Code,
        france_shp_path,
        bassinHydro_shp_path,
        regionHydro_shp_path,
        secteurHydro_shp_path,
        entiteHydro_shp_path, entiteHydro_coord,
        entitePiezo_shp_path=NULL,
        river_shp_path,
        river_selection=river_selection,
        river_length=river_length,
        toleranceRel=toleranceRel)
}


if ('plot_sheet' %in% to_do & !('plot_doc' %in% to_do)) {
    Pages = NULL
    doc_chunk = ""
    doc_title = default_doc_title
    doc_subtitle = NULL
    sheet_list = plot_sheet
}
if ('plot_doc' %in% to_do) {
    Pages = dplyr::tibble()
    doc_chunk = plot_doc$chunk
    doc_title = plot_doc$title
    doc_subtitle = plot_doc$subtitle
    sheet_list = plot_doc$sheet
}


if (doc_chunk == "") {    
    chunkCode = list(codes_to_use)#list(CodeALL10)
    plotCode = list(codes_to_use)
    
}


nChunk = length(chunkCode)

for (i in 1:nChunk) {

    chunk = chunkCode[[i]]
    Code_to_plot = plotCode[[i]]
    chunkname = names(chunkCode)[i]

    doc_title_ns = gsub(" ", "_", doc_title)

    if (!is.null(doc_subtitle)) {
        doc_title_ns = paste0(doc_title_ns,
                              "_",
                              gsub(" ", "_", doc_subtitle))
    }
    
    if (!is.null(chunkname)) {
        doc_chunkname = gsub(" ", "_",
                             gsub(" [-] ", "_",
                                  chunkname))
        today_figdir_leaf = file.path(today_figdir,
                                      doc_title_ns,
                                      doc_chunkname, "PDF")
    } else if ('plot_doc' %in% to_do) {
        today_figdir_leaf = file.path(today_figdir,
                                      doc_title_ns,
                                      "PDF")
    } else {
        today_figdir_leaf = today_figdir
    }

    data_chunk = data[data$Code %in% chunk,]
    meta_chunk = meta[meta$Code %in% chunk,]

    # if (exists("dataEX_criteria")) {
    #     if (nrow(dataEX_criteria) > 0) {
    #         dataEX_criteria_chunk =
    #             dataEX_criteria[dataEX_criteria$Code %in% chunk,]
    #         if (nrow(dataEX_criteria_chunk) == 0 &
    #             nrow(dataEX_criteria) != 0) {
    #             next
    #         }
    #     }
    # }
    # if (exists("metaEX_criteria")) {
    #     metaEX_criteria_chunk = metaEX_criteria
    # }

    if (exists("trendEX_SMEAG_hydrologie")) {
        trendEX_serie_chunk = trendEX_SMEAG_hydrologie
    }
    if (exists("dataEX_SMEAG_hydrologie")) {
        dataEX_serie_chunk = list()
            for (j in 1:length(dataEX_SMEAG_hydrologie)) {
                dataEX_serie_chunk = append(
                    dataEX_serie_chunk,
                    list(dataEX_SMEAG_hydrologie[[j]][
                        dataEX_SMEAG_hydrologie[[j]]$Code %in%
                        chunk,]))
            }
        names(dataEX_serie_chunk) = names(dataEX_SMEAG_hydrologie)
    }
    if (exists("metaEX_SMEAG_hydrologie")) {
        metaEX_serie_chunk = metaEX_SMEAG_hydrologie
    }
    
    for (sheet in sheet_list) {
        
        if (sheet == 'sommaire') {
            print("### Plotting summary")
            Pages = tibble(section='Sommaire', subsection=NA, n=1)
        }

        if (sheet == 'carte_regime') {
            print("### Plotting regime map")
            sheet_regime_map(meta,
                             icon_path=icon_path,
                             logo_path=logo_path,
                             is_foot=FALSE,
                             figdir=today_figdir_leaf,
                             Pages=Pages,
                             Shapefiles=Shapefiles,
                             verbose=verbose)
        }
            
        
        if (grepl('carte[_]critere', sheet)) {
            print("### Plotting map")

            one_colorbar = TRUE
            metaEX_criteria_chunk =
                metaEX_criteria_chunk[metaEX_criteria_chunk$var ==
                                      chunkname,]

            Pages = sheet_criteria_map(
                dataEX_criteria_chunk,
                metaEX_criteria_chunk,
                meta,
                prob=prob_of_quantile_for_palette,
                ModelSelection=ModelSelection,
                Colors=Colors_of_models,
                subtitle=doc_subtitle,
                one_colorbar=one_colorbar,
                icon_path=icon_path,
                logo_path=logo_path,
                is_foot=FALSE,
                is_secteur=is_secteur,
                is_warning=is_warning,
                model_by_shape=model_by_shape,
                remove_warning_lim=remove_warning_lim,
                figdir=today_figdir_leaf,
                Pages=Pages,
                Shapefiles=Shapefiles,
                verbose=verbose)
        }

        if (sheet == 'fiche_stationnarity_station') {
            print("### Plotting sheet stationnarity station")
            Pages = sheet_stationnarity_station(
                data,
                meta,
                trendEX_serie_chunk,
                dataEX_serie_chunk,
                metaEX_serie_chunk,
                period_trend_show=period_trend,
                logo_path=logo_path,
                Shapefiles=Shapefiles,
                figdir=today_figdir_leaf,
                Pages=Pages,
                verbose=verbose)
        }
    }
    

    if ('sommaire' %in% sheet_list) {
        if (is.null(chunkname) & !is.null(doc_subtitle)) {
            subtitle = doc_subtitle
        } else if (!is.null(chunkname) & is.null(doc_subtitle)) {
            subtitle = chunkname
        } else if (!is.null(chunkname) & !is.null(doc_subtitle)) {
            subtitle = paste0(chunkname, " ", doc_subtitle)
        } else {
            subtitle = ""
        }
        sheet_summary(Pages,
                      title=doc_title,
                      subtitle=subtitle,
                      logo_path=logo_path,
                      figdir=today_figdir_leaf)
    }

    
    if ('plot_doc' %in% to_do) {
        print("### Merging pdf")
        details = file.info(list.files(today_figdir_leaf,
                                       recursive=TRUE,
                                       full.names=TRUE))
        details = details[with(details, order(as.POSIXct(mtime))),]
        listfile_path = rownames(details)

        if ('sommaire' %in% sheet_list) {
            summary_path = listfile_path[length(listfile_path)]
            listfile_path = listfile_path[-length(listfile_path)]
            listfile_path = c(summary_path, listfile_path)
        }

        if (!is.null(chunkname)) {            
            pdf_combine(input=listfile_path,
                        output=file.path(today_figdir,
                                         doc_title_ns,
                                         doc_chunkname,
                                         paste0(doc_chunkname,
                                                ".pdf")))
            
        } else {
            pdf_combine(input=listfile_path,
                        output=file.path(today_figdir,
                                         doc_title_ns,
                                         paste0(doc_title_ns,
                                                    ".pdf")))
        }
    }
}
