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
# realise plottings of data analyses.


add_path = function (x) {
    x = c(x, file.path(resources_path, logo_dir, x["file"]))
    names(x)[length(x)] = "path"
    return (x)
}
logo_info = lapply(logo_info, add_path)
icon_path = file.path(resources_path, icon_dir)
Code = levels(factor(meta$code))

ok = meta$is_long
ok[is.na(ok)] = FALSE
Code_long = meta$code[ok]
Code_long = Code_long[!duplicated(Code_long)]

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
    if (!all(codes_to_use == "all")) {
        Code = Code[grepl(pattern_codes_to_use, Code)]
    }
    chunkCode = list(Code)
    plotCode = list(Code)
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

    data_chunk = data[data$code %in% chunk,]
    meta_chunk = meta[meta$code %in% chunk,]

    if (exists("trendEX_SMEAG_hydrologie")) {
        trendEX_chunk =
            trendEX_SMEAG_hydrologie[
                trendEX_SMEAG_hydrologie$code %in% chunk,]
    }
    if (exists("dataEX_SMEAG_hydrologie") &
        exists("dataEX_SMEAG_hydrologie_regime")) {
        dataEX_serie_chunk = list()
        for (j in 1:length(dataEX_SMEAG_hydrologie)) {
            dataEX_serie_chunk = append(
                dataEX_serie_chunk,
                list(dataEX_SMEAG_hydrologie[[j]][
                    dataEX_SMEAG_hydrologie[[j]]$code %in%
                    chunk,]))
        }
        for (j in 1:length(dataEX_SMEAG_hydrologie_regime)) {
            dataEX_serie_chunk = append(
                dataEX_serie_chunk,
                list(dataEX_SMEAG_hydrologie_regime[[j]][
                    dataEX_SMEAG_hydrologie_regime[[j]]$code %in%
                    chunk,]))
        }
        names(dataEX_serie_chunk) =
            c(names(dataEX_SMEAG_hydrologie),
              names(dataEX_SMEAG_hydrologie_regime))
    }
    
    if (exists("metaEX_SMEAG_hydrologie")) {
        metaEX_serie_chunk = metaEX_SMEAG_hydrologie
    }
    
    for (sheet in sheet_list) {
        
        if (sheet == 'sommaire') {
            print("### Plotting summary")
            Pages = tibble(section='Sommaire', subsection=NA, n=1)
        }

        if (sheet == 'carte_stationnarity_Sen') {
            print("### Plotting stationnarity map")
            sheet_stationnarity_map(
                trendEX_chunk,
                metaEX_serie_chunk,
                meta,
                prob=prob_of_quantile_for_palette,
                suffix_names=suffix_names,
                icon_path=icon_path,
                logo_path=logo_path,
                is_foot=FALSE,
                is_secteur=FALSE,
                zoom=NULL,
                # zoom=c(0.08, 0.04, 0.005, 0.005),
                x_echelle_pct=10,
                y_echelle_pct=1,
                echelle=c(0, 20, 50, 100),
                figdir=today_figdir_leaf,
                Pages=Pages,
                Shapefiles=Shapefiles,
                verbose=verbose)
        }

        if (sheet == 'carte_stationnarity_MK') {
            print("### Plotting stationnarity map")
            sheet_stationnarity_map(
                trendEX_chunk,
                metaEX_serie_chunk,
                meta,
                prob=prob_of_quantile_for_palette,
                suffix_names=suffix_names,
                icon_path=icon_path,
                logo_path=logo_path,
                is_foot=FALSE,
                is_secteur=FALSE,
                zoom=NULL,
                # zoom=c(0.08, 0.04, 0.005, 0.005),
                x_echelle_pct=10,
                y_echelle_pct=1,
                echelle=c(0, 20, 50, 100),
                figdir=today_figdir_leaf,
                Pages=Pages,
                Shapefiles=Shapefiles,
                verbose=verbose)
        }
        


        if (sheet == 'fiche_stationnarity_station_nat') {
            print("### Plotting sheet stationnarity station nat")

            # Nat
            trendEX_chunk_type =
                trendEX_chunk[grepl("[_]nat",
                                    trendEX_chunk$variable_en),]
            trendEX_chunk_type$variable_en =
                gsub("[_]nat", "", trendEX_chunk_type$variable_en)
            
            data_chunk_type =
                dplyr::select(data_chunk, -"Q_inf", Q="Q_nat")
            meta_chunk_type =
                dplyr::filter(meta_chunk, type == "nat")
            meta_chunk_type =
                dplyr::select(meta_chunk_type,
                              -"tLac_pct_inf",
                              tLac_pct="tLac_pct_nat",
                              -"meanLac_inf",
                              meanLac="meanLac_nat")
            
            dataEX_serie_chunk_type = dataEX_serie_chunk
            for (j in 1:length(dataEX_serie_chunk_type)) {
                var = names(dataEX_serie_chunk_type)[j]
                dataEX_serie_chunk_type[[j]] =
                    dplyr::select(dataEX_serie_chunk_type[[j]],
                                  -dplyr::all_of(paste0(var,
                                                        "_inf")),
                                  !!var:=
                                      dplyr::all_of(paste0(var,
                                                           "_nat")))
            }

            Pages = sheet_stationnarity_station(
                data=data_chunk_type,
                meta=meta_chunk_type,
                trendEX=trendEX_chunk_type,
                dataEX_serie=dataEX_serie_chunk_type,
                metaEX_serie=metaEX_serie_chunk,
                axis_xlim=periodAll,
                code_selection=Code_long,
                subtitle="Débits reconstitués",
                logo_info=logo_info,
                Shapefiles=Shapefiles,
                zoom=NULL,
                map_limits=c(250000, 790000, 6100000, 6600000),
                x_echelle_pct=10,
                y_echelle_pct=5,
                echelle=c(0, 100),
                figdir=today_figdir_leaf,
                suffix="reconstitués",
                Pages=Pages,
                verbose=verbose)
        }
        

        if (sheet == 'fiche_stationnarity_station_inf') {
            print("### Plotting sheet stationnarity station inf")

            # Inf
            trendEX_chunk_type =
                trendEX_chunk[grepl("[_]inf",
                                    trendEX_chunk$variable_en),]
            trendEX_chunk_type$variable_en =
                gsub("[_]inf", "", trendEX_chunk_type$variable_en)
            
            data_chunk_type =
                dplyr::select(data_chunk, -"Q_nat", Q="Q_inf")
            meta_chunk_type =
                dplyr::filter(meta_chunk, type == "inf")
            meta_chunk_type =
                dplyr::select(meta_chunk_type,
                              -"tLac_pct_inf",
                              tLac_pct="tLac_pct_inf",
                              -"meanLac_inf",
                              meanLac="meanLac_inf")
            
            dataEX_serie_chunk_type = dataEX_serie_chunk
            for (j in 1:length(dataEX_serie_chunk_type)) {
                var = names(dataEX_serie_chunk_type)[j]
                dataEX_serie_chunk_type[[j]] =
                    dplyr::select(dataEX_serie_chunk_type[[j]],
                                  -dplyr::all_of(paste0(var,
                                                        "_nat")),
                                  !!var:=
                                      dplyr::all_of(paste0(var,
                                                           "_inf")))
            }

            Pages = sheet_stationnarity_station(
                data=data_chunk_type,
                meta=meta_chunk_type,
                trendEX=trendEX_chunk_type,
                dataEX_serie=dataEX_serie_chunk_type,
                metaEX_serie=metaEX_serie_chunk,
                axis_xlim=periodAll,
                # code_selection=Code_long,
                subtitle="Débits influencés",
                logo_info=logo_info,
                Shapefiles=Shapefiles,
                zoom=NULL,
                map_limits=c(250000, 790000, 6100000, 6600000),
                x_echelle_pct=10,
                y_echelle_pct=5,
                echelle=c(0, 100),
                figdir=today_figdir_leaf,
                suffix="influencés",
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
                      logo_info=logo_info,
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
