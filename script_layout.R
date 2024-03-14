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
Code_AEAG = meta$code[grepl("AEAG", meta$origin)]
Code_MK = sort(c(Code_long, Code_AEAG))
Code_Sen = meta$code[!grepl("AEAG", meta$origin)]
Code_Sen = Code_Sen[!duplicated(Code_Sen)]


trendEX_SMEAG_hydrologie_Sen =
    dplyr::select(trendEX_SMEAG_hydrologie_Sen,
                  -a_normalise_min, -a_normalise_max)

trendEX_SMEAG_hydrologie_Sen =
    dplyr::left_join(
               trendEX_SMEAG_hydrologie_Sen,
               dplyr::summarise(
                          dplyr::group_by(trendEX_SMEAG_hydrologie_MK,
                                          variable_en),
                          a_normalise_min=a_normalise_min[1],
                          a_normalise_max=a_normalise_max[1]),
               by=c("variable_en"))


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
    doc_title = default_doc_title
    doc_subtitle = NULL
    sheet_list = plot_sheet
}
if ('plot_doc' %in% to_do) {
    Pages = dplyr::tibble()
    doc_title = plot_doc$title
    doc_subtitle = plot_doc$subtitle
    sheet_list = plot_doc$sheet
}


doc_title_ns = gsub(" ", "_", doc_title)

if (!is.null(doc_subtitle)) {
    doc_title_ns = paste0(doc_title_ns,
                          "_",
                          gsub(" ", "_", doc_subtitle))
}

if ('plot_doc' %in% to_do) {
    today_figdir_leaf = file.path(today_figdir,
                                  doc_title_ns,
                                  "PDF")
} else {
    today_figdir_leaf = today_figdir
}


for (sheet in sheet_list) {
    
    if (sheet == 'sommaire') {
        print("### Plotting summary")
        Pages = tibble(section='Sommaire', subsection=NA, n=1)
    }

    if (sheet == 'carte_stationnarity_Sen') {
        print("### Plotting stationnarity map Sen")

        # Nat
        metaEX_serie = metaEX_SMEAG_hydrologie_Sen
        trendEX = trendEX_SMEAG_hydrologie_Sen
        trendEX_type =
            trendEX[grepl("[_]nat",
                                trendEX$variable_en),]
        trendEX_type$variable_en =
            gsub("[_]nat", "", trendEX_type$variable_en)
        meta_type =
            dplyr::filter(meta, type == "nat")
        meta_type =
            dplyr::select(meta_type,
                          -"tLac_pct_inf",
                          tLac_pct="tLac_pct_nat",
                          -"meanLac_inf",
                          meanLac="meanLac_nat")

        Pages = sheet_stationnarity_map(
            trendEX_type,
            metaEX_serie,
            meta_type,
            # code_selection=Code_Sen,
            show_MK=FALSE,
            period_to_show=format(as.Date(period_Sen),
                                  "%d/%m/%Y"),
            icon_path=icon_path,
            logo_path=logo_path,
            is_foot=TRUE,
            foot_resume=FALSE,
            is_secteur=FALSE,
            zoom=NULL,
            map_limits=c(280000, 790000, 6100000, 6600000),
            x_echelle_pct=10,
            y_echelle_pct=7,
            echelle=c(0, 20, 50, 100),
            figdir=today_figdir_leaf,
            suffix="Sans test de Mann-Kendall",
            Pages=Pages,
            Shapefiles=Shapefiles,
            verbose=verbose)
    }

    if (sheet == 'carte_stationnarity_MK') {
        print("### Plotting stationnarity map MK")

        # Nat
        metaEX_serie = metaEX_SMEAG_hydrologie_MK
        trendEX = trendEX_SMEAG_hydrologie_MK
        trendEX_type =
            trendEX[grepl("[_]nat",
                                trendEX$variable_en),]
        trendEX_type$variable_en =
            gsub("[_]nat", "", trendEX_type$variable_en)
        meta_type =
            dplyr::filter(meta, type == "nat")
        meta_type =
            dplyr::select(meta_type,
                          -"tLac_pct_inf",
                          tLac_pct="tLac_pct_nat",
                          -"meanLac_inf",
                          meanLac="meanLac_nat")
        

        Pages = sheet_stationnarity_map(
            trendEX_type,
            metaEX_serie,
            meta_type,
            # code_selection=Code_MK,
            show_MK=TRUE,
            period_to_show=format(as.Date(period_MK),
                                  "%d/%m/%Y"),
            icon_path=icon_path,
            logo_path=logo_path,
            is_foot=TRUE,
            foot_resume=FALSE,
            is_secteur=FALSE,
            zoom=NULL,
            map_limits=c(280000, 790000, 6100000, 6600000),
            x_echelle_pct=10,
            y_echelle_pct=7,
            echelle=c(0, 20, 50, 100),
            figdir=today_figdir_leaf,
            suffix="Avec test de Mann-Kendall",
            Pages=Pages,
            Shapefiles=Shapefiles,
            verbose=verbose)
    }
    


    if (sheet == 'fiche_stationnarity_station_nat') {
        print("### Plotting sheet stationnarity station nat")

        # Nat
        dataEX_serie =
            append(dataEX_SMEAG_hydrologie_MK,
                   dataEX_SMEAG_hydrologie_regime)
        metaEX_serie =
            dplyr::bind_rows(metaEX_SMEAG_hydrologie_MK,
                             metaEX_SMEAG_hydrologie_regime)
        trendEX = trendEX_SMEAG_hydrologie_MK
        
        trendEX_type =
            trendEX[grepl("[_]nat",
                                trendEX$variable_en),]
        trendEX_type$variable_en =
            gsub("[_]nat", "", trendEX_type$variable_en)

        data_type =
            dplyr::select(data, -"Q_inf", Q="Q_nat")
        meta_type =
            dplyr::filter(meta, type == "nat")
        meta_type =
            dplyr::select(meta_type,
                          -"tLac_pct_inf",
                          tLac_pct="tLac_pct_nat",
                          -"meanLac_inf",
                          meanLac="meanLac_nat")
        
        dataEX_serie_type = dataEX_serie
        for (j in 1:length(dataEX_serie_type)) {
            var = names(dataEX_serie_type)[j]
            dataEX_serie_type[[j]] =
                dplyr::select(dataEX_serie_type[[j]],
                              -dplyr::all_of(paste0(var,
                                                    "_inf")),
                              !!var:=
                                  dplyr::all_of(paste0(var,
                                                       "_nat")))
        }

        Pages = sheet_stationnarity_station(
            data=data_type,
            meta=meta_type,
            trendEX=trendEX_type,
            dataEX_serie=dataEX_serie_type,
            metaEX_serie=metaEX_serie,
            axis_xlim=period_MK,
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
            suffix="Débits reconstitués",
            foot_resume=FALSE,
            color_to_switch=color_to_switch,
            Pages=Pages,
            verbose=verbose)
    }
    

    if (sheet == 'fiche_stationnarity_station_inf') {
        print("### Plotting sheet stationnarity station inf")

        # Inf
        dataEX_serie =
            append(dataEX_SMEAG_hydrologie_MK,
                   dataEX_SMEAG_hydrologie_regime)
        metaEX_serie =
            dplyr::bind_rows(metaEX_SMEAG_hydrologie_MK,
                             metaEX_SMEAG_hydrologie_regime)
        trendEX = trendEX_SMEAG_hydrologie_MK
        
        trendEX_type =
            trendEX[grepl("[_]inf",
                                trendEX$variable_en),]
        trendEX_type$variable_en =
            gsub("[_]inf", "", trendEX_type$variable_en)
        
        data_type =
            dplyr::select(data, -"Q_nat", Q="Q_inf")
        meta_type =
            dplyr::filter(meta, type == "inf")
        meta_type =
            dplyr::select(meta_type,
                          -"tLac_pct_inf",
                          tLac_pct="tLac_pct_inf",
                          -"meanLac_inf",
                          meanLac="meanLac_inf")
        
        dataEX_serie_type = dataEX_serie
        for (j in 1:length(dataEX_serie_type)) {
            var = names(dataEX_serie_type)[j]
            dataEX_serie_type[[j]] =
                dplyr::select(dataEX_serie_type[[j]],
                              -dplyr::all_of(paste0(var,
                                                    "_nat")),
                              !!var:=
                                  dplyr::all_of(paste0(var,
                                                       "_inf")))
        }

        Pages = sheet_stationnarity_station(
            data=data_type,
            meta=meta_type,
            trendEX=trendEX_type,
            dataEX_serie=dataEX_serie_type,
            metaEX_serie=metaEX_serie,
            axis_xlim=period_MK,
            code_selection=Code_long,
            subtitle="Débits influencés",
            logo_info=logo_info,
            Shapefiles=Shapefiles,
            zoom=NULL,
            map_limits=c(250000, 790000, 6100000, 6600000),
            x_echelle_pct=10,
            y_echelle_pct=5,
            echelle=c(0, 100),
            figdir=today_figdir_leaf,
            suffix="Débits observés",
            foot_resume=FALSE,
            color_to_switch=color_to_switch,
            Pages=Pages,
            verbose=verbose)
    }
}


if ('sommaire' %in% sheet_list) {
    sheet_summary(Pages,
                  title=doc_title,
                  subtitle=doc_subtitle,
                  logo_info=logo_info,
                  one_column_width=11,
                  page_margin=c(t=1, r=1, b=0.5, l=1),
                  figdir=today_figdir_leaf,
                  verbose=verbose)
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

    pdf_combine(input=listfile_path,
                output=file.path(today_figdir,
                                 doc_title_ns,
                                 paste0(doc_title_ns,
                                        ".pdf")))
}
