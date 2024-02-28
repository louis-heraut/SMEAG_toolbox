

dir = "../raw_data"

is_not_external_NA = function(X) {
    is_nNA = !is.na(X)
    id_nNA = which(is_nNA)
    first_nNA = min(id_nNA)
    last_nNA = max(id_nNA)
    is_nNA[first_nNA:last_nNA] = TRUE
    return(is_nNA)
}



dataNat_old = ASHE::read_tibble(file.path(dir, "Qnat_old.csv"))
dataNat_old$date = as.Date(dataNat_old$date, "%d/%m/%Y")
dataNat_new = ASHE::read_tibble(file.path(dir, "Qnat_new.csv"))
dataNat_new$date = as.Date(dataNat_new$date)

code_old = names(dataNat_old)[names(dataNat_old) != "date"]
code_new = names(dataNat_new)[names(dataNat_new) != "date"]
code_new_not_old = code_new[!(code_new %in% code_old)]
date_cut = min(dataNat_new$date)

dataNat_long =
    dplyr::bind_rows(dplyr::filter(dataNat_old,
                                   date < date_cut),
                     dplyr::select(dataNat_new,
                                   dplyr::all_of(c("date",
                                                   code_old))))
dataNat_short = dplyr::select(dataNat_new,
                           dplyr::all_of(c("date",
                                           code_new_not_old)))
dataNat = dplyr::full_join(dataNat_long, dataNat_short)

output_dir = file.path(dir, "SMEAG_naturel")
unlink(output_dir)
dir.create(output_dir)
for (code in code_new) {
    dataNat_code = dplyr::select(dataNat, dplyr::all_of(c("date", code)))
    dataNat_code = dplyr::rename(dataNat_code,
                                 Qm3s=dplyr::all_of(code))
    dataNat_code = dplyr::filter(dataNat_code,
                                 is_not_external_NA(Qm3s))
    ASHE::write_tibble(dataNat_code, filedir=output_dir,
                       filename=paste0(code, "_naturaliser.txt"))
}








dataInf_new = ASHE::read_tibble(file.path(dir, "Qinf_new.csv"))
dataInf_new$date = as.Date(dataInf_new$date)

code_new = names(dataInf_new)[names(dataInf_new) != "date"]
date_cut = min(dataInf_new$date)

output_dir = file.path(dir, "SMEAG_influencer")
unlink(output_dir)
dir.create(output_dir)

eq_codeHYDRO2 = list("O2620020",
                     "O2000010")
eq_codeHYDRO3 = list("O262002002",
                     c("O020004001", "O020002001"))


for (code in code_new) {
    dataInf_code = dplyr::select(dataInf_new,
                                 dplyr::all_of(c("date", code)))
    dataInf_code = dplyr::rename(dataInf_code,
                                 Qm3s=dplyr::all_of(code))
    
    if (code %in% eq_codeHYDRO2) {
        code10 = eq_codeHYDRO3[[which(eq_codeHYDRO2 == code)]]
        dataHYDRO = ASHE::create_data_HYDRO(dir, "HYDRO3",
                                            paste0(code10,
                                                   "_HYDRO_QJM.txt"),
                                            variable_to_load="Qm3s",
                                            format="HYDRO3")
        
        
    } else {
        dataHYDRO = ASHE::create_data_HYDRO(dir, "HYDRO2",
                                            paste0(code,
                                                   "_HYDRO_QJM.txt"),
                                            variable_to_load="Qm3s")
    }

    if (!is.null(dataHYDRO)) {
        dataHYDRO = dplyr::select(dataHYDRO, -code)
        dataHYDRO = dplyr::arrange(dataHYDRO, date)
        dataHYDRO = dplyr::filter(dataHYDRO,
                                  date < date_cut)
        dataHYDRO = dplyr::rename(dataHYDRO, Qm3s=Q)
        dataInf_code = dplyr::bind_rows(dataHYDRO,
                                        dataInf_code)
    }

    dataInf_code = dplyr::filter(dataInf_code,
                                 is_not_external_NA(Qm3s))
    ASHE::write_tibble(dataInf_code, filedir=output_dir,
                       filename=paste0(code, "_influencer.txt"))
}
