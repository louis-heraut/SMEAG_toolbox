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


dir = "../raw_data"
res = "formated"

is_not_external_NA = function(X) {
    is_nNA = !is.na(X)
    id_nNA = which(is_nNA)
    first_nNA = min(id_nNA)
    last_nNA = max(id_nNA)
    is_nNA[first_nNA:last_nNA] = TRUE
    return(is_nNA)
}


## SMEAG naturels ____________________________________________________
print("SMEAG naturels")
dataNat_old = ASHE::read_tibble(file.path(dir, "Qnat_old.csv"))
dataNat_old$date = as.Date(dataNat_old$date, "%d/%m/%Y")
dataNat_new = ASHE::read_tibble(file.path(dir, "Qnat_new.csv"))
dataNat_new$date = as.Date(dataNat_new$date)

code_old = names(dataNat_old)[names(dataNat_old) != "date"]
code_new = names(dataNat_new)[names(dataNat_new) != "date"]
# code_new_not_old = code_new[!(code_new %in% code_old)]


output_dir = file.path(dir, res, "SMEAG_naturels")
unlink(output_dir)
dir.create(output_dir)
for (code in code_new) {

    if (code %in% code_old) {
        dataNat_new_code =
            dplyr::select(dataNat_new,
                          dplyr::all_of(c("date", code)))
        dataNat_old_code =
            dplyr::select(dataNat_old,
                          dplyr::all_of(c("date", code)))

        ok = !is.na(dataNat_new_code[[code]])
        date_cut = min(dataNat_new_code$date[ok])
        
        dataNat_code =
            dplyr::bind_rows(dplyr::filter(dataNat_old_code,
                                           date < date_cut),
                             dplyr::filter(dataNat_new_code,
                                           date_cut <= date))
        
    } else {
        dataNat_code = dplyr::select(dataNat_new,
                                     dplyr::all_of(c("date", code)))
    }
    
    dataNat_code = dplyr::rename(dataNat_code,
                                 Qm3s=dplyr::all_of(code))
    dataNat_code = dplyr::filter(dataNat_code,
                                 is_not_external_NA(Qm3s))

    tmp = dplyr::tibble(date=seq.Date(min(dataNat_code$date),
                                      max(dataNat_code$date),
                                      "days"))
    dataNat_code = dplyr::left_join(tmp, dataNat_code,
                                    by="date")
    
    ASHE::write_tibble(dataNat_code, filedir=output_dir,
                       filename=paste0(code, "_SMEAG.txt"))
}


## SMEAG influences __________________________________________________
print("SMEAG influences")
dataInf_new = ASHE::read_tibble(file.path(dir, "Qinf_new.csv"))
dataInf_new$date = as.Date(dataInf_new$date)

code_new = names(dataInf_new)[names(dataInf_new) != "date"]

output_dir = file.path(dir, res, "SMEAG_influences")
unlink(output_dir)
dir.create(output_dir)

eq_codeHYDRO2 = list("O2620020",
                     "O2000010")
eq_codeHYDRO3 = list("O262002002",
                     c("O020004001", "O020002001"))


for (code in code_new) {
    dataInf_code = dplyr::select(dataInf_new,
                                 dplyr::all_of(c("date", code)))
    ok = !is.na(dataInf_code[[code]])
    date_cut = min(dataInf_code$date[ok])
   
    dataInf_code = dplyr::rename(dataInf_code,
                                 Qm3s=dplyr::all_of(code))
    
    if (code %in% eq_codeHYDRO2) {
        code10 = eq_codeHYDRO3[[which(eq_codeHYDRO2 == code)]]
        dataHYDRO_code =
            ASHE::create_data_HYDRO(dir, "HYDRO3",
                                    paste0(code10,
                                           "_HYDRO_QJM.txt"),
                                    variable_to_load="Qm3s",
                                    format="HYDRO3")
    } else {
        dataHYDRO_code =
            ASHE::create_data_HYDRO(dir, "HYDRO2",
                                    paste0(code,
                                           "_HYDRO_QJM.txt"),
                                    variable_to_load="Qm3s")
    }

    if (!is.null(dataHYDRO_code)) {
        dataHYDRO_code = dplyr::select(dataHYDRO_code, -code)
        dataHYDRO_code = dplyr::arrange(dataHYDRO_code, date)
        dataHYDRO_code = dplyr::filter(dataHYDRO_code,
                                       date < date_cut)
        dataHYDRO_code = dplyr::rename(dataHYDRO_code, Qm3s=Q)
        dataInf_code = dplyr::bind_rows(dataHYDRO_code,
                                        dataInf_code)
    }

    dataInf_code = dplyr::filter(dataInf_code,
                                 is_not_external_NA(Qm3s))
    
    tmp = dplyr::tibble(date=seq.Date(min(dataInf_code$date),
                                      max(dataInf_code$date),
                                      "days"))
    dataInf_code = dplyr::left_join(tmp, dataInf_code,
                                    by="date")
    
    ASHE::write_tibble(dataInf_code, filedir=output_dir,
                       filename=paste0(code, "_SMEAG.txt"))
}




## HYDRO influences __________________________________________________
print("HYDRO influences")
Paths = list.files(file.path(dir, "HYDRO0_inf"), full.names=TRUE)
output_dir = file.path(dir, res, "HYDRO_influences")
unlink(output_dir)
dir.create(output_dir)
for (path in Paths) {
    code = gsub("[_].*", "", basename(path))
    print(code)
    dataHYDRO_code = ASHE::read_tibble(path)
    dataHYDRO_code = dplyr::mutate(dataHYDRO_code,
                                   date=as.Date(t),
                                   Qm3s=v*1E-3,
                                   .keep="used")
    dataHYDRO_code = dplyr::select(dataHYDRO_code, -v, -t)

    tmp = dplyr::tibble(date=seq.Date(min(dataHYDRO_code$date),
                                      max(dataHYDRO_code$date),
                                      "days"))
    dataHYDRO_code = dplyr::left_join(tmp, dataHYDRO_code,
                                    by="date")
    
    ASHE::write_tibble(dataHYDRO_code, filedir=output_dir,
                       filename=paste0(code, "_HYDRO0.txt"))
}


## HYDRO naturels ____________________________________________________
print("HYDRO naturels")
Paths = list.files(file.path(dir, "HYDRO0_nat"), full.names=TRUE)
output_dir = file.path(dir, res, "HYDRO_naturels")
unlink(output_dir)
dir.create(output_dir)
for (path in Paths) {
    code = gsub("[_].*", "", basename(path))
    print(code)
    dataHYDRO_code = ASHE::read_tibble(path)
    dataHYDRO_code = dplyr::mutate(dataHYDRO_code,
                                   date=as.Date(t),
                                   Qm3s=v*1E-3,
                                   .keep="used")
    dataHYDRO_code = dplyr::select(dataHYDRO_code, -v, -t)

    tmp = dplyr::tibble(date=seq.Date(min(dataHYDRO_code$date),
                                      max(dataHYDRO_code$date),
                                      "days"))
    dataHYDRO_code = dplyr::left_join(tmp, dataHYDRO_code,
                                      by="date")
    
    ASHE::write_tibble(dataHYDRO_code, filedir=output_dir,
                       filename=paste0(code, "_HYDRO0.txt"))
}
