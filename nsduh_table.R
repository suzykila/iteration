drug_import=function(name, num_table,html){
  drug_df=
    nsduh_html |>  ##looking outside the function body, add into input
    html_table() |> 
    nth(num_table) |> 
    slice(-1) |> 
    mutate(drug ="name") |> 
    select(-contains("P Value"))
  
  return(drug_df)
}