#' 查询销售员
#'
#' @param dms_token 口令
#'
#' @return 返回列表
#' @export
#'
#' @examples
#' saleMan_query()
saleMan_query <- function(dms_token='9B6F803F-9D37-41A2-BDA0-70A7179AF0F3') {

sql <- paste0("SELECT distinct
      [FSaleManName]
  FROM [rds_hr_sales_saleOutStockAll]")
data <- tsda::sql_select2(token = dms_token,sql = sql)
ncount <- nrow(data)
if(ncount>0){
  res <- tsdo::vect_as_list(data$FSaleManName)
}else{
  res <- list('无销售员')
}
  return(res)
}
