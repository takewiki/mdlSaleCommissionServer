#业务组织
# 来源表：rds_vw_sales_saleOrder
# 目标表：rds_hr_organizations
#' Title
#'
#' @param erp_token  ERP口令
#' @param dms_token DMS口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' organization_erp2dms()
organization_erp2dms <- function(erp_token='4D181CAB-4CE3-47A3-8F2B-8AB11BB6A227',
                              dms_token='9B6F803F-9D37-41A2-BDA0-70A7179AF0F3'
                             ) {
  sql_erp <- paste0("select  FORGID,FNUMBER,FNAME   from rds_vw_organizations")
  data = tsda::sql_select2(erp_token,sql_erp)
  ncount = nrow(data)
  if(ncount>0){
    #如果存在数据
    #如果存在数据，需要先删除数据，然后再进行先增
     tsda::db_truncateTable(token = dms_token,table_name = 'rds_hr_organizations')
    #先增数据
    tsda::db_writeTable2(token = dms_token,table_name = 'rds_hr_organizations',r_object = data,append = T)
  }
  return(data)


}
