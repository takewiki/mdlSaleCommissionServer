#' 删除DMS中的销售杨文意
#'
#' @param dms_token 口令
#' @param FStartDate 日期1
#' @param FEndDate 日期2
#' @param FCompanyName 公司名称
#'
#' @return 返回值
#' @export
#'
#' @examples
#' dms_saleOrder_del()
dms_saleOrder_del <- function(dms_token='9B6F803F-9D37-41A2-BDA0-70A7179AF0F3',
                              FStartDate='2022-08-01',
                              FEndDate='2022-08-31',
                              FCompanyName='苏州赛普生物科技有限公司'){
  sql <- paste0("delete   FROM [dbo].[rds_hr_sales_saleOrder]
  where  FDate >='",FStartDate,"' and FDate<='",FEndDate,"'
  and FSaleOrgName='",FCompanyName,"'")
  tsda::sql_update2(token = dms_token,sql_str = sql)

}

#销售订单
# 来源表：rds_vw_sales_saleOrder
# 目标表：rds_hr_sales_saleOrder
#' Title
#'
#' @param erp_token  ERP口令
#' @param FStartDate 开始日期
#' @param FEndDate 结束日期
#' @param FCompanyName 公司名称
#' @param dms_token DMS口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' saleOrder_erp2dms()
saleOrder_erp2dms <- function(erp_token='4D181CAB-4CE3-47A3-8F2B-8AB11BB6A227',
                              dms_token='9B6F803F-9D37-41A2-BDA0-70A7179AF0F3',
                          FStartDate='2022-08-01',
                          FEndDate='2022-08-31',
                          FCompanyName='苏州赛普生物科技有限公司') {
sql_erp <- paste0("SELECT [FSaleOrgNumber]
      ,[FSaleOrgName]
      ,[FDate]
      ,[FId]
      ,[FBillNo]
      ,[FBillTypeNumber]
      ,[FBillTypeName]
      ,[FSaleDeptNumber]
      ,[FSaleDeptName]
      ,[FSaleGroupNumber]
      ,[FSaleGroupName]
      ,[FSaleManNumber]
      ,[FSaleManName]
      ,[FCustomerNumber]
      ,[FCustomerName]
      ,[FCurrentNumber]
      ,[FCurrentName]
      ,[FDOCUMENTSTATUS]
      ,[FAPPROVEDATE]
      ,[FSEQ]
      ,[FENTRYID]
      ,[FMaterialNumber]
      ,[FMaterialName]
      ,[FMaterialModel]
      ,[FUnitNumber]
      ,[FUnitName]
      ,[FQTY]
      ,[FPRICE]
      ,[FTAXPRICE]
      ,[FISFREE]
      ,[FTAXRATE]
      ,[FTAXAMOUNT]
      ,[FAMOUNT]
      ,[FALLAMOUNT]
      ,[FTAXAMOUNT_LC]
      ,[FAMOUNT_LC]
      ,[FALLAMOUNT_LC]
  FROM [dbo].[rds_vw_sales_saleOrder]
  where  FDate >='",FStartDate,"' and FDate<='",FEndDate,"'
  and FSaleOrgName='",FCompanyName,"'")
data = tsda::sql_select2(erp_token,sql_erp)
ncount = nrow(data)
if(ncount>0){
  #如果存在数据
  #如果存在数据，需要先删除数据，然后再进行先增
  dms_saleOrder_del(dms_token = dms_token,FStartDate = FStartDate,FEndDate = FEndDate,FCompanyName = FCompanyName)
  #先增数据
  tsda::db_writeTable2(token = dms_token,table_name = 'rds_hr_sales_saleOrder',r_object = data,append = T)
}
return(data)


}






#' 数据中台订单查询
#'
#' @param dms_token 口令
#' @param FStartDate 开始日期
#' @param FEndDate 结束日期
#' @param FCompanyName 公司名称
#'
#' @return 返回值
#' @export
#'
#' @examples
#' dms_saleOrder_query()
dms_saleOrder_query <- function(dms_token='9B6F803F-9D37-41A2-BDA0-70A7179AF0F3',
                                FStartDate='2022-08-01',
                                FEndDate='2022-08-31',
                                FCompanyName='苏州赛普生物科技有限公司') {
sql <- paste0("SELECT [FSaleOrgNumber] as 销售组织代码
      ,[FSaleOrgName]  as 销售订单名称
      ,[FDate]  as 销售订单日期
      ,[FId]  as 销售订单内码
      ,[FBillNo]  as 销售订单编号
      ,[FBillTypeNumber] as 单据类型代码
      ,[FBillTypeName]  as 单据类型名称
      ,[FSaleDeptNumber]  as 销售部门代码
      ,[FSaleDeptName]    as 销售部门名称
      ,[FSaleGroupNumber] as 销售组代码
      ,[FSaleGroupName]  as 销售组名称
      ,[FSaleManNumber]   as 销售员代码
      ,[FSaleManName]  as  销售员名称
      ,[FCustomerNumber] as 客户代码
      ,[FCustomerName]  as 客户名称
      ,[FCurrentNumber]  as 币别代码
      ,[FCurrentName]   as  币别名称
      ,[FDOCUMENTSTATUS]  as 单据状态
      ,[FAPPROVEDATE] as 审核日期
      ,[FSEQ]  as 行号
      ,[FENTRYID] as 销售订单表体内码
      ,[FMaterialNumber] as 物料代码
      ,[FMaterialName] as 物料名称
      ,[FMaterialModel]  as 规格型号
      ,[FUnitNumber] as 单位代码
      ,[FUnitName]  as 单位名称
      ,[FQTY]  as 订单数量
      ,[FPRICE] as 单价
      ,[FTAXPRICE] as 含税单价
      ,[FISFREE] as 是否赠品
      ,[FTAXRATE] as 税率
      ,[FTAXAMOUNT] as 税额
      ,[FAMOUNT] as  金额
      ,[FALLAMOUNT] as 价税合计
      ,[FTAXAMOUNT_LC] as 税额本位币
      ,[FAMOUNT_LC]   as  金额本位币
      ,[FALLAMOUNT_LC] as 价税合计本位币
  FROM [dbo].[rds_hr_sales_saleOrder]
  where  FDate >='",FStartDate,"' and FDate<='",FEndDate,"'
  and FSaleOrgName='",FCompanyName,"'")
data = tsda::sql_select2(token = dms_token,sql = sql)
return(data)
}


#' 同步订单数据
#'
#' @param input  输入
#' @param session 会话
#' @param output 输出
#' @param erp_token  口令1
#' @param dms_token  口令2
#'
#' @return 返回值
#' @export
#'
#' @examples
#' server_saleOrder_click()
server_saleOrder_click <- function(input,output,session,erp_token,dms_token) {

  #步同按续
  var_sync_src_saleOrder_read <-tsui::var_dateRange('sync_src_saleOrder_read')
  var_sync_src_saleOrder_org <- tsui::var_ListChoose1('sync_src_saleOrder_org')
  shiny::observeEvent(input$sync_src_saleOrder_click,{

    dates = var_sync_src_saleOrder_read()
    FStartDate = dates[1]
    FEndDate = dates[2]
    FCompanyName = var_sync_src_saleOrder_org()
    #更新数据
    try({
      saleOrder_erp2dms(erp_token = erp_token,
                        dms_token = dms_token,
                        FStartDate =FStartDate ,
                        FEndDate =FEndDate ,
                        FCompanyName = FCompanyName)
    })

    tsui::pop_notice('销售订单同步成功！')





  })


}





#' 同步订单数据
#'
#' @param input  输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' server_saleOrder_click()
server_saleOrder_view <- function(input,output,session,dms_token) {

  #步同按续
  var_sync_src_saleOrder_read <-tsui::var_dateRange('sync_src_saleOrder_read')
  var_sync_src_saleOrder_org <- tsui::var_ListChoose1('sync_src_saleOrder_org')
  shiny::observeEvent(input$sync_src_saleOrder_view,{

    dates = var_sync_src_saleOrder_read()
    FStartDate = dates[1]
    FEndDate = dates[2]
    FCompanyName = var_sync_src_saleOrder_org()
    #查看数据
    data = dms_saleOrder_query(dms_token = dms_token,FStartDate =FStartDate ,FEndDate =FEndDate ,FCompanyName = FCompanyName)
    #显示数据
    tsui::run_dataTable2(id = 'sync_src_saleOrder_dt',data)
    #下载数据
    file_name = paste0('销售订单同步数据',FStartDate,"_",FEndDate,".xlsx")
    tsui::run_download_xlsx(id = 'sync_src_saleOrder_dl',data = data,filename =file_name )





  })


}



