#' 删除DMS中的销售出库
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
#' dms_saleOutStock_mfg_del()
dms_saleOutStock_mfg_del <- function(dms_token='9B6F803F-9D37-41A2-BDA0-70A7179AF0F3',
                              FStartDate='2022-08-01',
                              FEndDate='2022-08-31',
                              FCompanyName='苏州赛普生物科技有限公司'){
  sql <- paste0("delete   FROM  rds_hr_sales_saleOutStockAll_mfg
  where  FDate >='",FStartDate,"' and FDate<='",FEndDate,"'
  and FSaleOrgName='",FCompanyName,"'")
  tsda::sql_update2(token = dms_token,sql_str = sql)

}

#这个有点意思
#销售出库
# 来源表：rds_vw_sales_saleOutStockAll
# 目标表：rds_hr_sales_saleOutStockAll
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
#' saleOutStock_erp2dms_mfg()
saleOutStock_erp2dms_mfg <- function(erp_token='4D181CAB-4CE3-47A3-8F2B-8AB11BB6A227',
                              dms_token='9B6F803F-9D37-41A2-BDA0-70A7179AF0F3',
                              FStartDate='2022-08-01',
                              FEndDate='2022-08-31',
                              FCompanyName='苏州赛普生物科技有限公司') {
  sql_erp <- paste0("SELECT [FID]
      ,[FBILLNO]
      ,[FBillTypeNumber]
      ,[FBillTypeName]
      ,[FSaleOrgNumber]
      ,[FSaleOrgName]
      ,[FSaleDeptNumber]
      ,[FSaleDeptName]
      ,[FSaleGroupNumber]
      ,[FSaleGroupName]
      ,[FSaleManNumber]
      ,[FSaleManName]
      ,[FDate]
      ,[FCustomerNumber]
      ,[FCustomerName]
      ,[FDOCUMENTSTATUS]
      ,[FAPPROVEDATE]
      ,FCusomerProperty
      ,[FSEQ]
      ,[FENTRYID]
      ,[FMaterialNumber]
      ,[FMaterialName]
      ,[FMaterialModel]
      ,[FUnitNumber]
      ,[FUnitName]
      ,[FOutStockQty]
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
      ,[FSRCBILLNO]
      ,isnull([FSoBillNo],'') as FSoBillNo
      ,isnull([FSoSeq],0) as FSoSeq
      ,[FSOENTRYID]
      ,isnull([FSoId],0) as FSoId,
      F_SZSP_SettleAmt,
      F_SZSP_SettleDate
  FROM  rds_vw_sales_saleOutStockAll_mfg
  where  FDate >='",FStartDate,"' and FDate<='",FEndDate,"'
  and FSaleOrgName='",FCompanyName,"'")
  data = tsda::sql_select2(erp_token,sql_erp)
  print(data)
  ncount = nrow(data)
  if(ncount>0){
    #如果存在数据
    #如果存在数据，需要先删除数据，然后再进行先增
    dms_saleOutStock_del(dms_token = dms_token,FStartDate = FStartDate,FEndDate = FEndDate,FCompanyName = FCompanyName)
    #先增数据
    #进行分页处理
    page_info = tsdo::paging_setting(volume = ncount,each_page = 500)
    page_count = nrow(page_info)
    lapply(1:page_count, function(row){
      start = page_info[row,'FStart']
      end = page_info[row,'FEnd']
      item = data[start:end, ]
      print(item)
      tsda::db_writeTable2(token = dms_token,table_name = 'rds_hr_sales_saleOutStockAll_mfg',r_object = item,append = T)
    })

  }
  return(data)


}



#' 数据中台订单查询,其中结算金额需要扣税,已在数据源处扣除了税金
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
dms_saleOutStock_mfg_query <- function(dms_token='9B6F803F-9D37-41A2-BDA0-70A7179AF0F3',
                                FStartDate='2022-08-01',
                                FEndDate='2022-08-31',
                                FCompanyName='苏州赛普生物科技有限公司') {
  sql <- paste0("SELECT [FID] as 销售出库内码
      ,[FBILLNO]  as 销售出库单号
      ,[FBillTypeNumber] as 单据类型代码
      ,[FBillTypeName]   as 单据类型名称
      ,[FSaleOrgNumber] as 组织代码
      ,[FSaleOrgName]  as 组织名称
      ,[FSaleDeptNumber] as 销售部门代码
      ,[FSaleDeptName] as 销售部门名称
      ,[FSaleGroupNumber] as 销售组代码
      ,[FSaleGroupName] as 销售组名称
      ,[FSaleManNumber] as 销售员代码
      ,[FSaleManName]  as 销售员名称
      ,[FDate]    as  出库日期
      ,[FCustomerNumber] as 客户代码
      ,[FCustomerName]  as 客户名称
      ,[FDOCUMENTSTATUS] as 单据状态
      ,[FAPPROVEDATE]  as 审核日期
      ,FCusomerProperty as 客户公司属性
      ,[FSEQ]  as 行号
      ,[FENTRYID]  as 销售出库表体内码
      ,[FMaterialNumber] as 物料代码
      ,[FMaterialName]   as 物料名称
      ,[FMaterialModel] as 规格型号
      ,[FUnitNumber]  as 单位代码
      ,[FUnitName]  as 单据名称
      ,[FOutStockQty] as 出库数量
      ,[FPRICE]   as 单价
      ,[FTAXPRICE] as 含税单价
      ,[FISFREE]  as 是否赠品
      ,[FTAXRATE] as 税率
      ,[FTAXAMOUNT]  as 税额
      ,[FAMOUNT]  as 金额
      ,[FALLAMOUNT] as 价税合计
      ,[FTAXAMOUNT_LC] as 税额本位币
      ,[FAMOUNT_LC]  as 金额本位币
      ,[FALLAMOUNT_LC] as 价税合计本位币
      ,[FSRCBILLNO]  as 源单编号
      ,[FSoBillNo]   as  订单编号
      ,[FSoSeq]  as 订单行号
      ,[FSOENTRYID] as 订单表体内码
      ,[FSoId]  as 订单内码
      ,F_SZSP_SettleAmt as 结算金额
      ,F_SZSP_SettleDate as 结算日期
  FROM rds_hr_sales_saleOutStockAll_mfg
  where  FDate >='",FStartDate,"' and FDate<='",FEndDate,"'
  and FSaleOrgName='",FCompanyName,"'")
  data = tsda::sql_select2(token = dms_token,sql = sql)
  return(data)
}




#' 同步销售出库及销售退货数据,生产数据口径
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
#' server_saleOutStock_mfg_click()
server_saleOutStock_mfg_click <- function(input,output,session,erp_token,dms_token) {

  #步同按续
  var_sync_src_saleOutStock_read <-tsui::var_dateRange('sync_src_saleOutStock_mfg_read')
  var_sync_src_saleOutStock_org <- tsui::var_ListChoose1('sync_src_saleOutStock_mfg_org')
  shiny::observeEvent(input$sync_src_saleOutStock_mfg_click,{

    dates = var_sync_src_saleOutStock_read()
    FStartDate = dates[1]
    FEndDate = dates[2]
    FCompanyName = var_sync_src_saleOutStock_org()
    #更新数据
    try({
      saleOutStock_erp2dms_mfg(erp_token = erp_token,
                        dms_token = dms_token,
                        FStartDate =FStartDate ,
                        FEndDate =FEndDate ,
                        FCompanyName = FCompanyName)
    })

    tsui::pop_notice('销售出库同步成功！')





  })


}





#' 同步出库数据,生产成本口径
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
#' server_saleOutStock_mfg_view()
server_saleOutStock_mfg_view <- function(input,output,session,dms_token) {

  #步同按续
  var_sync_src_saleOutStock_read <-tsui::var_dateRange('sync_src_saleOutStock_read')
  var_sync_src_saleOutStock_org <- tsui::var_ListChoose1('sync_src_saleOutStock_org')
  shiny::observeEvent(input$sync_src_saleOutStock_view,{

    dates = var_sync_src_saleOutStock_read()
    FStartDate = dates[1]
    FEndDate = dates[2]
    FCompanyName = var_sync_src_saleOutStock_org()
    #查看数据
    data = dms_saleOutStock_query(dms_token = dms_token,FStartDate =FStartDate ,FEndDate =FEndDate ,FCompanyName = FCompanyName)
    #显示数据
    tsui::run_dataTable2(id = 'sync_src_saleOutStock_dt',data)
    #下载数据
    file_name = paste0('销售出库及退货同步数据',FStartDate,"_",FEndDate,".xlsx")
    tsui::run_download_xlsx(id = 'sync_src_saleOutStock_dl',data = data,filename =file_name )





  })


}

