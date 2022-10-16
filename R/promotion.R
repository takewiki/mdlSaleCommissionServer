#' 提成数据查询
#'
#' @param dms_token 口令
#' @param FSaleOrgName 销售组织
#' @param FSaleManName 销售员
#' @param FStartDate  开始结算日期
#' @param FEndDate   as 结束结算日期
#'
#' @return
#' @export
#'
#' @examples
#' promotion_query()
promotion_query <- function(dms_token='9B6F803F-9D37-41A2-BDA0-70A7179AF0F3',
                 FSaleOrgName ='苏州赛普生物科技有限公司',
                 FSaleManName ='陈龙',
                 FStartDate ='2022-08-01',
                 FEndDate ='2022-09-30'

                 ) {

sql <- paste0("SELECT
        FBILLNO as 单据编码
      ,[FBillTypeName] as   单据类型
      ,[FSaleOrgNumber]  as 销售组织代码
      ,[FSaleOrgName] as  销售组织名称
      ,[FSaleDeptNumber] as 销售部门代码
      ,[FSaleDeptName] as 销售部门名称
      ,[FSaleGroupNumber] as 销售组代码
      ,[FSaleGroupName]  as 销售组名称
      ,[FSaleManNumber] as 销售员代码
      ,[FSaleManName] as 销售员名称
      ,[FDate]   as  出库日期
      ,[FYearPeriod] as 出库年月
      ,[FCustomerNumber] as 客户代码
      ,[FCustomerName]  as 客户名称
      ,[FDOCUMENTSTATUS] as 单据状态
      ,[FAPPROVEDATE] as 审核日期
      ,[FCusomerProperty] as 客户公司属性
      ,[FSEQ]  as 行号
      ,[FENTRYID] as  销售出库表体内码
      ,[FMaterialNumber] as 物料代码
      ,[FMaterialName] as 物料名称
      ,[FMaterialModel] as 规格型号
      ,[FUnitNumber] as 计量单位代码
      ,[FUnitName] as  计量单位名称
      ,[FOutStockQty]  as  出库数量
      ,[FPRICE] as 不含税单价
      ,[FTAXPRICE] as 含税单价
      ,[FTAXPRICE_LC]  as 含税单位本位币
      ,[FISFREE] as 是否赠品
      ,[FTAXRATE] as 税率
      ,[FTAXAMOUNT] as 税额
      ,[FAMOUNT] as 金额
      ,[FALLAMOUNT] as 价税合计
      ,[FTAXAMOUNT_LC] as 税额本位币
      ,[FAMOUNT_LC] as 金额本位币
      ,[FALLAMOUNT_LC] as 价税合计本位币
      ,[FSRCBILLNO] as 源单编号
      ,[FSoBillNo]  as 销售订单编号
      ,[FSoSeq]   as 销售订单行号
      ,[FSOENTRYID] as 销售订单表体内码
      ,[FSoId]   as 销售订单内码
      ,[F_SZSP_SettleAmt]  as 结算金额
      ,[F_SZSP_SettleDate] as 结算日期
      ,[FSettleYearPeriod] as 结算年月
      ,[FSettleDays] as  回款天数
      ,[FMngrCost] as 单位管理成本
      ,[FTotalMngrCost] as 总管理成本
      ,[FCustomerCoef] as 客户系数
      ,[FProfitAmt] as 净利润
      ,[FProfitRate] as 净利率
      ,[FType] as 提成类型
      ,[FPromotion] as 提成金额_新账套
      ,[FPromotion_old] as 提成金额_旧账套
      ,[FPromotion_diff] as 提成差异
      ,[FNote]  as 备注
  FROM [cprds].[dbo].[rds_hr_sales_promotion_WithCompare]

  where F_SZSP_SettleDate >='",FStartDate,"' and  F_SZSP_SettleDate  <='",FEndDate,"'
  and FSaleOrgName ='",FSaleOrgName,"'
  and  FSaleManName ='",FSaleManName,"'")
data = tsda::sql_select2(token = dms_token,sql = sql)
return(data)

}



#' 提成查询
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' promotion_server()
promotion_server <- function(input,output,session,dms_token) {


  #提成查询
  var_cp_pfm_res_org <-tsui::var_ListChoose1('cp_pfm_res_org')
  #暂估使用新账套

  var_cp_pfm_res_db <- tsui::var_ListChoose1('cp_pfm_res_db')

  var_cp_pfm_res_dateRange_query <-tsui::var_dateRange('cp_pfm_res_dateRange_query')


  shiny::observeEvent(input$cp_pfm_res_query,{

    dates = var_cp_pfm_res_dateRange_query()
    FStartDate = dates[1]
    FEndDate = dates[2]
    FSaleManName = input$cp_pfm_res_saleMan_selector
    FSaleOrgName = var_cp_pfm_res_org()
    data = promotion_query(dms_token = dms_token,
                           FSaleOrgName = FSaleOrgName,
                           FSaleManName = FSaleManName,FStartDate = FStartDate,FEndDate = FEndDate

                           )
    tsui::run_dataTable2(id = 'cp_pfm_res_dataView',data = data)
    file_name = paste('提成明细',FSaleOrgName,FSaleManName,FStartDate,FEndDate,'.xlsx',sep = '_')
    tsui::run_download_xlsx(id = 'cp_pfm_res_download',data = data,filename =file_name )




  })

}
