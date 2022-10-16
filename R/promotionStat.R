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
#' promotionEmp_Query()
promotionEmp_Query <- function(dms_token='9B6F803F-9D37-41A2-BDA0-70A7179AF0F3',
                            FSaleOrgName ='苏州赛普生物科技有限公司',
                            FYearPeriod ='202208'


) {

  sql <- paste0("SELECT
      [FSaleOrgNumber]  as 销售组织代码
      ,[FSaleOrgName] as  销售组织名称
      ,[FSaleDeptNumber] as 销售部门代码
      ,[FSaleDeptName] as 销售部门名称
      ,[FSaleManNumber] as 销售员代码
      ,[FSaleManName] as 销售员名称
	   ,[FSettleYearPeriod] as 结算年月
      ,sum([F_SZSP_SettleAmt])  as 结算金额
      ,sum([FPromotion]) as 提成金额_新账套
      ,sum([FPromotion_old]) as 提成金额_旧账套
      ,sum([FPromotion_diff]) as 提成差异
     FROM [cprds].[dbo].[rds_hr_sales_promotion_WithCompare]

  where FSettleYearPeriod=  ",FYearPeriod,"
  and FSaleOrgName ='",FSaleOrgName,"'
  group by FSaleOrgNumber,FSaleOrgName,FSaleDeptNumber,FSaleDeptName,FSaleManNumber,FSaleManName,FSettleYearPeriod")
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
#' promotionEmp_server()
promotionEmp_server <- function(input,output,session,dms_token) {

  var_cp_pfm_emp_org <- tsui::var_ListChoose1('cp_pfm_emp_org')

  var_cp_pfm_emp_YearPeriod <- tsui::var_text('cp_pfm_emp_YearPeriod')




  shiny::observeEvent(input$cp_pfm_emp_query,{


    FSaleOrgName = var_cp_pfm_emp_org()
    FYearPeriod =var_cp_pfm_emp_YearPeriod()


    data = promotionEmp_Query(dms_token = dms_token ,FSaleOrgName =FSaleOrgName ,FYearPeriod = FYearPeriod)
    #print(data)
    tsui::run_dataTable2(id = 'cp_pfm_emp_dataView',data = data)
    file_name = paste('提成汇总',FSaleOrgName,FYearPeriod,'.xlsx',sep = '_')
    tsui::run_download_xlsx(id = 'cp_pfm_emp_download',data = data,filename =file_name )

  })






}
