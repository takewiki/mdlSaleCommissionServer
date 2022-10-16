
#' 查询数据
#'
#' @param dms_token 口令
#' @param FYear 年份
#' @param FPeriod 月份
#'
#' @return 返回值
#' @export
#'
#' @examples
#' mngrCost_query()
#'
mngrCost_query <- function(dms_token='9B6F803F-9D37-41A2-BDA0-70A7179AF0F3',
                                 FYear=2022,
                                 FPeriod=8){
  sql <- paste0("SELECT  [FNumber] as 编码
      ,[Fname] as 名称
      ,[Fmodel] as  规格型号
      ,[FOldNumber] as  旧物料编码
      ,[FPCSPerPkg] as 单箱标准数量
      ,[FMiddleSmallCoefficent] as 中小包装换算比
      ,[FBigMiddleCoefficent] as 大中包装换算比
      ,[FMngrCost] as  管理成本
      ,[FItemProperty] as 物料属性
      ,[FGroupName] as 物料分组
      ,[FBaseUnit] as 基本单位
      ,[FGroupNumber] as 物料分组编码
      ,[FCategoryName] as 存货类别
      ,[FPrdCategory] as 产品大类
      ,[Fyear] as 年份
      ,[Fperiod] as 月份
  FROM  [rds_bd_mngrCost]
  where  fyear = ",FYear,"  and fperiod = ",FPeriod," ")
  data = tsda::sql_select2(token = dms_token,sql = sql)
  return(data)

}

#' 查询管理成本
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
#' mngrCost_Server()
mngrCost_Server <- function(input,output,session,dms_token){


  var_cp_item_mngrcost_year <- tsui::var_text('cp_item_mngrcost_year')
  var_cp_item_mngrcost_period <- tsui::var_text('cp_item_mngrcost_period')
  shiny::observeEvent(input$cp_item_mngrcost_query,{

    FYear = as.integer(var_cp_item_mngrcost_year())
    FPeriod =as.integer(var_cp_item_mngrcost_period())
    data = mngrCost_query(FYear = FYear,FPeriod = FPeriod,dms_token = dms_token  )

    tsui::run_dataTable2(id = 'cp_item_mngrcost_dt',data = data)
    file_name = paste0('产品管理成本_',FYear,'_',FPeriod,'.xlsx')

    #下载管理成本
    tsui::run_download_xlsx(id = 'cp_item_mngrcost_dl',data = data,filename =file_name )






  })



}


