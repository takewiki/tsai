#' 针对查询结果格式化为DF
#'
#' @param data 查询结果数据
#'
#' @return 返回数据框
#'
#' @examples
#' aibot_as_df(letters)
aibot_as_df <-function(data){

  n <- length(data)
  FQues <- character(n)
  FScore <- numeric(n)
  FQuesMatch <- character(n)
  FAnsw <- character(n)
  for (i in 1:n) {
    FQues[i] <- data[[i]][[1]]
    FScore[i] <-data[[i]][[2]]
    FQuesMatch[i] <- data[[i]][[3]]
    FAnsw[i] <- data[[i]][[4]]

  }
  res <- data.frame(FQues,FScore,FQuesMatch,FAnsw,stringsAsFactors = F)
  return(res)



}

#' 创建查询机器人
#'
#' @param keyword 关键词
#' @param n 返回结果数
#' @param python 指定python路径
#'
#' @return 返回值
#' @import reticulate
#' @export
#'
#' @examples
#' bb <- aibot('发现神行多少钱')
#' print(bb)
aibot <- function(keyword='发现神行多少钱',
                  n=3,
                  python="/usr/local/bin/python3"){
  use_python(python,required = T);
  rdlaiye <- import("rdlaiye")
  res <-rdlaiye$api$aibot$query(keyword)
  ncount <- length(res)
  if(ncount >=n){
    ncount <- n
  }
  res <- head(res,ncount)
  res <- aibot_as_df(res)
  return(res)

}

#'增加答案的回复类型
#'
#' @param aibot_res 原始答案
#' @param min 最小值
#' @param high 最大值
#'
#' @return 返回值
#' @export
#'
#' @examples
#' aibot_answer_type()
aibot_answer_type <- function(aibot_res,min=0.6,high=0.9){
  n <- nrow(aibot_res)

  if(n>0){
    item <- aibot_res$FScore[1]
    if(item >high){
      res <-'A'
    }else if(item >min & item <=high){
      res <-'B'
    }else{
      res <-'C'
    }

  }else{
    res <-'C'
  }
  return(res)
}


#' 处理结果
#'
#' @param keyword 关键词
#' @param n 返回数
#' @param low  最小值
#' @param high  最大值
#' @param detail 是否显示明细
#'
#' @return 返回列表
#' @export
#'
#' @examples
#' ai()
ai<- function(keyword,n=3,low=0.6,high=0.9,detail=FALSE){
  aibot_res <- aibot(keyword,n)
  type <- aibot_answer_type(aibot_res,min = low,high = high)

  if(detail){
    if(type =='A'){
      answ = aibot_res$FAnsw[1]
    }else if(type=='B'){
      answ =aibot_res
    }else{
      answ='内部支持'
    }

  }else{
    if(type =='A'){
      answ = aibot_res$FAnsw[1]
    }else if(type=='B'){
      answ =aibot_res[,c('FQues','FAnsw')]
    }else{
      answ='内部支持'
    }

  }

  res=list(type,answ)
  names(res) =c('type','answ')
  return(res)
  }






