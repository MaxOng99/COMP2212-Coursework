" Vim syntax file 
" Language: StreamSpl
" Maintainer: Yi Cheng Ong, Julian Zhan Feng Chong
" Latest Revision: 17 April 2020

if exists("b:current_syntax")
	finish
endif

syn keyword typeKeywords Int Bool
syn keyword controlConstructKeywords if else return while do
syn keyword sequenceKeyword sequences
syn keyword operatorKeywords not length empty push pop print
syn keyword booleanKeywords true false
syn match variableNames '\w\+'
syn match digits '\d\+'
syn match singleLineComment '%%.*$'
syn match allExp contained 'v(.*%)&(.*\*)@!'
syn region multiLineCommentRegion start='%\*' end='\*%' contains=allExp

let b:current_syntax = "spl"

hi def link typeKeywords Type
hi def link controlConstructKeywords Statement
hi def link sequenceKeyWord Constant
hi def link operatorKeywords Identifier
hi def link booleanKeywords Ignore
hi def link variableNames Ignore
hi def link digits Special
hi def link singleLineComment Comment
hi def link multiLineCommentRegion Comment
hi def link allExp Comment
