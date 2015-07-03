## LL(1) 递归下降词法解析与语法解析

* 词法解析 -> 将输入的字符串流解析为嵌套的Token列
* 语法解析 -> 判断输入的字符串是否符合格式化的语法结构

## grammar NameList;

* parser

```
list     : '[' elements ']' ;        // match bracketed list
elements : element (',' element)* ;  // match comma-separated list
element  : NAME | list ;             // element is name or nested list
```

* lexer

```
NAME     : LETTER+ ;                 // name is sequence of >=1 letter
LETTER   : 'a'..'z'|'A'..'Z';        // define what a letter is
WS       : (' '|'\t'|'\n'|'\r')+ {skip();} ; // throw out (ignore) whitespace
```
