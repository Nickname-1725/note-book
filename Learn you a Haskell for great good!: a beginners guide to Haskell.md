<title>Learn you a Haskell for great good!: a beginners guide to Haskell</title>
<link rel="stylesheet" href="style.css"/>
<div class="tableofcontents">

- [Introduction](#150A5765)
  - [关于此教程](#94E9F8A7)
  - [所以什么是Haskell](#EE3D6601)
  - [你需要深入的事情](#BCB3F693)
- [Starting Out](#31960169)
  - [预备，go!](#9A5FB92C)
  - [蹒跚学函数](#5EF0CC46)
  - [介绍列表](#93A466F5)
  - [Texas ranges](#F4D420FF)
  - [我是列表推导式(list comprehension)](#D019D8A4)
  - [元组(Tuples)](#DC2A7208)
- [Types and Typeclasses](#FACBD084)
</div>

<div class="main">
<h1 id="150A5765">Introduction</h1>
<h2 id="94E9F8A7">关于此教程</h2>
<div class="sheet-wrap"><div class="sheet-caption">欢迎</div>


作者学习的方式是查各种资料, 这本书的目的在于从自己的观点教会人们学习Haskell，也就是增加一种参考方式

</div>
<div class="sheet-wrap"><div class="sheet-caption">目标人群</div>


目标人群
- 熟悉命令式编程的人
- 没有用函数式语言编成过的人
- 即使没有明显的编程经历的人或许也可以弄明白

推荐网上社交平台

</div>
<div class="sheet-wrap"><div class="sheet-caption">个人经历</div>


- 有过两次失败经历，但最终学会了
- 学习Haskell就像初次学习编程一样
- 很有趣，迫使你用不同的方式思考

</div>
<h2 id="EE3D6601">所以什么是Haskell</h2>
<div class="sheet-wrap"><div class="sheet-caption">纯函数式语言</div>


Haskell是纯函数式编程语言
- 命令式编程语言中
  - 通过给电脑一系列命令来执行
  - 执行过程中会改变状态
- 纯函数式编程中
  - 不是告诉电脑做什么，而是告诉电脑是什么
  - 函数没有副作用，函数唯一能做的就是计算并返回一个结果
  - 虽然听起来很限制，但是有数个很好的结果
    1. 函数多次用相同参数调用，能确保返回相同值——引用透明性(referential transparency)
    2. 允许编译器推理程序的行为
    3. 允许你容易地推断出程序是正确的，并且通过粘接简单的函数来搭建更加复杂的功能


</div>
<div class="sheet-wrap"><div class="sheet-caption">惰性</div>


惰性( **lazy** )
- 除非特别说明，Haskell将不会执行函数并计算值，直到被迫向您展示结果
- 与引用透明性搭配很好，并且允许你将程序看作一系列 **数据的变换**
- 同样允许酷的事情，如无限数据结构
- 当你想要惰性语言中的一些结果时，你可以只取一些初始数据并高效地转换整理它，让它重组为你最终想要的样子

</div>
<div class="sheet-wrap"><div class="sheet-caption">静态类型</div>


- 当你编译您的程序，编译器知道哪一块代码是数字，哪一块是字符串等等
- 很多可能的错误都在编译时间被捕获了
- Haskell使用具有 **类型推断(type inference)** 的类型系统
  - 不需要显式标出每一段代码的类型，系统会自动弄明白很多信息
  - 类型推断也允许你的代码更加通用

</div>
<div class="sheet-wrap"><div class="sheet-caption">优雅和简明</div>


- 使用了大量高阶概念
- Haskell程序通常比命令式等价代码更短
- 更短的代码更容易维护，具有更少的bug

</div>
<div class="sheet-wrap"><div class="sheet-caption">发明人很聪明</div>


略...

</div>
<h2 id="BCB3F693">你需要深入的事情</h2>
<div class="sheet-wrap"><div class="sheet-caption">编译器和平台</div>


- 编译器: GHC, 下载自Haskell Platform
- 交互模式: `ghci`, 基础操作
  - `:l myfunctions`
  - `:r`

</div>
<h1 id="31960169">Starting Out</h1>
<h2 id="9A5FB92C">预备，go!</h2>
<div class="sheet-wrap"><div class="sheet-caption">进入环境</div>


1. 从上一小节阅读所需环境
2. 进入交互命令行：`ghci`
3. 提示词`Prelude>`，若要更换为`ghci>`，可输入`:set prompt "ghci>"`

</div>
<div class="sheet-wrap"><div class="sheet-caption">简单的算术</div>


- 遵循通常的算术优先规则
- 可以使用括号来显式构造优先
- 负数需要用括号括起来，否则会导致警告

</div>
<div class="sheet-wrap"><div class="sheet-caption">逻辑运算</div>


1. `&&`, `||`, `not`. 
2. `True`, `False`. 


</div>
<div class="sheet-wrap"><div class="sheet-caption">类型错误</div>


1. `5 + "llama"`: 错误
2. `5 == True`: 类型不匹配警告
3. `5 + 4.0`: 5变为浮点数，参与运算
</div>
<div class="sheet-wrap"><div class="sheet-caption">中缀函数、前缀函数、函数应用</div>


中缀函数与前缀函数举例
1. 中缀函数: `*`
2. 前缀函数: `succ`, `min`, `max`

函数应用具有最高优先级
- 例如
  ``` Haskell
  ghci> succ 9 + max 5 4 + 1
  ```
  等同于
  ``` Haskell
  ghci> (succ 9) + (max 5 4) + 1
  ```
- 例如
  应当编写 `succ (9 * 10)` 而非 `succ 9 * 10`

具有两个参数的函数也可以当作中缀函数调用
- 需要给函数名包裹反引号
- 例如：``92 `div` 10`` ，相当于 `div 92 10`

函数的应用通过空格而不是括号进行

</div>
<h2 id="5EF0CC46">蹒跚学函数</h2>
<div class="sheet-wrap"><div class="sheet-caption">简单示例: doubleMe, doubleUs</div>


略过

</div>
<div class="sheet-wrap"><div class="sheet-caption">说明：不必有顺序</div>


Haskell中的函数不需要有任何特别的先后顺序

</div>
<div class="sheet-wrap"><div class="sheet-caption">简单示例: doubleSmallNumber</div>


略

</div>
<div class="sheet-wrap"><div class="sheet-caption">说明：表达式</div>


if 语句
- 示例
  ``` Haskell
  doubleSmallNumber x = if x > 100
                          then x
                          else x * 2
  ```
- Haskell的if语句与其它命令式语言的if语句不同之处在于Haskell中的else部分是强制的
- Haskell中的if语句也是个表达式，一定会返回一些值
- if语句可以单独一行写出，增加括号会影响优先级
  ``` Haskell
  doubleSmallNumber' x = (if x > 100 then x else x * 2) + 1
  ```
  若忽略括号，则会导致只对else部分起作用

</div>
<div class="sheet-wrap"><div class="sheet-caption">说明：'符号可用于函数名</div>


- `'` 符号在Haskell中没有特殊含义
- 可以作为合法的函数变量名字母
- 通常用来代表
  - 函数的严格版本（非惰性）
  - 变量或函数的轻微修改版本

</div>
<div class="sheet-wrap"><div class="sheet-caption">说明：关于函数的2点说明</div>


``` Haskell
conan0'Brien = "It's a-me, Conan O'Brien!"
```

两点注意
1. 函数名称首字母不大写
2. 当函数不接受任何参数时，当场称为“定义”（或名称）
   - 一旦定义后，无法改变该名称（函数）
   - 名称和函数内的东西可以相互替换使用

</div>
<h2 id="93A466F5">介绍列表</h2>
<h2 id="F4D420FF">Texas ranges</h2>
<h2 id="D019D8A4">我是列表推导式(list comprehension)</h2>
<h2 id="DC2A7208">元组(Tuples)</h2>
<h1 id="FACBD084">Types and Typeclasses</h1>
</div>
