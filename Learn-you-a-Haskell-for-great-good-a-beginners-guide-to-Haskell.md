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
  - [相信类型(Believe the type)](#9D9FE022)
  - [类型变量(Type variables)](#2F4ABA0A)
  - [类型类(Typeclasses)](#93BB1E55)
- [函数中的语法](#83D31AA0)
  - [模式匹配](#6388DE55)
  - [守卫(Guards, guards!)](#BCEB1FBC)
  - [Where!?](#F7583AAB)
  - [Let it be](#C02EB15E)
  - [Case表达式](#7A1F7EDA)
- [递归](#C7B40BC9)
  - [你好递归](#F0FD8021)
  - [最棒(maximum awesome)](#C049B7D4)
  - [再来几个递归函数](#042EAF63)
  - [快，排序！](#3F53D80C)
  - [递归地思考](#B36C606A)
- [高阶函数](#B09C0DB1)
  - [柯里化函数](#0984FB78)
  - [某些高阶主义/更高秩序主义(higher-orderism)是合理的](#F2BF0696)
  - [Map和filter](#D9F15B6D)
  - [Lambda](#D8846B0B)
  - [只有fold和马](#C99A2B2A)
  - [函数应用与$](#508D3D8D)
  - [函数组合](#0F76AED0)
- [模块](#197FCD04)
  - [加载模块](#32788B02)
  - [Data.List](#919CB162)
  - [Data.Char](#98AC801F)
  - [Data.Map](#962E96E6)
  - [Data.Set](#CB10689F)
  - [制造我们自己的模块](#EB01D03E)
- [我们自己的类型以及类型类](#ADC1965B)
  - [代数数据类型介绍](#B4BF6FA7)
  - [记录语法](#4D4A2DBE)
  - [类型参数](#2BD2E412)
  - [派生实例](#2C826D8B)
  - [类型同义词](#A7CD6634)
  - [递归数据结构](#6468FD6E)
  - [类型类102](#7989C8EE)
  - [一种是-否类型类](#BE3DEDFC)
  - [函子(Functor)类型类](#C1FB41C2)
  - [Kinds和一些type-foo](#6613BA46)
- [输入和输出](#69A48341)
  - [Hello, world!](#E4123C56)
  - [文件和流](#2876763C)
  - [命令行变量](#8C165134)
  - [随机性](#94B993AA)
  - [字节串](#F5941F61)
  - [异常](#6C548C8F)
- [函数化解决问题](#551B54A4)
- [函子，应用函子以及Moniods](#4CC1C54A)
  - [重提函子](#379CE627)
  - [应用函子](#EDE43895)
  - [newtype关键字](#03774CDA)
    - [使用newtype来创造类型类实例](#5A4EA31E)
    - [关于newtype惰性](#87E25935)
    - [type vs. newtype vs. data](#97C34E42)
  - [Monoids](#3E57A703)
    - [列表是幺半群](#4FABC9ED)
    - [乘和积](#DCD87A48)
    - [Any和All](#874DF295)
    - [排序幺半群](#9ECFE9F0)
    - [Maybe幺半群](#2A67F49F)
    - [使用幺半群折叠数据结构](#43521602)
- [一把Monad](#3A9E3C0F)
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
<div class="sheet-wrap"><div class="sheet-caption">简介</div>


lisp在Haskell中很有用
- 三最常用的数据结构
- 可以用许多不同的方式解决所有问题
- 本小节探讨列表基础、字符串以及列表推导式

</div>
<div class="sheet-wrap"><div class="sheet-caption">列表的类型</div>


在Haskell中，list是同质化数据结构
- 存储数个相同类型的数据
- 列表标记为方括号，列表的值用都好分隔
- 字符串也是列表，如`"hello"`是`['h','e','l','l','o']`的语法糖
  - 可以对字符串使用列表的函数

</div>
<div class="sheet-wrap"><div class="sheet-caption">列表的语法糖与基本操作</div>


1. 列表的拼接：`++`
   - `[1,2,3,4] ++ [9,10,11,12]`得到`[1,2,3,4,9,10,11,12]`
   - `"hello" ++ " " ++ "world"`得到`"hello world"`
   - `['w','0'] ++ ['o','t']`得到`"woot"`
   - 对于短列表的拼接并没有什么问题，但如果第一个列表长达五千万元素，则会花很长时间
2. 向列表头部追加元素：`:`
   - `'A':" SMALL CAT"`得到`"A SMALL CAT"`
   - `5:[1,2,3,4,5]`得到`[5,1,2,3,4,5]`
   - `1:2:3:[]`得到`[1,2,3]`
   > 注意：`[]`, `[[]]`，`[[],[],[]]`是不同的东西
3. 以索引的方式获取列表元素：`!!`，索引从0开始
   - `"Steve Buscemi" !! 6`得到`'B'`
   - `[9.4, 33.2, 96.2, 11.2, 23.25] !! 1`得到`33.2`
   - 若超出最大索引，则会导致错误

</div>
<div class="sheet-wrap"><div class="sheet-caption">嵌套列表</div>


列表可以嵌套
- 列表可以包含列表，甚至可以包含数层
- 所包含的列表可以长度不同，但必须有相同的类型

</div>
<div class="sheet-wrap"><div class="sheet-caption">列表大小的比较</div>


若列表包含的元素可比较，列表也可比较
- 使用`<`, `<=`, `>`和`>=`比较李诶报
- 以字典顺序比较
- 首先比较头部，如果它们相等，接着比较第二个元素，依次类推
  - `[3,2,1] > [2,1,0]`得到`True`
  - `[3,2,1] > [2,10,100]`得到`True`
  - `[3,4,2] > [3,4]`得到`True`
    *前面的元素都相等，这里应该是列表长度更长者更大*
  - `[3,4,2] > [2,4]`得到`True`
  - `[3,4,2] == [3,4,2]`得到`True`
    *在比较等于号的时候，应该是不存在短路的情况，我也试验了`>=`，应该必须是严格不等于才能逻辑短路*

</div>
<div class="sheet-wrap"><div class="sheet-caption">列表的更多操作</div>


列表的取用
1. `head`
2. `tail`
3. `last`
4. `init`
- 说明：当列表为空，不能使用这些操作，否则会报错

更多实用的基本操作
1. `length`
2. `null`
3. `reverse`
4. `take`
5. `drop`
6. `maximum`
7. `minimum`
8. `sum`
9. `product`
10. `elem`

</div>
<h2 id="F4D420FF">Texas ranges</h2>
<div class="sheet-wrap"><div class="sheet-caption">区间</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">指定步长与注意事项</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">无穷列表</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">列表的重复</div>

</div>
<h2 id="D019D8A4">我是列表推导式(list comprehension)</h2>
<div class="sheet-wrap"><div class="sheet-caption">比方：集合推导式</div>


$$S = \{2\cdot x|x\in \mathbb{N}, x\le 10\}$$
- 左侧的部分称为输出函数
- $x$是变量，$\mathbb N$是输入集合，$x\le 10$是谓词
- 表示集合包含所有满足谓词的自然数的双倍

</div>
<div class="sheet-wrap"><div class="sheet-caption">使用列表推导式表达</div>


- 用列表推导式表达\
  `[x * 2| x <- [1..10]]`
- 增加谓词\
  - `ghci> [x*2 | x <- [1..10], x*2 >= 12]`
  - ``ghci> [x | x <- [50..100], x `mod` 7 == 3]``
- 通过谓词修剪列表也称为"过滤"（filtering）
</div>
<div class="sheet-wrap"><div class="sheet-caption">示例：boomBangs</div>


略（定义一个函数，处理所输入的列表）

</div>
<div class="sheet-wrap"><div class="sheet-caption">说明：多个谓词、多个输入列表</div>


1. 在一个列表推导式中可以使用多个谓词来过滤列表，结果会满足所有的谓词
2. 可以从多个列表中获取数据，在没有过滤的情况下，推导式将会产生所有的组合
   - 例如，从两个长度为4的列表中生成列表，在不过滤的情况下生成的列表长度为16
   - `ghci> [x*y | x <- [2,5,10], y <- [8,10,11]]`得到`[16,20,22,40,50,55,80,100,110]`
   - 也可以加入谓词\
     `ghci> [x*y | x <- [2,5,10], y <- [8,10,11], x*y > 50]`得到`[55,80,100,110]`

</div>
<div class="sheet-wrap"><div class="sheet-caption">若干个示例</div>


1. 将形容词和名词列表组合
2. 自己实现一个`length`
3. 除掉非大写字母
   - 代码
     ``` Haskell
     removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]
     ```
   - 测试
     ``` Haskell
     ghci > removeNonUppercase "Hahaha! Ahahaha!"
     "HA"
     ghci > removeNonUppercase "IdontLIKEFROGS"
     "ILIKEFROGS"
     ```

</div>
<div class="sheet-wrap"><div class="sheet-caption">列表推导式的嵌套</div>


列表嵌套使用
- 代码
  ``` Haskell
  ghci> let xxs [[1,3,5,2,3,1,2,4,5], [1,2,3,4,5,6,7,8,9],
  [1,2,4,2,1,6,3,1,3,2,3,6]]
  ghci> [[x | x <- xs, even x] | xs <- xxs]
  [[2,2,4],[2,4,6,8],[2,4,2,6,2,6]]
  ```
- 说明：可以跨行写列表推导式

</div>
<h2 id="DC2A7208">元组(Tuples)</h2>
<div class="sheet-wrap"><div class="sheet-caption">元组与列表的区别</div>


相同点：都是将数个值存储成一个值

不同点
1. 容纳的个数
   - 列表可以包含无穷多个数字
   - 元组只能用于你已经知道确切想要组合多少个值
   - 元组的类型取决于它包含多少个值，以及内部元素的类型
2. 同质性
   - 元组不需要是同质的(homogenous)
   - 元组可以包含数种类型的组合

元组记为括号，包含的组件用逗号分隔

</div>
<div class="sheet-wrap"><div class="sheet-caption">示例：存储二维向量</div>


- 假设需要存储若干二维变量
- 问题在于，可能出现类似这样的错误`[[1,2],[8,11,5],[4,5]]`
- 由于大小为2的元组（对(pair)）具有它自己的类型，因此会避免不同个数的元组位于同一列表中
- 形如`[(1,2),(8,11,5),(4,5)]`的代码将会报错

</div>
<div class="sheet-wrap"><div class="sheet-caption">说明：元组可以用来表达很大范围的数据</div>


例如，某人的名称和年龄：`("Christopher", "Walken", 55)`

</div>
<div class="sheet-wrap"><div class="sheet-caption">元组的性质：更加固定</div>


- 元组中的每一片数据都有它自身的类型
- 不能编写一个通用的方程来向元组添加一个元素——不需要编写元素向一个对(pair)添加元素，或是其它做法
- 尽管存在单元素列表，并不存在单元素元组——没有意义
- 和列表类似，如果内部元素可以比较，元组也可以比较
  - 不能比较不同长度(size)的元组，但可以比较不同长度的列表
- `fst`和`snd`的用法
  > 注意：这两个函数只能向pair操作，后文将会介绍如何用不同的方式从元组提取数据

</div>
<div class="sheet-wrap"><div class="sheet-caption">zip：产生对(pair)的列表(list)</div>


- 获取两个列表，通过将对应的元素组合成对来将它们缝合进一个列表
- 因为对可以具有不同的类型，所以`zip`可以获取两个不同类型的列表
- 当两个列表长度不同时，较长者被截断
  - 可以将有限列表和无限列表缝合
  - ``` Haskell
    ghci> zip [1..] ["apple", "orange", "cherry", "mango"]
    [(1,"apple"),(2,"orange"),(3,"cherry"),(4,"mango")]
    ```

</div>
<div class="sheet-wrap"><div class="sheet-caption">示例：求得满足条件的三角形</div>


问题
- 求直角三角形
- 所有的边均为整数
- 所有的边都小于等于10
- 周长小于24

解决
1. 生成所有的三角形，满足所有边小于等于10
2. 增加条件：假定直角三角形，c为斜边，b不大于c，a不大于b
3. 增加谓词，周长为24
4. 得到结果`[(6,8,10)]`

函数式编程的通用方案：
- 获取初始的一组解
- 对这些解施加变换，过滤它们，直至获得正确的解

</div>
<h1 id="FACBD084">Types and Typeclasses</h1>
<h2 id="9D9FE022">相信类型(Believe the type)</h2>
<div class="sheet-wrap"><div class="sheet-caption">静态类型系统与类型推断</div>


- 静态类型系统：类型错误将不会通过编译，因此避免了这类错误导致程序崩溃
- 类型推断：不需要显式写出所有函数和表达式的类型，就可以完成事情

</div>
<div class="sheet-wrap"><div class="sheet-caption">查看类型命令:t</div>


使用`:t`命令来查看合法表达式的变量
- 打印出: 表达式+`::`+类型，类型首字母大写
- `'a'`的类型：`Char`
- `"HELLO!"`的类型：`[Char]`
- `(True,'a')`的类型：`(Bool,Char)`

</div>
<div class="sheet-wrap"><div class="sheet-caption">函数的显式类型声明</div>


- 函数也有类型
- 我们可以给函数编写显式类型声明
- 从现在开始，我们会给每个编写的函数类型声明
- 过滤大写字母的函数的类型声明
  ``` Haskell
  removeNonUppercase :: [Char] -> [Char]
  removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]
  ```
  也可写成`String -> String`
- 接收数个参数的函数的类型声明
  ``` Haskel
  addThree :: Int -> Int -> Int -> Int
  addThree x y z = x + y + z
  ```
  - 说明：略...
- 如果你想给出函数的类型签名，但拿不准，你可以写出函数然后用`:t`来检查；函数也是表达式

</div>
<div class="sheet-wrap"><div class="sheet-caption">一些常见的类型</div>


- `Int`：代表整数，有范围限制，通常在32位机器上的最大可能值为2147483647，最小值-2147483648
- `Integer`：也代表整数，没有范围限制，可以用来代表大数
- `Float`：实数单精度浮点数
- `Bool`：布尔类型，只有两个值`True`和`False`
- `Char`：代表字符，标记为单引号。一串字符列表是字符串
- `Tuple`：是类型，但是取决于它们的长度和它们包含的元素的类型；
  - 理论上有无穷多个元组类型
  - 空元组`()`也是一种类型，它只有一个值：`()`

</div>
<h2 id="2F4ABA0A">类型变量(Type variables)</h2>
<div class="sheet-wrap"><div class="sheet-caption">head函数的类型签名</div>


- head函数：返回任意类型的列表的头部
- 类型签名`head :: [a] -> a`
- 之前提到类型首字母大写，所以`a`不是一个类型，而是一个类型变量
- 这就表示它可以是任何类型
- 如同其它语言中的泛型(generics)，但是在Haskell中更加强大，因为它允许容易地写出非常通用的函数
- 具有类型变量的函数称为多态函数(polymorphic functions)
- 尽管类型变量可以有不止一个字母的名字，我们通常将其命名为`a`、`b`、`c`、`d`……

</div>
<div class="sheet-wrap"><div class="sheet-caption">fst函数的类型签名</div>


- `fst`的类型签名`fst :: (a, b) -> a`
  - 并不是说`a`和`b`一定是不同的类型

</div>
<h2 id="93BB1E55">类型类(Typeclasses)</h2>
<div class="sheet-wrap"><div class="sheet-caption">类型类是一类接口</div>


- 类型类是一种接口，定义某些行为
- 如果一个类型是类型类的一部分，就表明它支持且实现类型类描述的行为

</div>
<div class="sheet-wrap"><div class="sheet-caption">等于函数的类型约束</div>


- `==`函数的类型签名`(==) :: (Eq a) => a -> a -> Bool`
- 
  > 注意：等于操作符，是一个函数，加减乘除和其它所有的操作符也是。\
  > 如果一个函数只由特殊符号组成，它就会被默认看作是中缀函数\
  > 如果我们想要检验它的类型、将其传入另一个函数，或者见其作为中缀函数调用，我们必须将其包裹括号
- `=>`符号：该符号之前的东西被称为 **类型约束**
- 我们可以将之前的类型声明读作：等于函数后去人以两个具有相同类型的值并返回布尔类型，这两个值的类型必须是`Eq`类的一个成员（是类型约束）
- 任何比较相等有意义的类型都应当为`Eq`类的一个成员
- 所有标准的Haskell类型，除了IO（处理输入输出）和函数，都是`Eq`类型类的一个部分

</div>
<div class="sheet-wrap"><div class="sheet-caption">一些基础的类型类</div>


- `Eq`: 用于支持相等检测的类型
  - 它的成员实现的函数有`==`和`/=`
  - 我们之前提到的所有的类型，除了函数，都是`Eq`的一部分，他们都可以检测相等
- `Ord`: 用于具有顺序的类型
  - 覆盖了所有标准的比较函数，如`>`、`<`、`>=`、`<=`，这些函数接收`Ord`成员中相同类型的两个值返回一个顺序(ordering)
  - 顺序(ordering)是一种类型，可以是`GT`, `LT`, 或者`EQ`，表示大于、小于和等于
  - `Ord`的成员必须首先是`Eq`的成员
- `Show`: 用于可以作为字符串呈现的类型
  - 处理该类型类的，最常用的函数是`show`，它接收一个值，其类型是`Show`的成员，将其用字符串表达
- `Read`: 有点和`Show`反过来
  - `read`函数接收一个字符串并且返回`Read`类型类中的一个类型
  - 如果`read "4"`，将会错误，此时GHCI还无法推断应当返回的类型
  - 一种显式声明表达式雷习惯的方式是输入注解(annotation)
    ``` Haskell
    ghci> read "5" :: Int
    5
    ghci> read "5" :: Float
    5.0
    ghci> (read "5" :: Float) * 4
    20.0
    ghci> read "[1,2,3,4]" :: [Int]
    [1,2,3,4]
    ghci> read "(3, 'a')" :: (Int, Char)
    (3, 'a')
    ```
- `Enum`: 是顺序排列的类型，他们可以被枚举
  - `Enum`类型类的主要优点是我们可以将其类型应用于列表区间
  - 它们也有定义的后继和前继，你可以用`succ`、`pred`函数得到它们
  - 这一类中的类型有：`()`、`Bool`、`Char`、`Ordering`、`Int`、`Integer`、`Float`、`Double`
- `Bounded`成员有一个上限和下限
  - `minBound`和`maxBound`是多态常量
- `Num`是数值类型类
  - 它的成员具有类似数字的特性，包括`Int`、`Integer`、`Float`、`Double`
  - 一种类型如果要加入`Num`，它必须已经是`Show`和`Eq`
  - `Integral`也是一个类型类，包括`Int`和`Integer`
  - `Floating`也是一个类型类，包括`Float`和`Double`
  - `fromIntegral`函数(`(Num b, Integral a) => a -> b`)可以将整数变为更加通用的数字
    - 注意它的类型签名里面有数个类型约束这是完全合法的，这些类型约束用逗号分隔

</div>
<h1 id="83D31AA0">函数中的语法</h1>
<h2 id="6388DE55">模式匹配</h2>
<div class="sheet-wrap"><div class="sheet-caption">开始介绍模式匹配</div>


简介
- 模式匹配由特定的数据组合模式组成
- 需要检查是否符合，并根据模式解构(deconstruct)数据

使用
- 定义函数时可以为不同的模式定义单独的函数体
- 从而编写简洁、可阅读的代码
- 可以对任何数据类型模式匹配

</div>
<div class="sheet-wrap"><div class="sheet-caption">示例：匹配1到5的数字</div>


- 可以使用if then else分支，但会产生错综复杂的树状分支
- 使用条件匹配
  ``` Haskell
  sayMe :: (Integral a) => a -> String
  sayMe 1 = "One!"
  sayMe 2 = "Two!"
  sayMe 3 = "Three!"
  sayMe 4 = "Four!"
  sayMe 5 = "Five!"
  sayMe x = "Not between 1 and 5"
  ```
- 条件匹配是从顶部向底部依次匹配的，如果匹配到一个模式，对应的函数体将会被使用

</div>
<div class="sheet-wrap"><div class="sheet-caption">示例：阶乘的模式匹配方式实现</div>


*略...*

</div>
<div class="sheet-wrap"><div class="sheet-caption">模式匹配的失败：非穷举模式</div>


- 当存在非穷举模式(non-exhaustive pattern)时，模式匹配无法捕捉到一些输入
- 当构造模式时，应当总是包含一个包罗万象的模式，从而防止意外输入导致的崩溃

</div>
<div class="sheet-wrap"><div class="sheet-caption">示例：2D向量相加(模式匹配与元组)</div>


模式匹配可以用于元组

构造一个函数，将两个二维空间向量相加
- 非模式匹配方式
  ``` Haskell
  addVectors :: (Num a) => (a,a) -> (a,a) -> (a,a)
  addVectors a b = (fst a + fst b, snd a + snd b)
  ```
- 模式匹配方式
  ``` Haskell
  addVectors :: (Num a) =>  (a,a) -> (a,a) -> (a,a)
  addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
  ```
- 这是一个包罗万象的模式

</div>
<div class="sheet-wrap"><div class="sheet-caption">示例：实现三元组的提取</div>


*略...*

说明：和列表推导式一样，`_`代表不关心这一部分量是什么

</div>
<div class="sheet-wrap"><div class="sheet-caption">列表推导式的模式匹配</div>


``` Haskell
ghci> let xs = [(1,3), (4,3), (2,4), (5,3), (5,6), (3,1)]
ghci> [a+b | (a,b) <- xs]
[4,7,6,8,11,4]
```

</div>
<div class="sheet-wrap"><div class="sheet-caption">列表的模式匹配</div>


- 可以匹配
  - 空列表`[]`
  - 任何涉及`:`和`[]`的模式
- 模式`x:xs`
  - 会将列表头绑定到`x`，以及剩下的部分绑定到`xs`
  - 即使只有一个元素，`xs`的部分也会变成空列表
  - 注意，这个模式用得很多，尤其是在递归函数中
- 一次性绑定列表的前几个元素
  - 例如，用数个变量绑定前三个元素，另一个变量绑定剩下的部分
  - 使用`x:y:z:zs`
  - 只会匹配到具有三个或更多元素的列表

</div>
<div class="sheet-wrap"><div class="sheet-caption">示例：手动实现head函数</div>


例子：手动实现`head`函数( *代码略*)

说明
- 若要绑定数个变量，仍然需要将其包裹在圆括号中
- 所使用的`error`函数获取一个字符串，产生一个运行时错误

</div>
<div class="sheet-wrap"><div class="sheet-caption">示例：列表元素的英语描述</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">示例：length函数的模式匹配方式</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">示例：sum函数的模式匹配实现</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">另一种模式匹配：@</div>


- 将某个东西根据模式分解，但是仍然能够获取整个东西
- 例如模式`xs@(x:y:ys)`，会按照`x:y:ys`的方式匹配，但是仍然通过`xs`获取整个列表，而不是重复`x:y:ys`

*示例略...*

</div>
<div class="sheet-wrap"><div class="sheet-caption">说明：不能使用++来匹配</div>


不能使用`++`进行模式匹配
- `(xs ++ ys)`将没有意义
- `(xs ++ [x])`将违背列表的性质(nature)

</div>
<h2 id="BCEB1FBC">守卫(Guards, guards!)</h2>
<div class="sheet-wrap"><div class="sheet-caption">开始介绍卫语句</div>


卫语句
- 用来检测值的某些特性为真或假
- 当你有数个条件时，卫语句非常更加可读，和模式配合很好

</div>
<div class="sheet-wrap"><div class="sheet-caption">示例：BMI判断器</div>


根据BMI来痛斥你
- 代码
  ``` Haskell
  bmiTell :: (RealFloat a) => a -> String
  bmiTell bmi
      | bmi <= 18.5 = "You're underweight, you emo, you!"
      | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
      | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
      | otherwise   = "You're a whale, congratulations!"
  ```
- 卫语句用竖线表示，在函数名以及参数后面
- 通常在稍微右侧对齐，竖着排下来
- 一条卫语句基本上是一个布尔表达式
- 如果它的值为真，对应的函数体将会使用，如果为假，依次顺到下一条检查
- 很多时候，最后一条卫语句是`otherwise`
  - `otherwise`简单地定义为真，能够捕获所有东西
- 卫语句类似模式
  - 唯一不同的是模式匹配检查输入是否满足模式
  - 而卫语句检查布尔条件
- 卫语句和模式匹配合作
  - 如果一个函数内所有的卫语句都为假，求值将会落入下一个模式中
  - 如果没有合适的卫语句或者模式，将会抛出错误

改进示例：用体重和身高计算BMI，然后分类处理( *代码略*)

</div>
<div class="sheet-wrap"><div class="sheet-caption">说明：等号的位置</div>


注意：函数名和参数之后、第一个卫语句之前，没有等号。很多新手在这里遇到了语法错误

</div>
<div class="sheet-wrap"><div class="sheet-caption">示例：自己实现max</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">卫语句的行内(inline)写法</div>


卫语句也可以写在行内(inline)
- 
  ``` Haskell
  max' :: (Ord a) => a -> a -> a
  max' a b | a > b = a | otherwise = b
  ```
- 但是会变得更加不可读

</div>
<div class="sheet-wrap"><div class="sheet-caption">示例：自己实现compare</div>


``` Haskell
myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
    | a > b     = GT
    | a == b    = EQ
    | otherwise = LT
ghci> 3 `myCompare` 2
GT
```

注意：我们不仅可以用反引号以中缀的方式调用函数，也可以以中缀的方式定义函数

</div>
<h2 id="F7583AAB">Where!?</h2>
<div class="sheet-wrap"><div class="sheet-caption">示例：bmi讨论函数，用where减少重复</div>


- 减少重复
  ``` Haskell
  bmiTell :: (RealFloat a) => a -> a -> String
  bmiTell weight height
      | bmi <= 18.5 = "You're underweight, you emo, you!"
      | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
      | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
      | otherwise   = "You're a whale, congratulations!"
      where bmi = weight / height ^ 2
  ```
- 我们将关键词`where`放置在卫语句后面（通常要和竖线对齐），随即定义名称或函数
- 这些名称在卫语句中间可见，减少重复
- 进一步增加可读性
  ``` Haskell
  bmiTell :: (RealFloat a) => a -> a -> String
  bmiTell weight height
      | bmi <= skinny = "You're underweight, you emo, you!"
      | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
      | bmi <= fat    = "You're fat! Lose some weight, fatty!"
      | otherwise     = "You're a whale, congratulations!"
      where bmi = weight / height ^ 2
            skinny = 18.5
            normal = 25.0
            fat = 30.0
  ```
- 定义的名称仅在函数内部可见，无需担心污染其它函数的命名空间


</div>
<div class="sheet-wrap"><div class="sheet-caption">说明：绑定在不同模式的函数体中不共享</div>


- 绑定在不同模式的函数体中不共享
- 如果你想一个函数的数个模式都能访问共享的命名，你必须全局定义它
-

</div>
<div class="sheet-wrap"><div class="sheet-caption">用where绑定来匹配模式</div>


- 可以用`where`绑定来模式匹配
- 重写前面函数的`where`部分
  ``` Haskell
  ...
  where bmi = weight / height ^2
        (skinny, normal, fat) = (18.5, 25.0, 30.0)
  ```

</div>
<div class="sheet-wrap"><div class="sheet-caption">示例：用where和模式匹配获取名和姓的首字母</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">在where中定义函数</div>


*略...*

</div>
<div class="sheet-wrap"><div class="sheet-caption">where绑定也可以嵌套</div>

</div>
<h2 id="C02EB15E">Let it be</h2>
<div class="sheet-wrap"><div class="sheet-caption">开始介绍Let绑定</div>


- `let`绑定和`where`绑定很类似
  - `where`：在整个函数末尾
  - `let`：
    - 在任何地方都可以绑定变量
    - 它们自己也是表达式
    - 非常局部，不能延伸到卫语句中
- `let`绑定也可以用于模式匹配

</div>
<div class="sheet-wrap"><div class="sheet-caption">示例：圆柱体表面积</div>


定义一个函数，根据圆柱体的高度和半径给出表面积
- 
  ``` Haskell
  cylinder :: (RealFloat a) => a -> a -> a
  cylinder r h = 
      let sideArea = 2 * pi * r *h
          topArea = pi * r ^ 2
      in  sideArea + 2 * topArea
  ```
- 格式就是`let <bindings> in <expression>`

</div>
<div class="sheet-wrap"><div class="sheet-caption">let是表达式，而where不是</div>


- `let`和`where`的不同是：`let`是表达式，`where`绑定只是语法结构
  ``` Haskell
  ghci> [let square x = x * x in (square 5, square 3, square 2)]
  [(25,9,4)]
  ```

</div>
<div class="sheet-wrap"><div class="sheet-caption">分号分隔绑定</div>


- 如果我们想要在行内绑定数个变量，我们无法将其对齐，因此可以用分号分隔
  ``` Haskell
  ghci> (let a = 100; b = 200; c = 300 in a*b*c, let foo="Hey "; bar = "there!" in foo ++ bar)
  (6000000,"Hey there!")
  ```
- 你不需要在最后一个绑定之后放置分号，但是如果你想的话，也可以

</div>
<div class="sheet-wrap"><div class="sheet-caption">可以用let绑定来模式匹配</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">可以把let放进列表推导中</div>


将let绑定放入列表推广式中
- 重写之前的计算一列体重-身高对的示例，无需用`where`定义帮助函数
  ``` Haskell
  calcBmis :: (RealFloat a) => [(a,a)] -> [a]
  calcBmis xs = [bmi | (w,h) <- xs, let bmi = w / h ^ 2]
  ```
- 包含了`let`，很类似于包含谓词，只是它并不过滤列表，它只绑定名称
- ( **应该比较重要** )绑定的名称对于以下区域可见
  - 输出函数(`|`之前的部分)
  - 所有的谓词
  - 在绑定之后的区域
- 我们忽略了绑定中`in`的部分，因为名称的可见性在那里已经预定义了
- 然而，也可以在谓词中使用`let`、`in`绑定，定义的名称只会在那个谓词中可见

</div>
<div class="sheet-wrap"><div class="sheet-caption">let绑定中省略in(非列表推导式)</div>


- `in`部分也可以在GHCi中定义函数和常量时省略
- 如果这样做了，名称将会在整个交互中都可见

</div>
<div class="sheet-wrap"><div class="sheet-caption">为什么有时候用where</div>


为什么有时仍然使用`where`？
- `let`表达式在范围内相对局限，不能跨越卫语句使用
- 有些人更喜欢`where`绑定，因为功能之后紧接着就是用到的名称，函数体更接近函数名和类型声明，对于某些人来说更加可读

</div>
<h2 id="7A1F7EDA">Case表达式</h2>
<div class="sheet-wrap"><div class="sheet-caption">Haskell中的case表达式</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">示例：函数定义的参数中的模式匹配只是case表达式的语法糖</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">case的语法结构</div>


``` Haskell
case expression of pattern -> result
                   pattern -> result
                   pattern -> result
```

</div>
<div class="sheet-wrap"><div class="sheet-caption">case表达式可以到处用得很多</div>

</div>
<h1 id="C7B40BC9">递归</h1>
<h2 id="F0FD8021">你好递归</h2>
<div class="sheet-wrap"><div class="sheet-caption">开始介绍递归</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">示例：斐波那契数列</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">递归在Haskell中很重要</div>


递归在Haskell中很重要
- 和命令式语言不同，Haskell通过声明某物是什么，而非如何得到它来进行计算
- 这就是为什么在Haskell中没有while循环或者for循环，相反，很多时候我们必须使用递归来声明某物是什么

</div>
<h2 id="C049B7D4">最棒(maximum awesome)</h2>
<div class="sheet-wrap"><div class="sheet-caption">最大值函数在命令式语言中的做法</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">最大值的递归做法</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">更聪明的做法</div>

</div>
<h2 id="042EAF63">再来几个递归函数</h2>
<div class="sheet-wrap"><div class="sheet-caption">replicate函数的实现</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">说明：Num不是Ord的一个子类</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">take函数的实现</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">reverse函数的实现</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">repeat函数的实现</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">zip函数的实现</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">elem函数的实现</div>

</div>
<h2 id="3F53D80C">快，排序！</h2>
<div class="sheet-wrap"><div class="sheet-caption">快排序在Haskell中更简单优雅</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">Haskell构造快排序的思路</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">代码实现与测试</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">讲解</div>

</div>
<h2 id="B36C606A">递归地思考</h2>
<div class="sheet-wrap"><div class="sheet-caption">边界情形和递归应用</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">单位元(identity)</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">结束语</div>

</div>
<h1 id="B09C0DB1">高阶函数</h1>
<div class="sheet-wrap"><div class="sheet-caption">高阶函数——解决问题和思考程序的有力方式</div>


- 高阶函数：获取函数作为参数，返回函数作为返回值；做到其中一点就是高阶函数\
  *和其它途径中了解的高阶函数不太一样(没有后者)*
- 很大程度上就是Haskell的实践
- 如果你想要通过定义东西是什么来定义计算，而不是定义步骤改变状态，高阶函数是不可或缺的
- 高阶函数是解决问题和思考程序的有力方式

</div>
<h2 id="0984FB78">柯里化函数</h2>
<div class="sheet-wrap"><div class="sheet-caption">目前为只所有获取多于一个参数的函数都是柯里化函数(后文略...)</div>

</div>
<h2 id="F2BF0696">某些高阶主义/更高秩序主义(higher-orderism)是合理的</h2>
<h2 id="D9F15B6D">Map和filter</h2>
<h2 id="D8846B0B">Lambda</h2>
<h2 id="C99A2B2A">只有fold和马</h2>
<div class="sheet-wrap"><div class="sheet-caption">fold函数的参数结构</div>


1. 一个双参函数(binary function)
   - 获取积累器(accumulator)和第一个（或者最后一个）元素
   - 产生一个新的积累器
2. 一个开始值(应该叫`accumulator` ( *ps:我也觉得* ))
3. 一个用来折叠的列表

</div>
<div class="sheet-wrap"><div class="sheet-caption">数个fold函数：处理list的通用方式</div>


- 回忆处理递归的时候，注意到许多递归函数操作列表
- 通常我们对于空列表有边界情形，我们引入了`x:xs`模式，我们进行一些行为，涉及到单个元素以及剩下的列表
- 这看起来是个有用的模式，所以有数个非常有用的函数来封装它
- 这些函数被称作`fold`，它们优点像`map`函数，只是它们把列表降为一些单个值

</div>
<div class="sheet-wrap"><div class="sheet-caption">看看foldl，也称为左折叠</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">示例：sum函数的实现</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">foldr，右折叠</div>


- 右折叠和左折叠工作方式相似，只是积累器从右侧吃掉值
- 左折叠双参函数：积累器是第一个参数，当前值在右侧(`\acc x ->...`)
- 右折叠双参函数：当前值是第一个参数，积累器是第二个(`\x acc ->...`)
- 这很合理，因为右折叠从右侧开始折叠，所以积累器放在右边

</div>
<div class="sheet-wrap"><div class="sheet-caption">积累器可以是任何类型</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">一个大的差异：无穷列表</div>


一个大的差异
- 右折叠可用于无穷列表，然而左折叠不能
- 如果你从无穷列表的某个点开始从右侧折叠，你会最终到达列表的开始
- 如果你从无穷列表的某个点开始从左侧折叠，你将永远不会到达尽头

*我觉得这是错的！两个函数都不能处理无穷列表，作者在胡扯*

</div>
<div class="sheet-wrap"><div class="sheet-caption">折叠可以用来实现任何一次遍历列表的函数</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">foldl1和foldr1：不需要显式初始值</div>


- `foldl1`和`foldr1`类似于`foldl`和`foldr`
- 只是不需要显式提供初始值
- 它们假定列表第一个（最后一个）元素是初始值
- 它们折叠的列表必须含有至少一个元素，如果给空列表，将会产生运行时错误

</div>
<div class="sheet-wrap"><div class="sheet-caption">scanl和scanr很类似于foldl和foldr</div>


- `scanl`和`scanr`类似于`foldl`和`foldr`，值是它们以列表的形式报告所有中间积累器状态
- 还有`scanl1`和`scanr1`，类似于`foldl1`和`foldr1`
- 
  ``` Haskell
  ghci> scanl (+) 0 [3,5,2,1]
  [0,3,8,10,11]
  ghci> scanl (+) 0 [3,5,2,1]
  [11,8,3,1,0]
  ghci> scanl1 (\acc x -> if x > acc then x else acc) [3,4,5,3,7,9,2,1]
  [3,4,5,5,7,9,9,9]
  ghci> scanl (flip (:)) [] [3,2,1]
  [[],[3],[2,3],[1,2,3]]
  ```

</div>
<div class="sheet-wrap"><div class="sheet-caption">示例：多少个自然数的平方根之和能超过1000</div>


``` Haskell
sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scan1 (+ (map sqrt[1..])))) + 1
ghci> sqrtSums
121
ghci> sum (map sqrt [1...131])
1005.0942035344083
ghci> sum (map sqrt [1...130])
993.6486803921487
```

说明
- 使用`takeWhile`，而不是`filter`，因为`filter`不能用于无穷列表
- 使用`takeWhile`，在求和开始大于1000时就截断扫描列表

</div>
<h2 id="508D3D8D">函数应用与$</h2>
<div class="sheet-wrap"><div class="sheet-caption">$的定义</div>


`$`的定义

``` Haskell
($) :: (a -> b) -> a -> b
f $ x = f x
```

</div>
<div class="sheet-wrap"><div class="sheet-caption">$的用处？</div>


- 尽管普通函数应用（空格）具有很高的优先级
- `$`函数具有最低的优先级
- 空格函数应用是左结合性的(left-associative)
- `$`函数应用是右结合性的

</div>
<div class="sheet-wrap"><div class="sheet-caption">为什么$能帮助我们</div>


- 这是一个方便的函数，我们无需再写这么多括号了
- 考虑表达式`sum (map sqrt [1..130])`
  - 因为`$`的优先级如此低，我们可以把表达式重写为`sum $ map sqrt [1..130]`
  - 当遇到`$`，右侧的表达式作为参数被应用于它左侧的函数
- 当我们想写`sqrt (3 + 4 + 9)`
  - 我们可以写成`sqrt $ 3 + 4 + 9`
  - `$`可以看作在此位置写一个`(`，然后在最右侧写一个`)`

</div>
<div class="sheet-wrap"><div class="sheet-caption">函数应用也可以被应用</div>


- `$`代表函数应用可以被视为另一个函数
- 我们可以把函数应用`map`到一列函数上
  ``` Haskell
  ghci> map ($ 3) [(4+), (10*), (^2), sqrt]
  [7.0,30.0,9.0,1.7320508075688772]
  ```
  *为什么这里的`$`可以把`3`放在旁边*
  
  *可能因为`$`是右结合性的，实际上获取到的第一个参数对应右边*

</div>
<h2 id="0F76AED0">函数组合</h2>
<div class="sheet-wrap"><div class="sheet-caption">通过.函数进行函数组合</div>


通过`.`函数进行函数组合
``` Haskell
(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = \x -> f (g x)
```

</div>
<div class="sheet-wrap"><div class="sheet-caption">.函数的作用</div>


- 函数组合的其中一个作用是使函数像飞一般传递到另一些函数

</div>
<div class="sheet-wrap"><div class="sheet-caption">示例：将列表中的数字转为负数</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">右结合性</div>


- `.`是右结合性的
- 所以意味着可以将许多函数一次性组合在以其
- `f(g(z x))`等价于`(f . g . z) x`
- 可以将
  ``` Haskell
  ghci> map (\xs -> negate (sum (tail xs))) [[1..5],[3..6],[1..7]]
  [-14,-15,-27]
  ```
  转换为
  ``` Haskell
  ghci> map (negate . sum . tail) [[1..5],[3..6],[1..7]]
  [-14,-15,-27]
  ```

</div>
<div class="sheet-wrap"><div class="sheet-caption">那多参函数呢</div>


- 如果想要将多参函数用在函数组合中，我们通常只能部分应用它们，直到每个函数只获取一个参数
- `sum (replicate 5 (max 6.7 8.9))`可以重写为`(sum . replicate 5 . max 6.7) 8.9`
- 如果你想将一个使用很多括号的表达式用函数组合重写
  - 将件内部的函数的最后一个参数放在`$`之后
  - 然后组合其它所有的函数应用
- `replicate 100 (product (map (*3) (zipWith max [1,2,3,4,5] [4,5,6,7,8])))`可以改写为`replicate 100 . product . map (*3) .zipWith max [1,2,3,4,5] $ [4,5,6,7,8]`
- 如果表达式用三个括号结束，有可能如果你将它翻译为函数组合，它将会有三个组合算子

</div>
<div class="sheet-wrap"><div class="sheet-caption">另一种通常的用法：point free风格</div>


函数组合的另一种通常的用法是用point free风格(pointless风格)定义函数
- 例子
  ``` Haskell
  sum' :: (Num a) => [a] -> a
  sum' xs = foldl (+) 0 xs
  ```
  可以改写为`sum' = foldl (+) 0`
- 函数`fn x = ceiling (negate (tan (cos (max 50 x))))`可以改写为`fn = ceiling . negate . tan . cos . max 51`

</div>
<div class="sheet-wrap"><div class="sheet-caption">可读性问题</div>


- 多数时候，point free风格更加可读、清晰，因为它让你思考函数以及什么样的函数组合成结果，而不是思考数据以及它如何回转
- 你可以使用简单的函数并运用组合来组成更加复杂的函数
- 然而，很多时候，如果一个函数过于复杂，用point free风格书写可能会减少可读性
- 这就是为什么不鼓励构造长链函数组合
- 首选的风格是使用`let`绑定为中间结果或者将问题拆分为子问题然后组合在一起以便阅读

</div>
<div class="sheet-wrap"><div class="sheet-caption">示例：重写寻找所有奇数平方小于10000的总和</div>


- 用一个函数编写
  ``` Haskell
  oddSquareSum :: Integer
  oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))
  ```
- 函数组合写法
  ``` Haskell
  oddSquareSum :: Integer
  oddSquareSum = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]
  ```
- 然后，为了可读性，宁愿写成这样
  ``` Haskell
  addSquareSum :: Integer
  oddSquareSum = 
      let oddSquares = filter odd $ map (^2) [1..]
          belowLimit = takeWhile (<10000) oddSquares
      in sum belowLimmit
  ```

</div>
<h1 id="197FCD04">模块</h1>
<h2 id="32788B02">加载模块</h2>
<div class="sheet-wrap"><div class="sheet-caption">Haskell模块是啥</div>


- Haskell模块是一组相关函数、类型、类型类
- 一个Haskell程序是一组模块，其中主模块家在其它模块并使用定义在它们之中的方程并做一些事
- 将代码分到若干模块中于一系列优势
  - 如果一个模块足够通用，它导出的函数可以在不同的程序中复用
  - 如果你自己的代码分离到互相不太依赖的独立模块中，你可以之后复用它们
  - 它让编写代码更加可管理，每一部分都有一些意图

</div>
<div class="sheet-wrap"><div class="sheet-caption">Haskell标准库模块</div>


- Haskell标准库分成了模块，每个模块包含函数和类型，以某种方式关联，并发挥一些常见的目的
- 不同的模块
  - 操作李诶报
  - 并发式编程
  - 处理复数
- 我们目前为止所有处理的函数、类型都是Prelude模块的一部分，是默认加载的
- 本章我们将会检验数个有用的模块和函数


</div>
<div class="sheet-wrap"><div class="sheet-caption">Haskell脚本中导入模块的语法</div>


- Haskell脚本中导入模块的语法`import <module name>`
- 必须在定义函数之前完成，所以导入通常在文件顶部完成
- 一个脚本可以，例如，导入数个模块
- 只需要把导入语句写到单独一行即可

</div>
<div class="sheet-wrap"><div class="sheet-caption">示例：导入Data.Lisp模块</div>


- 创建一个函数告诉我们一个列表有多少个不同的元素
  ``` Haskell
  import Data.List

  numUniques :: (Eq a) => [a] -> Int
  numUniques = length . nub
  ```
- 当你导入`Data.List`，`Data.List`所有导出的函数都在全局命名空间可用
- 你可以在脚本的任何地方调用它
- `nub`是一个定义在`Data.List`模块的函数，获取一个列表，去除重复元素
- 组合

</div>
<div class="sheet-wrap"><div class="sheet-caption">在GHCI中导入模块</div>


- 在`GHCI`中将模块的函数导入全局命名空间
  ``` Haskell
  ghci> :m + Data.List
  ```
- 在`GHCI`中导入多个模块
  ``` Haskell
  ghci> :m + Data.List Data.Map Data.Set
  ```
- 如果你加载了一个脚本，其已经导入了一个模块，你不需要使用`:m + `来获取它

</div>
<div class="sheet-wrap"><div class="sheet-caption">部分导入模块</div>


- 如果你只需要模块中的数个函数，你可以选择性导入这些函数
  ``` Haskell
  import Data.List (nub, sort)
  ```
- 你也可以选择导入模块中的所有函数，除了数个选择的函数，通常在数个模块导出同名函数，你希望去除冲突项时有用
  ``` Haskell
  import Data.List hiding (nub)
  ```
- 另一种处理命名冲突的方式是进行有限导入(qualified imports)
  - 
    ``` Haskell
    import qualified Data.Map
    ```
  - 如果希望使用`Data.Map`的过滤函数，我们应该`Data.Map.filter`，因此`filter`仍然指向普通的过滤函数
  - 使用更短的名称来有限导入
    ``` Haskell
    import qualified Data.Map as M
    ```
  - 只需使用`M.filter`

</div>
<div class="sheet-wrap"><div class="sheet-caption">了解标准库中的模块</div>


- 使用该书参考
- 点击标准库参考并探索模块和函数
- 也可以每个模块的Haskell源码
- 使用`Hoogle`，可以根据名称、模块名甚至了性签名检索

</div>
<h2 id="919CB162">Data.List</h2>
<div class="sheet-wrap"><div class="sheet-caption">intersperse函数</div>


- 获取一个元素和一个列表，然后将该元素放到列表中每一对元素中间
- 示例
  ``` Haskell
  ghci> intersperse '.' "MONKEY"
  ghci> intersperse 0 [1,2,3,4,5,6]
  [1,0,2,0,3,0,4,0,5,0,6]
  ```
- 

</div>
<div class="sheet-wrap"><div class="sheet-caption">intercalate函数</div>


- 获取一列列表和一个列表，将该列表茶道所有列表之间然后扁平化结果
- 示例
  ``` Haskell
  ghci> intercalate " " ["hey","there","guys"]
  "hey there guys"
  ghci> intercalate [0,0,0] [[1,2,3],[4,5,6],[7,8,9]]
  [1,2,3,0,0,0,4,5,6,0,0,0,7,8,9]
  ```

</div>
<div class="sheet-wrap"><div class="sheet-caption">transpose函数</div>


- 转置（transpose）一列列表
- 如果你将一列列表看作2D矩阵，列变成行，反之亦然
- 示例
  ``` Haskell
  ghci> transpose [[1,2,3],[4,5,6],[7,8,9]]
  [[1,4,7],[2,5,8],[3,6,9]]
  ghci> transpose ["hey","there","guys"]
  ["htg","ehu","yey","rs","e"]
  ```
  *感觉有些费解，不过可以理解为逐次从每个列表头部取元素并组合成对应位置的新的列表*
- 应用：多项式相加
  - 有多项式$3x^2+5x+9$, $10x^3+9$, $8x^3+5x^2+x-1$
  - 希望将其相加
  - 使用列表`[0,3,5,9]`,`[10,0,0,9]`,`[8,5,1,-1]`
  - 代码
    ``` Haskell
    ghci> map sum $ transpose [[0,3,5,9],[10,0,0,9],[8,5,1,-1]]
    [18,8,6,17]
    ```

</div>
<div class="sheet-wrap"><div class="sheet-caption">fold'和fold1'函数</div>




</div>
<div class="sheet-wrap"><div class="sheet-caption">concat函数</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">concatMap函数</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">and, or函数</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">any, all函数</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">iterate函数</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">splitAt函数</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">takeWhile, dropWhile函数</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">span函数</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">sort, group函数</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">inits, tails函数</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">isInfixOf, isPrefixOf, isSuffixOf函数</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">elem, notElem函数</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">partition函数</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">find函数</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">elemIndex, elemIndices函数</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">findIndex函数</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">zip3,zip4,...,zipWith3, zipWith4,...函数</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">lines, unlines函数</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">words, unwords函数</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">nub函数</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">delete函数</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">\,union,intersect函数</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">insert函数</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">为什么存在genericLength,genericTake,enericDrop,genericSplitAt,genericIndx,genericReplicate</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">更加通用的nubBy,deleteBy,unionBy,intersectBy,groupBy</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">更加通用的sortBy,insertBy,maximumBy,minimumBy</div>

</div>
<h2 id="98AC801F">Data.Char</h2>
<h2 id="962E96E6">Data.Map</h2>
<h2 id="CB10689F">Data.Set</h2>
<h2 id="EB01D03E">制造我们自己的模块</h2>
<div class="sheet-wrap"><div class="sheet-caption">示例介绍：计算几何对象的体积和面积</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">命名模块、指定导出函数</div>


1. 如果文件名"Geometry.hs"，那么模块名应该为"Geometry"
2. 指定导出的函数名
3. 代码
   ``` Haskell
   module Geometry
   ( sphereVolume
   , sphereArea
   , cubeVolume
   , cubeArea
   , cuboidArea
   , cuboidVolume) where
   ```
4. 定义函数
   ``` Haskell
   sphereVolume :: Float -> Float
   sphereVolume radius = (4.0 / 3.0) * pi * (radius ^ 3)

   sphereArea :: Float -> Float
   sphereArea radius = 4 * pi * (radius ^ 2)

   cubeVolume :: Float -> Float
   cubeVolume side = cuboidVolume side side side

   cubeArea :: Float -> Float
   cubeArea side = cuboidArea side side side

   cuboidVolume :: Float -> Float -> Float -> Float
   cuboidVolume a b c = rectangleArea a b * c
   
   cuboidArea :: Float -> Float -> Float -> Float
   cuboidArea a b c = rectangleArea a b * 2 + rectangleArea a c * 2 + rectangleArea c b * 2

   rectangleArea :: Float -> Float -> Float
   rectangleArea a b = a * b
   ```

</div>
<div class="sheet-wrap"><div class="sheet-caption">说明：只导出作为接口的函数</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">使用模块</div>


- 使用我们的模块，只需
  ``` Haskell
  import Geometry
  ```
- "Geometry.hs"必须和导出它的程序在同一个文件夹下

</div>
<div class="sheet-wrap"><div class="sheet-caption">模块可以有层级化结构</div>


- 每个模块可以拥有数个子模块，它们也可以拥有它们自己的子模块
- 我们分组这些函数，因此"Geometry"拥有子模块的模块，每一种都是一类对象
- 首先，创建一个名称为"Geometry"的文件夹，注意首字母大写
- 在文件夹中放置三个文件"Sphere.hs"、"Cubiod.hs"、"Cube.hs"
  - "Sphere.hs"
    ``` Haskell
    module Geometry.Sphere
    ( volume
    , area
    ) where

    volume :: Float -> Float
    volume radius = (4.0 / 3.0) * pi * (radius ^ 3)
    ```
  - "Cuboid.hs"
    ``` Haskell
    module Geometry.Cuboid
    ( volume
    , area
    ) where

    volume :: Float -> Float -> Float -> Float
    volume a b c = ractangleAera a b * c

    area :: Float -> Float -> Float -> Float
    area a b c = rectangleArea a b * 2 + rectangleArea a c * 2 + rectangleArea c b * 2

    rectangleArea :: Float -> Float -> Float
    rectangleArea a b = a * b
    ```
  - "Cube.hs"
    ``` Haskell
    module Geometry.Cube
    ( volume
    , area
    ) where

    import qulified Geometry.Cuboid as Cubeoid

    volume :: Float -> Float
    volume side = Cuboid.volume side side side

    area :: Float -> Float
    area side = Cuboid.area side side side
    ```

</div>
<div class="sheet-wrap"><div class="sheet-caption">说明：不同模块中相同的函数名</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">导入模块</div>

</div>
<h1 id="ADC1965B">我们自己的类型以及类型类</h1>
<h2 id="B4BF6FA7">代数数据类型介绍</h2>
<div class="sheet-wrap"><div class="sheet-caption">使用data关键字定义一种类型</div>


使用`data`关键字定义一种类型
- 布尔类型在标准库的定义
  ``` Haskell
  data Bool = False | True
  ```
- `data`表示定义一种新的数据类型
- `=`前面的部分记为类型，它是`Bool`
- `=`后面的部分是值构造器，他们指定该类型可以拥有的不同值
- `|`读作“或”
- 所以读作：`Bool`类型可以拥有`True`或者`False`值

类似的方式定义`Int`类型
- 代码
  ``` Haskell
  data Int = -2147483648 | - 2147483647 | ... | -1 | 0 | 1 | 2 | ... | 2147483647
  ```
- 第一个和最后一个值构造器是`Int`可能值的最大最小值
- 实际上并不是这样定义的，只是为了说明才这样写

</div>
<div class="sheet-wrap"><div class="sheet-caption">表示形状：值构造器字段</div>


- 圆可以定义为`(43.1, 55.0, 10,4)`，第一个和第二个字段代表圆心坐标，第三个字段代表半径
- 但是为了区分数据的含义，更好的解决方案是创建自己的类型，用来表示形状
- 形状可以是圆形、长方形
  ``` Haskell
  data Shape = Circle Float Float Float | Rectangle Float Float Float Float
  ```
  - 当我们编写一个值构造器，我们可选择添加一些类型，这些类型定义它将会包含的值
  - `Circle`值构造器有三个字段，前两个字段代表圆心坐标，第三个代表半径
  - `Rectangle`值构造器有四个字段，前两个是左上角坐标，后两个是右下角坐标
- 字段实际上是参数，值构造器实际上是函数，最终返回一种数据类型的值
- 观察两种值构造器的类型签名
  ``` Haskell
  ghci> :t Circle
  Circle :: Float -> Float -> Float -> Shape
  ghci> :t Rectangle
  Rectangle :: Float -> Float -> Float -> Float -> Shape
  ```
- 所以值构造器也是函数，和其它任何东西一样

</div>
<div class="sheet-wrap"><div class="sheet-caption">示例：获取形状返回面积</div>


创造一个函数，获取形状并返回它的面积
- 代码
  ``` Haskell
  surface :: Shape -> Float
  surface (Circle _ _ r) = pi * r ^ 2
  surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)
  ```
- 第一个值得注意的事情是类型声明
  - 不能编写`Circle->Float`，因为`Circle`不是一个类型，`Shape`才是
  - 正如不能够编写一个函数类型声明`True->Int`
- 下一个值得注意的事情是可以针对构造器模式匹配
  - 我们先模式匹配构造器，然后才匹配不含有任何字段的值
  - 我们写构造器然后将它的字段绑定到名称上
- 代码
  ``` Haskell
  ghci> surface $ Circle 10 20 10
  314.15927
  ghci> surface $ Rectangle 0 0 100 100
  10000.0
  ```

</div>
<div class="sheet-wrap"><div class="sheet-caption">打印类型的值</div>


- 如果直接尝试打印`Circle 10 20 5`，将会报错
- 尝试打印值的时候，Haskell首先运行`show`函数来获取我们值的字符串表示，然后打印到终端
- 为了让`Shape`类型是`Show`类型类的一部分，修改为
  ``` Haskell
  data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)
  ```
- 关于`deriving`目前不考虑太多，应该说如果添加`deriving (Show)`到`data`声明的末尾，Haskell自动让该类型作为Show类型类的一部分
- 因此可以
  ``` Haskell
  ghci> Circle 10 20 5
  Circle 10.0 20.0 5.0
  ghci> Rectangle 50 230 60 90
  Rectangle 50.0 230.0 60.0 90.0
  ```

</div>
<div class="sheet-wrap"><div class="sheet-caption">值构造器作为函数可以map、部分应用等</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">改良：增加Point数据类型</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">示例：平移（nudge）形状的函数</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">导出模块内的数据类型</div>


- 如果我们想要导出定义在模块中的函数和类型
  ``` Haskell
  module Shapes
  ( Point(..)
  , Shape(..)
  , surface
  , nudge
  , baseCircle
  , baseRect
  ) where
  ```
- 通过`Shape(..)`，我们导出Shape的所有值构造器，表示任何导入我们模块的人都可以通过`Rectangle`和`Circle`值构造器创建形状
- 也可以不导出`Shape`的任何值构造器，这样的话，导入我们模块的人只能通过使用帮助函数`baseCircle`和`baseRectangle`来创建形状
- `Data.Map`模块也使用了这种方式，你不能通过`Map.Map[(1,2),(3,4)]`来创建一个映射，因为它没有导出那个值构造器

</div>
<div class="sheet-wrap"><div class="sheet-caption">说明：隐藏实现与抽象</div>


- 不导出数据类型的值构造器会让数据类型更加抽象，我们可以借此隐藏实现
- 另外，使用我们模块的任何人不能根据值构造器模式匹配

</div>
<h2 id="4D4A2DBE">记录语法</h2>
<div class="sheet-wrap"><div class="sheet-caption">示例：存储个人信息</div>


- 存储个人的信息：名，姓，年龄，身高，手机号，最爱的冰淇淋口味
- 代码（代数结构）`data Person = Person String String Int Float String String deriving (Show)`
  ``` Haskell
  ghci> let guy = Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"
  ghci> guy
  Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"
  ```
- 有点难以阅读，比如说我们想要创造一个函数，从一个人获得单独的信息；将会定义非常多的函数
- 更好的办法是通过"记录"语法来实现
  ``` Haskell
  data Person = Person { firstName :: String
                       , lastName :: String
                       , age :: Int
                       , height :: Float
                       , flavor :: String
                       } deriving (Show)
  ```
- 因此可以使用弯括号，而不是一个个定义字段类型并且用空格分开

</div>
<div class="sheet-wrap"><div class="sheet-caption">说明：使用记录的好处</div>


- 主要的好处是它会自动创造函数来查询数据类型字段
  ``` Haskell
  ghci> :t flavor
  flavor :: Person -> String
  ghci> :t firstName
  firstName :: Person -> String
  ```
- 使用记录语法还有另一个好处，当给类型派生"Show"时，会用不同的方式展示
  ``` Haskell
  data Car = Car {company :: String, model :: String, year :: Int} deriving (Show)
  ghci> Car {company="Ford", model="Mustang", year=1967}
  Car {company = "Ford", model = "Mustang", year = 1967}
  ```
  不需要专门用合适的位置放置字段
  - 使用记录语法的情形：构造器具有数个字段，并且不明显哪个字段是哪个的时候
  - 如果我们创造3D向量数据类型`Vector = Vector Int Int Int`，已经很清晰字段是向量的三个分量
  - 然而，如果是"Person"或者"Car"类型，使用"记录"语法有很有好处

</div>
<h2 id="2BD2E412">类型参数</h2>
<div class="sheet-wrap"><div class="sheet-caption">值构造器与类型构造器</div>


- 类比值构造器能够获取数值参数并且产生新的值
- 类型构造器可以获取类型作为参数并且产生新的类型
- 如果你熟悉C++的模板，你会看到类比

</div>
<div class="sheet-wrap"><div class="sheet-caption">示例：Maybe类型构造器</div>


用一个我们已经见过的类型如何实现来举例
- `data Maybe a = Notion | Just a`
- 这里有类型参数，因为涉及到类型参数，我们称`Maybe`为类型构造器
- 取决于我们想要这个数据类型当不是"Nothing"时捕获什么，这个类型构造器可以产生一个类型`Maybe Int`, `Maybe Car`, `Maybe String`等等
- 如果传入"Char"作为Maybe的类型参数，我们获得类型`Maybe Car`
- 例如，值`Just 'a'`具有类型`Maybe Char`

</div>
<div class="sheet-wrap"><div class="sheet-caption">另一个具有类型参数的类型：列表</div>


- 另一个具有参数的数据类型，列表：
- 你可能不知道，在使用Maybe之前，我们使用了具有类型参数的数据类型，即列表
- 列表的值可以有`[Int]`类型、`[Char]`类型、`[[String]]`类型，但是不能只有`[]`类型的值

</div>
<div class="sheet-wrap"><div class="sheet-caption">Maybe的玩耍</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">说明：Nothing的类型是Maybe a，多态类型</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">示例：Car不适合使用类型参数</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">示例：Data.Map模块中的Map k v</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">说明：一般不往数据声明中加类型类约束</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">示例：3D向量类型以及操作符</div>


实现一个3D向量类型并且添加一些操作符
1. 代码
   ``` Haskell
   data Vector a = Vector a a a deriving (Show)

   vplus :: (Num t) => Vector t -> Vector t -> Vector t
   (Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

   vectMult :: (Num t) => Vector t -> t -> Vector
   (Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)

   scalarMult :: (Num t) => Vector t -> Vector t -> t
   (Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n
   ```
   - `vplus`向量相加
   - `vectMult`向量数乘
   - `scalarMult`向量数量积
2. 这样就能够支持不同的数值类型，同时只有相同类型的向量才能参与运算
3. 注意并没有放`Num`类型约束，因为反正都会在函数里面重复它

</div>
<div class="sheet-wrap"><div class="sheet-caption">区分类型构造器和值构造器</div>


- 声明数据类型时，"="左侧的部分是类型构造器，后面的部分（有可能由"|"分隔）是值构造器
- 函数声明`Vector t t t -> Vector t t t -> t`可能是错的，因为
  - 应该把类型放到类型声明中
  - vector类型构造器只获取一个参数
  - 而值构造器获取三个
- 玩一玩vector
  ``` Haskell
  ghci> Vector 3 5 8 `vplus` Vector 9 2 8
  Vector 12 7 16
  ghci> Vector 3 5 8 `vplus` Vector 9 2 8 `vplus` Vector 0 2 3
  Vector 12 9 19
  ghci> Vector 3 9 7 `vectMult` 10
  Vector 30 90 70
  ghci> Vector 4 9 5 `scalarMult` Vector 9.0 2.0 4.0
  74.0
  ghci> Vector 2 9 3 `vectMult` (Vector 4 9 5 `scalarMult` Vector 9 2 4)
  Vector 148 666 222
  ```

</div>
<h2 id="2C826D8B">派生实例</h2>
<div class="sheet-wrap"><div class="sheet-caption">类型类与接口函数</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">类型类更像是接口</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">示例：Person类派生行为</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">示例：令Bool类型派生自Ord</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">示例：使用代数数据类型构造枚举量</div>

</div>
<h2 id="A7CD6634">类型同义词</h2>
<div class="sheet-wrap"><div class="sheet-caption">类型同义词：取别名</div>


- `String`和`[Char]`类型是等同且可互换的
- 这是用类型同义词实现的
- 代码
  ``` Haskell
  type String = [Char]
  ```
- 有点误导的地方在于，我们没有制造任何新的类型（需要用`data`关键字），而是在给已有类型创造同义词

</div>
<div class="sheet-wrap"><div class="sheet-caption">示例：Data.Map模块表示号码本</div>


- 使用`Data.Map`模块，首先用关系表表达电话本，然后转换为映射
  ``` Haskell
  phoneBook :: [(String, String)]
  phoneBook = 
      [("betty","555-2938")
      ,("bonnie","452-2928")
      ,("patsy","493-2928")
      ,("lucille","205-2928")
      ,("wendy","939-8282")
      ,("penny","853-2492")]
  ```
- 创造类型同义词来传达更多信息
  ``` Haskell
  type PhoneBook = [(String, String)]
  ```
  现在我们电话本的类型声明可以写成`phoneBook :: PhoneBook`
- 现在给"String"也造个类型同义词
  ``` Haskell
  type PhoneNumber = String
  type Name = String
  type PhoneBook = [(Name,PhoneNumber)]
  ```
- 现在，当我们实现函数获取名称和号码，查看名称和号码组合是否在电话本中，我们可以给出漂亮且描述性的类型声明
  ``` Haskell
  inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
  inPhoneBook name pnumber pbook = (name,pname) `elem` pbook
  ```

</div>
<div class="sheet-wrap"><div class="sheet-caption">说明：引入类型同义词的原因</div>


- 引入类型同义词的原因
  1. 为了已有的类型更好的描述
  2. 长类型多次重复，但是在函数的上下文中具有更特别的含义

</div>
<div class="sheet-wrap"><div class="sheet-caption">类型同义词也可以参数化</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">说明：具体类型、未充分应用类型、类型构造器</div>


1. 具体类型（充分应用类型）：例如`Map Int String`
2. 未充分应用类型：例如处理多态函数，`[a]`或者`(Ord a) => Maybe a`，等等
3. 类型构造器：`Maybe`实际上是类型构造器，如果将额外一个类型应用到`Maybe`，例如`Maybe Int`，就能得到一个具体类型

</div>
<div class="sheet-wrap"><div class="sheet-caption">部分应用类型参数：获得新的类型构造器</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">注意：从Data.Map库中部分导入</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">说明：区分类型构造器和值构造器</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">Either a b类型的介绍</div>


另一种酷的数据类型是`Either a b`类型
- 大概定义为
  ``` Haskell
  data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)
  ```
- 具有两个值构造器，如果使用`Left`，内容就是`a`类型，如果右边被使用，内容就是`b`类型
- 我们可以用这个类型来封装一个或另一个类型的值，然后当我们获取`Either a b`类型的值时，我们通常对`Left`和`Right`都模式匹配，我们根据其中一种讨论
- 代码示例
  ``` Haskell
  ghci> Right 20
  Right 20
  ghci> Left "w00t"
  Left "w00t"
  ghci> :t Right 'a'
  Right 'a' :: Either a Char
  ghci> :t Left True
  Left True :: Either Bool b
  ```

</div>
<div class="sheet-wrap"><div class="sheet-caption">Maybe和Either的使用区别</div>


- 目前为止，我们看到`Maybe`最常用来表示可能失败或成功的计算结果
- 但是有时候，`Maybe`不够好，因为`Nothing`并没有真正表达很多信息，只有某事情失败了
- 使用`Maybe`的情形
  - 函数只能有一种方式失败
  - 我们不关心某些函数如何或者为何失败
- 如果我们关心某些函数如何、为何失败，我们通常使用`Either a b`的结果类型
  - `a`是某种告诉我们关于可能的失败相关信息的类型
  - `b`是成功计算的类型
  - 错误使用`Left`值构造器，结果使用`Right`值构造器

</div>
<div class="sheet-wrap"><div class="sheet-caption">示例：高中储物柜</div>


- 介绍
  - 高中提供储物柜给学生使用
  - 当学生想要新的储物柜，他们会告诉管理员他们想要哪个储物柜号码，管理员告诉他们密码
  - 如果储物柜已经被占用，管理员不能告诉密码，他们需要挑选另一个
- 实现途径：来自`Data.Map`的`map`来代表储物柜，用来将储物柜号码映射到储物柜是否使用以及储物柜密码的一个对（pair）
- 代码
  ``` Haskell
  import qualified Data.Map as Map
  data LockerState = Taken | Free deriving (Show, Eq)
  type Code = String
  type LockerMap = Map.Map Int (LockerState, Code)
  ```
- 接下来创造一个函数在储物柜映射中查询密码，我们将会使用`Either String Code`类型来代表我们的结果，因为查询结果会以两种方式失败
  1. 储物柜已被占用
  2. 储物柜并不存在
  如果查询失败，我们将会用`String`来说明发生了什么
- 代码
  ``` Haskell
  lockerLookup :: Int -> LockerMap -> Either String Code
  lockerLookup lockerNumber map =
      case Map.lookup lockerNumber map of
          Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
          Just (state, code) -> if state /= Taken
                                  then Right code
                                  else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"
  ```
- 解释：
  - 我们在映射中进行了正常的查询
  - 如果获得`Nothing`，返回`Left String`类型之，声称储物柜不存在
  - 如果我们确实找到，我们进行一个额外的检查，看储物柜是否占用
    - 是：返回`Left`声称已经占用
    - 否：返回`Right Code`类型值，其中我们给学生储物柜正确的密码
- 示例映射
  ``` Haskell
  lockers :: LockerMap
  lockers = Map.fromList
      [(100,(Taken,"ZD39I"))
      ,(101,(Free,"JAH3I"))
      ,(103,(Free,"IQSA9"))
      ,(105,(Free,"QOTSA"))
      ,(109,(Taken,"893JJ"))
      ,(110,(Taken,"99292"))
      ]
  ```
- 现在我们查询一些储物柜密码
  ``` Haskell
  ghci> lockerLookup 101 lockers
  Right "JAH3I"
  ghci> lockerLookup 100 lockers
  Left "Locker 100 is already taken!"
  ghci> lockerLookup 102 lockers
  Left "Locker number 102 doesn't exist!"
  ghci> lockerLookup 110 lockers
  Left "Locker 110 is already taken!"
  ghci> lockerLookup 105 lockers
  Right "QOTSA"
  ```
- 如果我们用`Maybe`来表达结果，我们将会不知道为什么我们不能得到密码，但是现在，我们有关于失败的信息

</div>
<h2 id="6468FD6E">递归数据结构</h2>
<div class="sheet-wrap"><div class="sheet-caption">递归数据类型的概念</div>


- 我们看到代数数据类型的一个构造器可能具有多个（或者没有）字段，每个字段必须有一些具体类型
- 我们可以创造一些类型，其构造器具有的字段是相同类型！
- 这样，我们可以创造递归数据类型，其中某些类型的一个值包含该类型的数个值，后者又包含同样类型的更多值，一直下去

</div>
<div class="sheet-wrap"><div class="sheet-caption">示例：列表是一种递归数据结构</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">示例：使用代数数据类型实现自己的列表</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">说明：模式匹配实际上是在匹配构造器</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">示例：实现一个二叉搜索树</div>


什么是二叉搜索树
1. 一个元素指向两个元素，一个在左一个在右
2. 左边的元素更小，右边的元素更大
3. 每个元素也可以指向两个元素（或者一个，或者没有）
4. 结果上，每个元素具有最多两个子树
5. 我们知道的一件酷的事情是所有左边子树的元素小于根节点的元素，右子树的元素会更大
6. 如果我们需要寻找8是否在树中，我们会从根节点（比如5）开始，向右寻找……
7. 现在如果这是一个普通的列表（或者树，但是非常不平衡），将会需要更多轮查找

关于树
- 来自`Data.Set`和`Data.Map`的集合`set`和映射`map`是用树实现的
- 它们实际上使用平衡二叉搜索树，其总是平衡，而不是普通的二叉搜索树
- 现在，我们只会实现普通的二叉搜索树

思路
1. 树要么是空树，要么是一个包含某些值的元素以及两个树，听起来完美符合代数数据类型
   ``` Haskell
   data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)
   ```
2. 我们将会创造函数来获取树和元素并插入该元素，而不是手动创建树
3. 我们通过将我们希望插入的值和根节点比较来实现
   1. 如果插入值更小，向左
   2. 如果插入值更大，向右
   3. 我们对于每个后续的节点这样做，直到我们到达空树
   4. 一旦我们到达空树，我们只需向该节点插入值，而不是空树
4. 在C语言中，我们会通过修改树内的指针和值
5. 在Haskell中，我们不能真正修改我们的树，我们只能每次在选择向左或者向右时，创建新的子树；最终，插入函数返回完全新的树
6. 因为Haskell并不真正具有指针的概念，只有值
7. 所以，我们插入函数的类型会是某种类似于`a -> Tree a -> Tree a`的东西，获取一个元素和树然后返回一个新的树，包含该元素
8. 听起来可能低效率，但是惰性会处理这个问题

函数
- 两个函数
  1. 工具函数创造一个单树（只有一个节点的树）
  2. 将一个元素插入到树中
  ``` Haskell
  singleton :: a ->  Tree a
  singleton x = Node x EmptyTree EmptyTree

  treeInsert :: (Ord a) => a -> Tree a -> Tree a
  treeInsert x EmptyTree = singleton x
  treeInsert x (Node a left right)
      | x == a = Node x left right
      | x < a  = Node a (treeInsert x left) right
      | x > a  = NOde a left (treeInsert x right)
  ```
- 单体（singleton）函数只是一个创造一个具有元素以及两个空子树的快捷方式
- 插入函数中
  - 我们首先具有边界条件作为模式
  - 如果我们已经到达了空子树，表示我们到达了我们想到的地方，我们放置一个具有我们的元素的单树
  - 如果我们没有插入到空树，需要检查一些东西
    1. 如果插入的元素等于根元素，返回一个相同的树
    2. 如果更小，那么返回一个具有相同根值的树，相同的右子树，以及防止一个插入我们值的左子树
    3. 如果更大，也一样
- 接下来创建一个函数检查某些元素是否在树中
  1. 定义边界条件，如果在查找一棵空树，肯定不在（注意这和从列表中搜索元素相同）
  2. 如果元素在根节点，找到
  3. 如果寻找的元素小于根节点，搜索左子树；如果更大，搜索右子树
  ``` Haskell
  treeElem :: (Ord a) => a -> Tree a -> Bool
  treeElem x EmptyTree = False
  treeElem x (Node a left right)
      | x == a = True
      | x < a  = treeElem x left
      | x > a  = treeElem x right
  ```
- 从一个列表中使用`fold`来创建一棵树
  ``` Haskell
  ghci> let nums = [8,6,4,1,7,3,5]
  ghci> let numsTree = foldr treeInsert EmptyTree nums
  ghci> numsTree
  ```

</div>
<div class="sheet-wrap"><div class="sheet-caption">总结：代数数据结构真的酷</div>

</div>
<h2 id="7989C8EE">类型类102</h2>
<div class="sheet-wrap"><div class="sheet-caption">本节内容</div>


- 之前的学习
  - 学习了一些Haskell标准类型类，以及它们里面有哪些类型
  - 我们学习了自动创造我们自己的类型示例，使用标准类型类，让Haskell为我们派生示例
- 本小节
  - 学习如何创造我们自己的类型类，如何手动创造它们的类型实例

</div>
<div class="sheet-wrap"><div class="sheet-caption">类型类的简要重述</div>


- 类型类就像接口
- 一个类型类定义了一些行为（比如比较相等性，比较顺序，枚举）
- 类型被规定为这个类型类的实例就可以具有那样的行为
- 类型类的行为通过定义函数或者就是我们可以做的类型声明来实现的
- 当我们说一个类型是类型类的实例，我们的意思是我们可以对那个类型使用类型类定义的函数
- 类型类和Java或者Python等语言中的类并没有什么关系，这困惑住了很多人

</div>
<div class="sheet-wrap"><div class="sheet-caption">示例：Eq类型类</div>


- Eq类型类用于可以比较相等的类型
- 他定义了函数`==`和`/=`
- 如果我们具有一个类型，比如`Car`，然后用`==`比较两个`Car`有意义，那么`Car`作为`Eq`的一个实例就有意义
- `Eq`类型在标准prelude中定义的方式
  ``` Haskell
  class Eq a where
      (==) :: a -> a -> Bool
      (/=) :: a -> a -> Bool
      x == y = not (x /= y)
      x /= y = not (x == y)
  ```
- 当我们写`class Eq a where`，就表示我们在定义一个新的类型类叫`Eq`
- `a`将会代表我们马上用`Eq`创造的实例，这是一个类型变量
- 我们定义数个函数。实现函数体本身并不是强制性的，我们只需要指定函数的类型声明
- 我们只是用相互递归的方式实现了函数体，我们不需要做这件事，但是我们做了，我们会看到这如何有帮助
- 注：如果我们定义了比如类型`Eq a where`，然后在类型中定义一个类型声明`(==) :: a -> a -> Bool`，当我们之后检查函数的类型时，将会具有类型`(Eq a) => a -> a -> Bool`
- 一旦我们拥有了类，我们可以干什么？
- 一旦我们开始创建那个类的类型实例，我们开始获得一些好的机制
  - `data TrafficLight = Red | Yellow | Green`
  - 定义了交通灯的状态
  - 注意我们没有从它派生任何类型实例，因为我们将要手动编写一些实例，即使我们可以派生它们
  - 我们让它成为`Eq`的一个实例
    ``` Haskell
    instance Eq TrafficLight where
        Red == Red = True
        Green == Green = True
        Yellow == Yellow = True
        _ == _ = False
    ```
  - 我们通过使用`instance`关键字来做这件事，所以`class`用于定义新的类型类，`instance`是用来创造我们的类型类的类型实例
- 因为`==`已经靠`/=`定义，反之亦然，我们只需要在实例声明中覆盖掉其中一个
- 这称为类型类的最小完全定义——我们需要实现的最少函数，使我们的类型可以像它声称的那样做出行为
- 为了完成`Eq`的最小完全定义，我们必须覆盖`==`或者`/=`的其中一个
- 我们只通过模式匹配就实现了`==`，因为不同的情况很多，所以我们用兜底模式（catch-all pattern）

</div>
<div class="sheet-wrap"><div class="sheet-caption">示例：Show类型类</div>


- 让`TrafficLight`手动成为`Show`的示例
- 实现`show`函数，其获取一个值然后返回字符串
  ``` Haskell
  show Red = "Red light"
  show Yellow = "Yellow light"
  show Green = "Green light"
  ```
  我们又一次使用模式匹配来实现了我们的目标
- 尝试它能否运行\\
  *略*
- 我们可以只要派生`Eq`，然后它就会具有相同的效果；但是派生`Show`将会直接把值转换成字符串，如果我们想让灯显示出"Red light"，我们只能手动编写实例声明

</div>
<div class="sheet-wrap"><div class="sheet-caption">创造类型类的子类</div>


- 你也可以创造类型类，作为其它类型类的子类，比如`Num`的类声明（局部）
  ``` Haskel
  class (Eq a) => Num a where
      ...
  ```
- 如我们之前提到的，有很多地方可以塞上类约束
- 本质上我们是在说我们只有在一个类型是`Eq`的实例时，才能让它作为`Num`的实例
- 它其实只是一个类型声明上的类约束！

</div>
<div class="sheet-wrap"><div class="sheet-caption">为什么Maybe或者list类型不能作为类型类的实例？</div>


- 因为函数中的所有类型必须是具体的（concrete），所以`a`作为一个具体类型使用
- 这就是为什么我们不能：
  ``` Haskell
  instance Eq Maybe where
      ...
  ```
- 我们可以编写
  ``` Haskell
  instance Eq (Maybe m) where
      Just x == Just y = x == y
      Nothing == Nothing = True
      _ == _ False
  ```
- 有一个问题，我们并没有确认Maybe包含的东西可以用来比较相等！所以我们要修改成这样
  ``` Haskell
  instance (Eq m) => Eq (Maybe m) where
      Just x == Just y = x == y
      Nothing == Nothing = True
      _ == _ False
  ```
- 我们需要添加类型约束！
- 这也正是Haskell会如何派生实例

</div>
<div class="sheet-wrap"><div class="sheet-caption">关于类声明中的类约束</div>


- 大多数时候
  - 类声明中的类约束用于让一个类型类成为另一个类型类的子类
  - 实例声明中的类约束用于表达关于某些类型的内容的要求
- 例如，我们要求了`Maybe`的内容也是`Eq`类型类的一部分
- 当创建实例时，如果你看到一个类型作为具体类型在类型声明中使用（如`a`在`a -> a -> Bool`），你必须提供类型参数并且添加括号，因此最终形成具体类型
- 注：考虑你将会创建实例的类型将会在类声明中替换参数
- 哦，还有一件事
  - 如果你想要看是哪种类型类的实例，只要在GHCI中做`:iinfo YourTypeClass`
  - `:info`也可用于类型和类型构造器
  - 如果你`:info Maybe`，它将会展示所有Maybe作为实例的类型类
  - `:info`可以向你展示函数的类型声明

</div>
<h2 id="BE3DEDFC">一种是-否类型类</h2>
<div class="sheet-wrap"><div class="sheet-caption">实现类似JavaScript的Bool行为</div>


- 在JavaScript和其它弱类型语言中，你可以几乎放任何东西到if表达式中
- 尽管严格使用Bool作为布尔语义（sementics）在Haskell中工作得非常好，让我们尝试实现类似JavaScript的行为


</div>
<div class="sheet-wrap"><div class="sheet-caption">类声明</div>


从类声明开始
- 代码
  ``` Haskell
  class YesNo a where
      yesno :: a -> Bool
  ```
- YesNo类型类定义了一个函数，这个函数获取一个类型的一个值，可以被看作包含一些真值（true-ness）概念，然后告诉我们它是否为真
- 注意我们定义函数的方式，a必须为具体类型

</div>
<div class="sheet-wrap"><div class="sheet-caption">实例</div>


- 对于数字，我们假定（和JavaScript一样）任何非0数字都为真（true-ish），0为假（false-ish）
  ``` Haskell
  instance YesNo Int where
      yesno 0 = False
      yesno _ = True
  ```
- 空列表（以及字符串）是非值（no-ish value），而非空列表是是值（yes-ish value）
  ``` Haskell
  instance YesNo [a] where
      yesno [] = False
      yesno _  = True
  ```
- Bool类型本身也包含了真假值
  ``` Haskell
  instance YesNo Bool where
      yesno = id
  ```
  什么是`id`？只是一个标准库函数，获取一个参数并返回相同的东西
- Matbe作为一个示例
  ``` Haskell
  instance YesNo (Maybe a) where
      yesno (Just _) = True
      yesno Nothing  = False
  ```
  现在，任何种类的形式"Maybe 某某"都是YesNo的一部分，某某是什么并不重要
- 之前，我们定义了一个Tree类型，它代表了一个二叉搜索树，我们可以说一个空树是假（false-ish），任何非空树都是真（true-ish）
  ``` Haskell
  instance YesNo (Tree a) where
      yesno EmptyTree = False
      yesno _ True
  ```
- 交通灯能够有是和否的值吗？当然，如果是红，你停止，如果是绿，你通过。如果是黄？我通常跑过去
  ``` Haskell
  instance YesNo TrafficLight where
      yesno Red = False
      yesno _   = True
  ```
- 现在测试\\
  *略*

</div>
<div class="sheet-wrap"><div class="sheet-caption">模仿if语句的函数</div>


- 创造一个函数模仿if语句，但是它通过YesNo值运行
  ``` Haskell
  yesnoIf :: (YesNo y) => y -> a -> a -> a
  yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult
  ```
- 非常直观，它获取是否值和两个东西，如果是否值更加"是"，它返回两个东西的第一个，否则它返回第二个
- 测试\\
  *略*

</div>
<h2 id="C1FB41C2">函子(Functor)类型类</h2>
<div class="sheet-wrap"><div class="sheet-caption">列表类型是函子类型类的一部分</div>


- 我们遇到了标准库里的很多类型类
- 现在你可能在想列表，因为列表之间的映射是Haskell中非常主导的习惯
- 列表（list）类型是函子（Functor）类型类的一部分

</div>
<div class="sheet-wrap"><div class="sheet-caption">演示一个函子类型类的实现</div>


- 演示函子类型类的实现
  ``` Haskell
  class Functor f where
      fmap :: (a -> b) -> f a -> f b
  ```
- 我们看到它定义了一个函数，`fmap`，不提供任何默认实现
- `fmap`的类型很有趣，之前类型类定义中的类型变量所代表的类型是具体类型；但是现在，`f`不是一个具体类型，而是一个类型构造器，获取一个类型参数
- 我们看到`fmap`获取一个从一个类型到另一个类型的函数，以及一个函子，其应用于一个类型，最终返回应用于另一个类型的函子
- 这听起来有点迷，不要担心。只要看看几个示例就明白了

</div>
<div class="sheet-wrap"><div class="sheet-caption">示例：map的启发——[]作为函子</div>


- 看看map的类型签名：`map :: (a -> b) -> [a] -> [b]`
- 有趣，它获取一个从一个类型到另一个类型的函数，以及一种类型的列表，返回另一种类型的列表
- 我们发现了一个函子！
- 实际上，map就是一个只针对列表的fmap，这就是列表作为Functor类型类的示例的方式
  ``` Haskell
  instance Functor [] where
      fmap = map
  ```
- 注意我们没有写`instance Functor [a] where`，因为`fmap :: (a -> b) -> f a -> f b`中的`f`是获取一个类型的类型构造器，而`[a]`已经是一个具体类型了
- 尝试：`fmap`和`map`用在列表上\\
  *略*

</div>
<div class="sheet-wrap"><div class="sheet-caption">示例：Maybe作为函子</div>


- 可以像盒子一样的类型可以作为函子
  - 你可以把列表想成一个盒子，具有无数个小隔间
  - Maybe也可有类似于盒子的特性；它可以要么什么都不包含，要么包含一个项目
- Maybe作为一个函子
  ``` Haskell
  instance Functor Maybe where
      fmap f (Just x) = Just (f x)
      fmap f Nothing = Nothing
  ```
- 请注意我们没有写`instance Functor (Maybe m) where`，函子想要的是获取一个类型的类型构造器，而不是一个具体类型
- `fmap`的实现非常简单
  1. 如果是Nothing的空值，那么就返回Nothing；类比map获取空盒子，我们得到空盒子；很合理
  2. 如果不是一个空值，而是一个包含在Just中的单个值，我们将函数应用到Just的内容上
- 测试代码
  ``` Haskell
  ghci> fmap (++ " HEY GUYS IM INSIDE THE JUST") (Just "Something serious.")
  Just "Something serious. HEY GUYS IM INSIDE THE JUST"
  ghci> fmap (++ " HEY GUYS IM INSIDE THE JUST") Nothing
  Nothing
  ghci> fmap (*2) (Just 200)
  Just 400
  ghci> fmap (*2) Nothing
  Nothing
  ```

</div>
<div class="sheet-wrap"><div class="sheet-caption">示例：Tree类型作为函子</div>


- 另一种可以被映射并且作为Functor的一个实例的类型是我们的Tree类型
  - 它可以被想成一个盒子
  - 并且Tree类型构造器正是获取一个类型参数
- 如果将fmap当作用在Tree上的函数，它的类型签名`(a -> b) -> Tree a -> Tree b`
- 我们会在这里使用迭代
  - 自空树映射会产生空树
  - 自非空树映射会产生一个包含被应用以我们的函数的根值，其左右子树将会也会被映射以我们函数
  ``` Haskell
  instance Functor Tree where
      fmap f EmptyTree = EmptyTree
      fmap f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub)
  ```
  测试
  ``` Haskell
  ghci> fmap (*2) EmptyTree
  EmptyTree
  ghci> fmap (*4) (foldr treeInsert EmptyTree [5,7,3,2,1,7])
  Node 28 (Node 4 EmptyTree (Node 8 EmptyTree (Node 12 EmptyTree (Node 20 EmptyTree EmptyTree)))) EmptyTree
  ```

</div>
<div class="sheet-wrap"><div class="sheet-caption">示例：Either的部分应用作为函子</div>


- `Either a b`能够作为函子吗？
- Functor类型类想要一个类型构造器，其只获取一个类型参数但是Either获取了两个
- 我们会喂给它一个类型参数，部分应用Either，因此它具有一个游离（free）的参数
- `Either a`作为一个函子在标准库的实现方式
  ``` Haskell
  instance Functor (Either a) where
      fmap f (Right x) = Right (f x)
      fmap f (Left x)  = Left x
  ```
- 如果fmap对于Either a特化，类型签名应该为`(b -> c) -> Either a b -> Either a c`，因为它和`(b -> c) -> (Either a) b -> (Either a) c`一个意思
- 实现中，我们映射了Right值构造器的情形，但是Left的情形没有做
  - 因为`data Either a b = Left a | Right b`
  - 如果我们想让函数映射两个值，a和b应该要为同样的类型
  - 或者映射两个不同类型的值不合理
  - 我们看到第一个参数必须保持原样，而第二个值可以改变，第一个值实际上是Left值构造器实施（actualized）的
- 这也和我们的盒子类比（analogy）符合，如果我们把Left部分作为某种空盒子，包含写在这边的错误信息，告诉我们为何为空值

</div>
<div class="sheet-wrap"><div class="sheet-caption">示例：Data.Map的Map也可以作为函子</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">本节小节</div>


- 通过Functor类型类，我们看到了类型类如何能够表达非常炫酷的高阶概念
- 我们也进一步练习了部分应用类型（partially applying types）和制造实例
- 之后的其中一个章，我们也要看某些适用于函子的规则
- 注意：函子应该遵循一些规则，这样它们会具有一些性质，我们可以依赖且不需要多想
  - 如果我们使用`fmap (+1)`至列表`[1,2,3,4]`我们期待结果是`[2,3,4,5]`，而不是反过来`[5,4,3,2]`
  - 如果我们使用`fmap (\a -> a)`至某些列表，我们期待获得相同的列表作为结果
  - 例如如果将错误的函子实例给到我们的Tree类型，使用`fmap`应用到树上，产生的结果树违反先前树的规则
- 接下来的某一章，我们会深入了解函子规则

</div>
<h2 id="6613BA46">Kinds和一些type-foo</h2>
<div class="sheet-wrap"><div class="sheet-caption">本节介绍</div>


- 我们看到了类型构造器可以部分应用，就像函数可以部分应用一样
- 本节我们看形式化定义类型如何应用到类型构造器，就如同我们看如何通过类型声明形式化定义值如何应用到函数
- 如果你不理解，你并不真的需要阅读本节来继续你的魔法Haskell学习，不用担心
- 理解这个将会让你对类型系统理解非常透彻

</div>
<div class="sheet-wrap"><div class="sheet-caption">种类——类型的小标签</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">使用:k检查种类</div>


- 整数
  ``` Haskell
  ghci> :k Int
  Int :: *
  ```
- 其中'*'表示这个类型是一个具体类型，一个具体类型是不获取任何类型参数和值
- 现在看看`Maybe`的种类
  ``` Haskell
  ghci> :k Maybe
  Maybe :: * -> *
  ```
- "Maybe"类型构造器获取一个具体类型（如`Int`），然后返回一个具体类型（如`Maybe Int`）
- "* -> *"代表类型构造器获取一个具体类型并返回一个具体类型
  ``` Haskell
  ghci> :k Maybe Int
  Maybe Int :: *
  ```
- 一种类比（类型和种类是两种不同的东西）是，如果我们进行`:t isUpper`和`:t isUpper 'A'`
  - `isUpper`具有`Char -> Bool`类型
  - 而`isUpper 'A'`具有`Bool`类型
  然而，两种类型，都有种类`*`
- 我们对类型使用`:k`来获取它的种类，如同我们对值使用`:t`获取它的类型
- Either类型
  ``` Haskell
  ghci> :k Either
  Either :: * -> * -> *
  ```
  看起来有点像函数类型声明。类型构造器柯里化，所以我们可以应用它们
- 当我们想要让Either成为Functor类型类的一部分，我们必须部分应用它

</div>
<div class="sheet-wrap"><div class="sheet-caption">示例：做Tofu类型类和Frank类型</div>


- 看这个类型类
  ``` Haskell
  class Tofu t where
      tofu :: j a -> t a j
  ```
- 我们怎么能够让一个类型变成这么奇怪的类型类的一个实例呢？
- 我们看看它的种类是什么
  - 因为"j"用作"tofu"函数的一个值的类型，并且被作为参数获取，`j a`具有种类"*"
  - 我们假定a是`*`因此我们可以推断j具有种类`* -> *`
  - 我们看到`t`也应该产生一个具体值，j具有种类`* -> *`，我们推断t应该为种类`* -> (* -> *) -> *`
  - 所以t获取一个具体类型（a），以及获取一个具体类型的类型构造器（j），并且产生一个具体类型，哇
- 现在我们创建一个具有种类`* -> (* -> *) -> *`的类型
  ``` Haskell
  data Frank a b = Frank {frankField :: b a} dering (Show)
  ```
  - 我们怎么知道这个类型的种类是`* -> (* -> *) -> *`呢？
  - ADT的字段是用来捕获值的，所以它们一定是"*"种类
  - 我们假定a是"*"，这就表示b获取一个类型参数，所以它的种类是`* -> *`
  - 现在我们知道了a和b的种类，因为它们是Frank的参数，我们发现Franck的种类是`* -> (* -> *) -> *`
- 测试检查
  ``` Haskell
  ghci> :t Frank {frankField = Just "HAHA"}
  Frank {frankField = Just "HAHA"} :: Frank [Char] Maybe
  ghci> :t Frank {frankField = Node 'a' EmptyTree EmptyTree}
  Frank {frankField = Node 'a' EmptyTree EmptyTree} :: Frank Char Tree
  ghci> :t Frank {frankField = "YES"}
  Frank {frankField = "YES"} :: Frank Char []
  ```
- 让Frank成为Tofu的一个实例非常简单
  - 我们看到tofu获取一个`j a`，返回一个`t a j`
  - 结果类型应该为`Frank Int Maybe`
  ``` Haskell
  instance Tofu Frank where
      tofu x = Frank x
  ghci> tofu (Just 'a') :: Frank Char Maybe
  Frank {frankField = Just 'a'}
  ghci> tofu ["HELLO"] :: Frank [Char] []
  Frank {frankField = ["HELLO"]}
  ```

</div>
<div class="sheet-wrap"><div class="sheet-caption">示例：做Barry类型</div>


- 创建一个这个数据类型
  ``` Haskell
  data Barry t k p = Barry { yabba :: p, dabba :: t k}
  ```
- 现在我们想要让它成为Functor的一个实例
- 分析种类为`(* -> *) -> * -> * -> *`
- 我们需要部分应用前两个类型参数，表示实例声明开头应该是`instance Functor (Barry a b) where`
- 如果fmap对Barry特化，以及我们看到fmap的类型为`fmap :: (a -> b) -> Barry c d a -> Barry c d b`，因为我们把Functor的f替换成了`Barry c d`，Barry的第三个类型参数应该变化
  ``` Haskell
  instance Functor (Barry a b) where
      fmap f (Barry {yabba = x, dabba = y}) = Barry {yabba = f x, dabba = y}
  ```
- 我们在第一个字段上影射了f

</div>
<div class="sheet-wrap"><div class="sheet-caption">本节小结</div>


- 我们看了类型参数如何工作，以及有点用种类规范化(formalized)了它们，正如同我们用类型声明规范化了函数参数
- 我们看到函数和类型构造器之间存在有趣的类比，然而它们是完全不同的东西
- 当实际操作Haskell的时候，你通常不需要弄种类，以及像我们现在这样做一些干扰
- 通常，你只需要部分应用你自己的类型到`* -> *`或者`*`，当使其作为某个标准类型类的一个实例时
- 同样有趣的是看类型拥有它们自己的小类型
- 再一次，你并不需要真的理解我们在这里做的所有事情才能继续读下去，但是如果你理解了种类如何工作，有可能你对Haskell类型系统掌握已经很牢固了

</div>
<h1 id="69A48341">输入和输出</h1>
<div class="sheet-wrap"><div class="sheet-caption">概述</div>


纯函数式语言的特点
- 我们提到了Haskell是一个纯函数语言
- 在命令式语言中，你通常通过给计算机一系列执行步骤来完成事情，而函数式编程则是定义东西是什么
- Haskell中，函数不能改变一些状态，例如改变变量的内容（当函数改变状态，我们说函数具有副作用）
- Haskell中的函数唯一能做的事情就是基于我们给它的参数，返回给我们一些结果
- 如果函数用相同的参数调用两次，它必须返回相同结果
- 如果你来自一个命令式世界，这可能看起来有点局限，我们已经看到了这实际上很酷
- 在命令式语言中，你无法保证一个简单的函数只是处理一些数字，而不是在处理这些数字的时候还在绑架你的狗、用土豆刮你的车
- 例如说，我们实现的二叉搜索树，我们的函数实际上是在返回一棵新的树，因为它并不改变原先的那个

纯函数式有个问题
- 尽管函数不能改变状态很好，因为它帮助我们推理程序，但是有个问题：如果函数在世界上不改变任何东西，它怎么告诉我们它计算了什么？
- 为了告诉我们它计算了什么，它必须改变我们输出设备的状态（通常是屏幕的状态），然后发出光子到达我们的大脑改变我们思想的状态

处理副作用
- 看来Haskell确实有一个非常聪明的系统，用于处理具有副作用的函数，它干净地分离我们程序纯的部分以及不纯的部分，后者做了与键盘、屏幕沟通的脏活
- 这两个部分分开后，我们仍然可以推理我们的纯程序，并且利用纯度提供的所有东西，例如惰性、健壮性以及模块化，同时有效地和外部世界沟通

</div>
<h2 id="E4123C56">Hello, world!</h2>
<h2 id="2876763C">文件和流</h2>
<h2 id="8C165134">命令行变量</h2>
<h2 id="94B993AA">随机性</h2>
<h2 id="F5941F61">字节串</h2>
<h2 id="6C548C8F">异常</h2>
<div class="sheet-wrap"><div class="sheet-caption">不同语言如何处理异常</div>


- 所有的语言具有过程（procedures）、函数，以及一些可能以某些方式失败的代码片段
- 不同的语言具有不同处理这些失败的方式
  - C语言中，我们通常使用一些非正常返回值（例如-1或者空指针）来指示函数返回的东西不应该被视作一个正常值
  - Java和C#，则倾向于使用异常（exceptions）来处理失败；当抛出异常时，控制流跳转到一些我们已经定义的代码，其做某些清理然后可能重新抛出异常，因此一些其它的错误处理代码可以处理另一些事情
- Haskell有一个非常好的系统
  - 代数数据类型允许类型如Maybe和Either，我们使用这些类型的值来代表结果可能在或者不在
  - C语言中，失败时返回，比如说，-1完全是管理的问题，它对人类只有特别的含义；如果我们不小心，我们可能将这些非正常值作为普通值处理，然后它们可能在我们的代码中导致大破坏（havoc）或者沮丧（dismay）
- 在这一方面，Haskell的类型系统给我们一些更需要的安全
- 函数`a -> Maybe b`清晰地指出它必须产生包裹在Just中的`b`或者它必须返回`Nothing`，这个类型不同于普通的`a -> b`，如果我们尝试互换使用这两个函数，编译器将会对我们抱怨

</div>
<div class="sheet-wrap"><div class="sheet-caption">Haskell处理外部世界的异常</div>


- Haskell仍然有关于异常的支持，因为它在I/O语境下更加有意义
- 当处理外部世界的时候，很多事情都可以出错，因为它非常不可信赖
  - 例如当打开一个文件，一堆事情可以出错
  - 文件可能锁住了，它可能根本不存在或者硬盘驱动或者某些东西可能根本不存在
  - 因此，当这些错误发生的时候，能够跳转到我们代码的一些错误处理部分是好的
- I/O代码（也就是非纯代码）可以抛出异常
  - 它有意义
  - 那如果是纯代码呢？它也可以抛出异常；
  - 想想div和head函数，它们分别具有类型`(Integral a) => a -> a -> a`以及`[a] -> a`，返回类型没有Maybe或者Either因为它们都可能会失败！
  - 如果你除以零，div在你面前爆炸；当你给它一个空列表，head开始发脾气
    ``` Haskell
    ghci> 4 `div` 0
    *** Exception: divide by zero
    ghci> head []
    *** Exception: Prelude.head: empty list
    ```
- 纯代码可能抛出异常，但是它们只能在我们代码的I/O部分被捕获（当我们在进入main的do块中时）
  - 这是因为你不知道什么东西何时在纯代码中计算，因为它是惰性的，且具有良好定义的执行顺序；但I/O知道

</div>
<div class="sheet-wrap"><div class="sheet-caption">要尽可能减少程序中的I/O部分</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">示例：打开文件</div>


- 例如说，我们可以尝试打开一个文件，然而表明文件已被删除或者怎么着的
- 看看这个程序，其打开一个文件，它的名称作为一个命令行参数给定，并且告诉我们文件有多少行
  ``` Haskell
  import System.Environment
  import System.IO

  main = do (fileName:_) <- getArgs
            contents <- readFile fileName
            putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"
  ```
  - 我们进行`getArgs`IO行为然后将列表中第一个字符串绑定，产生文件名；
  - 然后我们用那个名称内容调用函数的内容
  - 最后，我们将lines应用到这些内容来获取一列行，然后我们获得了列表的长度并送给show来获得那个数字的字符串表示
- 它如预期运行，但是如果我们给它不存在的文件名会发生什么？
  ``` 
  $ runhaskell linecount.hs i_dont_exist.txt
  ```
  > linecount.hs: i_dont_exist.txt: openFile: does not exist (No such file or directory)
- 我们从GHC获得了一个错误，告诉我们文件不存在，我们的程序崩溃了
- 如果我们想要在文件不存在时，打印出一条更好的信息呢？
- 一种方式是在尝试用来自`System.Directory`的`doesFileExist`函数打开文件之前先检查文件是否存在
  ``` Haskell
  import System.Environment
  import System.IO
  import System.Directory

  main = do (fileName:_) <_ getArgs
         fileExists <- doesFileExist fileName
         if fileExists
             then do contents <- readFile fileName
                     putString $ "The file has " ++ show (length (lines contents)) ++ " lines!"
             else do putStrLn "The file does't exist!"
  ```
  - 我们做了`fileExists <- doesFileExist 文件名`，因为`doesFileExist`具有类型`doesFileExist :: FilePath -> IO Bool`，它表示它返回I/O行为具有布尔值，告诉我们文件是否存在，我们不能直接在if表达式里面使用`doesFileExist`

</div>
<div class="sheet-wrap"><div class="sheet-caption">使用异常（继续示例：打开文件）</div>


- 另一种解决方式是使用异常
  - 为了通过使用异常来处理这个问题，我们将要利用来自于`System.IO.Error`的`catch`函数
  - 它的类型是`catch :: IO a -> (IOError -> IO a) -> IO a`
  - 它获取两个参数
    1. I/O行为，例如说，可以是尝试打开一个文件的I/O行为
    2. 所谓的handler
  - 如果第一个传入的I/O行为抛出一个I/O异常，那个异常传入handler，其决定做什么
  - 所以最终的结果是I/O行为，要么行使和第一个参数相同的行为，要么它将会做handler让其做的事情，如果第一个I/O行为抛出异常的话
- 如果你熟悉Java或者Python里面的try-catch块的话，catch函数和它们类似
  1. 第一个参数是尝试的事情，有点像其它命令式语言中的try块
  2. 第二个参数是handler，其获取一个异常，如同大多数catch块捕获异常，然后你随即可以检验查看发生了什么
  handler在异常抛出时触发
- handler获取类型IOError的值，它是一个类型
  - 是I/O异常发生的信号；
  - 它也携带了关于被抛出异常的类型的信息
  - 这种类型如何实现取决于语言它本身，这意味着我们不能通过对其模式匹配来检查类型IOError的值，正如我们不能模式匹配类型IO某某的值
  - 我们可以用一堆有用的断言来弄明白关于类型IOError的值，我们将会一秒钟学会
- 让我们使用我们的新朋友！
  ``` Haskell
  import System.Environment 
  import System.IO
  import System.Error

  main = toTry `catch` handler

  toTry :: IO ()
  toTry = do (fileName:_) <- getArgs
             contents <- readFile fileName
             putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"

  handler :: IOError -> IO ()
  handler e = putStrLn "Whoops, had some trouble!"
  ```
  - handler中，我们没有检查我们获得了什么样的IOError
  - 我们光说“有一些麻烦！”，适用任何种类的错误
- 如果某些其它的异常发生了，而我们不想捕获，比如我们正在介入程序或者其它的事情，该怎么样？
  - 这就是为什么我们要做其它语言中我们同样经常做的事情：我们将要检查我们获得了何种异常
  - 如果是我们在等的异常种类，我们做我们的事情；如果不是，我们把那个异常抛回去放归野外
  ``` Haskell
  import System.Environment
  import System.IO
  import System.IO.Error

  main = toTry `catch` handler

  toTry :: IO ()
  toTry = do (fileName:_) <- getArgs
             contents <- readFile fileName
             putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"

  handler :: IOError -> IO ()
  handler e 
      | isDoesNotExistError e = putStrLn "The file doesn't exist!"
      | otherwise = ioError e
  ```
  - 除了handler，一切都保持原样；我们修改了handler让其只匹配特定一组I/O异常
  - 这里我们使用了`System.IO.Error`里面的两个新的函数——`isDoesNotExistError`和`ioError`
    - `isDoesNotExist`: 判断`IOErrors`的谓词，接受一个`IOError`并返回True或者False，类型为`DoesNotExistError :: IOError -> Bool`
    - `ioError`: 如果异常不是由文件不存在触发的，我们重新抛出异常，让其通过handler用ioError函数传递，类型为`ioError :: IOException -> IO a`，它获取`IOError`然后产生可以抛出异常的I/O行为，IO行为的类型为`IO a`，因为它永远不会真的产生一个结果，所以它可以表现为IO任何东西
- 所以在toTryI/O里面抛出了异常，我们将其与一个不会因文件存在而触发的do块胶粘在一起，``toTry `catch` handler``将会捕获异常，然后重新抛出，很酷？

</div>
<div class="sheet-wrap"><div class="sheet-caption">对IOError工作的谓词</div>


对IOError工作的谓词有：
1. isAlreadyExistsError
2. isDoesNotExistsError
3. isAlreadyInUseError
4. isFullError
5. isEOFError
6. isIllegalOperation
7. isPermissionError
8. isUserError

这些大多数都是自解释的
- `isUserError`在我们使用函数`userError`产生异常的时候求值为真，它被用于从我们的代码中创造异常并且使其带有字符串
- 例如，你可以做`ioError $ userError "remote computer unplugged!"`，尽管你使用像`Either`和`Maybe`来表达可能的失败更好，而不是自己用userError抛出异常

</div>
<div class="sheet-wrap"><div class="sheet-caption">System.IO.Error导出的有用函数</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">说明：更倾向于使用Either等类型</div>

</div>
<h1 id="551B54A4">函数化解决问题</h1>
<h1 id="4CC1C54A">函子，应用函子以及Moniods</h1>
<div class="sheet-wrap"><div class="sheet-caption">本章概述</div>


- Haskell的纯、高阶函数组合，参数化代数数据类型，以及类型类允许我们以一个比其它语言可能的，还要高得多的层次实现多态
- 我们不必思考类型属于一个大的类型层级（a big hierarchy of types）
- 相反，我们考虑类型可以如何行为，然后将它们连接到合适的类型类
- 一个Int可以表现得像很多东西；它可以表现得像可比较相等的东西，像可排序的东西，像可枚举的东西，等等
- 类型类是开放的，这意思是我们可以定义我们自己的数据类型，考虑它可以表现得像什么，并且将其连接到定义它的行为的类型类
- 因此以及因为Haskell的伟大的类型系统，其允许我们通过一个函数的类型声明指导关于它的很多东西，我们可以定义一些类型类，其定义了非常通用和抽象的行为
- 我们已经见识了定义比较相等和比较排序的类型类，这些是非常抽象以及优雅的行为；
- 但是我们并没有把它们想象成任何非常特别的东西，因为我们已经在我们生活大多数时候都在处理它们
- 我们最近见识了函子，其基本上是可以被映射于的东西；这是一个示例，非常有用但是仍然非常抽象的特性，类型类可以描述
- 本章中，我们将会更进一步观察函子，以及些微更加强大更加有用的函子版本，叫做应用函子（applicative functors）
- 我们也将会看到monoids，它有点像插座

</div>
<h2 id="379CE627">重提函子</h2>
<div class="sheet-wrap"><div class="sheet-caption">函子：快速回顾</div>


- 函子（Functor）是可以被映射于的东西，像列表、Maybe、树等等
- Haskell中，它们通过类型类Functor描述，其只有一个类型类方法，即fmap，其类型为`fmap:: (a -> b) -> f a -> f b`
- 它是在说，给我一个函数获取a返回b，一个装着a（或者好几个a）的盒子，我将会给你一个装着b（或者好几个b）的盒子
- 它有点应用函数到盒子里面的元素

</div>
<div class="sheet-wrap"><div class="sheet-caption">注意：一句话建议</div>


注意： **一句话建议**
- 很多时候，盒子比喻用来帮助你获得函子如何工作的直觉
- 过后，我们可能将相同的比喻用于应用函子（applicative functor）和monad
- 它是一个可以的比喻，能够帮助人们首先理解函子，但是不要太扣字面意义，因为对于某些函子，盒子比喻被拉伸得很薄才能保持一些真相
- 对于函子是什么，更加正确的术语应该是计算上下文（computational context）
- 上下文可能是，计算可以具有值或者它可能失败（Maybe和Either a）或者是，可能有更多的值（list），像这样类似

</div>
<div class="sheet-wrap"><div class="sheet-caption">函子必须具有的种类</div>


- 如果我们想让一个类型构造器作为Functor的一个实例，它类型必须是`* -> *`，其代表着它必须获取恰好一个具体类型作为类型参数
- 例如，Maybe可以作为一个参数因为它获取一个类型参数来产生具体类型
- 如果类型构造器获取两个参数，像Either，我们必须部分应用类型构造器，直到其只获取一个类型参数
- 我们已经学了很多种类型如何成为Functor的函子
- 本小节，我们将会再看到函子的两个实例，即`IO`和`(->) r`

</div>
<div class="sheet-wrap"><div class="sheet-caption">IO如何作为Functor的一个实例</div>


- 当我们fmap一个函数到一个I/O行为，我们想要获得一个I/O行为做同样的事情，但是我们的函数应用在它的结果值
  ``` Haskell
  instance Functor IO where
      fmap f action = do
          result <- action
          return (f result)
  ```
  - 将某些东西映射到一个I/O行为的结果将会是一个IO行为
  - 所以马上开始，我们使用do语法来胶粘两个行为然后创造一个新的行为
  - 在fmap的实现中，我们创造一个新的I/O行为，其首先实施原I/O行为，然后把它的结果称作result，然后，我们做`return (f result)`
  - `return`是一个函数，制造一个I/O行为，它不做任何事情，但是只呈现某些东西作为它的结果
  - 一个do块产生的行为将总会具有它最后一个行为的结果
  - 这就是为什么我们使用return来制造一个I/O行为，它不做任何事情，它只是呈现`f result`作为新的I/O行为的结果
- 我们可以玩一玩来获得一些直觉
  ``` Haskell
  main = do line <- getLine
            let line' = reverse line
            putStrLn $ "You said " ++ line' ++ " backwards!"
            putStrLn $ "Yes, you really said " ++ line' ++ " backwards!"
  ```
- 也可以用fmap重写
  ``` Haskell
  main = do line <- fmap reverse getLine
            putStrLn $ "You said " ++ line ++ " backwards!"
            putStrLn $ "Yes, you really said " ++ line ++ " backwards!"
  ```
  - 就如同我们可以应用函数到Maybe盒子中的东西，我们也可以应用一个函数到IO盒子中的一个东西，只是它必须要走出进入到真实世界去获取某些东西
  - 然后当我们用`<-`将它绑定到一个命名，这个命名将会反映结果，其已经被应用了reverse
- IO行为`fmap (++ "!") getLine`表现的像是getLine，只是它的结果总是追加一个"!"
- 如果你发现你自己正在将一个IO行为的结果绑定到一个命名，最终只是应用了一个函数到那个结果并且将其称为另一个东西，考虑使用`fmap`，因为它看起来更好看
- 如果你想要应用多个变换于一些包含在函子内的数据，你可以在顶层（top level）声明你自己的函数，创造一个lambda函数，或者理想情况下创造一个函数组合
  ``` Haskell
  import Data.Char
  import Data.List

  main = do line <- fmap (intersperse '-' . reverse . map toUpper) getLine
            putStrLn line

  $ runhaskell fmapping_io.hs
  hello there
  E-R-E-H-T- -O-L-L-E-H
  ```
- 你可能知道，``intersperse '-' . reverse . map toUpper``是一个函数，就像写成`(\xs -> intersperse '-' (reverse (map toUpper xs)))`，只是更加漂亮

</div>
<div class="sheet-wrap"><div class="sheet-caption">(->) r如何作为函子</div>


- 另一个Functor的实例我们可能一直在打交道，但是不知道是个Functor，它是`(->) r`
- 你现在可能有点困惑，因为到底`(->) r`是什么意思？
- 函数类型`r -> a`可以写成`(->) r a`，当我们把它看成后者，我们用一种些微不同的眼光看待`(->)`，因为我们看到它只是个类型构造器，获取两个类型参数，正如同Either
- 但是记住，我们说一个类型构造器应当更好获取一个类型参数，它才能作为Functor的一个实例
  - 这就是为三的很么我们不能让`(->)`作为Functor的一个实例
  - 但是如果我们部分应用，它不会产生任何问题
- 函数怎么能成为函子？我们看看实现，它在`Control.Monad.Instances`中
  - 注意： *没用的信息，忽略*
  ``` Haskell
  instance Functor ((->) r) where
      fmap f g = (\x -> f (g x))
  ```
  - 首先，考虑fmap类型`fmap :: (a -> b) -> f a -> f b`
  - 我们考虑fmap对于这个特定的实例如何表现：`fmap :: (a -> b) -> ((->) r a) -> ((->) r b)`
  - 我们重写成`fmap :: (a -> b) -> (r -> a) -> (r -> b)`
- 映射一个函数于一个函数来产生一个函数，就像映射一个函数于一个Maybe应当产生一个Maybe，以及映射到列表产生列表
  - 类型`fmap :: (a -> b) -> (r -> a) -> (r -> b)`告诉我们什么？
  - 这让你想起函数组合！你会看到它就是个函数组合
  - 另一种编写这个实例的方式可能是
    ``` Haskell
    instance Functor ((->) r) where
        fmap = (.)
    ```
- 试玩
  - 首先做`:m + Control.Monad.Instances`
  - 然后玩
    ``` Haskell
    ghci> :t fmap (*3) (+100)
    fmap (*3) (+100) :: (Num a) => a -> a
    ghci> fmap (*3) (+100) 1
    303
    ghci> (*3) `fmap` (+100) $ 1
    303
    ghci> (*3) . (+100) $ 1
    303
    ghci> fmap (show . (*3)) (*100) 1
    "300"
    ```
- 盒子的类比在这里如何成立呢？如果你拉伸它，它就成立

</div>
<div class="sheet-wrap"><div class="sheet-caption">让我们思考fmap的类型（提升函数）</div>


- 在我们继续看fmap应该遵循的规则，让我们再想一想fmap
- 类型`fmap :: (a -> b) -> f a -> f b`我们忘记了类约束`(Functor f) =>`，但是我们为了简洁忽略掉了它，因为我们不管怎么说都是在讨论函子
- 当我们第一次了解柯里化函数时，我们说所有Haskell函数实际上都获取一个参数
- 我们用很少的参数调用函数（即部分应用），我们获得新的函数，其获取剩下个数的参数
- 同样地，如果我们写出`fmap :: (a -> b) -> (f a -> f b)`
  - 我们可以把fmap不看作获取一个函数以及一个函子、返回一个函子的函数
  - 而是看成一个获取一个函数的函数，其返回新的函数；它和原来的函数很像，只不过它获取一个函子作为参数，并且返回一个函子作为结果
  - 所以是获取`a -> b`函数并且返回`f a -> f b`函数
  - 这被称为提升函数（lifting a function）
- 让我们用GHCI的`:t`命令玩玩
  ``` Haskell
  ghci> :t fmap (*2)
  fmap (*2) :: (Num a, Functor f) => f a -> f a
  ghci> :t fmap (replicate 3)
  fmap (replicate 3) :: (Functor f) => f a -> f [a]
  ```
- 注意：当我们说函子下数（a functor over number），你可以想象成函子里面有个数字；前者有点炫，并且严格上更加正确，但是后者通常更容易理解
- 如果你部分应用，比如说`fmap(++"!")`，然后在GHCI中把它绑定到一个命名，这甚至更加明显
- *以下略*

</div>
<div class="sheet-wrap"><div class="sheet-caption">看看函子规则</div>


我们期望所有的函子都表现特定种类的像是函子的（functor-like）特性和行为
- 它们应该可靠地表现为可以被映射于的东西；调用fmap于一个函子应该只是对函子映射一个函数，没别的
- 这个行为在函子规则中被描述
- 有两个规则，Functor所有的实例都应该遵守；它们不是被Haskell自动强制要求的，所以你必须自己测试它们
  1. 第一个函子规则是说，如果我们对一个函子映射id函数，我们拿回来的函子应该和最初的函子一样
     - 如果形式化地说，它表示`fmap id = id`
     - 看看这条规则是否成立 *略*
  2. 第二个规则是说，组合两个函数然后将结果函数映射于函子，应该和首先映射一个函数于函子，然后映射另一个函数结果相同
     - 形式化写，它代表着`fmap (f . g) = fmap f . fmap g`
     - 或者另一种方式写对于任何函子F，应该满足：`fmap (f . g) F = fmap f (fmap g F)`
     - 看看这条规则如何适用 *略*

</div>
<div class="sheet-wrap"><div class="sheet-caption">让我们看看一个病态例子：无法成为函子</div>


- 病态的示例，类型构造器可以作为Functor类型类的实例但是不能真正成为一个函子，因为不满足规则
- 我们有类型
  ``` Haskell
  data CMaybe a = CNothing | CJust Int a deriving (Show)
  ```
  C这里代表计数器
- 这个类型看起来很像是`Maybe a`，只是`Just`部分包含两个字段而不是一个
  - `Cjust`中第一个字段总是具有类型Int，它会是某种计数器
  - 第二个字段类型是`a`，其来自于一个类型参数，它的类型将会取决于我们为`CMaybe a`选择的具体类型
- 让我们玩
  ``` Haskell
  ghci> CNothing
  CNothing
  ghci> CJust 0 "haha"
  CJust 0 "haha"
  ghci> :t CNothing
  CNothing :: CMaybe a
  ghci> :t CJust 0 "haha"
  Cjust 0 "haha" :: CMaybe [Char]
  ghci> CJust 100 [1,2,3]
  Cjust 100 [1,2,3]
  ```
- 如果我们使用CNothign构造器，没有字段；如果我们使用CJust构造器，第一个字段是一个整数而第二个字段是任何类型
- 我们让它成为Functor的一个实例，从而每次我们使用fmap，类型应用到第二个字段，因此第一个字段增加1
  ``` Haskell
  instance Functor CMaybe where
      fmap f CNothing = CNothing
      fmap f (CJust counter x) = CJust (counter+1) (f x)
  ```
  这有点像Maybe的实例实现，除了当我们对一个不表示空盒子的值（一个CJust值）做fmap，我们不只是将函数应用到内容，我们同样将计数器增加1
- 我们可以玩
  ```
  ghci> fmap (++"ha") (CJust 0 "ho")
  CJust 1 "hoha"
  ghci> fmap (++"he") (fmap (++"ha") (CJust 0 "ho"))
  CJust 2 "hohahe"
  ghci> fmap (++"blah") CNothing
  CNothing
  ```
- 这遵循函子规则吗？为了看某个东西不遵循规则，只需要一个计数器例子就够了
  ``` Haskell
  ghci> fmap id (CJust 0 "haha")
  CJust 1 "haha"
  ghci> id (CJust 0 "haha")
  CJust 0 "haha"
  ```
- CMaybe无法作为函子，即使它假装是个函子，使用它作为函子可能导致一些错误的代码

</div>
<div class="sheet-wrap"><div class="sheet-caption">说明：函子规则可以产生对于函子更加可靠的函数</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">总结（略）</div>

</div>
<h2 id="EDE43895">应用函子</h2>
<div class="sheet-wrap"><div class="sheet-caption">本节介绍</div>


- 该小节中，我们将会看到应用函数（applicative functors），它们是增强版函子（beefed up functors）
- 在Haskell中用Applicative类型类表示
- 在Control.Applicative模块中可以找到

</div>
<div class="sheet-wrap"><div class="sheet-caption">能否将获取两个参数的函数映射于函子</div>


- Haskell中多参函数：柯里化
- 目前，我们将函数映射到了函子上，我们经常映射只获取一个参数的函数
- 如果我们映射函数，如`*`（乘法），它获取两个参数，于一个函子呢？
- 让我们看几个具体的示例
- 如果我们有`Just 3`，我们做`fmap (*) (Just 3)`，我们获得什么？
  - 从Maybe实例对于Functor的实现来说，我们知道如果它是Just某个值，它将会将函数应用到Just里面的某个东西
  - 因此，做`fmap (*) (Just 3)`结果是`Just ((*) 3)`，也可以写成`Just (* 3)`
  - 有趣！我们将一个函数包裹在了一个Just里面
  ``` Haskell
  ghci> :t fmap (++) (Just "hey")
  fmap (++) (Just "hey") :: Maybe ([Char] -> [Char])
  ghci> :t fmap compare (Just 'a')
  fmap compare (Just 'a') :: Maybe (Char -> Ordering)
  ghci> :t fmap compare "A LIST OF CHARS"
  fmap compare "A LIST OF CHARS" :: [Char -> Ordering]
  ghci> :t fmap (\x y z -> x + y / z) [3,4,5,6]
  fmap (\x y z -> x + y / z) [3,4,5,6] :: (Fractional a) => [a -> a -> a]
  ```
  - 如果我们映射compare，它有类型`(Ord a) => a -> a -> Ordering`对于一列表字符，我们获得一列表函数，类型`Char -> Ordering`
  - 因为函数compare被部分应用于列表中的字母
- 我们年看到了通过映射“多参”函数于函子上，我们获得包含有函数的函子
- 所以现在我们可以对它们做什么？
  1. 一个，我们可以映射函数，其获取这些函数作为参数，因为函子内的任何东西都会被给到函数，所以我们映射于它，使其作为参数
     ``` Haskell
     ghci> let a = fmap (*) [1,2,3,4]
     ghci> :t a
     a :: [Interger -> Integer]
     ghci> fmap (\f -> f 9) a
     [9,18,27,36]
     ```
- 但是如果我们具有`Just (3 *)`的函子，然后一个值为`Just 5`的函子，我们想要从`Just (3 *)`中拿出函数，然后映射到`Just 5`上呢？
- 使用普通的函子，我们无计可施（we're out of luck）；因为它们所有支持的只是映射普通的函数于已有的函子
- 我们不能映射一个函子内的函数于另一个函子
- 我们可以模式匹配于Just构造器来获取函数，然后映射于`Just 5`，但是我们在找一种更加通用且抽象的方式来做这件事，它工作在函子之间

</div>
<div class="sheet-wrap"><div class="sheet-caption">开始见识Applicative类型类</div>


- 见识Applicative类型类，位于`Control.Applicative`模块，定义了两个方法
  - pure
  - <*>
  并没有提供默认实现，所以如果我们像某个东西作为应用函子，我们必须定义它们
- 这个类定义如
  ``` Haskell
  class (Functor f) => f a
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
  ```
- 这简单的三行告诉了我们很多东西！
  1. 第一行，定义Applicative类，也引入了一个类约束；
     - 它说如果我们想要让一个类构造器作为Applicative类型类的一部分，它必须先为Functor；
     - 这就是为什么我们知道如果一个类型构造器是Applicative类型类的一部分，它也在Functor，所以我们可以对其使用fmap
  2. 第一个定义的方法是pure，类型声明是`pure :: a -> f a`，f的角色是我们的应用函子实例；
     - pure将会获取任何类型的一个值，然后返回一个应用函子，其内包含一个值；
     - 当我们说在里面，我们又在使用盒子类比，尽管它并不总是经得起琢磨
     - 更好地思考pure的方式可能是它获取一个值，并且将其放置在某种默认（或者纯的）上下文中——一个最小的上下文，其仍然产生那个值
  3. <*>函数非常有趣
     - 它有类型声明`f (a -> b) -> f a -> f b`，这提醒了你什么吗？
     - 当然，`fmap :: (a -> b) -> f a -> f b`
     - 它是一种增强版fmap
     - fmap获取一个函数以及一个函子，并将函数在函子内应用；而<*>获取一个函子，其内含有一个函数，以及另一个函子，然后某种方式从第一个函子内提取出那个函数，然后映射于第二个函子
     - 当我说提取，我实际有点想表达运行然后提取，可能甚至序列
     - 我们将会马上看见为什么

</div>
<div class="sheet-wrap"><div class="sheet-caption">Maybe的applicative实现</div>


- 让我们看看Maybe的Applicative实例的实现
  ``` Haskell
  instance Applicative Maybe where
      pure = Just
      Nothing <*> _ = Nothing
      (Just f) <*> something = fmap f something
  ```
  - 我们看到f的角色是应用函子，应该获取一个具体类型作为参数
    - 所以我们写的是`instace Applicative Maybe where`而不是`instance Applicative (Maybe a) where`
  - 首先，pure
    - 我们之前说过它应该获取某些东西然后将其包裹在一个应用函子内
    - 我们写`pure = Just`，因为像Just的值构造器是普通的函数
    - 我们其实也可以写`pure x = Just x`
  - 然后，我们定义了<*>
    - 我们不能从Nothing中提取出函数，因为它里面每函数
    - 所以我们说，如果我们尝试从Nothing中提取函数，结果就是Nothing
    - 如果你看Applicative的类定义，你会看到有一个Functor类约束，它意味着我们可以假定<*>的两个参数都是函子
    - 如果第一个参数不是Nothign，而是Just，包含有某个函数，我们说我们随即想要将该函数映射于第二个参数
    - 这同样考虑到了一种情形，即第二个参数是Nothing，因为用任何函数做fmap于Nothing，将会返回Nothing
    - 所以对于Maybe，<*>从左边的值提取出了函数，如果它是Just，并且映射于右边的值；如果任何参数是Nothing，结果就是Nothing
- 让我们玩玩
  ``` Haskell
  ghci> Just (+3) <*> Just 9
  Just 12
  ghci> pure (+3) <*> Just 10
  Just 13
  ghci> pure (+3) <*> Just 9
  Just 12
  ghci> Just (++"hahah") <*> Nothing
  Nothing
  ghci> Nothing <*> Just "woot"
  Nothing
  ```
- 用普通的函子，你可以map一个函数于函子，然后你不能通过任何通常的方式提取出结果，即使结果是部分应用函数
- 应用函子，则是允许你用一个函数操作数个函子
  ``` Haskell
  ghci> pure (+) <*> Just 3 <*> Just 5
  Just 8
  ghci> pure (+) <*> Just 3 <*> Nothing
  Nothing
  ghci> pure (+) <*> Nothing <*> Just 5
  Nothing
  ```
- 让我们一步步分解
  1.  `<*>`是左结合的，表明`pure (+) <*> Just 3 <*> Just 5`跟`(pure (+) <*> Just 3) <*> Just 5`一样
  2. `+`函数放在函子内，我们有`pure (+)`，它是`Just (+)`
  3. 然后`Just (+) <*> Just 3`发生了，结果是`Just (3+)`，这是因为部分应用
  4. 最后`Just (3+) <*> Just 5`执行了，结果是`Just 8`

</div>
<div class="sheet-wrap"><div class="sheet-caption">应用风格的好处</div>


- 应用函子和应用风格进行`pure f <*> x <*> y <*>`允许我们
  - 使用一个函数，它获取的参数不一定包裹在函子内，并且使用这个函数来操作数个值
  - 这些值位于函子上下文中
  - 这个函数可以获取要多少有多少个参数，因为它总是在`<*>`中间一步步部分应用
- 如果我们考虑`pure f <*> x`等于`fmap f x`就更方便了，这是应用规则（applicative laws）之一
  - 我们稍后会更仔细观察，但是现在，我们可以有点直觉性地发现确实是这样
  - `pure`将一个值放到默认的上下文中，如果我们将函数放到默认上下文中，然后提取然后将其应用到另一个应用函子中的值，我们做的事情就跟将函数映射到应用函子一样
  - 其实不需要编写`pure f <*> x <*> y <*> ...`，我们可以写`fmap f x <*> y <*> ...`
  - 这就是为什么Control.Applicative导出了一个函数叫作`<$>`，它只是作为中缀运算符的`fmap`，它是这样定义的
  ``` Haskell
  (<$>) :: (Functor f) => (a -> b) -> f a -> f b
  f <$> x = fmap f x
  ```
- 注意：
  - 快速提醒：类型变量独立于参数名称（parameter names）或者其它值名称（other value names）
  - 函数声明这里的`f`是一个类型变量，并且具有类型约束，声称任何代替`f`的类型构造器应该是`Functor`类型类
  - 函数体中的`f`表示我们映射于x的一个函数
  - 我们使用`f`来表示两者并不表示它们不知怎么的表示同一个东西
  - 通过使用`<$>`，应用风格大显光彩，因为现在如果我们想要在三个应用函子之间应用一个函数`f`，我们可以写`f <$> x <*> y <*> z`，如果参数不是应用函子，而是普通值，我们就写`f x y z`

</div>
<div class="sheet-wrap"><div class="sheet-caption">示例：Maybe与字符串拼接（略）</div>


略

</div>
<div class="sheet-wrap"><div class="sheet-caption">List作为应用函子</div>


实际上list类型构造器，[]是应用函子
- 定义
  ``` Haskell
  instance Applicative [] where
    pure x = [x]
    fs <*> xs = [f x | f <-fs, x <- xs]
  ```
- 前面，我们说过pure获取一个值并且将它放到一个默认的上下文
  - 换句话说，就是产生那个值的最小上下文（minimal context）
  - 列表的最小上下文可以是空列表，`[]`，但是空列表表示缺值，所以它里面不能保留我们用在`pure`上的值，这就是为什么`pure`获取了一个值并且将它放在但列表中
  - 类似地，Maybe应用函子的的最小上下文是`Nothing`，但是它代表缺值而不是有值，所以`pure`在`Maybe`的实例实现中作为`Just`实现
  ``` Haskell
  ghci> pure "Hey" :: [String]
  ["Hey"]
  ghci> pure "Hey" :: Maybe String
  Just "Hey"
  ```
- 那`<*>`呢？
  - 如果我们观察当它仅限于列表时，`<*>`的类型是什么，我们得到`(<*>) :: [a -> b] -> [a] -> [b]`
  - 这是用列表推导式（list comprehension）实现的
  - `<*>`必须用某种方式从它的左参数中提取出函数然后应用到右参数上
  - 但是问题在于做列表里面可以有另个函数，一个函数，或者数个函数，有列表也可以有数个值
  - 这就是为什么我们使用列表推导式来从两个列表中抽取
  - 我们将来自左列表的每一个可能的函数应用到来自右列表的值，结果列表具有来自左列表函数应用到来自右边的值的每个可能的组合
- 示例
  1. 代码
     ``` Haskell
     ghci> [(* 0), (+ 100), (^ 2)] <*> [1, 2, 3]
     [0,0,0,101,102,103,1,4,9]
     ```
     左边列表具有三个函数，右边列表具有三个值，所以结果列表具有九个元素
  2. 如果有一列获取两个参数的函数，我们可以把这些函数在两列表之间应用
     ``` Haskell
     ghci> [(+),(*)] <*> [1,2] <*> [3,4]
     ```
     - 因为`<*>`是左结合的，所以`[(+),(*)] <*> [1,2]`首先发生
     - 结果是相当于列表`[(1+),(2+),(1*),(2*)]`，因为左边的每一个函数应用于右边的每一个值
     - 然后，`[(1+),(2+),(1*),(2*)] <*> [3,4]`发生，它产生最终的结果
  3. 有趣
     ``` Hskell
     ghci> (++) <$> ["ha","heh","hmm"] <*> ["?","!","."]
     ["ha?","ha!","ha.","heh?","heh!","heh.","hmm?","hmm!","hmm."]
     ```

</div>
<div class="sheet-wrap"><div class="sheet-caption">把列表看作非确定性计算</div>


- 你可以把列表看作非确定性的计算（non-deterministic computations）
  - 一个100或者`"what"`的值可以看作确定性计算（deterministic computation），只有一个结果
  - 而列表如`[1,2,3]`可以看作一个计算，无法选择它想要有的结果是那个，所以它呈现给我们所有可能的结果
  - 所以当你做某些事情，如`(+) <$> [1,2,3] <*> [4,5,6]`你可以把它看成把两个非确定性计算用`+`加起来，只是为了产生另外一个非确定性计算，结果的把握甚至还要低
- 在列表中使用应用风格通常是列表推导式的很好的平替
  - 第二章中，我们想要看`[2,5,10]`和`[8,10,11]`相乘的所有可能的结果，所以我们做了
    ``` Haskell
    ghci> [ x*y | x <- [2,5,10], y <- [8,10,11]]
    [16,20,22,40,50,55,80,100,110]
    ```
  - 我们从两个列表中抽取并且把一个函数应用到元素的每一个组合，也可以用应用风格来做
    ``` Haskell
    ghci> (*) <$> [2,5,10] <*> [8,10,11]
    [16,20,22,40,50,55,80,100,110]
    ```
    这个看起来更加干净，因为更容易看到我们在两个非确定性计算之间调用`*`
  - 如果我们吸纳更知道这两个列表所有可能的乘积中超过50的，我们只要做
    ``` Haskell
    ghci> filter (>50) $ (*) <$> [2,5,10] <*> [8,10,11]
    [55,80,100,110]
    ```

</div>
<div class="sheet-wrap"><div class="sheet-caption">IO作为应用函子</div>


实例实现方式
``` Haskell
instance Applicative IO where
  pure = return 
  a <*> b = do
    f <- a
    x <- b
    return (f x)
```
- 因为`pure`是关于把值放到一个最小上下文中，并且将其保留作为结果，因此`pure`就是`return`也很合理，因为`return`干的就是这个
- 它产生一个不做任何事情的I/O动作，它只是产生某些值作为结果，但是它并不真的做任何像打印到终端或者读取文件的I/O操作

*为什么可以用`do`来进行IO的应用函子定义？我以为`do`和里面的`<-`是`>=`的语法糖？*

如果`<*>`为IO特化，它将会具有类型`(<*>) :: IO (a -> b) -> IO a -> IO b`
- 它将会获取一个I/O行为，产生一个函数作为结果，以及另一个I/O行为，并且从这两个I/O行为创建一个新的I/O行为
- 当执行时，首先执行第一个来获取函数，然后执行第二个来获取值，然后它将会产生函数应用到值作为它的结果
- 我们这里使用了`do`语法来实现它
- 记住`do`语法是关于获取数个I/O行为然后把它们胶粘到一起的，我们这里做的就是这

</div>
<div class="sheet-wrap"><div class="sheet-caption">说明：<*>用于Maybe、[]、IO的理解</div>


- 对于Maybe和[]
  - 我们可以把`<*>`想成简单地从做参数提取出函数然后某种方式应用到右参数
- 对于IO
  - 提取仍然成立，但是现在我们也有了次序（sequencing）的记号，因为我们在执行两个`I/O`行为并且在排次序，或者是将它们胶粘到一起
  - 我们必须从第一个I/O行为中提取出函数，但是为了从I/O行为中提取出一个结果，它必须被执行

</div>
<div class="sheet-wrap"><div class="sheet-caption">示例：提示用户输入两行文本</div>


考虑
``` Haskell
myAction :: IO String
myAction = do
  a <- getLine
  b <- getLine
  return $ a ++ b
```
- 这是一个I/O行为，将会提示用户输入两行文本并且产生结果，将两行文本拼接
- 我们通过将两个`getLine`I/O行为胶粘在一起以及一个`return`来做到这一点
- 因为我们想要我们新的胶粘I/O行为保持`a ++ b`的结果

另一种编写这个的方式可能是使用应用风格
  ``` Haskell
  myAction :: IO String
  myAction = (++) <$> getLine <*> getLine
  ```
- 我们之前做的事情是创建一个I/O行为，将一个函数应用到另外两个I/O行为之间，这是一回事
- 记住，`getLine`是一个具有类型`getLine :: IO String`的I/O行为
- 当我们在两个应用函子之间应用`<*>`是，结果是一个应用函子，所以这一切很合理

</div>
<div class="sheet-wrap"><div class="sheet-caption">说明：盒子比喻与应用风格</div>


- 如果我们回到盒子比喻，我们可以想象`getLine`是一个盒子，它将会出去到现实世界，并且给我们抓取（fetch）一些字符串
- 进行`(++) <$> getLine <*> getLine`产生一个新的，更大的盒子，能够送出去两个盒子，来从终端抓取文本行，然后呈现这两行文本的拼接作为结果
- `(++) <$> getLine <*> getLine`表达式的类型是`IO String`，它代表这个表达式完全是个普通的I/O行为，跟其它任何的一样，在内部保留一个结果值
- 这就是为什么我们可以做这个
  ``` Haskell
  main = do
    a <- (++) <$> getLine <*> getLine
    putStrLn $ "The two lines concatenated turn out to be: " ++ a
  ```
- 如果你有发现自己正在将某些I/O行为绑定到命名（names）然后对其调用某些函数，并且使用`return`将其表达为结果，考虑使用应用风格，因为它更加简洁明了（arguably a bit more concise and terse）

</div>
<div class="sheet-wrap"><div class="sheet-caption">(->) r作为应用函子</div>


另一个Applicative的实例是`(->) r`，也就是函数
- 它们很少在代码高尔夫（code golf）外部以应用风格使用
- 但是它们作为应用函子仍然有趣，所以我们看看函数实例是如何被实施的

注意：如果你困惑于`(->) r`是什么意思，检查之前的小节，其中我们解释了`(->) r`如何是一个函子

``` Haskell
instance Applicative ((->) r) where
  pure x = (\_ -> x)
  f <*> g = \x -> f x (g x)
```
- 当我们将一个值用`pure`包裹在一个应用函子内部，它产生的结果永远只能是那个值
- 一个最小默认上下文，仍然产生那个值作为结果
- 这就是为什么在函数实例实现中，`pure`获取一个值并且创建一个函数，其忽略参数并且永远返回那个值
- 如果我们看`pure`的类型，但是为`(->) r`实例而特化，它是`pure :: a -> (r -> a)`

例子
``` Haskell
ghci> (pure 3) "blah"
3
```
- 因为柯里化，函数应用是左结合的，所以我们可以忽略括号
  ``` Haskell
  ghci> pure 3 "blah"
  3
  ```

`<*>`的实例实现有点晦涩难懂，所以我们只要看看如何将函数作为应用函子以应用风格使用就最好
``` Haskell
ghci> :t (+) <$> (+3) <*> (*100)
(+) <$> (+3) (*100) :: (Num a) => a -> a
ghci> (+) <$> (+3) <*> (*100) $ 5
508
```
- 用`<*>`调用两个应用函子结果是一个应用函子，所以，如果我们在两个函数上使用它，我们会得到一个函数
- 所以这里发生了什么？
- 当我们做`(+) <$> (+3) <*> (* 100)`，我们在创造一个函数，其将会对`(+3)`和`(*100)`的结果使用`+`，并且返回它
- 为了演示真正的示例，当我们做了`(+) <$> (+3) <*> (*100) $ 5`，5首先应用到了`(+3)`和`(*100)`上，结果是8和500，然后，+被8和500调用，结果是508

例子
``` Haskell
ghci> (\x y z -> [x,y,z]) <$> (+3) <*> (*2) <*> (/2) $ 5
[8.0,10.0,2.5]
```
- 这里也一样
- 我们创建了一个函数，它将会调用函数`\x y z -> [x,y,z]`，使用来自`(+3)`、`(*2)`、`(/2)`的最终结果
- 5被喂入三个函数然后`\x y z -> [x,y,z]`被用这些结果调用

</div>
<div class="sheet-wrap"><div class="sheet-caption">说明：用盒子来看待函数</div>


你可以把函数看成盒子，包含有它们的最终结果
- 所以，做`k <$> f <*> g`创建一个函数，其将会用来自f和g的最终结果调用k
- 当我们做某些事情，如`(+) <$> Just 3 <*> Just 5`，我们在对一些可能在或不在的值使用+，它也会导致一个值在或是不再
- 当我们做`(+) <$> (+10) <*> (+5)`，我们对`(+10)`和`(+5)`的未来返回值使用+，结果仍然是某些只会在用一个参数调用时产生值的“东西”

我们不是经常把函数用成应用函子，但是这仍然很有趣
- 你能否明白`(->) r`示例作为函子如何运作其实不是很重要，所以如果你现在没明白也不用绝望
- 尝试玩一玩应用风格和函子，来建立起函数作为函子的直觉

</div>
<div class="sheet-wrap"><div class="sheet-caption">ZipList作为应用函子</div>


一个我们还没有遇到的Applicative实例是`ZipList`，它住在`Control.Applicative`中
- 列表作为应用函子的两种可能
  - 看来实际上还有办法让列表成为应用函子，一种方式我们已经介绍了，也就是用`<*>`调用一列表的函数和一列表的值，结果是一个列表，其具有将左列表函数应用到右列表值的所有可能组合
  - 如果我们做`[(+3),(*2)] <*> [1,2]`，`(+3)`将会被应用到1和2,`(*2)`将也会被应用到1和2，结果是一个列表具有4个元素，也就是`[4,5,2,4]`
  - 然而，`[(+3),(*2)] <*> [1,2]`也可以以这种方式工作，其中左列表第1个函数应用到右列表第一个值，第二个函数应用到第二个值，依次类推
  - 这可能产生一个带有两个元素的列表，也就是`[4,4]`，你也可以把它看作`[1+3, 2*2]`
- 因为一种类型不能具有同一类型类的两个实例，`ZipList a`类型被引入了，它具有一个构造器`ZipList`，其只有一个字段，这个字段是一个列表
  - 这是实例
    ``` Haskell
    instance Applicative ZipList where
      pure x = ZipList (repeat x)
      ZipList fs <*> ZipList xs = ZipList (zipWith (\f x -> f x) fs xs)
    ```
    *这是怎么回事？repeat会产生无穷重复的序列*
  - `<*>`做了正如我们说的事情，它将第一个函数应用到第一个值，第二个函数应用到第二个值，类推
    - 这是通过`zipWith (\f x -> f x) fs xs`来做到的
    - 因为`zipWith`的工作方式，结果列表将会和两列表的较短者一样长
  - `pure`在这里也很有趣，它获取了一个值，并且将其放置在列表中，并且只是把值复制了无穷次
    - 这有点令人困惑因为我们说过`pure`应该将值放在一个最小的可产生那个值的上下文中
      *懂了*
    - 你可能认为某东西的无穷列表肯定不是最小的（minimal）
    - 但是它对于zip列表（zip lists）来说很合理，因为它必须在每一个位置产生值
    - 这也满足了`pure f <*> xs`应该等于`fmap f xs`的规则
    - 如果`pure 3`只是返回`ZipList [3]`，`pure (*2) <*> ZipList [1,5,10]`结果将会是`ZipList [2]`
- zip列表如何用应用风格工作呢？我们看看吧
  - 哦，因为`ZipList a`类型并不具有Show实例，所以我们只能使用`getZipList`函数来从zip列表中提取出原始的列表（a raw list）
    *现在不是了，目前在ghci上测试可以直接打印*
    ``` Haskell
    -- 在ghci中需要使用 :m Control.Applicative 来加载所需的模块（module）
    ghci> getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [100,100,100]
    [101,102,103]
    -- 直接(+) <$> ZipList [1,2,3] <*> pure 100 也行
    ghci> getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [100,100..]
    [101,102,103]
    ghci> getZipList $ max <$> ZipList [1,2,3,4,5,3] <*> ZipList [5,3,1,2]
    [5,3,3,4]
    ghci> getZipList $ (,,) <$> ZipList "dog" <*> ZipList "cat" <*> ZipList "rat"
    [('d','c','r'),('o','a','a'),('g','t','t')]
    -- (,)的作用是把两个元素放进二元组；(,,)的作用是把三个元素放进三元组
    ```

注意：`(,,)`函数和`\x y z -> (x,y,z)`一样，同样的`(,)`函数和`\x y -> (x,y)`一样

除了`zipWith`，标准库还有这些函数：`zipWith3`、`zipWith4`，一直到7
- `zipWith`获取一个函数，其获取两个参数，然后用该函数将两列表压缩到一起
- `zipWith3`获取一个函数，其获取三个参数……类推
- 通过以应用风格使用zip列表，我们不需要单独针对我们想要压缩到一起的列表的每种个数使用zip函数，我们只需要使用应用风格，用一个函数来压缩任意数量的列表，这太酷了

</div>
<div class="sheet-wrap"><div class="sheet-caption">Control.Applicative中的liftA2</div>


`Control.Applicative`定义了一个函数，`liftA2`
- 类型为`liftA2::(Applicative f) => (a -> b -> c) -> f a -> f b -> f c`
- 定义
  ``` Haskell
  liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
  liftA2 f a b = f <$> a <*> b
  ```
- 没什么特别的，它只是把一个函数应用到了两个函子之间，隐藏了我们熟悉的应用风格
- 我们看这个的原因是，它清晰地展示了为什么应用函子比普通的函子更加强大
- 对于普通函子，我们只能对一个函子映射函数
- 但是对于应用函子，我们可以将一个函数应用到数个函子之间

把这个函数的类型看作`(a -> b -> c) -> (f a -> f b -> f c)`也很有趣
- 当我们像这样看它，我们可以说`liftA2`拿到一个普通的双参函数并且将其提升为一个操作两个函子的函数

有一个有趣的概念
- 我们可以拿两个应用函子并且将其组合为一个应用函子，在内部具有两个应用函子的列表作为结果
- 例如，我们有`Just 3`和`Just 4`，我们假定第二个内部有一个但列表，因为这很容易实现
  ``` Haskell
  ghci> fmap (\x -> [x]) (Just 4)
  Just [4]
  ```
- 我们说我们有`Just 3`和`Just [4]`，我们如何获得`Just [3,4]`？简单
  ``` Haskell
  ghci> liftA2 (:) (Just 3) (Just [4])
  Just [3,4]
  ghci> (:) <$> Just 3 <*> Just [4]
  Just [3,4]
  ```
- 记住，`:`是一个函数，获取一个元素以及一个列表，并且返回一个新列表，带有一开始的元素
- 既然我们有了`Just [3,4]`，我们可以把它和`Just 2`组合到一起来产生`Just [2,3,4]`吗？当然
- 似乎我们可以组合任意数量的应用函子，组成一个应用函子，其具有这些应用函子内部的结果的列表

</div>
<div class="sheet-wrap"><div class="sheet-caption">sequenceA函数</div>


让我们尝试实现一个函数，其获取一列应用函子并且返回一个应用函子，具有一个列表作为它的结果值，我们叫它`sequenceA`
  ``` Haskell
  sequenceA :: (Applicative f) => [f a] -> f [a]
  sequenceA [] = pure []
  sequenceA (x:xs) = (:) <$> x <*> sequenceA xs
  ```
  啊，递归！
  - 首先，我们看看类型，它将会将一应用函子的列表转换为带有列表的应用函子
  - 因此，我们可以为边界条件（edge condition）铺垫一些准备工作（groundwork）
  - 如果我们想把一空列表转换为列表作为结果的函子，我们只需要把空列表放到默认上下文中即可
  - 现在到了递归
  - 如果我们具有一个列表，它有头部（head）和尾部（tail）（记住，x是应用函子，而xs是它们的列表），我们对尾部调用`sequenceA`，它结果是带有列表的应用函子，然后，我们只需要把应用函子`x`里的值前追加（prepend）到代列表的应用函子中，完事！
- 所以如果我们做`sequenceA [Just 1, Just 2]`
  - 也就是`(:) <$> Just 1 <*> sequenceA [Just 2]`
  - 就等于`(:) <$> Just 1 <*> ((:) <$> Just 2 <*> sequenceA [])`
  - 啊！我们知道`sequenceA []`作为`Just []`结束，所以这个表达式现在是`(:) <$> Just 1 <*> ((:) <$> Just 2 <*> Just [])`
  - 也就是`(:) <$> Just 1 <*> Just [2]`
  - 就是`Just [1,2]`！

*其实sequanceA已经有了，甚至不限于列表*

另一种实现`sequanceA`的方法是用`fold`
``` Haskell
sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA = foldr (liftA2 (:)) (pure [])
```
- 记住，很多时候任何遍历（go over）逐元素遍历列表以及顺便积累结果的函数都可以用`fold`实现
- *关于遍历和积累的过程（略）* \
  *我明白了，实际的sequenceA的类型签名是`sequenceA :: (Traversable t, Applicative f) => t (f a) -> f (t a)`，这里"traversable"指的就是可遍历*

让我们玩一点例子（ *已玩，略* ）
``` Haskell
ghci> sequenceA [[1,2,3],[4,5,6]]
[[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]
ghci> sequenceA [[1,2,3],[4,5,6],[3,4,4],[]]
[]
-- 关于列表的例子很令人费解，虽然可以用上面的过程推出来
-- 不过建立的直觉仍然是有趣的
```
- *各种例子的分析（略）*

</div>
<div class="sheet-wrap"><div class="sheet-caption">示例：相同的输入应用到一列函数</div>


当我们想要把相同的输入喂到一列函数中，使用`sequenceA`就会很酷
- 例如，我们有一个数字，想知道它是否符合列表中的所有断言（predicates）
- 一种方式是可以是这样
  ``` Haskell
  ghci> map (\f -> f 7) [(>4),(<10),odd]
  [True,True,True]
  ghci> and $ map (\f -> f 7) [(>4),(<10),odd]
  True
  ```
- `sequenceA [(>4),(<10),odd]`创造了一个函数
  - 将会获取一个数字然后将其喂入`[(>4),(<10),odd]`中所有的断言，然后返回一列布尔值
  - 他把一个具有`(Num a) => [a -> Bool]`类型的列表转换成具有`(Num a) => a -> [Bool]`类型的函数，很漂亮，是吧？
- 因为列表是同质的，列表中所有的函数当然都只能是相同类型的函数
  - 你不能有一个像`[ord,(+3)]`这样的列表，因为`ord`获取一个字符然后返回一个数字
  - 而`(+3)`获取一个数字并且返回一个数字

当用在`[]`上，`sequenceA`获取一列表列表并且返回一列表列表
- 它实际上创造了列表元素的所有可能的组合
- 为了演示，下面代码中，上面的用`sequenceA`下面的用列表推导式 \
  *略*
- 这可能有点难以理解，但是如果你玩一会的话，你就会看到它是如何工作的
  - 让我们假定我们在做`sequenceA [[1,2],[3,4]]`
  - 为了看这是怎么会是，我们使用`sequenceA (x:xs) = (:) <$> x <*> sequenceA xs`这个`sequenceA`的定义，并且使用边界条件`sequenceA [] = pure []`
  - 你不需要跟着这个求值，但是如果你难以想象`sequenceA`怎么处理列表的列表的，它可能会帮助你，因为它有一点绕（a bit mind-bending）
  1. 我们从`sequenceA [[1,2],[3,4]]`开始
  2. 这求值为`(:) <$> [1,2] <*> sequenceA [[3,4]]`
  3. 进一步求值内部的`sequenceA`，我们得到`(:) <$> [1,2] <*> ((:) <$> [3,4] <*> sequenceA [])`
  4. 我们已经到达了边界条件，所以现在是`(:) <$> [1,2] <*> ((:) <$> [3,4] <*> [[]])`
  5. 现在，我们求值`(:) <$> [3,4] <*> [[]]`的部分，它对左列表的任何可能值与有列表的任何可能值使用了`:`，结果是`[3:[],4:[]]`，也就是`[[3],[4]]`，所以现在我们有`(:) <$> [1,2] <*> [[3], [4]]`
  6. 现在，`:`被用于左列表的任意可能值以及右列表的任何可能值，结果是`[1:[3],1:[4],2:[3],2:[4]]`，也就是`[[1,3],[1,4],[2,3],[2,4]]`
- 做`(+) <$> [1,2] <*> [4,5,6]`
  - 结果是非确定性运算（non-deterministic computation）x+y，其中x从`[1,2]`拿到每一个值，而y从`[4,5,6]`拿到每一个值
  - 我们把它表达为一个列表，其包含了所有可能的值
  - 相思地，当我们做`sequence [[1,2],[3,4],[5,6],[7,8]]`，结果是一个非确定性运算`[x,y,z,w]`，其中x从`[1,2]`中拿到每一个结果，y从`[3,4]`中拿到每一个结果，类推
  - 为了表达那个非确定性计算的结果，我们使用列表，其中列表中的每一个元素是一个可能列表
  - 这就是为什么结果是列表的列表

</div>
<div class="sheet-wrap"><div class="sheet-caption">sequenceA用于I/O行为</div>


当用在I/O行为时，`sequenceA`和`sequence`是同一回事！
- 它获取I/O行为的列表并且返回一个I/O行为，将会运行这些行为的每一个，并且将这些I/O行为的结果的列表作为结果
- 这是因为，为了把`[IO a]`值转换成`IO [a]`值，为了创建一个I/O行为，其在被执行时产生结果的列表，所有这些I/O行为必须被顺序执行（have to be sequenced），从而它们会在求值被强行发生时（when evaluation is forced）一个接一个地执行
- 你不能没有执行一个I/O行为时就得到结果
``` Haskell
ghci> sequenceA [getLine, getLine, getLine]
heyh
ho
woo
["heyh","ho","woo"]
```

和普通的函数一样，应用函子有数条规则
- 最重要的一个是我们已经提到的，就是`pure f <*> x = fmap f x`所说的，作为联系你可以对我们在这章遇到的某些应用函子证明这条规则
- 其他的规则是
  - `pure id <*> v = v` \
    *pure id :: Applicative f => f (a -> a)*
  - `pure (.) <*> u <*> v <*> w = u <*> (v <*> w)` \
    *无法理解*
  - `pure f <*> pure x = pure (f x)` \
    *应用函子的用途*
  - `u <*> pure y = pure ($ y) <*> u` \
    *不理解这个*
- 我们现在不会详细展开这些规则，因为它可能会占用很多页数，它可能也有点无聊，但是如果你很喜欢这项任务，你可以进一步看看它们，看它们能否对一些例子成立

</div>
<div class="sheet-wrap"><div class="sheet-caption">小结</div>


总而言之，应用函子不仅仅是有趣，它们也很有用，因为它们允许组合不同的计算
- 例如
  - I/O计算
  - 非确定性计算
  - 可能失败的计算，等等
  通过使用应用风格
- 仅仅是通过使用`<$>`和<*>，我们可以使用普通的函数来一致地（uniformely）操作任何数量的应用函子，并且利用每一个的语义（take advantage of the semantics of each one）

</div>
<h2 id="03774CDA">newtype关键字</h2>
<div class="sheet-wrap"><div class="sheet-caption">本节预告</div>


目前为止，我们学习了
- 如何通过`data`关键字制造我们自己的代数数据类型
- 也学习了如何用`type`关键字给已有的类型赋上同义词

本小节，我们将会看
- 如何用`newtype`关键字从已有的数据类型制造新的类型
- 以及从一开始，为什么我们想要这样做

</div>
<div class="sheet-wrap"><div class="sheet-caption">回顾如何用两种方式将列表作为应用函子</div>


前面的小节，我们看到列表类型实际上有更多方式成为应用函子
- 一种方式是让`<*>`从左参数的列表中获取每个函数，并且应用到右边列表的每一个值中，返回来自左边列表函数应用到右边列表的值的每一个可能组合
- 第二种方式是获取`<*>`左边的第一个元素并将其应用到有哦便的第一个值，第二个函数到第二个值，类推。最终，这有点像把两个列表压缩到一起
- 但是列表已经是Applicative的实例，所以我们怎么让列表以第二种方式作为Applicative的实例的呢？
- 如果你记得，我们说`ZipList`是一种为此引入的类型，它有一个类型构造器，`ZipList`，只有一个字段。我们把列表包裹在那个字段中
- 然后`ZipList`被作为了Applicative的一个实例，从而当我们想要把列表以压缩方式作为应用函子的时候，我们只是把它们包裹在`ZipList`构造器里面
- 然后一旦我们完成，就用`getZipList`把它们的包裹剥掉

</div>
<div class="sheet-wrap"><div class="sheet-caption">newtype的用法：包裹住某一类型，把它表示成另一种类型</div>


想想可能如何编写`ZipList`类型的data声明
1. 一种方式可以是
   ``` Haskell
   data ZipList a = ZipList [a]
   ```
   - 一种类型只有一个类型构造器，那个类型构造器只有一个字段，并且是某些东西的列表
2. 我们可能也想要使用`record`语法，因此我们自动获取函数，从`ZipList`里面提取出列表
   ``` Haskell
   data ZipList a = ZipList { getZipList :: [a] }
   ```
- 这看起来很好，我们已经有了两种方式来把一个已有的类型作为一个类型类的实例

*为什么这里自动产生了一个函数`getZipList`？* \
*这是记录选择器，与record中的“字段”同名，是自动生成的函数*

Haskell里的`newtype`关键字正是为了这些情况的
- 也就是当我们想要拿到一种类型并且将其包裹在某种东西里面，来把它表示为另一种类型
- 实际的库中，`ZipList a`像这样定义
  ``` Haskell
  newtype ZipList a = ZipList { getZipList :: [a] }
  ```
- 没有使用data关键字，而是使用了newtype关键字，为什么？
  - 一个原因是（for one），`newtype`更快
    - 如果你使用data关键字来包裹一个类型，你的程序运行时有一些有关包裹和去包裹的开销（overhead）
    - 但是如果你使用newtype，Haskell知道你只是用它包裹一个已有类型变成一个新的类型（顾名思义）
    - 因为你想让它内部相同但是具有不同类型
    - 知道了这个，一旦Haskell解析了那个值是什么类型，它可以去除包裹和解包裹
- 那为什么不直接总是使用newtype而不是data？
  - 当你用newtype关键字从已有类型创建一个新类型，你只能有一个值构造器，那个值构造器之后一个字段
  - 但是用data，你可以制作具有数个值构造器的数据类型，每个构造器可以有另个或更多字段
- 我们也对newtype使用derving关键字，就如同data一样
  - 可以把实例派生到Eq、Ord、Enum、Bounded、Show和Read
  - 如果我们把实例派生到一个类型类，我们包裹的类型必须从一开始也在那个类型类
  - 这说得通，因为newtype只是包裹了一个已有类型
- 例子
  ``` Haskell
  newtype CharList = CharList { getCharList :: [Char] } deriving (Eq, Show)
  ```
  - 在这个特定的newtype中，值构造器具有下列类型
    ``` Haskell
    CharList :: [Char] -> CharList
    ```
    它获取CharList值并且转换成[Char]值
  - 你可以把它看成包裹和解包裹，也可以看成把值从一个类型转换为另一种类型

</div>
<h3 id="5A4EA31E">使用newtype来创造类型类实例</h3>
<div class="sheet-wrap"><div class="sheet-caption">从Maybe作为函子开始</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">想让元组作为函子，并且fmap将函数作用到第一个元素上</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">解决办法：用newtype来“交换”两个类型参数</div>

</div>
<h3 id="87E25935">关于newtype惰性</h3>
<div class="sheet-wrap"><div class="sheet-caption">newtype更快更惰性</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">首先说明：Haskell的惰性与undefined</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">示例：data定义的类型与模式匹配</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">示例：newtype定义的类型与模式匹配</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">总结：data和newtype其实是两种机制</div>

</div>
<h3 id="97C34E42">type vs. newtype vs. data</h3>
<div class="sheet-wrap"><div class="sheet-caption">type的使用情形：同义词混用</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">newtype的使用情形：包裹已有类型</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">data的使用情形：随心所欲自定义类型</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">总结说明</div>

</div>
<h2 id="3E57A703">Monoids</h2>
<div class="sheet-wrap"><div class="sheet-caption">类型类的回顾</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">1对于*、[]对于++的共同特性</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">两种操作另一个共同性：结合性</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">Haskell世界里的monoid</div>


通过注意到和写下这些特性，我们碰巧发现了幺半群（monoids）
- 幺半群就是你有一个结合性（associative）的双参函数，以及一个值，它对于那个函数表现为同一（identity）
- 当某东西对于一个函数表现为同一，它表示当被那个函数和另外某个值一起调用，结果总是等于那个另外值
- 1对于*是同一、[]对于++是同一
- Haskell世界中有很多其它的幺半群有待发现，这就是为什么Monoid类型类存在

</div>
<div class="sheet-wrap"><div class="sheet-caption">Monoid类型类定义以及解释</div>


看看类型类如何定义的
``` Haskell
class Monoid m where
  mempty :: m
  mappend :: m -> m -> m
  mconcat :: [m] -> m
  mconcat = foldr mappend mempty
```
- Monoid类型类定义在`Data.Monoid`中

让我们花点时间好好熟悉它
- 我们看到只有具体类型可以作为Monoid的实例，因为类型类定义中的m没有获取任何类型参数
  - 这和Functor和Applicative有所不同，它要求实例是类型构造器，其获取一个参数
- 第一个函数是mempty
  - 它不是一个函数，因为它不获取参数
  - 它是一个多态常量（polymorphic constant），有点像Bounded里面的minBound
  - mempty表示对于一个特定的单位值
- 接下来，我没有mappend
  - 你可能猜到了，是一个双参函数
  - 它获取同一类型的两个参数并且返回同为那个类型的值
  - 值得注意的是我们在用某种方式追加两个东西
  - 尽管++确实获取两个列表并且将一个追加到另一个上，*并不真的做任何追加，它只是将两个数字乘起来
  - 当我们遇到Monoid的其它实例时，我们将会看到它们大多数也不会追加值
  - 所以请不要想象成追加，只要想象mappend是一个双参函数，获取两个幺半群值然后返回第三个
- 这个类型类定义的最后一个函数是mconcat
  - 它获取一列幺半群值并且通过在列表元素之间做mappend将它们reduce成单个值
  - 它有一个默认实现，只是获取mempty作为初始值，然后用mappend从右边折叠列表
  - 因为默认实现对于绝大多数实例很好，我们从现在开始不会过多地关注mconcat
  - 当我们让一个类型成为Monoid的一个实例时，只要实现mempty和mappend就足够了
  - mconcat在这里的原因只是因为，对于某些类型，可能有更加高效的方式实现mconcat
  - 但是对于大多数实例默认的实现就已经很好了

</div>
<div class="sheet-wrap"><div class="sheet-caption">简要看看幺半群规则</div>


- 规则的意义：
  - 有可能创造Monoid实例不遵守规则
  - 但是这样的实例不再有用，因为当使用Monoid类型类时，我们依靠这个实例像幺半群一样运作
  - 否则，意义何在？
- 遵循以下规则
  1. mempty `mappend` x = x
  2. x `mappend` mempty = x
  3. (x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)
  - 前两个是说mempty必须对于mappend表现为identity
  - 第三个说mappend必须要是结合性的，因此我们将数个幺半群值reduce为一个值时使用的mappend顺序并不重要
  - Haskell不会强行保证这些规则，所以我们作为程序员需要小心我们的实例确实遵循它们

</div>
<h3 id="4FABC9ED">列表是幺半群</h3>
<div class="sheet-wrap"><div class="sheet-caption">实例定义</div>


是的，列表是幺半群
- 我们看到了，++函数和空列表[]组成了一个幺半群
- 实例非常简单
  ``` Haskell
  instance Monoid [a] where
    mempty = []
    mappend = (++)
  ```
- 列表是Monoid类型类，无论它们包含的元素类型
- 注意我们写的`instance Monoid [a]`而不是`instance Monoid []`，因为Monoid对于一个实例要求一个具体类型

</div>
<div class="sheet-wrap"><div class="sheet-caption">测试</div>


*略*

</div>
<div class="sheet-wrap"><div class="sheet-caption">性质</div>


幺半群规则确实对于列表实例成立
- 当我们有数个列表，我们把它们mappend（或者++）在一起，我们先做哪个并不影响，因为它们最终都会结合到一起
- 同样的空列表表现为幺元（identity），所以一切都很好
- 注意幺半群不要求``a `mappend` b``等于``b `mappend` a``
- 乘法`3 * 5`等于`5 * 3`只是乘法的性质，它并不对所有的（实际上，大多数）幺半群成立

</div>
<h3 id="DCD87A48">乘和积</h3>
<div class="sheet-wrap"><div class="sheet-caption">有两种方式让数字变成幺半群</div>

</div>
<div class="sheet-wrap"><div class="sheet-caption">Data.Monoid模块</div>


Data.Monoid模块导出两种类型，也就是Product和Sum
- Product定义为这样
  ``` Haskell
  newtype Product a = Product { getProduct :: a }
    deriving (Eq, Ord, Read, Show, Bounded)
  ```
  - 简单，单个类型参数的newtype包裹器，一些派生的实例
  - 这个Monoid的实例有点像这样
    ``` Haskell
    instance Num a => Monoid (Product a) where
      mempty = Product 1
      Product x `mappend` Product y = Product (x * y)
    ```
  - 为了将`Pruduct a`作为一个幺半群使用，我们必须做一些newtype包裹和去包裹
    ``` Haskell
    ghci> getProduct $ Product 3 `mappend` Product 9
    27
    ghci> getProduct $ Product 3 `mappend` mempty
    3
    ghci> getProduct $ Product 3 `mappend` Product 4 `mappend` Product 2
    24
    ghci> getProduct . mconcat . map Product $ [3,4,2]
    24
    ```
  - 这是Monoid类型类的一个很好的示例，但是没有人在头脑清醒的情况下会用这种方式求数字的积，而是应该写`3 * 9`和`3 * 1`
  - 但是稍后，我们会看到这些看起来可能很简单的Monoid类型类可以变得方便
- Sum定义得像是Product，实例也是相似
  - 我们用相同的方式使用
    ``` Haskell
    ghci> getSum $ Sum 2 `mappend` Sum 9
    11
    ghci> getSum $ mempty `mappend` Sum 3
    3
    ghci> getSum . mconcat . map Sum $ [1,2,3]
    6
    ```

</div>
<h3 id="874DF295">Any和All</h3>
<div class="sheet-wrap"><div class="sheet-caption">布尔作为幺半群的两种方式</div>


另一种可以以两种截然不同，但同样可行方式表现为幺半群的类型，就是Bool
- 第一种方式是让or函数表现为双参函数，让False表现为幺元
  - or在逻辑中工作的方式是…… *略*
  - Any的newtype构造器是Monoid的一个实例，定义为
    ``` Haskell
    newtype Any = Any { getAny :: Bool }
      dering (Eq, Ord, Read, Show, Bounded)
    ```
  - 这个实例看起来这样
    ``` Haskell
    instance Monoid Any where
      mempty = Any False
      Any x `mappend` Any y = Any (x || y)
    ```
  - 它叫作Any的理由是，如果两个之中任何一个为真，``x `mappend` y``将会为真，即使是三个或者更多的Any包裹的Bool被mappend在一起，结果仍然会在任何一个值为真的时候保持为真
    ``` Haskell
    ghci> getAny $ Any True `mappend` Any False
    True
    ghci> getAny $ mempty `mappend` Any True
    True
    ghci> getAny . mconcat . map Any $ [False, False, False, True]
    True
    ghci> getAny $ mempty `mappend` mempty
    False
    ```
- Bool作为Monoid实例的另一种方式是反过来，让&&作为双参函数，并且让True作为幺元
  - 只有两个参数都为True，逻辑才会返回True
  - 这是newtype声明，没什么神奇的
    ``` Haskell
    newtype All = All { getAll :: Bool }
      deriving (Eq, Ord, Read, Show, Bounded)
    ```
  - 这是实例
    ``` Haskell
    instance Monoid All where
      mempty = All True
      All x `mappend` All y = All (x && y)
    ```
  - 当我们All类型所有的值都mappend之后，结果为True仅当所有在mappend操作中使用的值都为True
    ``` Haskell
    ghci> getAll $ mempty `mappend` All True
    True
    ghci> getAll $ mempty `mappend` All False
    False
    ghci> getAll . mconcat . map All $ [True, True, True]
    True
    ghci> getAll . mconcat . map All $ [True, True, False]
    False
    ```


</div>
<div class="sheet-wrap"><div class="sheet-caption">小结</div>


- 正如乘法和加法，我们通常显式地声明双参函数，而不是将它们包裹在newtype里面然后用mappend和mempty
- mconcat看起来对Any和All有用，但是通常使用or和and函数更简单，它获取Bool列表，然后返回True，分别当它们任何（any）一个为True或者它们所有（all）为True

</div>
<h3 id="9ECFE9F0">排序幺半群</h3>
<div class="sheet-wrap"><div class="sheet-caption">回顾Ordering类型</div>


回顾Ordering类型
- Ordering类型用于当比较事物时的结果
- 它可以有三个值
  1. LT
  2. EQ
  3. GT
  ``` Haskell
  ghci> 1 `compare` 2
  LT
  ghci> 2 `compare` 2
  EQ
  ghci> 3 `compare` 2
  ```
- 对于列表、数字和布尔值，发现幺半群只是在于观察已经存在的通常使用的函数，然后看它们是否表现出某种幺半群行为
- 对于Ordering，我们必须看得更加认真一点来发现一个幺半群，但是最终表明它的Monoid实例和我们目前为止看到的一样符合直觉，并且非常有用
  ``` Haskell
  instance Monoid Ordering where
    mempty = EQ
    LT `mappend` _ = LT
    EQ `mappend` y = y
    GT `mappend` _ = GT
  ```
- 这个实例被设定像这样
  - 当我们mappend两个Ordering值，左边的被保留
  - 除非左边的值是EQ，情况下右边的值是结果
- 一开始，这个可能看起来有点随意，但是它实际上类似于（resemble）我们按字母顺序比较单词
  - 我们比较最开始的两个字母，如果它们不同，我们已经可以决定哪个单词排在字典的前面
  - 然而，如果最开始的两个字母相等，我们继续比较下一对字母并且重复过程
  - 例如，如果我们要字典序比较"ox"和"on"…… \
    *略*
- 需要注意Ordering的Monoid实例中，``x `mappend` y``不等于``y `mappend` x``

</div>
<div class="sheet-wrap"><div class="sheet-caption">示例：字符串先比较长度后字典序</div>


我们假设
- 你在编写一个函数获取两个字符串，比较长度返回Ordering
- 但是如果字符串长度相同，并不直接返回EQ，我们想要字典序比较它们

一种实现方式
``` Haskell
lengthCompare :: String -> String -> Ordering
lengthCompare x y = let a = length x `compare` length y
                        n = x `compare` y
                    in if a == EQ then b else a
```
- 我们命名比较长度的结果为a，字典序比较的结果为b，然后看长度是否相等，我们返回字典序

但是通过利用我们对Ordering作为幺半群的理解，我们将函数重写为这种更简单的形式
``` Haskell
import Data.Monoid

lengthCompare :: String -> String -> Ordering
lengthCompare x y = (length x `compare` length y) `mappend`
                    (x `compare` y)
```
- 我们可以试试这个
  ``` Haskell
  ghci> lengthCompare "zen" "ants"
  LT
  ghci> lengthCompare "zen" "ant"
  GT
  ```
- 记住，当我们使用mapoend，左边的值永远保留，除非它是EQ，这种情况下右边的被保留
- 这就是为什么我们把更重要的标准放在左边的参数
- 如果我们想要扩展这个函数，让其也比较元音（vowel）个数，然后把它设置为第二重要的标准，我们只需要像这样修改
  ``` Haskell
  import Data.Monoid

  lengthCompare :: String -> String -> Ordering
  lengthCompare x y = (length x `compare` length y) `mappend`
                      (vowels x `compare` vowels y) `mappend`
                      (x `compare` y)
    where vowvels = length . filter (`elem` "aeiou")
  ```
- 我们创建了一个帮助函数，它获取字符串并且告诉我们有几个元音，首先通过滤出"aeiou"中的字符，然后对其应用length
- 示例
  ``` Haskell
  ghci> lengthCompare "zen" "anna"
  LT
  ghci> lengthCompare "zen" "ana"
  LT
  ghci> lengthCompare "zen" "ann"
  GT
  ```

</div>
<div class="sheet-wrap"><div class="sheet-caption">小结</div>


Ordering幺半群非常酷
- 因为它允许我们很容易地通过许多不同的标准比较东西
- 然后把这些标准本身排顺序，从最重要的到最不重要的

</div>
<h3 id="2A67F49F">Maybe幺半群</h3>
<h3 id="43521602">使用幺半群折叠数据结构</h3>
<div class="sheet-wrap"><div class="sheet-caption">概述：借助幺半群定义fold函数</div>


- 还有一种更有趣的，让幺半群工作的方式，就是让它们帮助我们对不同的数据结构定义fold
- 目前为止，我们只是对列表做了fold，但是列表也不是唯一可以折叠的数据结构
- 我们可以在几乎任何数据结构上定义fold
- Tree尤其适合折叠（Trees especially lend themselves well to folding）


</div>
<div class="sheet-wrap"><div class="sheet-caption">Foldable类型类</div>


因为这么多数据结构很适合折叠，引入了Foldable类型类
- 很类似于Functior是可以被映射于的东西，Foldable是可以被折叠的东西
- 它可以在Data.Foldable中被发现，因为它导出的函数的名称和Prelude中的名称冲突，最好有限导入（imported qualified）
  ``` Haskell
  import qualified Foldable as F
  ```
- 为了省得我们键盘敲击，我们选择把它有限导入为F
- 好的，所以这个类型类的某些函数的定义是什么？是，foldr、foldl、foldr1、foldl1
- 我们已经知道了这些函数，有什么新奇的吗？
- 让我们比较Foldable的foldr和Prelude的foldr的类型，来看它们如何不同
  ``` Haskell
  ghci> :t foldr
  foldr :: (a -> b -> b) -> b -> [a] -> b
  ghci> :t F.foldr
  F.foldr :: (F.Foldable t) => (a -> b -> b) -> b -> t a -> b
  ```
  - Data.Foldable的foldr接受任何可以被折叠的类型，不只是列表！
  - 如你所期望的，两种foldr函数对列表做同样的事情
- 其它哪些数据结构支持fold？
  - 有Maybe
    ``` Haskell
    ghci> F.foldl (+) 2 (Just 9)
    11
    ghci> F.foldl (||) False (Just True)
    True
    ```
  - 但是折叠Maybe值非常不有趣，因为当它折叠时，如果它是Just值它只是表现为只有一个元素的列表；如果它是Nothing值，它表现为空列表

</div>
<div class="sheet-wrap"><div class="sheet-caption">树作为Foldable</div>


还记得“创造我们自己的类型年和类型类”章节里面的树数据结构吗？
- 定义
  ``` Haskell
  data Tree a = Empty | Node a (Tree a) (Tree a) derving (Show, Read, Eq)
  ```
- 我们说一个树要么是不包含任何值的空树，要么是一个包含一个值以及另外两个树的节点
- 通过定义，我们将其作为了Functor的一个实例，并且借此我们获得了对其fmap的能力
- 现在，我们幺让它作为Foldable的一个实例，因此我们可以获得将其折叠的能力
- 一种让类型构造器变成Foldable的一个实例的方式就是直接帮它实现foldr
- 但是另一种简单得多的方式，就是实现foldMap函数，它也是Foldable类型类的一部分
- foldMap函数具有下列类型
  ``` Haskell
  foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
  ```
  - 它的第一个参数是一个函数，获取我们可折叠结构包含的类型的类型的值（这里记为a），然后返回一个幺半群值
  - 它的第二个参数是一个可折叠结构，包含类型a的一个值
  - 它将函数映射于可折叠结构，从而产生一个包含哪个幺半群值的可折叠结构
  - 然后，通过在这些幺半群值之间做mappend，它将它们都结合为单个幺半群值
- 函数可能现在听起来有点奇怪，但是我们会看到它非常容易实现
- 另一个很酷的事情是，我们的类型成为Foldable实例唯一需要的就是实现这个函数
- 所以如果我们为某类型实现了foldMap，我们免费获得了那个类型的foldr和foldl！

让Tree作为Foldable的一个实例
``` Haskell
instance F.Foldable Tree where
  foldMap f Empty = mempty
  foldMap f (Node x l r) = F.foldMap f l `mappend`
                           f x           `mappend`
                           F.foldMap f r
```
- 我们像这样再想想
  - 如果提供我们一个函数，从我们的树中获得一个元素并且返回一个幺半群值，我们如何把我们的整个树reduce成单个幺半群值呢？
  - 当我们对我们的树做fmap时，我们应用了一个函数，将其映射于节点然后我们递归地将函数映射于左子树和右子树
  - 这里，我们的任务不仅是映射一个函数，还有通过mappend将结果结合为单个幺半群值
- 首先我们考虑空树的情况——一个忧郁且孤独的树，没有值也没有子树
  - 它不包含任何我们可以给我们的幺半群制造函数的值，所以我们只能说我们的树是空的
  - 它变成的幺半群值是mempty
- 非空节点的情况稍微更加有趣
  - 它包含了两个子树和一个值
  - 这种情况下，我们递归地将相同的函数f给foldMap到左右子树
  - 记住，我们的foldMap结果是单个幺半群值
  - 我们也将我们的函数f应用到节点的值
  - 现在我们有三个幺半群值，我们必须把他们整合成一个值
  - 出于我们使用mappend的目的，自然地左子树先来，然后是节点值，接着是右子树
- 注意我们不需要提供获取值并且返回幺半群值的函数，我们将那个函数作为foldMap的参数接收，我们所有需要决定的事情就是在哪里应用那个函数，以及如何将来自它的结果幺半群结合起来

</div>
<div class="sheet-wrap"><div class="sheet-caption">示例：将树折叠</div>


考虑这个树
``` Haskell
testTree = Node 5
             (Node 3
               (Node 1 Empty Empty)
               (Node 6 Empty Empty)
             )
             (Node 9
               (Node 8 Empty Empty)
               (Node 10 Empty Empty)
             )
```

我们可以做任何对列表做的折叠 \
*这里为什么没有转换幺半群值和类型呢？* \
*有可能是先转换成列表实现的*
``` Haskell
ghci> F.foldl (+) 0 testTree
42
ghci> F.foldl (*) 1 testTree
648800
```
- 同样地，foldMap不仅仅创建Foldable的新实例有用，它也对于把我们的结构reduce成单个幺半群值很方便
- 例如，我们想要知道我们树的任何值等于3,我们可以做：
  ``` Haskell
  ghci> getAny $ F.foldMap (\x -> Any $ x == 3) testTree
  True
  ```
- 这里，`\x -> Any $ x == 3`是一个函数，其获取一个数字然后返回一个幺半群值，也就是一个Bool包裹在Any里面；foldMap将这个函数应用到我们的树中的每一个元素然后用mappend将结果幺半群reduce到一个简单的幺半群
- 如果我们这样做
  ``` Haskell
  ghci> getAny $ F.foldMap (\x -> Any $ x > 15) testTree
  False
  ```
  我们树中所有的节点将会在lambda中的函数应用到它们之后保留Any False值
- 通过foldMap和`\x -> [x]`函数，我们也可以很容易地将我们的树变成列表
  ``` Haskell
  ghci> F.foldMap (\x -> [x]) testTree
  [1,3,6,5,8,9,10]
  ```

酷的是，这些伎俩不限于树，它们同样适用任何Foldable的实例

</div>
<h1 id="3A9E3C0F">一把Monad</h1>
</div>
