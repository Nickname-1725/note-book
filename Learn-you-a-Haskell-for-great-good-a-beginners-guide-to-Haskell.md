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
  - [Monoids](#3E57A703)
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

</div>
<h2 id="03774CDA">newtype关键字</h2>
<h2 id="3E57A703">Monoids</h2>
</div>
