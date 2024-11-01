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
<h2 id="C049B7D4">最棒(maximum awesome)</h2>
<h2 id="042EAF63">再来几个递归函数</h2>
<h2 id="3F53D80C">快，排序！</h2>
<h2 id="B36C606A">递归地思考</h2>
</div>
