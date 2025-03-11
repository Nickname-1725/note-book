<title>Real World Haskell</title>
<link rel="stylesheet" href="style.css"/>
<div class="tableofcontents">

- [开始吧](#67ACDC26)
  - [你的Haskell环境](#213D9F32)
  - [从ghci解释器开始](#BB522891)
  - [基本互动: 使用ghci作为计算器](#421FAE11)
  - [ghci中的命令行编辑](#A62C6E93)
  - [列表](#CF239E4D)
  - [字符串和字符](#2B42EE31)
  - [类型的第一步](#E88F1AFA)
  - [一个简单的程序](#0B039B90)
- [类型和函数](#03F17ADB)
  - [为什么关注类型？](#AB56454F)
  - [Haskell的类型系统](#C7BCFC3C)
    - [强类型](#C6951B28)
    - [静态类型](#75600D88)
    - [类型推断](#EDEC3D2F)
  - [类型系统有哪些精彩看点](#D0D63DCD)
  - [某些普通的基础类型](#A02889AD)
  - [函数应用](#580BC7BB)
  - [有用的复合数据类型：列表和元组](#5E5501A5)
  - [列表和元组之上的函数](#62BDECBD)
  - [函数类型和纯度](#2EFA9644)
  - [Haskell源文件，编写简单的函数](#500085E0)
  - [通过例子理解求值](#AFD083DB)
  - [Haskell里的多态](#CFD6F924)
  - [多于一个入参的函数的类型](#305F5B0C)
  - [为什么对纯度大惊小怪？](#9F997956)
  - [结论](#BEA4300C)
- [定义类型, 流水化函数(Streamlining Functions)](#5290106A)
  - [定义一个新的类型](#F2ECC125)
  - [类型同义词](#886741FC)
  - [代数数据类型](#332ADE50)
    - [元组、代数数据类型，各自何时使用](#E03DC9C3)
    - [其它语言中类似代数数据类型的概念](#3D759E10)
[结构体](#8A467BFE)
[枚举量](#352E9955)
[可区分联合体(The discriminated union)](#7A04F3E1)
  - [模式匹配](#CFC3BCB6)
  - [记录(Record)语法](#23E07770)
  - [参数化类型](#60EC8B7D)
  - [递归类型](#8B36D663)
  - [报告错误](#A4F2087E)
  - [介绍局部变量](#EAC83D5E)
  - [表达式中的越位规则(The Offside Rule)和空格](#CB7A1247)
  - [case表达式](#5C615E22)
  - [有关模式的常见新手错误](#D1500CB6)
  - [带卫语句的条件求值](#DEC48F52)
- [函数式编程](#D239C5CB)
- [编写库: 操作JSON数据](#0D9B6827)
- [使用类型类](#5A6899BA)
- [I/O](#8D899F74)
- [高效文件处理, 正则表达式, 文件名匹配](#3C1A8943)
- [I/O案例学习: 搜索文件系统的库](#6F6C42E3)
- [代码案例学习: 解析二进制数据格式](#0F4DB654)
- [测试和质量保证](#C1EBC21F)
- [条形码识别](#9340DED2)
- [数据结构](#A6E76B3B)
- [单子](#F4C53BAB)
- [用Monad编程](#F4772B41)
- [使用Parsec](#F12A3A21)
- [C接口: FFI](#7D84661C)
- [Monad变换器](#21C90078)
- [错误处理](#39320CA2)
- [Haskell的系统编程](#18B13E7F)
- [使用数据库](#7E10AD9C)
- [拓展示例: Web客户端编程](#6766C940)
- [用gtk2hs实现GUI编程](#45D3249C)
- [并发和多核编程](#3DDF7AE4)
- [特征提取和优化(Profiling and Optimization)](#35D657E1)
- [高级库设计: 建立一个布隆过滤器(Bloom Filter)](#63C5ECB9)
- [套接字(Sockets)和系统日志(Syslog)](#E09AEBE7)
- [软件事务内存](#C7BE9961)
</div>

<div class="main">
<h1 id="67ACDC26">开始吧</h1>
<div class="sheet-wrap"><div class="sheet-caption">本章介绍</div>


- 既然你读了这本书的前面章节（ *这些不做笔记* ），记住我们有时候会以有限、简化的形式介绍概念
- Haskell是一门深刻的语言，将一个给定的主题一次性呈现得面面俱到很可能是难以承受的
- 因为我们建立Haskell的一个坚实的基础，我们将会从这些初始的解释拓展延伸

</div>
<h2 id="213D9F32">你的Haskell环境</h2>
<h2 id="BB522891">从ghci解释器开始</h2>
<h2 id="421FAE11">基本互动: 使用ghci作为计算器</h2>
<h2 id="A62C6E93">ghci中的命令行编辑</h2>
<h2 id="CF239E4D">列表</h2>
<h2 id="2B42EE31">字符串和字符</h2>
<h2 id="E88F1AFA">类型的第一步</h2>
<div class="sheet-wrap"><div class="sheet-caption">ghci有关类型的特点</div>


尽管我们已经谈论了一点关于类型的东西，期间我们和`ghci`的互动没有很多于类型相关的考虑
- 我们没有告诉`ghci`我们在使用什么类型，而它大多数时候乐于接受我们的输入

命名
- Haskell要求类型名称以大写字母开头
- 变量名称必须以小写字母开头
- 读下去的时候记住这一点，会更容易理解命名

</div>
<div class="sheet-wrap"><div class="sheet-caption">让ghci告诉我们它在干什么</div>


我们开始探索类型世界的第一件事情，就是让`ghci`告诉我们它做做什么
- `ghci`有一个命令，`:set`，让我们改变它的几个默认行为
- 我们可以让它打印更多类型信息，如下
  ``` haskell
  ghci> :set +t
  ghci> 'c'
  'c'
  it :: Char
  ghci> "foo"
  "foo"
  it :: [Char]
  ```
- `+t`干的事情就是告诉`ghci`在打印表达式之后打印表达式的类型
- 输出里面那个难以理解的`it`可以很有用：它实际上是一个特殊变量的名字，其中`ghci`储存我们上一个求值的表达式的结果（这不是Haskell语言特性，它只是`ghci`特有的）
- 让我们分解`ghci`最后一行输出的含义：
  - 它告诉我们特殊变量`it`
  - 我们可以把`x :: y`形式的文字读成含义“表达式x具有类型y”
  - 这里，表达式“it”具有类型`[Char]`（通常使用`String`名称，而不是`[Char]`，它只是`[Char]`的同义词）

</div>
<div class="sheet-wrap"><div class="sheet-caption">框：it的趣味</div>


那个`it`变量是`ghci`的一个方便的捷径
- 它让我们做新的表达式中使用我们刚刚求值的表达式的结果
  ``` haskell
  ghci> "foo"
  "foo"
  it :: [Char]
  ghci> it ++ "bar"
  "foobar"
  it :: [Char]
  ```
- 当求值一个表达式时，如果求值失败，`ghci`不会改变`it`的值
- 这让你写出本质上伪造且带有某种安全网的表达式
  ``` haskell
  ghci> it
  "foobar"
  it :: [Char]
  ghci> it ++ 3

  <interactive>:1:6:
      No instance for (Num [Char])
        arising from the literal `3' at <interactive>:1:6
      Possible fix: add an instance declaration for (Num [Char])
      In the second argument of `(++)', namely `3'
      In the expression: it ++ 3
      In the definition of `it': it = it ++ 3
  ghci> it
  "foobar"
  it :: [Char]
  ghci> it ++ "baz"
  "foobarbaz"
  it :: [Char]
  ```
- 当我们把`it`和大量使用方向键结合起来，来唤回并编辑我们键入的上一个表达式，我们获得了一种方便的方式来互动地实验：错误的后果非常低
- 当你做探索语言的时候，要利用创造廉价、丰富错误的机会哦！

</div>
<div class="sheet-wrap"><div class="sheet-caption">整数和有理数类型</div>


这里有Haskell更多的几个类型名，我们已经看到了
``` haskell
ghci> 7 ^ 80
40536215597144386832065866109016673800875222251012083746192454448001
it :: Integer
```
- Haskell的整数类型叫`Integer`
- `Integer`值的大小只受到你的系统的内存容量的限制

有理数看起来和整数不太一样
- 为了构造有理数，我们使用`%`运算符。分子（numerator）在左边，分母（denominator）在右边：
  ``` haskell
  ghci> :m +Data.Ratio
  ghci> 11 % 29
  11%29
  it :: Ratio Integer
  ```
  - 方便起见，`ghci`允许我们缩略很多命令，所以我们可以写`:m`，而不是`:module`来加载一个模块
- 注意，前面的代码中，在`::`右边有 *两个* 词
- 我们可以把它读作“整数比”
- 我们可能猜测`Ratio`的分子和分母的值的类型都必须是`Integer`
- 当然是这样，如果我们尝试构建分子和分母不同类型，或者相同非整数类型的`Ratio`，`ghci`就会抱怨 \
  *代码略*

</div>
<div class="sheet-wrap"><div class="sheet-caption">:type命令</div>


- 尽管开始的时候`:set +t`给我们输入的每个表达式的类型信息很有用，这是个我们很快就放弃的机制
- 过一会，我们将会经常知道我们期望一个表达式有何种类型
- 我们可以在任何时候关闭额外的类型信息，使用`:unset`命令
  ``` haskell
  ghci> :unset +t
  ghci> 2
  2
  ```
- 即使这个机制关闭，我们仍然可以在需要的时候轻易地获得类型信息，使用另一个`ghci`命令
  *（使用的是:type，代码略）*
  - `:type`命令将会对于我们给它的任何表达式打印信息（包括it，我们在这里看到了）。它实际上不会求值表达式，它只会检查它的类型然后打印

为什么这两个表达式报告的类型不一样呢？
``` haskell
ghci> 3 + 2
5
ghci> :type it
it :: Integer
ghci> :type 3 + 2
3 + 2 :: (Num t) => t
```
- Haskell具有数个数字类型
  - 例如，一个字面数字`1`，可能，取决于它出现的上下文，成为浮点值整数
  - 当我们强迫`ghci`求值表达式`3 + 2`的时候，它必须选择一种类型，才能打印值，它默认为`Integer`
- 第二个例子中，我们让`ghci`打印表达式的类型，无需真正求值它，所以它无需如此特别
  - 它回答，实际上，“它的类型是数值。”

我们将会在第6章看到更多这种风格的类型注解（type annotation）

</div>
<h2 id="0B039B90">一个简单的程序</h2>
<div class="sheet-wrap"><div class="sheet-caption">本节介绍</div>


让我们前进一小步，写一个小程序，它数出输入的行数
- 先不要期待理解这个——只不过让我们脏脏手有点好玩
- 在文本编辑器中，把下列代码输入文件，保存为`WC.hs`
  ``` haskell
  -- file: ch01/WC.hs
  -- lines beginning with "--" are comments.
  main = interact wordCount
    where wordCount input = show (length (lines input)) ++ "\n"
  ```
- 找一个或者创建一个文本文件，让我们叫它`quux.txt`
  ``` sh
  $ cat quux.txt
  Teignmouth, England
  Paris, France
  Ulm, Germany
  Auxerre, France
  Brunswick, Germany
  Beaumont-en-Auge, France
  Ryazan, Russia
  ```
- 从一个shell或者命令行，运行下列命令
  ``` sh
  $ runghc WC < quux.txt
  7
  ```
- 我们成功地写了一个简单的程序，它与真实世界互动！
- 在以后的章节，我们会继续填补我们理解的鸿沟，知道我们可以写出我们自己的程序

</div>
<div class="sheet-wrap"><div class="sheet-caption">练习题</div>


1. 将下列表达式输入到`ghci`中，它们的类型是什么？ \
   *题目略，应该使用`:set +t`命令*
2. 在`ghci`，输入`:?`来打印一些帮助。定义一个变量，例如`let x = 1`，然后打印`:show bindings`，你看到了什么？ \
   *`:?`显示固定的帮助文字* \
   *`:show bindings`依次按时间顺序显示当前定义的函数、变量*
3. `words`函数数出字符串里的单词数。修改`WC.hs`示例，来数出文件里面的单词数。
   *做了*
4. 再次修改`WC.hs`示例，来输出文件里面的字符数
   *做了，办法就是把字符串看成字符列表，直接获取长度即可*


</div>
<h1 id="03F17ADB">类型和函数</h1>
<h2 id="AB56454F">为什么关注类型？</h2>
<div class="sheet-wrap"><div class="sheet-caption">什么是类型</div>


- Haskell里面每个表达式和函数都有一个 *类型*
- 一个值的类型表明它和同类型的其它值都有某些相同的特性

</div>
<div class="sheet-wrap"><div class="sheet-caption">类型有什么目的</div>


在我们开始更深入探讨Haskell类型系统之前，让我们谈论为什么我们应该关注类型——它们的目的是什么？
- *抽象*
  - 在底层，一个计算机跟字节有关，没有任何额外的结构
  - 类型系统给我们的是 *抽象*
  - 一个类型给普通的字节赋予含义
- 正确
  - 通常，一个类型系统不只是这样，而是防止我们不小心混淆类型

- 引入抽象的好处在于它让我们忘记或者忽略底层细节
- 如果我知道程序里面一个值是字符串，我不需要知道字符串如何实现的详细细节，我可以就假定我的字符串将会表现得和我处理的其它字符串一样

让类型系统有趣的是它们不都是一样的
- 实际上，不同的类型系统关注的通常甚至不是相同种类的问题
- 一门编程语言的类型系统深深地鲜明了我们用那门语言思考和写代码的方式
- Haskell的类型系统允许我们用一种非常抽象的层次思考，它允许我们编写简练、强大的程序

</div>
<h2 id="C7BCFC3C">Haskell的类型系统</h2>
<div class="sheet-wrap"><div class="sheet-caption">本章介绍</div>


Haskell的类型有三个有趣的方面
1. *强类型*
2. *静态类型*
3. 可以 *自动推断*

- 让我们更详细地讨论这些概念
- 条件允许的话，我们将会呈现Haskell的类型系统和其它语言中相关概念之间概念的相似之处
- 我们还会了解到这些特性各自的优点和不足

</div>
<h3 id="C6951B28">强类型</h3>
<div class="sheet-wrap"><div class="sheet-caption">强类型的特点</div>


当我们说Haskell具有 *强* 类型系统
- 我们是说类型系统保证了陈故乡不会包含特定种类的错误
- 这些错误来自尝试编写毫无意义的表达式

我们称
- *类型良态定义* ( *well typed* ): 遵循语言类型规则的表达式
- *类型病态定义* ( *ill typed* ): 不遵循类型规则的表达式，可能会造成 *类型错误* ( *type error* )

另一个强类型的Haskell角度是
- 它不会自动将值从一个类型转换(coerce)为另一个类型
- （强行转换(Coercion)也被称为casting或者conversion）
- 例如
  - C编译器会帮我们自动且默不作声地把一个 **int** 类型的值转换为一个 **float** 类型的值，如果函数期望一个参数是 **float** 类型
  - 但是Haskell编译器将会在类似情形中产生一个编译错误，我们必须显式地通过应用转换函数来转换类型

</div>
<div class="sheet-wrap"><div class="sheet-caption">强类型的优缺点</div>


优缺点
- 缺点：强类型确实偶尔会让特定种类的代码编写更加困难
  - 例如，C语言中编写编写底层代码的一种方式是给一个字节数组，把它转换类型，就好像它们确实是复杂数据的结构
  - 这非常高效，因为它不需要我们到处复制字节
  - Haskell的类型系统不允许这种转换
  - 为了获取数据的相同结构化的视角，我们需要做一些复制，它可能会在性能上有些许代价
- 有点：强类型的巨大好处是它在我们的代码中捕获真正的bug，使其不能导致问题
  - 例如，在强类型语言中，我们不会在期待整数的地方不小心使用字符串


</div>
<div class="sheet-wrap"><div class="sheet-caption">框：弱类型和强类型</div>


- 如果知道很多语言社区对强类型有他们自己的定义的话，很有用
- 无论怎么说，我们会在大致角度简单谈谈类型系统强弱的概念

- 在学术计算机科学中， *强* 和 *弱* 具有较为严格的含义
  - 强弱表示一个类型系统有多 *宽松* ( *permissive* )
  - 弱类型系统比强类型系统倾向于把更多表达式看作合法的
- 例如，在Perl中
  - 表达式`"foo" + 2`求值得到数字2，但是表达式`"13foo" + 2`求值得到数字15
  - Haskell拒绝认为两种表达式合法，因为`(+)`运算符要求两个操作数都是数值
  - 因为Perl的类型系统比Haskell的更加宽松，我们在这个严格的解释下说它更弱

有关类型系统的激烈争论在普通英语中有它们的源头
- 其中人们把“弱”和“强”赋予 *价值* 观念——我们通常认为强比弱更好
- 多得多的程序员说普通英语，而不是学术术语，而通常学者 *真的* 在痛骂任何不满足他们想象的类型系统
- 结果通常就是那个有名的互联网消遣（Internet pastime），口水战

</div>
<h3 id="75600D88">静态类型</h3>
<div class="sheet-wrap"><div class="sheet-caption">静态类型的含义</div>


拥有 *静态类型* 表示编译器在编译时（at compile time）知道每个值和表达式的类型，任何代码执行之前
- Haskell编译器或揭示其将会探测我们和是尝试使用类型不匹配的表达式，并且用错误信息拒绝我们的代码，在我们运行它之前

</div>
<div class="sheet-wrap"><div class="sheet-caption">静态类型的优缺点</div>


优缺点
- 缺点：静态类型有时候会更难写出某些有用的代码
  - 在Python这样的语言中， *鸭子类型* ( *duck typing* )很常见，其中一个对象表现得如果足够像另一个，就会被用做替代品 \
  *注释：“如果它走起来像鸭子，蹲起来像鸭子，那么我们不如就叫它鸭子”*
  - 幸运的是，Haskell的系统有 *类型类* ，我们会在第6章介绍它，它能提供动态类型几乎所有的优点，并且是以安全、方便的形式
- 优点：Haskell的强类型和静态类型的组合让它不可能在运行时发生类型错误
  - 尽管这表示我们需要预先更多做一点思考，它仍然消灭了非常多简单的错误，否则这些错误可能邪恶且很难找到
  - 在Haskell社区中，不言而喻的道理（a truism）是，一旦代码编译成功，那它就比其它代码更有可能正常运行（可能更现实地说应该是，Haskell代码通常更少有低级错误）
  - 用动态类型语言编写的程序需要有大量测试套件来为简单类型错误不会发生提供更多保障
  - 测试套件不能提供完全的覆盖：某些普通任务中，例如重构一个程序让它更加模块化，可能会引入新的类型错误，而测试套件可能无法暴露
  - 在Haskell中，编译帮我们证明了没有类型错误：一个通过编译的Haskell程序将不会在运行时有类型错误
  - 重构通常只是移动代码的问题，然后重新编译和数次整理，直到编译器给我们“全部完成”的消息

</div>
<div class="sheet-wrap"><div class="sheet-caption">特点总结</div>


理解静态类型值，一种有帮助的比喻是把它看作把一片拼图放进拼图谜题中
- Haskell中，如果一片拼图具有错误的形状，它就是放不进去
- 在动态类型语言中，所有的拼图都是1x1方形，并且永远放得进去，所以你必须一直检查结果图片，并且（在测试过程中）检查它是否正确

</div>
<h3 id="EDEC3D2F">类型推断</h3>
<div class="sheet-wrap"><div class="sheet-caption">类型推断的特点</div>


最终，Haskell编译器可以自动推断程序中绝大多数表达式类型
- 这个过程被称作 *类型推断* ( *type inference* )
- Haskell允许我们显式声明任何值的类型，但是类型推断意味着这几乎总是可做可不做的，不是我们强制要求的

</div>
<h2 id="D0D63DCD">类型系统有哪些精彩看点</h2>
<h2 id="A02889AD">某些普通的基础类型</h2>
<h2 id="580BC7BB">函数应用</h2>
<h2 id="5E5501A5">有用的复合数据类型：列表和元组</h2>
<h2 id="62BDECBD">列表和元组之上的函数</h2>
<h2 id="2EFA9644">函数类型和纯度</h2>
<div class="sheet-wrap"><div class="sheet-caption">示例：line函数和它的类型签名</div>


让我们看看一个函数的类型：
``` haskell
ghci> :type lines
lines :: String -> [String]
```
- 我们可以把`->`读作“到”(to)，它大致上可以翻译成“返回”(return)
- 整个的类型签名读作“ **lines** 具有从 **字符串** 到 **字符串** 列表的类型”
- 让我们尝试应用函数
  ``` haskell
  ghci> lines "the quick\nbrown fox\njumps"
  ["the quick","brown fox","jumps"]
  ```
- **liens** 函数以行作为界限将字符串分解
- 注意它的类型签名提示我们函数可能做什么。这是函数式语言中类型及其宝贵的特性

</div>
<div class="sheet-wrap"><div class="sheet-caption">副作用是什么</div>


*副作用* 引入了系统全局状态和函数行为的依赖关系
- 例如，让我们放下一会Haskell，想想命令式编程语言
  - 考虑一个函数，它读取并且返回全局变量的值
  - 如果其它一些代码可以修改全局变量，那么我们的函数的特定应用将会依赖于全局变量的当前值
  - 函数具有副作用，即使它自身从未修改变量

副作用本质上是函数不可见的输入或不可见的输出
- 在Haskell中，函数 *默认* 行为是没有副作用的：函数的结果只取决于我们显式提供的输入
- 我们可以称这些函数 *纯* ，带有副作用的函数 *不纯*

</div>
<div class="sheet-wrap"><div class="sheet-caption">副作用和类型系统有什么关系</div>


如果一个函数具有副作用，我们可以通过阅读它的类型签名知道——
- 函数结果的类型将会以IO开头
  ``` haskell
  ghci> :type readFile
  readFile :: FilePath -> IO String
  ```
- Haskell的类型系统阻止我们不小心把纯和不纯的代码弄混

</div>
<h2 id="500085E0">Haskell源文件，编写简单的函数</h2>
<h2 id="AFD083DB">通过例子理解求值</h2>
<h2 id="CFD6F924">Haskell里的多态</h2>
<h2 id="305F5B0C">多于一个入参的函数的类型</h2>
<h2 id="9F997956">为什么对纯度大惊小怪？</h2>
<h2 id="BEA4300C">结论</h2>
<div class="sheet-wrap"><div class="sheet-caption">本章小结</div>


在这一章
- 我们匆匆忙忙地一览了Haskell的类型系统，和它很多的语法
- 我们阅读了大多数常见的类型，并且发现了如何编写简单的函数
- 我们了解了多态、条件表达式、纯度和惰性求值

这些都是大量要消化的信息

在第3章
- 我们将会在此基础知识之上，进一步加深我们对Haskell的理解

</div>
<h1 id="5290106A">定义类型, 流水化函数(Streamlining Functions)</h1>
<h2 id="F2ECC125">定义一个新的类型</h2>
<h2 id="886741FC">类型同义词</h2>
<h2 id="332ADE50">代数数据类型</h2>
<div class="sheet-wrap"><div class="sheet-caption">示例：Bool是最简单的代数数据类型</div>


**Bool** 是最简单的一种被称为 *代数数据类型* （ *algebraic data type* ）的例子
- 一个代数数据类型可以具有不只一个值构造器
  ``` haskell
  -- file: ch03/Bool.hs
  data Bool = False | True
  ```
- **Bool** 类型具有两个值构造器， **True** 和 **False**
- 每个值构造器在定义中通过`|`字符分隔，我们读作“或”——我们可以构造一个 **Bool** ，具有值 **True** 或值 **False**
- 当一个类型具有不只一个值构造器时，它们通常被称作 *替代* （ *alternatives* ）或者 *情形* （ *cases* ）
- 我们可以使用任何一种替代来构造那个类型的一个值

</div>
<div class="sheet-wrap"><div class="sheet-caption">脚印：有关命名的笔记</div>


**有关命名的笔记**

- 尽管“代数数据类型”这个词很长，我们应该小心避免使用缩略词（acronym）“ADT”
- 它已经被广泛承认代表“ *抽象数据类型* ”（ *abstract data type* ）
- 既然Haskell既支持代数数据类型又支持抽象数据类型，我们应该表明清楚，并且完全避免这个缩略词


</div>
<div class="sheet-wrap"><div class="sheet-caption">示例：收银信息（值构造器获取参数）</div>


每个代数数据类型的值构造器可以获取零个或更多参数

- 例如，有一种方式我们可以表示收银信息
  ``` haskell
  -- file: ch03/BookStore.hs
  type CardHolder = String
  type CardNumber = String
  type Address = [String]
  ```
  - 如果你熟悉C或者C++，它有点类似于 **typedef**
  ``` haskell
  data BillingInfo = CreditCard CardNumber CardHolder Address
                   | CashOnDelivery
                   | Invoice CustomerID
                     deriving (Show)
  ```
  - 这里，我们说我们支持三种方式给我们的顾客开账单
    1. 如果他们想要用信用卡支付，他们必须提供卡号、持卡人姓名、持卡人地址作为 **CreditCard**
    2. 或者，他们可以付钱给那个给他们寄快递的人。既然我们不需要存储任何额外的信息，我们不给 **CashOnDelivery** （货到付款） 构造器指定任何参数
    3. 最后，我们可以给指定的顾客送发票，在这种情况下，我们需要她的 **CustomerID** 作为 **Invoice** 构造器的参数
  - 当我们使用值构造器创建一个类型为 **BillingInfo** 的值，我们必须提供它需要的参数 \
    *代码略*
    - **No instance** 错误信息唤起，因为我们没有给 **Invoice** 构造器提供一个参数
    - 结果我们尝试打印 **Invoice** 构造器本身
    - 那个构造器需要一个参数才能返回一个值，所以它是一个函数
    - 我们在Haskell中不能打印函数，这也就是为什么最终解释器抱怨

</div>
<h3 id="E03DC9C3">元组、代数数据类型，各自何时使用</h3>
<div class="sheet-wrap"><div class="sheet-caption">如果想，可以用元组代替代数数据类型</div>


元组和用户定义的代数数据类型之间有一些重叠
- 如果我们想，我们之前的 **BookInfo** 类型也可以表示为`(Int, String, [String])`元组
  ``` haskell
  ghci> Book 2 "The Wealth of Networks" ["Yochai Benlker"]
  Book 2 "The Wealth of Networks" ["Yochai Benkler"]
  ghci> (2, "The wealth of Networks", ["Yochai Benkler"])
  (2,"The Wealth of Networks",["Yochai Benkler"])
  ```


</div>
<div class="sheet-wrap"><div class="sheet-caption">代数数据类型和元组不同之处</div>


代数数据类型允许我们区分其它相同的信息
- 两个具有相同类型元素的元组结构上相同，所以它们具有相同类型 \
  *代码略*
- 因为代数数据类型具有不同的名称，即使它们在其它方面结构上相同，它们也具有不同的类型 \
  *代码略*
  - 这让我们能够使类型系统在编写程序时有更少的bug
  - 如果使用我们刚才定义的元组，我们可能可以设想将一头鲸鱼的描述传入一个接收椅子的函数，类型系统可能不会帮助我们
  - 如果使用代数数据类型，就没有这种可能的混乱了

</div>
<div class="sheet-wrap"><div class="sheet-caption">示例：表示二维向量</div>


有一个更为巧妙的示例
- 考虑下面二维向量的表示
  ``` haskell
  -- file: ch03/AlgebraicVector.hs
  -- x and y coordinates or lengths.
  data Cartesian2D = Cartesian2D Double Double
                     deriving (Eq, Show)
  -- Angle and distance (magnitude).
  data Polar2D = Polar2D Double Double
                 deriving (Eq, Show)
  ```
  - 笛卡尔和极坐标形式的两个元素使用相同的类型
  - 然而，元素的 *含义* 是不同的
  - 因为 **Cartesian2D** 和 **Polar2D** 是不同的类型，类型系统将不会允许我们不小心在期望 **Polar2D** 的地方使用 **Cartesian2D** 值，反之亦然
- 比较相等（错误）
  ``` haskell
  ghci> Cartesian2D (sqrt 2) (sqrt 2) == Polar2D (pi / 4) 2
  -- 报错略
  ```
  - `(==)`运算符要求它的参数具有相同类型

</div>
<div class="sheet-wrap"><div class="sheet-caption">脚印：相等性比较</div>


**相等性比较**

- 注意在我们的向量类型的`deriving`语句中，我们添加了另一个词，`Eq`
- 这个导致Haskell实现生成代码，允许我们比较值的相等性

</div>
<div class="sheet-wrap"><div class="sheet-caption">示例：如果用元组表示值，将很快混淆</div>


如果我们使用元组来代表这些值，我们可能很快地混淆两种表示，让我们处于水深火热之中
``` haskell
ghci> (1, 2) == (1, 2)
True
```
- 类型系统在这里不能帮助我们：据它所知，我们在比较两个`(Double, Double)`对，这绝对是合法的
- 实际上，我们不能通过检查判断这些值的哪一个应该是极坐标或是笛卡尔坐标，但是`(1,2)`在各个表示中具有不同的含义

</div>
<div class="sheet-wrap"><div class="sheet-caption">小结</div>


并没有可靠快速的规则来决定使用元组更好还是不同的数据类型更好，但是有个经验法则
- 如果你正在你的代码中广泛使用复合值（几乎所有不简单的程序都这样做），加上 **data** 声明将会对你的类型安全和可读性都有益
- 对于更短的、局部使用，元组通常就很好

</div>
<h3 id="3D759E10">其它语言中类似代数数据类型的概念</h3>
<div class="sheet-wrap"><div class="sheet-caption">C/C++中一些类似的东西</div>


代数数据类型提供了单一强大的方式表达数据类型
- 其它语言通常需要数种不同的特性来完成相同的表达力
- 这里有一些C和C++中类似的东西，可能做到我们用代数数据类型能做的
- 以及代数数据类型如何跟联系到可能更加熟悉更容易理解的概念

</div>
<h4 id="8A467BFE">结构体</h4>
<h4 id="352E9955">枚举量</h4>
<h4 id="7A04F3E1">可区分联合体(The discriminated union)</h4>
<div class="sheet-wrap"><div class="sheet-caption">结构体在替代方面的问题</div>


- 如果一个代数数据类型具有多个替代（alternatives），我们认为它类似于C或C++中的 **union**
- 两者的一个大问题是，联合体不会告诉我们它实际上呈现的是何种替代
- 我们必须显式且手动地追踪我们在用何种替代，通常使用一个封闭结构体的另一个字段
- 这就表明联合体可能成为危险bug的源头，其中我们使用何种替代的记号是错的
  ``` C
  enum shape_type {
    shape_circle,
    shape_poly,
  };

  struct circle {
    struct vector centre;
    float radius;
  };

  struct poly {
    size_t num_vertices;
    struct vector *vertices;
  };

  struct shape
  {
    enum shape_type type;
    union {
      struct circle circle;
      struct poly poly;
    } shape;
  };
  ```
  - 这个例子中， **union** 可以要么 **struct circle** 要么 **struct poly** 的包含有效的数据
  - 我们必须手动使用 **enum shape_type** 来表明 **union** 里面现在存储的是何种值
- 这个代码的Haskell版本比起C版本既大大缩短，又更加安全
  ``` Haskell
  -- file: ch03/ShapeUnion/hs
  type Vector = (Double, Double)
  type Shape = Circle Vector Double
             | Poly [Vector]
  ```
- 如果我们使用 **Circle** 构造器创建了一个 **Shape** 值，保存了我们创造了一个 **Circle** 的事实
- 当我们之后使用 **Circle** ，我们不会不小心把它作为 **Square** 使用。我们会在第50页，接下来的小节“模式匹配”看到原因

</div>
<div class="sheet-wrap"><div class="sheet-caption">脚印：几个要点</div>


**几个要点**

- 读完之前的小节后，现在应该很清楚我们用 **data** 关键字定义的 *所有* 数据类型都是代数数据类型
- 有一些可能只有一个替代，其它的则有数个，但是它们都是在使用同一个机制

</div>
<h2 id="CFC3BCB6">模式匹配</h2>
<h2 id="23E07770">记录(Record)语法</h2>
<h2 id="60EC8B7D">参数化类型</h2>
<h2 id="8B36D663">递归类型</h2>
<h2 id="A4F2087E">报告错误</h2>
<h2 id="EAC83D5E">介绍局部变量</h2>
<h2 id="CB7A1247">表达式中的越位规则(The Offside Rule)和空格</h2>
<h2 id="5C615E22">case表达式</h2>
<h2 id="D1500CB6">有关模式的常见新手错误</h2>
<h2 id="DEC48F52">带卫语句的条件求值</h2>
<h1 id="D239C5CB">函数式编程</h1>
<h1 id="0D9B6827">编写库: 操作JSON数据</h1>
<h1 id="5A6899BA">使用类型类</h1>
<h1 id="8D899F74">I/O</h1>
<h1 id="3C1A8943">高效文件处理, 正则表达式, 文件名匹配</h1>
<h1 id="6F6C42E3">I/O案例学习: 搜索文件系统的库</h1>
<h1 id="0F4DB654">代码案例学习: 解析二进制数据格式</h1>
<h1 id="C1EBC21F">测试和质量保证</h1>
<h1 id="9340DED2">条形码识别</h1>
<h1 id="A6E76B3B">数据结构</h1>
<h1 id="F4C53BAB">单子</h1>
<h1 id="F4772B41">用Monad编程</h1>
<h1 id="F12A3A21">使用Parsec</h1>
<h1 id="7D84661C">C接口: FFI</h1>
<h1 id="21C90078">Monad变换器</h1>
<h1 id="39320CA2">错误处理</h1>
<h1 id="18B13E7F">Haskell的系统编程</h1>
<h1 id="7E10AD9C">使用数据库</h1>
<h1 id="6766C940">拓展示例: Web客户端编程</h1>
<h1 id="45D3249C">用gtk2hs实现GUI编程</h1>
<h1 id="3DDF7AE4">并发和多核编程</h1>
<h1 id="35D657E1">特征提取和优化(Profiling and Optimization)</h1>
<h1 id="63C5ECB9">高级库设计: 建立一个布隆过滤器(Bloom Filter)</h1>
<h1 id="E09AEBE7">套接字(Sockets)和系统日志(Syslog)</h1>
<h1 id="C7BE9961">软件事务内存</h1>
</div>
