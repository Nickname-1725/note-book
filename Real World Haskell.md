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
