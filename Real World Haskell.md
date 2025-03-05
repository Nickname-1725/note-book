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
<h1 id="03F17ADB">类型和函数</h1>
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
