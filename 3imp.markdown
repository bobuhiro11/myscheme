# why scheme is best?

- データとプログラムがS式で統一されている．
- マクロで構文を追加できる．ifだけの言語でもunlessを追加できる．
- 継続がある，いわゆるCスタックとか違う構成の関数呼び出しがある．
  * goto,continue,returnをまとめてできる
  * 大域脱出，コルーチン，疑似マルチタスク，バックトラックもできる
- ribの意味がわからん
- stackとcall frameの使い分け
- コンパイラ(scheme),VM(scheme)の構成から，コンパイラ(scheme),VM(C)になるのが理想
- そうなれば，Cだけで動くようになる

Three Implementation Model for Scheme Chapel Hill 1987

# Abstract

- ヒープベース，スタックベース，stringベース(マルチプロセッサに適している)
- ヒープベースが最初
- ヒープベース
  * パラメータのリスト，環境の束縛，コールフレームはヒープに
- スタックベース
  * できる限りスタックに
  * 結果として，ヒープの割り当てが少なくなり，メモリの参照が減る，命令列が減る，GC対象が減る
  * 作者Chez Schemeで採用
- stringベース
  * schemeに適したFFP言語に変換される
  * FFP言語はFFPマシン(a multiple-processor string-reduction computer)で直接実効できる．

# Chapter 1: Introduction

- スタックベースはひとつのコンピュータ
- stringベースはマルチプロセッサ
- MLもChez Schemeとは独自に実装される
- stringベースはsequential computer上でのインタプリタではテストされたが，まだ実装されていない
- Schemeは他方言とちがい，静的スコープ,block-structuredであり，
- 関数と継続を第一級オブジェクトとする
- MLは静的スコープ，第一級関数があるが，継続と変数割り当て(variable assignments)がない
  * 第一級とは，引数，返り値にできて，独自に格納できる．
  * リスト，シンボル，文字列，数値など
  * ほとんどはスカラー量のみだった
- 非常にに似ているためML,CLにも応用可能
- *ベースの違いはデータ構造の表現，ソースコードからオブジェクトコードへの変換，
- オブジェクトコードの実行である
- クロージャと継続には，スタックフレームのヒープ割り当てが必要とかんがえられていた
- schemeはstatementsやループより，関数や再帰をよく使う
- 変数割り当ては少ない傾向がある
- 本章の残りで，関数型言語の背景，実装テクニック，マルチプロセッサとの関係をの述べる
- 2章はschemeの文法，特徴など
- 3,4,5章はそれぞれのモデルについて
- 付録Aでスタックベースとヒープベースの比較

### 2.1.1 Core Syntactic Forms.

- コア言語は以下のとおり
- ただし，２つの曖昧さがある
  * 全部はじめにマッチしてしまう(全ての表現はobjectだから)
  * quote,lambda,if,set!,call/ccにマッチするのは最後にもマッチしてしまう
- object
  * list,symbol以外の全ては定数とする
  * これらはもっと別の文法にマッチするはず
- variable
  * 全てのシンボルは変数束縛への参照
  * lambdaを取り囲むことで縛られる??
  * Any symbol is treated as a reference to a variable binding, which should be bound by an enclosing lambda.

```
<core>  <object>
<core>  <variable>
<core>  (quote <object> )
<core>  (lambda (<variable> ...) <core> )
<core>  (if <core> <core> <core>)
<core>  (set! <variable> <core>)
<core>  (call/cc <core>)
<core>  (<core> <core> ...)
```

# Chapter 3: The Heap-Based Model

- Scheme-84,C-Scheme,Scheme-311で使われた
- 1節で同期と問題点
- 2節でデータ構造
- 3節で命令
- 4節で実装
- 5節で最適化

# 3.1 Motivation and Problems

- Algol60,C,Pascalのような静的スコープ言語ではスタックはコールフレームに使われる
- コールフレームには，戻りアドレス，変数束縛，前のコールフレームへの参照が含まれる
- 変数束縛: 引数とローカル変数のこと
- 普通は呼び出し元で作られる
- 呼び出し先ではローカル変数を定義してコールフレームを拡張する
- ここまではCの関数呼び出しと同じ
- schemeでは第一級クロージャと第一級継続により，これは効率的でない
- 第一級クロージャでは，不明確に(indefinitely)引数を束縛する能力をもつ
- 特に，クロージャと保持された束縛は，callがreturnされたあと，スタックフレームが削除されたあとも残っているかもしれない
- この理由から引数束縛をスタックフレームに入れるのは出来ない
- 代わりに，ヒープ上に環境が作られる(引数のため)
- コールフレームにはこの環境への参照をいれる
- クロージャが作られたとき，この環境への参照はクロージャオブジェクトの中に追加される．
- Moving the variable bindings into the heap saves the bindings from being overwritten as the stack shrinks and grows
- コールフレームがスタック上にあれば，追加のオーバヘッドは環境の割り当てのみ
- しかし，第一級継続では，コールフレームに加え環境ののヒープ割り当てが必要
- これは普通の実装では継続はコールフレームに参照を保持するものだから
- ここで継続は，呼ばれた時継続が獲得されたポイントへ戻るクロージャであることを思い出してほしい
- 自然な解決方法は，ヒープ割り当てされたスタックフレームの連結リストを使う方法である
- スタックが大きくなるにつれて，新しいフレームがヒープの使われていない部分に割り当てられる
- なぜなら，古いスタックフレームはそのまま残っているから
- コールフレームと環境のヒープ割り当ての重大な問題は，ヒープの使用法によるオーバヘッドである
- 直接的なコスト
  * コールフレームや環境の割り当ての際，空き領域を探すこと
  * 環境やフレームをリンクでたどること
- 間接的なコスト
  * 環境やコールフレームための領域の再利用
  * (その他)これは巨大なメモリ空間をもつ仮想メモリでは問題ではないと期待する人もいる
  * しかし仮想メモリのパフォーマンスは参照局所性による
  * スタックベースでは同じスタックフレームを再利用する
  * ヒープベースでは頻繁に解放されない限り(GCのオーバヘッドを意味する)，新しいページを参照することになる
- さらに，スタックでなくヒープを採用すると，ハードウェア，マイクロコードが提供しているpush,pop,index命令，call,return命令が使えない

## 3.2 Representation of Data Structure

- core言語では5つのデータ構造がある(メモリ内に存在しなければならないデータのこと??)
  * 環境 environments
  * コールフレーム call frames
  * コントロールスタック control stack
  * クロージャ closures
  * 継続 continuations

### 3.2.1 Environments

- 環境は胸部に似ている

```
イメージ
(((変数1 変数2) .  (値1 値2))
 ((変数A) .  (値A)))
```

```
((lambda (a b)
  ((lambda (c)
    ((lambda (d e f) body ) 3 4 5))
   2))
 0 1)

(((d e f) . (3 4 5))
 ((c) . (2))
 ((a b) . (0 1)))
```

- 3.4節ではvariable ribsが環境から除かれら，環境はvalue ribsのリストに改良される

### 3.2.2 Frames and the Control Stack

- フレームは，未決定な計算を保持するため
- 普通関数呼び出し時に作られる
  * だけど，そのようなフレームはここでは使わない
- コールフレームには，戻りアドレス(または次に評価するS式)，環境(またはアクティブな変数の束縛に関する情報)，次のフレームへのポインタ，その他，が含まれる
- ヒープベースでは，単純に4つのフィールドを含むリスト
- 1つめはS式．
  * 次に評価されるべきS式．
  * それは，戻りアドレス(program counter,instruction counterを含む)を決定する
- 2つめは環境
  * 現在のアクティブな環境
- 3つめはrib field
  * During the evaluation of an application, this field contains a list of the arguments that have been evaluated so far.
- 4つめは次のフレームへの参照
- コントロールスタックは，現在のフレーム，前のフレーム，その前・・・のリスト構造
  * "next frame"を通して連結リストをつくる

### 3.2.3 Closures and Continuations

- closureは関数の実行部(またはテキスト)と，現在の環境を組み合わせたもの
- 3.5節の改良前では，変数(variables)は必要
- 3.4節では，closureは，本体，環境，変数リスト

```
((lambda (x)
  (lambda (y) (cons x y)))
  'a)
上の式が返すclosureは以下のようになる
((cons x y) ((x) . (a)) (y))
```

- except that the actual body would be a compiled version of (cons x y).
- 継続は，与えられた点からの続きの計算をするのに十分な情報をもつclosureである
- 本質的には，call/ccがそれを作った時点へ帰ることを意味する
- これを実現するのは，全てのスタックが何らかの方法で保存されている必要がある
- 継続は単純に，現在のフレームへのポインタ(つまり全体のコントロールスタック)を含む特殊なclosure
- closureに継続であるかどうかを示すタグをつけるのは可能
  * だけどこのチェックは効率が悪い
  * 普通のclosureに比べ，継続はそんなに現れない
  * Instead, continuation closures have the same structure as normal closures, but with a body that, when executed, restores the saved stack and returns the continuation’s argument to the routine that created the continuation.

## 3.3 Implementation Strategy

- 前節までを使って実装
- 多くの方針がある
  * ここでは理解が簡単で，一般化し易い方針を目指す
- 計算の状態の保持にレジスタ群を使い，繰り返し計算をすすめる
- An iterative approach is necessary because the more straightforward recursive approach of the meta-circular interpreter presented in Chapter 2 cannot properly support continuations or tail calls.
- 評価器は，継続において状態を保存するために明示的に計算の状態にアクセスできる必要がある
- 処理系が再帰を適切に実現しないと，末尾呼び出しを適切にサポートできない
- 5つのレジスタを使う
  * a: the accumulator
    + 値を返す命令の返り値
    + load命令や変数の参照など
    + 関数適用のときは，引数を次々に保持し，value ribに保存していく
    + 関数の返り値にも使う
    + if文では，条件式に使う
    + 計算後はトップレベルの計算結果を示す
  * x: the next expression
    + 次に評価される式
    + 定数ロードや，クロージャの生成，クロージャの割り当て，クロージャの適用など
    + ほとんどschemeコードと似ているが，効率的になるようコンパイルされる??
  * e: the current environments
    + アクティブな静的割り当てを保持する
    + クロージャ適用のときは，クロージャ生成時の環境の上に新しい環境が作られる
    + 変数参照，変数割り当て，lambda式(例えば生成時)に，現在の環境をつかう
    + 環境は関数適用により削除されるから，コールフレーム内に入れておく必要がある
  * r: the current value rib
    + 引数のリストを保持する．
    + accumulatorからcurrent ribにcosしていく
    + 全ての引数とクロージャの値が計算されれば，クロージャの環境と組み合わせて新しい環境を作る
  * s: the current stack
    + コールフレームのtopを表す
    + call/cc式の評価により継続オブジェクトに保存される
- 変数参照では環境から値を引っ張ってきてaccumulatorに渡す
- また，next expression xも同時に変わる
  * その他の命令でも特別な場合以外にはxを変化させる
- 定数とquote式は同じとみなす
  * どちらもオブジェクトをaccumulatorにいれる
- lambda式により作られたクロージャもaccumulatorにいれる
- if式では最初に条件式をaccumulatorに入れておき，それをもとに次の式を決定する
- set!は破壊的に現在の環境を変更する
- call/cc式は，あたらしいコールフレームを作る．
  * 現在の環境，current rib，戻りexpressoinを浮腫む
- その新しいスタックは継続オブジェクトである
- それには現在のribが追加される(空のはずである)
- The next expression is updated to an expression that first evaluates the function expression and then applies the resulting closure to the current rib.
- When this continuation is subsequently invoked, the saved stack is restored, the top frame is removed, and the argument to the continuation is placed in the accumulator.
- 関数適用にはいくつかのステップがある
  * はじめに，現在の環境，現在のrib，返り先の式を保存するための新しいスタックを作る
    + そのステップでcurrent ribは空リストに初期化される
  * 次に，全ての引数がそれぞれ評価されcurrent ribに追加される
  * 関数の式(expresson)が評価されその値がaccumulatorに残る
  * 最後にcurrent ribに対し，accumulator内のクロージャを適用する
  * 適用時に，クロージャの環境とcurrent ribにより作られる新しい環境はcurrent environmentレジスタに
  * クロージャ本体は，current expressionレジスタに入る．
- クロージャから帰るときは，スタックフレームの先頭を削除し，復元する．
  * closureの返り値は，accumulatorに残っている
- call/ccや再帰呼び出しの評価は特別である
  * 末尾呼び出しを最適化するには(コントロールスタックを作らないためには)，新しくコールフレームを追加してはならない．
  * コールフレームを追加する目的は，環境，valu rib,返り値先の式を保持すること
  * 末尾呼び出しでは，呼び出し後のコードはreturnだけなので，値をすぐに返せば良い．

## 3.4 Implementing the Heap-Based Model

- 本章と4章で，SchemeをVMのアセンブリコードへ変換する
- このアセンブリは，普通アセンブリコードに期待するもの(ラベル，ジャンプ）がない
  * ラベルやジャンプのない，非循環なグラフ
- この形式から伝統的なアセンブリへの変換は簡単
- VMバイトコードは簡単にする

## 3.4.1 Assembly Code

- 12命令
- (halt)
  * VMをhaltする．
  * accumulatorの値を結果とする
- (refer var x)
  * 変数varの値を現在の環境から取得し，accumulatorに突っ込む
  * 次の式をxにセットする．
- (constant obj x)
  * 定数objをaccumulatorにツッコミ，xをセット
- (close vars body x)
  * bodyとvarsと現在の環境からクロージャを作り，accumulatorに突っ込む
  * 次の式をxにセットする．
- (test then else)
  * accumulatorがnullかどうかチェックし，nextかelseをnext expressionにセットする．
- (assign var x)
  * varにaccumulatorを束縛し，現在の環境を変更する．
  * 次の式をxにセットする．
- (conti x)
  * 現在のスタックから継続を作り，accumulatorに突っ込む
  * 次の式をxにセットする．
- (nuate s var)
  * sを現在のスタックに復元する．
  * 現在の環境のvarにaccumulatorをセットする．
  * 次の式を(return)にする．
  * 継続の適用かな
- (frame x ret)
  * 現在の環境とcurrent ribとret(次の式として)から新しいフレームを作る
  * 現在のスタックにこれを追加
  * current ribを空リストに，次の式をxにする．??????
- (argument x)
  * accumulatorの値をcurrent ribに追加
  * 次の式をxにセットする．
- (apply)
  * accumulator内のclosureをcurrent ribsに適用する
  * 正確には，クロージャの環境とクロージャの変数リストとcurrent ribを展開する
  * 現在の環境を新しくする
  * current ribを空リストにする．
  * 次の式をclosure本体にする．
- (return)
  * スタックの最初のフレームを取り除き
  * 現在の環境，current rib,next expression,current stackをリセットする．

## 3.4.2 Translation

- コンパイラへの入力はschemeコードと，次に実行するコード
- next instructionは継続の表現といえる?
  * call/ccで作られる継続オブジェクトとは区別する
- シンボル，quote式，定数式はそのまま
  * (compile 'x '()) => (refer x ())
  * (compile '(quote x) '()) => (constant x ())
  * (compile '1 '()) => (constant 1 ())
- lambda式は，本体がコンパイルされる．
  * (compile '(lambda (x) x) '()) => (close (x) (refer x (return)) ())
- ifとset!は部分式が必要
- nextがthencとelsecのどちらにも現れる．
  * ラベルやジャンプが必要ない
  * (compile '(if x 1 2) '()) => (refer x (test (constant 1 ()) (constant 2 ())))
  * (compile '(set! x 1) '()) => (constant 1 (assign x ()))

```
(rec var exp)

=>

(let ([var '()])
  (set! var exp))

(recur f ([var init ...) exp ...))

=> 

((rec f (lambda (var ...) exp ...))
 init ...)
```

- 関数適用において，例えば(fcn arg1 arg2 ... argn)の命令列は，

```
frame
  argn
    argument
    .
    .
    .
      arg1
        argument
          fcn
            apply
```

- 最初の引数が最後に評価される．consで先頭の要素の追加するため
- (compile '(func 1 2) '(n)) =>  (frame (n) (constant 2 (argument (constant 1 (argument (refer func (apply)))))))
- call/ccは現在の継続を返す仮想的な式を引数にとる関数の特殊な適用
- (call/cc exp)は以下のように展開

```
frame
  conti
    argument
      exp
        apply
```

- フレームをプッシュし，現在の継続をcurrent ribに追加する
- expを計算する．最後に，expを適用(評価)する．
- 関数の適用(評価)と，call/ccは最後に現れるとほとんど同じ
- 最後の関数適用とcall/ccは，コールフレームを追加しない，
  * frame命令は省略する??

### 3.4.3 Evaluation

- 
