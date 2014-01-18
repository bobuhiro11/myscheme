# why scheme is best?

* データとプログラムがS式で統一されている．
* マクロで構文を追加できる．ifだけの言語でもunlessを追加できる．
* 継続がある，いわゆるCスタックとか違う構成の関数呼び出しがある．

Three Implementation Model for Scheme

Chapel Hill 1987

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

- core言語では5つのデータ構造がある
  * 環境 environments
  * コールフレーム call frames
  * コントロールスタック control stack
  * クロージャ closures
  * 継続 continuations

## 3.2.1 Environments

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

- 3.4節ではvariable ribsからvalue ribsに改良される

##
