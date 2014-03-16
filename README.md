# myscheme

## これは何?

VM型の小さな処理系です．

## 使い方

[Gauche](http://practical-scheme.net/gauche/index-j.html)をインストール後，以下のコマンドを実行してください．

```
git clone git@github.com:bobuhiro11/myscheme.git
cd myscheme
make run
```

## 構成

```
├── 3imp_sample     論文3impのサンプルコード
│   ├── chap4.1-stack-model.scm
│   ├── chap4.3-stack-model.scm
│   ├── chap4.4-stack-model.scm
│   ├── chap4.5-stack-model.scm
│   ├── chap4.6-stack-model.scm
│   ├── chap4.7-stack-model.scm
│   ├── heap-model.scm
│   └── p56.scm
├── compiler   バイトコードへのコンパイル
│   ├── compile.scm
│   ├── linear.scm
│   ├── macro.scm
│   ├── main.scm
│   ├── test.scm
│   ├── util.scm
│   ├── vm-3imp.scm
│   └── vm.scm
├── LICENSE
├── Makefile
├── README.md
├── test     テストケース
│   ├── callcc.scm
│   ├── display.scm
│   ├── fibo.scm
│   ├── gc.scm
│   ├── ltak.scm
│   ├── reverse.scm
│   └── tak.scm
└── vm     バイトコードの実行
    ├── common.h
    ├── gc.c
    ├── hashtable.c
    ├── myscmvm
    ├── symbol_table.c
    └── vm.c
```


## 参考
[slideshare: scheme処理系の実装](http://www.slideshare.net/bobuhiro11/scheme-32091601)
