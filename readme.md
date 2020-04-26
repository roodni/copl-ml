# CoPL ML

## これは何
[プログラミング言語の基礎概念(CoPL)](http://www.fos.kuis.kyoto-u.ac.jp/~igarashi/CoPL/index.cgi)の演習問題を解くプログラムです。

判断を標準入力に与えると導出システムを判別して導出を標準出力に吐きます。

### 対応する導出システム
- EvalML1
- EvalML2
- EvalML3
- EvalRefML3
- EvalML4
- EvalML5
- TypingML4 (オプション`--no-poly`が必要です)
- PolyTypingML4

### デモ
```
$ dune exec bin/main.exe
# @l = 2 / x = @l |- !x + 3 evalto 5 / @l = 2
ML version: EvalRefML3
@l = 2 / x = @l |- !x + 3 evalto 5 / @l = 2 by E-Plus {
  @l = 2 / x = @l |- !x evalto 2 / @l = 2 by E-Deref {
    @l = 2 / x = @l |- x evalto @l / @l = 2 by E-Var {};
  };
  @l = 2 / x = @l |- 3 evalto 3 / @l = 2 by E-Int {};
  2 plus 3 is 5 by B-Plus {};
};
```

## 依存するソフトウェア
- ocaml (4.09.1)
- dune (2.4.0)
- menhir (20200211)
- ounit2 (2.2.2)

## 使い方
### 実行
```
dune exec bin/main.exe [-- --no-poly]
```
- windows以外の環境でも拡張子`.exe`を付けます。
- TypingML4を使用する場合、オプション`--no-poly`が必要です。

### 入力の形式
`[]`内は省略可能です。

#### 評価
```
[[<Store> /] <Env> |-] <Exp> evalto [<Value> / <Store>]
[[<Store> /] <Env> |-] <Exp> ;;
```

- 導出システムは入力から判別されます。
- EvalRefML3ではStoreのLocの名前を得るため`evalto`以降を必要としますが、`evalto`のかわりに`;;`を用いた場合はLocに自動生成された名前を使用します。

#### 型推論
推論された型が型変数を含んでいない場合:
```
<TEnv> |- <Expr> :
```

推論された型が型変数を含んでいる場合:
```
<TEnv> |- <Expr> : <Types> EOF
<TEnv> |- <Expr> : EOF
```