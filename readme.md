# CoPL ML

## これは何
[プログラミング言語の基礎概念(CoPL)](https://www.amazon.co.jp/%E3%83%97%E3%83%AD%E3%82%B0%E3%83%A9%E3%83%9F%E3%83%B3%E3%82%B0%E8%A8%80%E8%AA%9E%E3%81%AE%E5%9F%BA%E7%A4%8E%E6%A6%82%E5%BF%B5-%E3%83%A9%E3%82%A4%E3%83%96%E3%83%A9%E3%83%AA%E6%83%85%E5%A0%B1%E5%AD%A6%E3%82%B3%E3%82%A2%E3%83%BB%E3%83%86%E3%82%AD%E3%82%B9%E3%83%88-%E4%BA%94%E5%8D%81%E5%B5%90-%E6%B7%B3/dp/4781912850)の演習問題を解くプログラムです。

判断を入力すると導出システムを判別して導出を吐きます。

### 対応する導出システム
- EvalML1
- EvalML2
- EvalML3
- EvalRefML3
- EvalML4
- EvalML5

### 使用例
```
# echo '@l = 2 / x = @l |- !x + 3 evalto 5 / @l = 2' | dune exec bin/main.exe
ML version: RefML3   
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
```
echo <Judgment> | dune exec bin/main.exe
echo [[<Store> /] <Env> |-] <Expr>;; | dune exec bin/main.exe
```
(windows以外の環境でも拡張子`.exe`を付けます)