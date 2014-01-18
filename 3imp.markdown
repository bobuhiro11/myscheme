# why scheme is best?

* $B%G!<%?$H%W%m%0%i%`$,(BS$B<0$GE}0l$5$l$F$$$k!%(B
* $B%^%/%m$G9=J8$rDI2C$G$-$k!%(Bif$B$@$1$N8@8l$G$b(Bunless$B$rDI2C$G$-$k!%(B
* $B7QB3$,$"$k!$$$$o$f$k(BC$B%9%?%C%/$H$+0c$&9=@.$N4X?t8F$S=P$7$,$"$k!%(B

Three Implementation Model for Scheme

Chapel Hill 1987

# Abstract

- $B%R!<%W%Y!<%9!$%9%?%C%/%Y!<%9!$(Bstring$B%Y!<%9(B($B%^%k%A%W%m%;%C%5$KE,$7$F$$$k(B)
- $B%R!<%W%Y!<%9$,:G=i(B
- $B%R!<%W%Y!<%9(B
  * $B%Q%i%a!<%?$N%j%9%H!$4D6-$NB+G{!$%3!<%k%U%l!<%`$O%R!<%W$K(B
- $B%9%?%C%/%Y!<%9(B
  * $B$G$-$k8B$j%9%?%C%/$K(B
  * $B7k2L$H$7$F!$%R!<%W$N3d$jEv$F$,>/$J$/$J$j!$%a%b%j$N;2>H$,8:$k!$L?NaNs$,8:$k!$(BGC$BBP>]$,8:$k(B
  * $B:n<T(BChez Scheme$B$G:NMQ(B
- string$B%Y!<%9(B
  * scheme$B$KE,$7$?(BFFP$B8@8l$KJQ49$5$l$k(B
  * FFP$B8@8l$O(BFFP$B%^%7%s(B(a multiple-processor string-reduction computer)$B$GD>@\<B8z$G$-$k!%(B

# Chapter 1: Introduction

- $B%9%?%C%/%Y!<%9$O$R$H$D$N%3%s%T%e!<%?(B
- string$B%Y!<%9$O%^%k%A%W%m%;%C%5(B
- ML$B$b(BChez Scheme$B$H$OFH<+$K<BAu$5$l$k(B
- string$B%Y!<%9$O(Bsequential computer$B>e$G$N%$%s%?%W%j%?$G$O%F%9%H$5$l$?$,!$$^$@<BAu$5$l$F$$$J$$(B
- Scheme$B$OB>J}8@$H$A$,$$!$@EE*%9%3!<%W(B,block-structured$B$G$"$j!$(B
- $B4X?t$H7QB3$rBh0l5i%*%V%8%'%/%H$H$9$k(B
- ML$B$O@EE*%9%3!<%W!$Bh0l5i4X?t$,$"$k$,!$7QB3$HJQ?t3d$jEv$F(B(variable assignments)$B$,$J$$(B
  * $BBh0l5i$H$O!$0z?t!$JV$jCM$K$G$-$F!$FH<+$K3JG<$G$-$k!%(B
  * $B%j%9%H!$%7%s%\%k!$J8;zNs!$?tCM$J$I(B
  * $B$[$H$s$I$O%9%+%i!<NL$N$_$@$C$?(B
- $BHs>o$K$K;w$F$$$k$?$a(BML,CL$B$K$b1~MQ2DG=(B
- *$B%Y!<%9$N0c$$$O%G!<%?9=B$$NI=8=!$%=!<%9%3!<%I$+$i%*%V%8%'%/%H%3!<%I$X$NJQ49!$(B
- $B%*%V%8%'%/%H%3!<%I$N<B9T$G$"$k(B
- $B%/%m!<%8%c$H7QB3$K$O!$%9%?%C%/%U%l!<%`$N%R!<%W3d$jEv$F$,I,MW$H$+$s$,$($i$l$F$$$?(B
- scheme$B$O(Bstatements$B$d%k!<%W$h$j!$4X?t$d:F5"$r$h$/;H$&(B
- $BJQ?t3d$jEv$F$O>/$J$$798~$,$"$k(B
- $BK\>O$N;D$j$G!$4X?t7?8@8l$NGX7J!$<BAu%F%/%K%C%/!$%^%k%A%W%m%;%C%5$H$N4X78$r$N=R$Y$k(B
- 2$B>O$O(Bscheme$B$NJ8K!!$FCD'$J$I(B
- 3,4,5$B>O$O$=$l$>$l$N%b%G%k$K$D$$$F(B
- $BIUO?(BA$B$G%9%?%C%/%Y!<%9$H%R!<%W%Y!<%9$NHf3S(B

### 2.1.1 Core Syntactic Forms.

- $B%3%"8@8l$O0J2<$N$H$*$j(B
- $B$?$@$7!$#2$D$N[#Kf$5$,$"$k(B
  * $BA4It$O$8$a$K%^%C%A$7$F$7$^$&(B($BA4$F$NI=8=$O(Bobject$B$@$+$i(B)
  * quote,lambda,if,set!,call/cc$B$K%^%C%A$9$k$N$O:G8e$K$b%^%C%A$7$F$7$^$&(B
- object
  * list,symbol$B0J30$NA4$F$ODj?t$H$9$k(B
  * $B$3$l$i$O$b$C$HJL$NJ8K!$K%^%C%A$9$k$O$:(B
- variable
  * $BA4$F$N%7%s%\%k$OJQ?tB+G{$X$N;2>H(B
  * lambda$B$r<h$j0O$`$3$H$GG{$i$l$k(B??
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

- Scheme-84,C-Scheme,Scheme-311$B$G;H$o$l$?(B
- 1$B@a$GF14|$HLdBjE@(B
- 2$B@a$G%G!<%?9=B$(B
- 3$B@a$GL?Na(B
- 4$B@a$G<BAu(B
- 5$B@a$G:GE,2=(B

# 3.1 Motivation and Problems

- Algol60,C,Pascal$B$N$h$&$J@EE*%9%3!<%W8@8l$G$O%9%?%C%/$O%3!<%k%U%l!<%`$K;H$o$l$k(B
- $B%3!<%k%U%l!<%`$K$O!$La$j%"%I%l%9!$JQ?tB+G{!$A0$N%3!<%k%U%l!<%`$X$N;2>H$,4^$^$l$k(B
- $BJQ?tB+G{(B: $B0z?t$H%m!<%+%kJQ?t$N$3$H(B
- $BIaDL$O8F$S=P$785$G:n$i$l$k(B
- $B8F$S=P$7@h$G$O%m!<%+%kJQ?t$rDj5A$7$F%3!<%k%U%l!<%`$r3HD%$9$k(B
- $B$3$3$^$G$O(BC$B$N4X?t8F$S=P$7$HF1$8(B
- scheme$B$G$OBh0l5i%/%m!<%8%c$HBh0l5i7QB3$K$h$j!$$3$l$O8zN(E*$G$J$$(B
- $BBh0l5i%/%m!<%8%c$G$O!$ITL@3N$K(B(indefinitely)$B0z?t$rB+G{$9$kG=NO$r$b$D(B
- $BFC$K!$%/%m!<%8%c$HJ];}$5$l$?B+G{$O!$(Bcall$B$,(Breturn$B$5$l$?$"$H!$%9%?%C%/%U%l!<%`$,:o=|$5$l$?$"$H$b;D$C$F$$$k$+$b$7$l$J$$(B
- $B$3$NM}M3$+$i0z?tB+G{$r%9%?%C%/%U%l!<%`$KF~$l$k$N$O=PMh$J$$(B
- $BBe$o$j$K!$%R!<%W>e$K4D6-$,:n$i$l$k(B($B0z?t$N$?$a(B)
- $B%3!<%k%U%l!<%`$K$O$3$N4D6-$X$N;2>H$r$$$l$k(B
- $B%/%m!<%8%c$,:n$i$l$?$H$-!$$3$N4D6-$X$N;2>H$O%/%m!<%8%c%*%V%8%'%/%H$NCf$KDI2C$5$l$k!%(B
- Moving the variable bindings into the heap saves the bindings from being overwritten as the stack shrinks and grows
- $B%3!<%k%U%l!<%`$,%9%?%C%/>e$K$"$l$P!$DI2C$N%*!<%P%X%C%I$O4D6-$N3d$jEv$F$N$_(B
- $B$7$+$7!$Bh0l5i7QB3$G$O!$%3!<%k%U%l!<%`$K2C$(4D6-$N$N%R!<%W3d$jEv$F$,I,MW(B
- $B$3$l$OIaDL$N<BAu$G$O7QB3$O%3!<%k%U%l!<%`$K;2>H$rJ];}$9$k$b$N$@$+$i(B
- $B$3$3$G7QB3$O!$8F$P$l$?;~7QB3$,3MF@$5$l$?%]%$%s%H$XLa$k%/%m!<%8%c$G$"$k$3$H$r;W$$=P$7$F$[$7$$(B
- $B<+A3$J2r7hJ}K!$O!$%R!<%W3d$jEv$F$5$l$?%9%?%C%/%U%l!<%`$NO"7k%j%9%H$r;H$&J}K!$G$"$k(B
- $B%9%?%C%/$,Bg$-$/$J$k$K$D$l$F!$?7$7$$%U%l!<%`$,%R!<%W$N;H$o$l$F$$$J$$ItJ,$K3d$jEv$F$i$l$k(B
- $B$J$<$J$i!$8E$$%9%?%C%/%U%l!<%`$O$=$N$^$^;D$C$F$$$k$+$i(B
- $B%3!<%k%U%l!<%`$H4D6-$N%R!<%W3d$jEv$F$N=EBg$JLdBj$O!$%R!<%W$N;HMQK!$K$h$k%*!<%P%X%C%I$G$"$k(B
- $BD>@\E*$J%3%9%H(B
  * $B%3!<%k%U%l!<%`$d4D6-$N3d$jEv$F$N:]!$6u$-NN0h$rC5$9$3$H(B
  * $B4D6-$d%U%l!<%`$r%j%s%/$G$?$I$k$3$H(B
- $B4V@\E*$J%3%9%H(B
  * $B4D6-$d%3!<%k%U%l!<%`$?$a$NNN0h$N:FMxMQ(B
  * ($B$=$NB>(B)$B$3$l$O5pBg$J%a%b%j6u4V$r$b$D2>A[%a%b%j$G$OLdBj$G$O$J$$$H4|BT$9$k?M$b$$$k(B
  * $B$7$+$72>A[%a%b%j$N%Q%U%)!<%^%s%9$O;2>H6I=j@-$K$h$k(B
  * $B%9%?%C%/%Y!<%9$G$OF1$8%9%?%C%/%U%l!<%`$r:FMxMQ$9$k(B
  * $B%R!<%W%Y!<%9$G$OIQHK$K2rJ|$5$l$J$$8B$j(B(GC$B$N%*!<%P%X%C%I$r0UL#$9$k(B)$B!$?7$7$$%Z!<%8$r;2>H$9$k$3$H$K$J$k(B
- $B$5$i$K!$%9%?%C%/$G$J$/%R!<%W$r:NMQ$9$k$H!$%O!<%I%&%'%"!$%^%$%/%m%3!<%I$,Ds6!$7$F$$$k(Bpush,pop,index$BL?Na!$(Bcall,return$BL?Na$,;H$($J$$(B

## 3.2 Representation of Data Structure

- core$B8@8l$G$O(B5$B$D$N%G!<%?9=B$$,$"$k(B
  * $B4D6-(B environments
  * $B%3!<%k%U%l!<%`(B call frames
  * $B%3%s%H%m!<%k%9%?%C%/(B control stack
  * $B%/%m!<%8%c(B closures
  * $B7QB3(B continuations

## 3.2.1 Environments

- $B4D6-$O6;It$K;w$F$$$k(B

```
$B%$%a!<%8(B
((($BJQ?t(B1 $BJQ?t(B2) .  ($BCM(B1 $BCM(B2))
 (($BJQ?t(BA) .  ($BCM(BA)))
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

- 3.4$B@a$G$O(Bvariable ribs$B$+$i(Bvalue ribs$B$K2~NI$5$l$k(B

##
