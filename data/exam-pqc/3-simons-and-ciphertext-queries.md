# Simon's and ciphertext queries

Let $E(k\Vert l,m)$ denote the encryption of $m$ using the Even-Mansour encryption scheme with key $k\Vert l$ and underlying permutation $P$. ($k,l\in\{0,1\}^n$. $\Vert$ means concatenation.)

Consider the following functions:

$$
\begin{align}
f_1(x) &:= P(x) \oplus E(kl,x)\\
f_2(x) &:= P(P(x)\oplus x)\\
f_3(x) &:= E(P(x)\Vert x, 0)\\
f_4(x) &:= E(k\Vert x, 0)
\end{align}
$$

For each $f_i$, we want to use Simon's algorithm to find some $t$ such that $f_i(x\oplus t)=f_i(x)$ for all $x$. (If such exists. You do not need to decide whether it actually exists for given $f_i$.)

We consider a chosen message attack on Even-Mansour.

----

In case of a **non-superposition** (but post-quantum) attack, for which functions $f_i$ can we (the adversary) use Simon's algorithm to find $t$?

{{ nonsup1: multiplechoice(options=Map("yes" -> "Yes", "no" -> "No"), correct="no", points=1.45) }}
$f_1$

{{ nonsup2: multiplechoice(options=Map("yes" -> "Yes", "no" -> "No"), correct="yes", points=1.45) }}
$f_2$

{{ nonsup3: multiplechoice(options=Map("yes" -> "Yes", "no" -> "No"), correct="yes", points=1.45) }}
$f_3$

{{ nonsup4: multiplechoice(options=Map("yes" -> "Yes", "no" -> "No"), correct="no", points=1.45) }}
$f_4$

----

In case of a **superposition attack**, for which functions $f_i$ can we (the adversary) use Simon's algorithm to find $t$?

{{ sup1: multiplechoice(options=Map("yes" -> "Yes", "no" -> "No"), correct="yes", points=1.4) }}
$f_1$

{{ sup2: multiplechoice(options=Map("yes" -> "Yes", "no" -> "No"), correct="yes", points=1.4) }}
$f_2$

{{ sup3: multiplechoice(options=Map("yes" -> "Yes", "no" -> "No"), correct="yes", points=1.4) }}
$f_3$

{{ sup4: multiplechoice(options=Map("yes" -> "Yes", "no" -> "No"), correct="no", points=1.4) }}
$f_4$


