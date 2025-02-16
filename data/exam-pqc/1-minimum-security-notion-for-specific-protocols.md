# Minimum security notion for specific protocols

You have an encryption scheme $(\mathit{Keygen},\mathit{Enc},\mathit{Dec})$ with message space $\mathcal M$. In each of the following situations, what is the weakest security notion that guarantees that the desired goal is fulfilled? ("Weaker" security means "less secure".)

$\mathit{pk},\mathit{sk}$ will denote the public and secret key. We assume that they are generated using the key generation algorithm, and that the adversary is given $\mathit{pk}$ but not $\mathit{sk}$. Bob and Charlie are considered honest parties throughout and know  $\mathit{sk}$ .

---

{{ question1:multiplechoice(
       options=Map("ow" -> "OW-CPA (one-way)",
                   "cpa" -> "IND-CPA (chosen plaintext attacks)",
                   "cca" -> "IND-CCA (chosen ciphertext attacks)",
                   "none" -> "Neither of them fulfills the goal"),
       correct="ow", points=3.7) }}

Bob picks a uniformly random $k\in\mathcal M$ and sends $\mathit{Enc}(\mathit{pk},k)$ over the network to Charlie. The adversary can intercept (i.e., read) this ciphertext.

If the adversary then manages to send $k$ to Bob, the adversary will successfully login.

**Goal:** the adversary should not be able to login (except with small probability).

---

{{ question2:multiplechoice(
       options=Map("ow" -> "OW-CPA (one-way)",
                   "cpa" -> "IND-CPA (chosen plaintext attacks)",
                   "cca" -> "IND-CCA (chosen ciphertext attacks)",
                   "none" -> "Neither of them fulfills the goal"),
       correct="cpa", points=3.7) }}

Bob picks uniformly random $k,l\in\{0,1\}^n$ and sends $\mathit{Enc}(\mathit{pk},k\Vert l)$ over the network to Charlie. ($\Vert$ means concatenation.) The adversary can intercept (i.e., read) this ciphertext.

If the adversary then manages to send $k$ (not $k\Vert l$) to Bob, the adversary will successfully login.

**Goal:** the adversary should not be able to login (except with small probability).

---

{{ question3:multiplechoice(
       options=Map("ow" -> "OW-CPA (one-way)",
                   "cpa" -> "IND-CPA (chosen plaintext attacks)",
                   "cca" -> "IND-CCA (chosen ciphertext attacks)",
                   "none" -> "Neither of them fulfills the goal"),
       correct="cca", points=3.7) }}

Bob and Charlie share the same uniformly random $k\in\{0,1\}^n$. Then Bob sends $\mathit{Enc}(\mathit{pk}, k\Vert\text{``transfer 1000 EUR to Bob''})$ over the network to Charlie. When Charlie receives a ciphertext, they decrypt it using $\mathit{sk}$. If the resulting plaintext is of the form $k\Vert\text{``transfer N EUR to P''}$ (where $k$ must be the shared value), then Charlie sends N Euro to the person P.

The adversary can intercept the cipehertext and modify it before passing it on to Charlie.

Dave is some other person who is friends with the adversary.

**Goal:** The adversary cannot make Charlie transfer 1000 Euro to Dave.
