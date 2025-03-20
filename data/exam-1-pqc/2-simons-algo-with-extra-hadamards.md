# Simon's algorithm with extra Hadamards

Recall Simon's algorithm for finding a XOR-period of the function $f:\{0,1\}^n\to\{0,1\}^m$. As its main step, it executes the following quantum circuit (multiple times):

[TODO PICTURE]

We have introduced gaps in the picture, depicted as numbered circles 1–5. In which of these gaps can we insert $H^{\otimes n}$ or $H^{\otimes m}$, respectively, so that the algorithm still works correctly? (I.e., so that Simon's algorithm will find a period if there is one when using the circuit with added Hadamard gates.)

Yes means you can introduce a Hadamard, no means you cannot.

{{ gap1: multiplechoice(options=Map("yes" -> "Yes", "no" -> "No"), correct="no", points=2.3) }}
Gap 1

{{ gap2: multiplechoice(options=Map("yes" -> "Yes", "no" -> "No"), correct="yes", points=2.3) }}
Gap 2

{{ gap3: multiplechoice(options=Map("yes" -> "Yes", "no" -> "No"), correct="no", points=2.3) }}
Gap 3

{{ gap4: multiplechoice(options=Map("yes" -> "Yes", "no" -> "No"), correct="yes", points=2.3) }}
Gap 4

{{ gap5: multiplechoice(options=Map("yes" -> "Yes", "no" -> "No"), correct="yes", points=2.3) }}
Gap 5
