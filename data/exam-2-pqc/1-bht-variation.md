# BHT Variation

[TODO TEXT]

Probability L_D=1? {{ prob_LD:textinput(correct=Nil, wrong=Nil, points=123) }}

Num evals for set D? {{ rt_D:textinput(correct=Nil, wrong=Nil, points=123) }}

Num evals for grover? {{ rt_Grover:textinput(correct=Nil, wrong=Nil, points=123) }}

Optimal c? {{ opt_c:textinput(correct=Nil, wrong=Nil, points=123) }}

Runtime overall? {{ rt_overall:textinput(correct=Nil, wrong=Nil, points=123) }}



{{
grader:grader(points=12, 
    check={ answers =>
        StackUtils.checkEquality(
            StackParser.parse(answers(ElementName("ans1"))).toSympy,
            StackParser.parse("mod(k_0^e, N)=c").toSympy)
        || StackUtils.checkEquality(
            StackParser.parse(answers(ElementName("ans1"))).toSympy,
            StackParser.parse("mod(k_1^e, N)=c").toSympy)
        })
}}