# Key Encapsulation Mechanisms

[TODO TEXT]

{{ ans1:textinput(correct=Seq("mod(k_0^e, N)=c", "mod(k_1^e, N)=c", "c=mod(k_0^e,N)", "mod(k_1^e,N)=c", "mod( k_0^e , N) = c",
                              "c = mod(k_0^e,N)", "mod(k_1^e,N) = c", "mod(k_0^e, N) = c", "c = mod(k_1^e, N)", "mod(k_0^e,N)=c",
                              "mod(k_0^e,N) = c", "c=mod(k_1^e,N)"),
                  partiallyCorrect=Map("c = k_1^e" -> (5.5 : assessments.Points),
                                       "c/(k_1^e) = 1" -> (5.5 : assessments.Points)),
                  wrong=Seq("k_0 = mod( (k^-e), N) or k_1= mod( (k^-e), N)", "none", "k_0=mod(c^(k_0/e),N)" ),
                  points=11) }}

{{
  grader:grader(points=11, check={ answers =>
    StackUtils.checkEquality(
        StackParser.parse(answers(ElementName("ans1"))).toSympy,
        StackParser.parse("mod(k_0^e, N)=c").toSympy)
      || StackUtils.checkEquality(
            StackParser.parse(answers(ElementName("ans1"))).toSympy,
            StackParser.parse("mod(k_1^e, N)=c").toSympy)
  })
}}