# Feistel Schemes

Consider the 3-round Feistel-Scheme:

[TODO PICTURE]

...

{{ id_1:textinput(correct=Seq("[m_2, m_1]", "[m_2,m_1]", "[m_2 , m_1]"),
                  wrong=Seq("(m_2,m_1)"),
                  points=5) }}

...

{{ const_1:textinput(correct=Seq("[m_1 + c, m_2]", "[m_1+c, m_2]", "[m_1+c,m_2]", "[c+m_1,m_2]", "[m_1 + c,m_2]"),
                     wrong=Seq("[c,c]", "[m_1 + m_2 + c, m_1]", "[m1 + c, m_2]", "(m_1+c,m_2)"),
                     points=6) }}

...
