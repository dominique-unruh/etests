/* MathJax configuration shared by the preview web app (loaded as
   `lib/problems/js/mathjax-config.js`, see `webapp/app/views/assessment.scala.html`) and by static
   HTML rendering (inlined by `Assessment.mathjaxConfigJS`). Must be evaluated *before* the MathJax
   script itself is loaded.

   Besides the delimiters, this makes typeset math copyable: MathJax's CHTML output draws glyphs
   with CSS-generated content, so selecting and copying it yields nothing. A render action appends
   the original LaTeX source as a visually hidden <mjx-copytext> node (styled in
   `problems/src/main/assets/stylesheets/_mathjax.scss`), which is what ends up in the clipboard and
   what assistive technology reads. */
window.MathJax = {
    tex: {
        inlineMath: [['$', '$'], ['\\(', '\\)']],
        displayMath: [['$$', '$$'], ['\\[', '\\]']]
    },
    startup: {
        ready() {
            // The state at which addCopyText below runs (just after the output is rendered).
            MathJax._.core.MathItem.newState('ADDTEXT', 156);
            MathJax.startup.defaultReady();
        }
    },
    options: {
        /* No <mjx-assistive-mml>: MathJax would append a node holding a real <math> element to
           each expression, and any later MathJax.typesetPromise() pass over the whole page
           re-renders those (the MathML input jax picks up every <math> element in the page and
           ignores skipHtmlTags), nesting a second rendering inside each expression. <mjx-copytext>
           takes over its role for assistive technology. The menu setting has to be turned off as
           well, since the menu writes its value into enableAssistiveMml on startup. */
        enableAssistiveMml: false,
        menuOptions: {settings: {assistiveMml: false}},
        // Never treat the LaTeX source we inject as math input (it would be rendered again by a
        // later typesetting pass over the whole page).
        skipHtmlTags: {'[+]': ['mjx-copytext']},
        renderActions: {
            addCopyText: [156,
                (doc) => { for (const math of doc.math) MathJax.config.addCopyText(math, doc); },
                (math, doc) => MathJax.config.addCopyText(math, doc),
                false
            ]
        }
    },
    addCopyText(math, doc) {
        const STATE = MathJax._.core.MathItem.STATE;
        if (math.state() >= STATE.ADDTEXT) return;
        if (!math.isEscaped) {
            const adaptor = doc.adaptor;
            const latex = math.start.delim + math.math + math.end.delim;
            const text = adaptor.node('mjx-copytext', {role: 'math', 'aria-label': latex},
                [adaptor.text(latex)]);
            // Hide the visual rendering from assistive technology and offer the LaTeX instead
            // (what MathJax does for its assistive MathML node).
            adaptor.setAttribute(adaptor.firstChild(math.typesetRoot), 'aria-hidden', 'true');
            adaptor.setStyle(math.typesetRoot, 'position', 'relative');
            adaptor.append(math.typesetRoot, text);
        }
        math.state(STATE.ADDTEXT);
    }
};
