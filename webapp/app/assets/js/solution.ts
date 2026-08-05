import {InteractiveElement} from "./interactive-element.js";

export class Solution extends InteractiveElement<null, string> {
    private body: HTMLSpanElement;

    constructor() {
        super(null);
        this.body = document.createElement("div")
        this.appendChild(this.body);
        this.classList.add("solution");
        const styling = this.getAttribute("styling");
        if (styling == "explanation" || styling == "grading")
            this.classList.add("solution-" + styling);
        else if (styling != null)
            this.showError("Unknown solution styling: " + styling);
    }

    protected feedbackExternallyChanged(oldValue: string, newValue: string) {
        if (newValue == null) newValue = '';
        // @ts-ignore
        MathJax.typesetClear([this.body]);
        this.body.innerHTML = newValue;
        // @ts-ignore
        MathJax.typesetPromise([this.body]);
    }
}

customElements.define("etest-solution", Solution);
