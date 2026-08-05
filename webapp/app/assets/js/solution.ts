import {InteractiveElement} from "./interactive-element.js";

type SolutionFeedback = {
    points?: number,
    text: string,
}


export class Solution extends InteractiveElement<null, SolutionFeedback> {
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

    protected feedbackExternallyChanged(oldValue: SolutionFeedback, newValue: SolutionFeedback) {
        if (newValue == null) newValue = {"text": ""};
        // @ts-ignore
        MathJax.typesetClear([this.body]);
        this.body.innerHTML = newValue.text;
        // @ts-ignore
        MathJax.typesetPromise([this.body]);
    }
}

customElements.define("etest-solution", Solution);
