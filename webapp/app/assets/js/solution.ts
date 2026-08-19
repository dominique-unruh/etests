import {InteractiveElement} from "./interactive-element.js";

type SolutionFeedback = {
    points?: number,
    outcome?: string,
    error?: string,
    text: string,
}


export class Solution extends InteractiveElement<null, SolutionFeedback> {
    private points: HTMLSpanElement;
    private outcome: HTMLSpanElement;
    private error: HTMLDivElement;
    private body: HTMLSpanElement;

    constructor() {
        super(null);
        this.points = document.createElement("div")
        this.points.classList.add("solution-points");
        this.appendChild(this.points);
        this.outcome = document.createElement("div")
        this.outcome.classList.add("solution-outcome");
        this.appendChild(this.outcome);
        this.error = document.createElement("div")
        this.error.classList.add("solution-error");
        this.appendChild(this.error);
        this.body = document.createElement("div")
        this.body.classList.add("solution-body");
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
        if (newValue.points == null)
            this.points.innerText = "";
        else
            this.points.innerText = newValue.points + " points";
        this.outcome.className = "solution-outcome";
        if (newValue.outcome == null)
            this.outcome.innerText = "";
        else {
            this.outcome.innerText = newValue.outcome;
            this.outcome.classList.add("outcome-" + newValue.outcome);
        }
        this.error.innerText = newValue.error == null ? "" : newValue.error;
        // @ts-ignore
        MathJax.typesetClear([this.body]);
        this.body.innerHTML = newValue.text;
        // @ts-ignore
        MathJax.typesetPromise([this.body]);
    }
}

customElements.define("etest-solution", Solution);
