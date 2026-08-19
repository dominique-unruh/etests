import {InteractiveElement} from "./interactive-element.js";

type GradingFeedback = {
    points?: number,
    outcome?: string,
    error?: string,
    text: string,
}


export class Grading extends InteractiveElement<null, GradingFeedback> {
    private points: HTMLSpanElement;
    private outcome: HTMLSpanElement;
    private error: HTMLDivElement;
    private body: HTMLSpanElement;

    constructor() {
        super(null);
        this.points = document.createElement("div")
        this.points.classList.add("grading-points");
        this.appendChild(this.points);
        this.outcome = document.createElement("div")
        this.outcome.classList.add("grading-outcome");
        this.appendChild(this.outcome);
        this.error = document.createElement("div")
        this.error.classList.add("grading-error");
        this.appendChild(this.error);
        this.body = document.createElement("div")
        this.body.classList.add("grading-body");
        this.appendChild(this.body);
        this.classList.add("grading");
    }

    protected feedbackExternallyChanged(oldValue: GradingFeedback, newValue: GradingFeedback) {
        if (newValue == null) newValue = {"text": ""};
        if (newValue.points == null)
            this.points.innerText = "";
        else
            this.points.innerText = newValue.points + " points";
        this.outcome.className = "grading-outcome";
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

customElements.define("etest-grading", Grading);
