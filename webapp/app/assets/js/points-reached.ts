import {InteractiveElement, JsonObject} from "./interactive-element.js";

type GradingResult = {
    points?: string,
    report?: string,
    error?: string,
    processing?: true,
}

export class PointsReached extends InteractiveElement<null, GradingResult> {
    private pointsSpan: HTMLSpanElement;

    constructor() {
        super(null);
        const reachable = this.getAttribute('reachable');
        this.innerHTML = `<h3>Grading (<span id="grading-points"></span>/${reachable} points)</h3>`
        this.pointsSpan = this.getElementsByTagName("span")[0]
    }

    protected feedbackExternallyChanged(_: GradingResult, result: GradingResult) {
        if (result.processing == true)
            this.pointsSpan.innerText = "⌛"
        else if (result.points == null)
            this.pointsSpan.innerText = "?"
        else
            this.pointsSpan.innerText = result.points
    }
}

customElements.define("etest-points-reached", PointsReached);
