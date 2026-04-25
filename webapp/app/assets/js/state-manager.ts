import {
    InteractiveElement,
    interactiveElementClass,
    JsonValue,
    ContentChangeEvent,
    contentChangeEventName
} from "./interactive-element.js";

export class StateManager {
    private interactiveElements: Map<string, InteractiveElement<JsonValue, JsonValue>> = new Map();
    private content: Map<string, Readonly<JsonValue>> = new Map();
    private examName: string;
    private assessmentName: string;
    private csrfToken: string;
    private needFeedbackUpdate: number = 0;
    private lastFeedbackUpdateRequest: number = 0;

    constructor(csrfToken: string, examName: string, assessmentName: string) {
        this.csrfToken = csrfToken
        this.examName = examName;
        this.assessmentName = assessmentName;
        if ((typeof examName !== "string") || (typeof assessmentName !== "string") || (typeof csrfToken !== "string"))
            throw Error("Invalid arguments")

        console.log("init")
        for (const element of document.getElementsByClassName(interactiveElementClass)) {
            if (!(element instanceof InteractiveElement))
                this.showError("Internal error: Not an InteractiveElement", element)
            const elementAs = element as InteractiveElement<JsonValue, JsonValue>;
            if (element.id == "")
                this.showError("Internal error: No HTML element ID", element)
            this.interactiveElements.set(element.id, elementAs)
            const content = elementAs.content;
            this.content.set(element.id, content);
            element.addEventListener(contentChangeEventName, event => this.elementContentChanged(event as ContentChangeEvent<JsonValue>))
        }
        window.setInterval(() => { this.askForFeedbackIfNeeded() }, 1000);
        this.needFeedbackUpdate = 1;
        this.lastFeedbackUpdateRequest = 0
    }

    private elementContentChanged(event: ContentChangeEvent<JsonValue>) {
        this.clearErrors()
        const element = event.target as InteractiveElement<JsonValue, JsonValue>;
        const id = element.id;
        const newElementContent = event.detail.newValue;
        // TODO check whether this is actually a change
        console.log(this, this.content);
        this.content.set(id, newElementContent);
        this.needFeedbackUpdate = .2;
        console.log("Content changed from interactive element", this.content, id);
    }

    setContent(newContent: Record<string, Readonly<JsonValue>>) {
        console.log("setContent", newContent)
        for (const elementId in newContent) {
            const state = newContent[elementId];
            // We don't check whether content even changed because `element.content = ...` checks it anyway.
            this.content.set(elementId, state);
            const element = this.interactiveElements.get(elementId);
            if (element == null)
                console.warn("Unknown element", elementId);
            else
                element.content = state;
        }
        console.log("Content changed by setting it", this.content);
    }

    private setFeedback(newFeedback: Record<string, Readonly<JsonValue>>) {
        console.log("Got feedback", newFeedback);
        for (const elementId in newFeedback) {
            const feedback = newFeedback[elementId];
            // We don't check whether feedback even changed because `element.feedback = ...` checks it anyway.
            const element = this.interactiveElements.get(elementId);
            if (element == null)
                console.warn("Unknown element", elementId);
            else
                element.feedback = feedback;
        }
    }


    private askForFeedbackIfNeeded() {
        if (this.needFeedbackUpdate > 0) {
            if (Date.now() - this.lastFeedbackUpdateRequest >= this.needFeedbackUpdate * 1000)
                this.askForFeedback()
            else
                console.log("Feedback request: delayed", this.needFeedbackUpdate)
        }
    }

    private ajaxCounter = 0
    private currentFeedbackCounter = -1;
    private async askForFeedback() {
        // @ts-ignore
        const url: string = jsRoutes.controllers.AssessmentController.getFeedback(this.examName, this.assessmentName).url
        // console.log(url);
        const myCounter = this.ajaxCounter;
        this.currentFeedbackCounter = myCounter;
        this.ajaxCounter += 1;
        const content = {};
        this.content.forEach((value, key) => { content[key] = value; });
        const prevNeedFeedbackUpdate = this.needFeedbackUpdate
        this.needFeedbackUpdate = 0;
        this.lastFeedbackUpdateRequest = Date.now()
        const response = await fetch(url, {
            method: "POST",
            headers: {
                "Content-Type": "application/json",
                'CSRF-Token': this.csrfToken,
            },
            body: JSON.stringify(content),
            })
        if (!response.ok) {
            this.showError(`HTTP error: ${response.status}`);
            return;
        }
        const parsed = await response.json()
        const feedback = parsed.feedback
        const timedout: boolean = parsed.timedout
        if (myCounter != this.currentFeedbackCounter)
            return;
        this.setFeedback(feedback);
        if (timedout) {
            console.log("(Feedback had timeout)")
            this.needFeedbackUpdate = prevNeedFeedbackUpdate * 2;
        }
    }

    async askForAnswers(kind: string) {
        this.clearErrors()
        // @ts-ignore
        const url: string = jsRoutes.controllers.AssessmentController.loadAnswers(this.examName, this.assessmentName, kind).url
        const content = {};
        this.content.forEach((value, key) => { content[key] = value; });
        const response = await fetch(url, {
            method: "POST",
            headers: {
                "Content-Type": "application/json",
                'CSRF-Token': this.csrfToken,
            },
            body: JSON.stringify(content),
        })
        if (!response.ok) {
            this.showError(`HTTP error: ${response.status}`);
            return;
        }
        const parsed = await response.json()
        if (typeof parsed === 'string') {
            this.showError("Ask for answers: Error: " + parsed)
            return;
        }
        this.setContent(parsed)
    }

    private errorTime = 0;

    clearErrors() {
        if (Date.now() - this.errorTime > 3000)
            document.getElementById("error").innerHTML = '';
    }


    showError(error: string, ...additional) {
        this.errorTime = Date.now();
        document.getElementById("error").innerText += error + "\n";
        console.error(error, ...additional)
    }

    async randomStudent() {
        this.clearErrors()
        // @ts-ignore
        const url: string = jsRoutes.controllers.AssessmentController.randomStudent(this.examName).url
        const response = await fetch(url, {
            method: "GET",
            headers: {
                'CSRF-Token': this.csrfToken,
            },
        })
        if (!response.ok)
            this.showError(`HTTP error: ${response.status}`);
        const regno = await response.text()
        // @ts-ignore
        document.getElementById("etest-registration-number").content = regno;
        await this.askForAnswers('student')
    }

    showDynexitePdf() {
        // @ts-ignore
        let regno: string = document.getElementById("etest-registration-number").content
        // @ts-ignore
        let url = jsRoutes.controllers.AssessmentController.dynexitePdf(this.examName, regno).url
        window.open(url, "_blank")
    }

    showDynexiteLink() {
        // @ts-ignore
        let regno = document.getElementById("etest-registration-number").content
        // @ts-ignore
        let url = jsRoutes.controllers.AssessmentController.dynexiteLink(this.examName, regno).url
        window.open(url, "_blank")
    }

    async showDynexiteAnswers() {
        this.clearErrors()
        // @ts-ignore
        let regno: string = document.getElementById("etest-registration-number").content
        // @ts-ignore
        const url: string = jsRoutes.controllers.AssessmentController.dynexiteAnswers(this.examName, this.assessmentName, regno).url

        const response = await fetch(url, {
            method: "GET",
            headers: {
                'CSRF-Token': this.csrfToken,
            },
        })
        if (!response.ok)
            this.showError(`HTTP error: ${response.status}`);
        this.showError(await response.text())
    }


}

