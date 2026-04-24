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

    constructor(csrfToken: string, examName: string, assessmentName: string) {
        this.csrfToken = csrfToken
        this.examName = examName;
        this.assessmentName = assessmentName;
        if ((typeof examName !== "string") || (typeof assessmentName !== "string") || (typeof csrfToken !== "string"))
            throw Error("Invalid arguments")

        console.log("init")
        for (const element of document.getElementsByClassName(interactiveElementClass)) {
            if (!(element instanceof InteractiveElement))
                console.error("Not an InteractiveElement", element)
            const elementAs = element as InteractiveElement<JsonValue, JsonValue>;
            if (element.id == "")
                console.error("No ID", element)
            this.interactiveElements.set(element.id, elementAs)
            const content = elementAs.content;
            if (content != null)
                this.content.set(element.id, content);
            element.addEventListener(contentChangeEventName, event => this.elementContentChanged(event as ContentChangeEvent<JsonValue>))
        }
        this.dump();
    }

    private elementContentChanged(event: ContentChangeEvent<JsonValue>) {
        const element = event.target as InteractiveElement<JsonValue, JsonValue>;
        const id = element.id;
        const newElementContent = event.detail.newValue;
        // TODO check whether this is actually a change
        console.log(this, this.content);
        this.content.set(id, newElementContent);
        this.dump();
        this.askForFeedback();
    }

    setContent(newContent: Record<string, Readonly<JsonValue>>) {
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
        this.dump();
    }

    setFeedback(newFeedback: Record<string, Readonly<JsonValue>>) {
        for (const elementId in newFeedback) {
            const feedback = newFeedback[elementId];
            // We don't check whether feedback even changed because `element.feedback = ...` checks it anyway.
            const element = this.interactiveElements.get(elementId);
            if (element == null)
                console.warn("Unknown element", elementId);
            else
                element.feedback = feedback;
        }
        this.dump();
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
        const response = await fetch(url, {
            method: "POST",
            headers: {
                "Content-Type": "application/json",
                'CSRF-Token': this.csrfToken,
            },
            body: JSON.stringify(content),
            })
        if (!response.ok)
            throw new Error(`HTTP error: ${response.status}`);
        const parsed = await response.json()
        const feedback = parsed.feedback
        if (myCounter != this.currentFeedbackCounter)
            return;
        this.setFeedback(feedback)
    }

    dump() {
        console.log(this.interactiveElements, this.content);
    }
}

/*
const stateManager = new StateManager();
// @ts-ignore
window.sm = stateManager;*/
