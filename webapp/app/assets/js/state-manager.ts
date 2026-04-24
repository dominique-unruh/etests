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

    constructor() {
        // TODO: fix
        document.body.onload = (event => this.init())
        // document.body.addEventListener("load", event => this.init());
    }

    init() {
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
    }

    dump() {
        console.log(this.interactiveElements, this.content);
    }
}

const stateManager = new StateManager();
// @ts-ignore
window.sm = stateManager;