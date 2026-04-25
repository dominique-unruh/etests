export type JsonPrimitive = string | number | boolean | null;
export type JsonValue = JsonPrimitive | JsonObject | JsonValue[];
export type JsonObject = { [key: string]: JsonValue };

export interface ContentChangeDetail<C extends JsonValue> {
    oldValue: C;
    newValue: C;
}

export type ContentChangeEvent<C extends JsonValue> =
    CustomEvent<ContentChangeDetail<C>>;

export const interactiveElementClass = "etest-interactive-element";
export const contentChangeEventName = "etest-content-change";

export abstract class InteractiveElement<C extends JsonValue, F extends JsonValue>
    extends HTMLElement {
    private _content: Readonly<C> = null;
    private _feedback: Readonly<F> = null;

    constructor(initialContent: C) {
        super();
        this._content = initialContent
        console.log("Initializing", this);
        this.classList.add(interactiveElementClass);
    }

    get content(): Readonly<C> {
        return this._content;
    }

    get feedback(): Readonly<F> {
        return this._feedback;
    }

    protected updateContent(newContent: C) {
        const oldContent = this._content;

        // TODO deep comparison
        if (Object.is(oldContent, newContent)) {
            return;
        }

        this._content = newContent;
        this.contentExternallyChanged(oldContent, newContent);

        this.dispatchEvent(
            new CustomEvent<ContentChangeDetail<C>>(contentChangeEventName, {
                detail: {
                    oldValue: oldContent,
                    newValue: newContent,
                },
                bubbles: true,
                composed: true,
            })
        );
    }

    set content(newContent: Readonly<C>) {
        this.updateContent(newContent)
    }

    set feedback(newFeedback: Readonly<F>) {
        const oldFeedback = this._feedback;

        // TODO deep comparison
        if (Object.is(oldFeedback, newFeedback)) {
            return;
        }

        this._feedback = newFeedback;
        this.feedbackExternallyChanged(oldFeedback, newFeedback);
    }

    protected contentExternallyChanged(oldValue: C, newValue: C): void {
        // Optional hook for subclasses
    }

    protected feedbackExternallyChanged(oldValue: F, newValue: F): void {
        // Optional hook for subclasses
    }
}