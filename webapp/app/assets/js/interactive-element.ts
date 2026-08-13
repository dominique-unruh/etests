/**
 * A JSON primitive: the leaf values allowed in JSON.
 */
export type JsonPrimitive = string | number | boolean | null;

/**
 * Any JSON-serializable value: a primitive, an object, or an array of values.
 * The content and feedback of an {@link InteractiveElement} must be JSON values
 * so they can be sent to/from the server and persisted.
 */
export type JsonValue = JsonPrimitive | JsonObject | JsonValue[];

/**
 * A JSON object: a string-keyed map of {@link JsonValue}s.
 */
export type JsonObject = { [key: string]: JsonValue };

/**
 * Payload of a {@link contentChangeEventName} event, describing a transition of
 * an interactive element's content from `oldValue` to `newValue`.
 *
 * @typeParam C - the content type of the element that changed.
 */
export interface ContentChangeDetail<C extends JsonValue> {
    oldValue: C;
    newValue: C;
}

/**
 * The concrete `CustomEvent` type dispatched under {@link contentChangeEventName}.
 * Its `detail` is a {@link ContentChangeDetail}.
 */
export type ContentChangeEvent<C extends JsonValue> =
    CustomEvent<ContentChangeDetail<C>>;

/**
 * CSS class added to every {@link InteractiveElement}. Lets the app find/style
 * all interactive elements on a page.
 */
export const interactiveElementClass = "etest-interactive-element";

/**
 * Name of the `CustomEvent` fired whenever an element's content changes. The
 * event bubbles and is composed, so listeners higher in the tree (e.g. the
 * state manager) can observe changes from any element. Payload is a
 * {@link ContentChangeDetail}.
 */
export const contentChangeEventName = "etest-content-change";

/**
 * Base class for all interactive page elements (text inputs, multiple choice,
 * etc.) rendered in the web app. It is a custom element (`HTMLElement`) that
 * carries two pieces of state:
 *
 * - **content** (`C`): the student's answer / current value. Mutating it (via
 *   the setter or {@link updateContent}) fires a {@link contentChangeEventName}
 *   event so the rest of the app can react and persist it.
 * - **feedback** (`F`): grading feedback pushed in from the outside; display-only
 *   from the element's perspective.
 *
 * Both must be {@link JsonValue}s so they can be serialized. Subclasses override
 * the `*ExternallyChanged` hooks to sync the DOM when these values are set from
 * outside, and call {@link updateContent} when the user edits the value.
 *
 * @typeParam C - the content (answer) type.
 * @typeParam F - the feedback type (`null` if the element has no feedback).
 */
export abstract class InteractiveElement<C extends JsonValue, F extends JsonValue>
    extends HTMLElement {
    /** Current content (student answer). Treated as immutable; replaced wholesale. */
    private _content: Readonly<C> = null;
    /** Current feedback. Treated as immutable; replaced wholesale. */
    private _feedback: Readonly<F> = null;

    /**
     * @param initialContent - the starting content, set before the element is
     *   inserted into the DOM. Does not fire a change event.
     */
    constructor(initialContent: C) {
        super();
        this._content = initialContent
        console.log("Initializing", this);
        this.classList.add(interactiveElementClass);
    }

    /** The current content (student answer). */
    get content(): Readonly<C> {
        return this._content;
    }

    /** The current feedback (grading result), or `null` if none set. */
    get feedback(): Readonly<F> {
        return this._feedback;
    }

    /**
     * Replace the content and, if it actually changed, notify both the subclass
     * (via {@link contentExternallyChanged}) and any listeners (via a bubbling
     * {@link contentChangeEventName} event). Subclasses call this when the user
     * edits the value through the UI.
     *
     * No-ops if `newContent` is identical (by `Object.is`) to the current value.
     *
     * @param newContent - the new content value.
     */
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

    /**
     * Set the content from outside the element (e.g. restoring a saved answer).
     * Delegates to {@link updateContent}, so it also fires a change event.
     */
    set content(newContent: Readonly<C>) {
        this.updateContent(newContent)
    }

    /**
     * Set the feedback from outside the element. If it changed (by `Object.is`),
     * notifies the subclass via {@link feedbackExternallyChanged}. Unlike
     * content, setting feedback does *not* dispatch a change event.
     */
    set feedback(newFeedback: Readonly<F>) {
        const oldFeedback = this._feedback;

        // TODO deep comparison
        if (Object.is(oldFeedback, newFeedback)) {
            return;
        }

        this._feedback = newFeedback;
        this.feedbackExternallyChanged(oldFeedback, newFeedback);
    }

    /**
     * Hook called after the content changes, so subclasses can update their DOM
     * (e.g. write the new value into an `<input>`). Default: no-op.
     *
     * @param oldValue - the previous content.
     * @param newValue - the new content.
     */
    protected contentExternallyChanged(oldValue: C, newValue: C): void {
        // Optional hook for subclasses
    }

    /**
     * Hook called after the feedback changes, so subclasses can render it.
     * Default: no-op.
     *
     * @param oldValue - the previous feedback.
     * @param newValue - the new feedback.
     */
    protected feedbackExternallyChanged(oldValue: F, newValue: F): void {
        // Optional hook for subclasses
    }

    /**
     * Report an error through the global state manager (shown to the user).
     *
     * @param error - the error message.
     * @param additional - extra detail forwarded to the state manager.
     */
    protected showError(error: string, ...additional) {
        // @ts-ignore
        window.stateManager.showError(error, ...additional)
    }
}
