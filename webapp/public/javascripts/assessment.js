function log_error(...data) {
    console.error(...data)
    errorTextarea = document.getElementById("errors")
    errorTextarea.textContent += data.toString()
    errorTextarea.textContent += "\n"
}

/** Invokes the `Assessment.elementEvent` method of the page element `elementName` on the backend.
 * (Via `AssessmentController.elementEvent`.)
 *
 * @param elementName
 * @param json JSON object to pass to `Assessment.elementEvent`
 */
function elementEvent(elementName, json) {
    // log_error(elementName, json)
    if (elementName == null)
        log_error("In elementEvent, elementName is null (internal eerror).")
    function failCallback(obj, statusMessage) {
        log_error("Failed to invoke elementEvent on " + elementName)
        console.log("Failed AJAX call: ", elementName, json, obj, statusMessage)
    }
    function doneCallback(json) {
        for (let action of json) {
            console.log("Invoking callback " + action.callback + " with:", action.data)
            window[action.callback](action.data)
        }
    }
    $.ajax(jsRoutes.controllers.AssessmentController.elementEvent(assessmentName, elementName).url,
        {method: "POST", dataType: 'json', data: JSON.stringify(json), contentType: 'application/json',
            headers: {'CSRF-Token': csrfToken}})
        .fail(failCallback)
        .done(doneCallback)
}