function elementAction(elementName, json, success) {
    function failCallback(obj, statusMessage) {
        console.log("Failed AJAX call: ", elementName, json, obj, statusMessage)
    }
    function doneCallback(json) {
        for (let action of json) {
            console.log("Invoking callback " + action.callback + " with:", action.data)
            window[action.callback](action.data)
        }
    }
    return $.ajax(jsRoutes.controllers.AssessmentController.elementAction(elementName).url,
        {method: "POST", dataType: 'json', data: JSON.stringify(json), contentType: 'application/json',
            headers: {'CSRF-Token': csrfToken}})
        .fail(failCallback)
        .done(doneCallback)
}