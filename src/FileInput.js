// Get the first file from a file input change event
export function getFileFromEvent_(event) {
  if (event.target && event.target.files && event.target.files[0]) {
    // console.log("got file: " + event.target.files[0].name)
    return event.target.files[0];
  }
  return null;
}

// Read a file as text and return it via Aff callbacks
export function readFileAsText_(onError, onSuccess, file) {
//   return function() {
    var reader = new FileReader();
    reader.onload = function(e) {
      onSuccess(e.target.result)();
    };
    reader.onerror = function(e) {
      onError(new Error(reader.error.message))();
    };
    reader.readAsText(file);
//   };
}
