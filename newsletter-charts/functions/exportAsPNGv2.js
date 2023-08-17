// This function exports the content of a div element as a PNG image. I am really unhappy with this, because the combo of tools I'm using
// seems to add a few rows of white pixels at the bottom. To fix this, the function takes a parameter that specifies how many rows to remove.
// This is a hack, but it works. I'm hoping to find a better solution in the future.
// Thank you to my co-authors - github co-pilot and chatGPT for helping me write this function. Any remaining mistakes are mine.

function exportAsPNGv2(divId, rowsToRemove) {
  // Get the div element by its ID
  var divElement = document.getElementById(divId);

  // Check if the div element exists
  if (!divElement) {
    console.log("Invalid div ID.");
    return;
  }

  // Define the options for html2canvas
  var options = {
    scale: 3, // Increase the scale to enhance image quality
    dpi: 300, // Set the DPI (dots per inch) for high resolution
  };

  // Use html2canvas to capture the div as a canvas with the specified options
  html2canvas(divElement, options).then(function(canvas) {
    // Create a new canvas to hold the modified image
    var newCanvas = document.createElement('canvas');
    newCanvas.width = canvas.width;
    newCanvas.height = canvas.height - rowsToRemove; // Remove the specified number of rows

    // Copy the original canvas content to the new canvas
    var ctx = newCanvas.getContext('2d');
    ctx.drawImage(canvas, 0, 0, canvas.width, canvas.height - rowsToRemove, 0, 0, canvas.width, canvas.height - rowsToRemove);

    // Convert the new canvas to a data URL
    var dataUrl = newCanvas.toDataURL("image/png");
    
    // Extract the file name from the div ID
    var fileName = divId + ".png";

    // Create a link element and trigger the download
    var link = document.createElement("a");
    link.href = dataUrl;
    link.download = fileName; // Specify the desired file name
    link.click();
  });
}

