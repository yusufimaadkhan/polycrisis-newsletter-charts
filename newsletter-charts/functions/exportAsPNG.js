function exportAsPNG(divId) {
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
    // Convert the canvas to a data URL
    var dataUrl = canvas.toDataURL("image/png");
    
    // Extract the file name from the div ID
    var fileName = divId + ".png";

    // Create a link element and trigger the download
    var link = document.createElement("a");
    link.href = dataUrl;
    link.download = fileName; // Specify the desired file name
    link.click();
  });
}

