function exportAsSVG(divId) {
  // Get the div element by its ID
  var divElement = document.getElementById(divId);

  // Check if the div element exists
  if (!divElement) {
    console.log("Invalid div ID.");
    return;
  }

  // Create a new SVG element
  var svg = document.createElementNS("http://www.w3.org/2000/svg", "svg");
  svg.setAttribute("width", divElement.offsetWidth);
  svg.setAttribute("height", divElement.offsetHeight);

  // Create a foreignObject element to embed the HTML content
  var foreignObject = document.createElementNS("http://www.w3.org/2000/svg", "foreignObject");
  foreignObject.setAttribute("width", "100%");
  foreignObject.setAttribute("height", "100%");

  // Append the div's content to the foreignObject
  foreignObject.appendChild(divElement.cloneNode(true));

  // Append the foreignObject to the SVG
  svg.appendChild(foreignObject);

  // Serialize the SVG to an XML string
  var serializer = new XMLSerializer();
  var svgXml = serializer.serializeToString(svg);

  // Create a Blob containing the SVG XML
  var blob = new Blob([svgXml], { type: "image/svg+xml;charset=utf-8" });

  // Create a URL for the Blob
  var url = URL.createObjectURL(blob);

  // Extract the file name from the div ID
  var fileName = divId + ".svg";

  // Create a link element and trigger the download
  var link = document.createElement("a");
  link.href = url;
  link.download = fileName; // Specify the desired file name
  link.click();

  // Clean up the URL object
  URL.revokeObjectURL(url);
}


