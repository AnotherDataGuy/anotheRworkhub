
Shiny.addCustomMessageHandler('loadSVG', function(svgContent) {
    document.getElementById('svg-container').innerHTML = svgContent;
    
    // Adjust the SVG to fit the container
    var draw = SVG().addTo('#svg-container').size('100%', 'auto');
    draw.svg(svgContent);
});

Shiny.addCustomMessageHandler('changeColor', function(data) {
    var element = SVG().findOne('#' + data.id);
    if (element) {
        element.fill(data.color);
    }
});

Shiny.addCustomMessageHandler('changeText', function(data) {
    var element = SVG().findOne('#' + data.id);
    if (element && element.nodeName === 'text') {
        element.text(data.text);
    }
});

