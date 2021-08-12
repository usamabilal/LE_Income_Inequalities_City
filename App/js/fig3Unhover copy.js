
myPlot.on('plotly_hover', function (data) {
  var pn = '',
    tn = '',
    colors = [];
  for (var i = 0; i < data.points.length; i++) {
    pn = data.points[i].pointNumber;
    tn = data.points[i].curveNumber;
    colors = data.points[i].data.marker.color;
  }
  colors[pn] = '#C54C82';

  var update = { marker: { color: colors, size: 16 } };
  Plotly.restyle('myDiv', update, [tn]);
});

myPlot.on('plotly_unhover', function (data) {
  var pn = '',
    tn = '',
    colors = [];
  for (var i = 0; i < data.points.length; i++) {
    pn = data.points[i].pointNumber;
    tn = data.points[i].curveNumber;
    colors = data.points[i].data.marker.color;
  }
  colors[pn] = '#00000';

  var update = { marker: { color: colors, size: 16 } };
  Plotly.restyle('myDiv', update, [tn]);
});


var updateUnhighlight = {
  opacity: 1,
};
updateHighlight
updateUnhighlight
[4, 5, 6, 7, 8, 9, 10, 11];

Plotly.restyle('plot_fig3', updateHighlight, [4, 5, 6, 7, 8, 9, 10, 11]);




unhoverEvent = function () {
  console.log('unhoverEvent() STARTEDD!!!');
  // Get get all subplot elements
  subplots = document
    .querySelector('#plot_fig3')
    .querySelectorAll('.scatterlayer');

  // For each sub plot get children then change opacity to 1
  subplots.forEach((plot) => {
    plotChildren = plot.childNodes;
    plotChildren.forEach((element) => {
      // console.log('RESTORE OPACITY FOR');
      // console.log(element);
      element.style.opacity = 1;
    });
  });
};


