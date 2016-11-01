"use strict";

var app = Elm.RestDojo.Main.fullscreen({baseUrl: 'http://localhost:3000/billboard'});

var restdojo_chart = undefined;

app.ports.chart.subscribe(function(data) {
  waitForElement('#chartPoints', attachChart, data);
});

function attachChart(selector, data) {
  var element = document.querySelector(selector);
  if (!element) {
    // Element doesn't exist yet.
    return false;
  }

  restdojo_chart = restdojo_chart || createChart(element, data);
}

function createChart(el, data) {
  var ctx = el.getContext('2d');
  var options = {
      elements: {
        line: {
          fill: false
        }
      },
      legend: {
        position: "bottom",
        labels: {
          padding: 60,
          fontColor: "#ffffff",
          fontFamily: "Roboto Condensed",
          fontSize: 16,
          usePointStyle: true
        }
      }
  }

  var myChart = new Chart(ctx, {
    type: 'line',
    data: data,
    options: options
  });
  return myChart;
}

/**
 * Wait for selector to appear before invoking related functions.
 */
function waitForElement(selector, fn, model, tryCount) {

  // Repeat the timeout only maximum 5 times, which sohuld be enough for the
  // element to appear.
  tryCount = tryCount || 5;
  --tryCount;
  if (tryCount == 0) {
    return;
  }

  setTimeout(function() {

    var result = fn.call(null, selector, model, tryCount);
    if (!result) {
      // Element still doesn't exist, so wait some more.
      waitForElement(selector, fn, model, tryCount);
    }
  }, 50);
}
