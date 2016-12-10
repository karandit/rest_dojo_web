use strict";

var app = Elm.RestDojo.Main.fullscreen({baseUrl: '/rest_dojo_web/api/billboard.json'});

// ----------------------------------- Elm ports -----------------------------------------------------------------------
app.ports.chart.subscribe(function(data) {
  waitForElement('#chartPoints', attachChart, data);
});

app.ports.auth0.subscribe(function(data) {
  lock.show();
});

// ----------------------------------- ChartJs -------------------------------------------------------------------------
var restdojo_chart = undefined;

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
          fontFamily: "D",
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

// ----------------------------------- Auth0 ---------------------------------------------------------------------------
// Initiating our Auth0Lock
var lock = new Auth0Lock(
  'XOYewkKtWXKkfUCik80F1866hoSJaIgF',
  'karandit.auth0.com',
  {
    allowedConnections: ['github'],
    auth: {
      redirect: false,
      responseType: 'token'
    }
  }
);

// Listening for the authenticated event
lock.on("authenticated", function(authResult) {
  // Use the token in authResult to getProfile() and save it to localStorage
  lock.getProfile(authResult.idToken, function(error, profile) {
    if (error) {
      // Handle error
      return;
    }
    app.ports.authentications.send({ name : profile.name, picture : profile.picture, nickname : profile.nickname});


    // localStorage.setItem('idToken', authResult.idToken);
    // localStorage.setItem('profile', JSON.stringify(profile));
  });
});
