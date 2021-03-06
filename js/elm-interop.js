"use strict";

var user = null;

var idToken = localStorage.getItem('idToken');
if (null != idToken) {
  var storedUserProfile = JSON.parse(localStorage.getItem('userProfile'));
  if (storedUserProfile.name && storedUserProfile.fullname && storedUserProfile.picture && storedUserProfile.idProvider) {
    user = {
      name : storedUserProfile.name,
      fullname : storedUserProfile.fullname,
      picture : storedUserProfile.picture,
      idProvider : storedUserProfile.idProvider
    };
  }
}

restdojo_main_args.user = user;
var app = Elm.RestDojo.Main.fullscreen(restdojo_main_args);
var restdojo_chart = undefined;

// ----------------------------------- Elm ports -----------------------------------------------------------------------
app.ports.chart.subscribe(function(data) {
  restdojo_chart = undefined;
  waitForElement('#chartPoints', attachChart, data);
});

app.ports.login.subscribe(function() {
  lock.show();
});

app.ports.logout.subscribe(function() {
  localStorage.removeItem('idToken');
  localStorage.removeItem('userProfile');
  app.ports.authentications.send(null);
});

// ----------------------------------- ChartJs -------------------------------------------------------------------------
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
    allowedConnections: ['github', 'bitbucket'],
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

    const userProfile = {
        idProvider : profile.identities[0].provider,
        name : profile.nickname,
        fullname : profile.name,
        picture : profile.picture
    };
    localStorage.setItem('idToken', authResult.idToken);
    localStorage.setItem('userProfile', JSON.stringify(userProfile));
    app.ports.authentications.send(userProfile);
  });
});
