exports.toggleLoadingImpl = function(selector) {
  return function() {
    if (selector) {
      var el = document.querySelector(selector);
      el.classList.toggle("loading");
    }
  }
}

exports.toggleErrorImpl = function(selector) {
  return function() {
      if (selector) {
      var el = document.querySelector(selector);
      el.classList.toggle("error");
    }
  }
}

exports.clearAllErrorsImpl = function() {
  return function() {
      document.querySelectorAll('.error').forEach( function(el){
      el.classList.remove('error');
    });
  }
}

exports.clearAllLoadingImpl = function() {
  return function() {
      document.querySelectorAll('.loading').forEach( function(el){
      el.classList.remove('loading');
    });
  }
}
