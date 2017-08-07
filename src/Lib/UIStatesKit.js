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

exports.clearAllErrorsImpl = function(parentSelector) {
  return function() {
      var parent = parentSelector ? document.querySelector(parentSelector) : document
      parent.querySelectorAll('.error').forEach( function(el){
      el.classList.remove('error');
    });
  }
}

exports.clearAllLoadingImpl = function(parentSelector) {
  return function() {
      var parent = parentSelector ? document.querySelector(parentSelector) : document
      parent.querySelectorAll('.loading').forEach( function(el){
      el.classList.remove('loading');
    });
  }
}
