var observer = new MutationObserver(function(mutations) {
  mutations.forEach(function(mutation) {
    mutation.target.focus();
  });
});
var target = document.querySelector('.card-number-inputs');
var config = { attributes: true, subtree: true };
observer.observe(target, config);
