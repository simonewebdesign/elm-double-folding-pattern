/* http://stackoverflow.com/a/33403829
 * create HTML elements by including Html.Attributes.attribute "data-autofocus" ""
 */
var observer = new MutationObserver(function(mutations) {
  mutations.forEach(function(mutation) {
    mutation.target.focus();
  });
});
var target = document.querySelector('.card-number-inputs');
var config = { attributes: true, subtree: true };
observer.observe(target, config);
