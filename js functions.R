# Function to allow submit guess after pressing "Enter" button
jscode <- '
$(function() {
  var $els = $("[data-proxy-click]");
  $.each(
    $els,
    function(idx, el) {
      var $el = $(el);
      var $proxy = $("#" + $el.data("proxyClick"));
      $el.keydown(function (e) {
        if (e.keyCode == 13) {
          $proxy.click();
        }
      });
    }
  );
});
'

# Function to add listeners to keys
listener <- '$(document).ready(function() {
  $(".key, .correct1, .in-word1, .not-in-word1").off("click").on("click", function() {
    var letter = $(this).data("value");
    var guess = $("#guess");
    if (guess.val().length < 5) {
      guess.val(guess.val() + letter);
      guess.trigger("input");
    }
  });
});
'


delete_b <- '$(document).ready(function() {
  $(".delete").off("click").on("click", function() {
    var guess = $("#guess");
    var currentVal = guess.val();
    if (currentVal.length > 0) {
      guess.val(currentVal.slice(0, -1));
      guess.trigger("input");
    }
  });
});
'
