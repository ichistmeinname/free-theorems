$(document).ready(function(){

   var disableStyleOptions = function(shouldBeDisabled) {
      /* Disable (or enable) the options for selecting a theorem style */
      $("input[name=style]").attr("disabled", shouldBeDisabled);
   };

   /* When the selected language model changes,
    * disable the options for selecting a theorem style for "basic"
    * but enable them for "fix" and "seq".
    */
   $("input[value=basic]").change(function(){
      disableStyleOptions(true);
   });
   $("input[value=fix]").change(function(){
      disableStyleOptions(false);
   });
   $("input[value=seq]").change(function(){
      disableStyleOptions(false);
   });

   /* Trigger the change event on the pre-selected option
    * to have the style options enabled/disabled correctly
    * after the page is loaded.
    */
   $("input[name=model]:checked").change();

});
