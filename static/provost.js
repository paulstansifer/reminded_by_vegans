$(function() {
    //hack: tell jQuery where sortable things are supposed to go
    $('.remlist').prepend("<li class='dummy-rem' style='display:none'></li>")
    
    $('.rem').disableSelection();
    $('.day').sortable({
        connectWith: ".day",
        cursor: "pointer",
        revert: 150,
        tolerance: "pointer",
        items: ".rem, .dummy-rem",
        /*placeholder: ".placeholder-rem"*/
    });
})