$(document).ready(function(){
    $("body").on("click", "span.prediction", function() {
        var isAppend = $(this).data("append") == "TRUE";
        var txt = $("#mainTextInput");
        var text = txt.val();

        if (isAppend) {
            text = text + $(this).text() + " ";
        } else {
            var otext = text.split(" ");
            text = otext.slice(0, otext.length - 1).join(" ") + " " + $(this).text() + " ";
        }

        txt.focus().val("").val(text);

        txt.trigger("change");
    });

    $("#predictedWords").html("<div class='progress'><div class='progress-bar progress-bar-striped active' role='progressbar' style='width: 100%'><span>Please wait... Application is still loading</span></div></div>")
})
