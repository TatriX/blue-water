$(function() {
    $('#date')
        .datepicker({
            format: "yyyy/mm/dd",
            language: "ru-RU",
            todayBtn: true,
            todayHighlight: true
        })
        .on("changeDate", function(event) {
            document.location = event.format("/yyyy/mm/dd");
        });

    $("#search").submit(function(event) {
        event.preventDefault();
        document.location = "?search=" + this.elements.title.value;
    });

    $("#search > input").click(function() {
        $(this).select();
    });

    $("body").click(function(event) {
        var $self = $(event.target);
        if ($self.parents("#history").length > 0)
            return;
        if ($self.hasClass("show-history"))
            return;

        $("#history").hide();
    });

    $(".show-history").click(function(event) {
        event.preventDefault();
        var url = "/history" + document.location.pathname;
        $.getJSON(url, {movie: $(this).data("id")}, function(data) {
            $("#history").show();
            renderHistory(data);
        });
    });

});

function renderHistory(data) {
    var ctx = $("#history-canvas").get(0).getContext("2d");

    $("#history-title").text(data.title);

    var history = data.history;

    var defaultRating = 0;
    var labels =  history.map(function(record) {
        defaultRating += record.rating;
        return record.date;
    });
    defaultRating = Math.round(defaultRating / history.length);

    var max = 0;
    var min = 0;
    var ratings = history.map(function(record) {
        var rating = record.rating || defaultRating;
        max = Math.max(max, rating);
        min = Math.min(min, rating);
        return rating;
    });

    var chartData = {
        labels: labels,
        datasets: [
            {
                fillColor: "rgba(220,220,220,0.2)",
                strokeColor: "rgba(220,220,220,1)",
                pointColor: "rgba(220,220,220,1)",
                pointStrokeColor: "#fff",
                pointHighlightFill: "#fff",
                pointHighlightStroke: "rgba(220,220,220,1)",
                data: ratings,
            },
        ]
    };

    max = Math.ceil(max);
    min = Math.floor(min);
    var steps = max - min;
    var N = 30;
    var chart = new Chart(ctx).Line(chartData, {
        scaleOverride: true,
        scaleSteps: steps,
        scaleShowLabels: (steps < N),
        scaleShowHorizontalLines: (steps < N),
        scaleStepWidth: -Math.ceil(max / steps),
        scaleStartValue: max + 1,
        datasetFill : true,
    });
}
