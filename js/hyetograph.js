function barChartPlotter(e) {
    var ctx = e.drawingContext;
    var points = e.points;
    // see http://dygraphs.com/jsdoc/symbols/Dygraph.html#toDomYCoord
    var y_bottom = e.dygraph.toDomYCoord(0);

    // This should really be based on the minimum gap
    var bar_width = 2/3 * (points[1].canvasx - points[0].canvasx);
    ctx.fillStyle = e.color;

    // Do the actual plotting.
    for (var i = 0; i < points.length; i++) {
        var p = points[i];
        var center_x = p.canvasx;  // center of the bar

        ctx.fillRect(center_x - bar_width / 2, p.canvasy,
            bar_width, y_bottom - p.canvasy);
        ctx.strokeRect(center_x - bar_width / 2, p.canvasy,
            bar_width, y_bottom - p.canvasy);
    }
}
