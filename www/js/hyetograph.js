function barChartPlotter(e) {

    // see http://dygraphs.com/jsdoc/symbols/Dygraph.html#toDomYCoord

    var ctx = e.drawingContext;
    ctx.fillStyle = e.color;
    var points = e.points;
    var y_bottom = e.dygraph.toDomYCoord(0);
    

    // set bar width the fast but error-prone way (see below for better).
    // this one doesn't work when there are multiple sites selected
    //var bar_width = 2/3 * (points[1].canvasx - points[0].canvasx); //use floor() here?

    // set bar width to the minimum separation between x-values.
    var canvasx = points.map(x => x.canvasx)
    var canvasx_uniq = [...new Set(canvasx)]
    var min_sep = Infinity;
    for (let i = 1; i < canvasx_uniq.length; i++) {
        let sep = canvasx_uniq[i] - canvasx_uniq[i - 1];
        if (sep <= 0){
            min_sep = 0;
            break
        }
        if (sep < min_sep) min_sep = sep;
    }
    var bar_width = Math.floor(2.0 / 3 * min_sep);

    // Do the actual plotting.
    for (let i = 0; i < points.length; i++) {
        let p = points[i];
        let center_x = p.canvasx;  // center of the bar

        ctx.fillRect(center_x - bar_width / 2, p.canvasy,
            bar_width, y_bottom - p.canvasy);
        ctx.strokeRect(center_x - bar_width / 2, p.canvasy,
            bar_width, y_bottom - p.canvasy);
    }
}
