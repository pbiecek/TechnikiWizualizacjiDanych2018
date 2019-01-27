// !preview r2d3 data=c(0.3, 0.6, 0.8, 0.95, 0.40, 0.20)
//
// r2d3::r2d3("~/Desktop/r2d3.js", data=c(0.3, 0.6, 0.8, 0.95, 0.40, 0.20))

var barHeight = Math.floor(height / data.length);
var g1 = svg.append('g')

g1.selectAll('rect')
  .data(data)
  .enter().append('rect')
    .attr('width', function(d) { return d * width; })
    .attr('height', barHeight - 2)
    .attr('y', function(d, i) { return i * barHeight; })
    .attr('fill', 'black')
    .on("mousemove", function(d) {
     tooltip
        .style('left',  function(d) { return d3.event.pageX + "px"} )
        .style('top',  function(d) { return d3.event.pageY + "px"} )
        .style('opacity', 1)
        .text("Mój tooltip: " + d);

   })
    .on("mouseout", function(d) {
     tooltip
        .style('opacity', 0);

   })



var tooltip = d3.select("body").append("div")
tooltip
    .text("Mój tooltip")
    .style('position', 'absolute' )
    .style('background', 'lightsteelblue')
    .style('left', '100px')
    .style('opacity', 0)
    .style('top', '100px')
    .style('width', '120px')
    .style('height', '40px')
    .attr('padding', 2)
