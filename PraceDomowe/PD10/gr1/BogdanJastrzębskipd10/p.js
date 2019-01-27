var barHeight = Math.floor(height / data.length);
var g1 = svg.append('g')

var s = true;

g1.selectAll('rect')
  .data(data)
  .enter().append('rect')
    .attr('height', function(d) { return (d.female - d.male) * height / 8000000 })
    .attr('width', barHeight - 2)
    .attr('x', function(d, i) { return i * barHeight; })
    .attr('y', function(d, i) {
      switch (s) {
        case true: ret = height - d.male * height / 8000000 - 30;
          break;
        default: ret = height - (d.female - d.male) * height / 8000000 - 30;
      }
      return ret; })
    .attr('fill', function (d) { return 'lightsteelblue'; })
    .on("mousemove", function(d) {
     tooltip
        .style('left',  function(d) { return (d3.event.pageX + 20) + "px"} )
        .style('width', '250px')
        .style('height', '30px')
        .style('top',  function(d) { return d3.event.pageY + "px"} )
        .style('opacity', 1)
        .text("The number of female - male: " + (d.female - d.male));

   })
    .on("mouseout", function(d) {
     tooltip
        .style('opacity', 0);

   })

g1.selectAll('rect')
     .data(data).on("click", function(d) {
     switch (s) {
       case true:
         s = false;
         break;
       default: s = true;
     };
     update();
   })

function update() {
   g1.selectAll('rect').data(data)
    .transition()
    .duration(1000)
     .attr('height', function(d) { return (d.female - d.male) * height / 8000000 })
     .attr('width', barHeight - 2)
     .attr('x', function(d, i) { return i * barHeight; })
     .attr('y', function(d, i) {
       switch (s) {
         case true: ret = height - d.male * height / 8000000 - 30;
           break;
         default: ret = height - (d.female - d.male) * height / 8000000 - 30;
       }
       return ret; })
     .attr('fill', function (d) {
       switch (s) {
       case true: ret = "lightsteelblue";
         break;
       default: ret = 'Crimson';
     }
     return ret; })
}

var tooltip = d3.select("body").append("div")
tooltip
    .text("---")
    .style('position', 'absolute' )
    .style('background', 'lightsteelblue')
    .style('left', '100px')
    .style('opacity', 0)
    .style('top', '100px')
    .style('width', '120px')
    .style('height', '40px')
.attr('padding', 2)


var scale = d3.scaleLinear()
              .domain([0, 111])
              .range([0, 111 * barHeight-1]);
var x_axis = d3.axisBottom()
               .scale(scale);
svg.append("g")
    .call(x_axis);

var scaley = d3.scaleLinear()
             .domain([8000000, 0])
             .range([0, height-30]);
var y_axis = d3.axisRight()
              .scale(scaley);
svg.append("g")
  .call(y_axis);
update();
