d3.select('svg')
  .selectAll('circle')
  .data(data)
  .enter().append('circle')
          .attr('cy', function (d) { return d.mw; } )
          .attr('cx', function (d) { return d.Diet * 100; } )
          .attr('r', 12);
          