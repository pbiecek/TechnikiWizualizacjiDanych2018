
var g1 = svg.append('g');

var margin = {top: 10, right: 0, bottom: 40, left: 80};
var barWidth = Math.floor((width-margin.left-margin.right) / data.length);
var y = d3.scaleLinear()
.range([(height-margin.bottom), margin.top])
.domain([0, 8000000]);

var x = d3.scaleBand()
        .range([margin.left, width-margin.left])
        .domain(data.map(function(d) { return d.Age; }));
        
var state = "unfolded";

var div = d3.select("body").append("div")	
    		.attr("class", "tooltip")				
    		.style("opacity", 0);

g1.selectAll('rect')
.data(data)
.enter().append('rect')
.attr('width', barWidth-1)
.attr('height', function(d, i) { return height-margin.bottom-y(d.rate); })
.attr('y', function(d, i) { return y(d.rate); })
.attr('x', function(d, i) { return x(d.Age)})
.attr('fill', 'darkblue')
.on("mouseover", function(d) {		
            		div.transition()		
                	.duration(200)		
                	.style("opacity", .99);		
            		div.html("Wiek: " + d.Age + "<br/>" + "Liczba kobiet: " + d.rate + "<br/>"  + "Liczba mężczyzn: " + d.minrate)	
                	.style("left", (d3.event.pageX) + "px")		
                	.style("top", (d3.event.pageY - 28) + "px");	
            	})					
        	.on("mouseout", function(d) {		
            		div.transition()		
                	.duration(500)		
                	.style("opacity", 0);	
        	})
.on("click", function(d) {
  if(state=="unfolded"){
    state = "folded";
    d3.selectAll('rect')
    .transition()
    .duration(1000)
    .attr('height', function(d, i) { return height-margin.bottom-y(d.delta);})
    .attr('fill', function(d,i) {return d.color});
    d3.select('.title')
    .text("Przewaga liczby kobiet nad liczbą mężczyzn");
  }else{
    state = "unfolded";
    d3.selectAll('rect')
    .transition()
    .duration(1000)
    .attr('height', function(d, i) { return height-margin.bottom-y(d.rate); })
    .attr('fill', 'darkblue');
    d3.select('.title')
    .text("Liczba żyjących kobiet w danym wieku");
  }
});

var x_axis = d3.axisBottom()
        	.scale(x);
	var xAxisTranslate = height-margin.bottom;

    	svg.append("g")
            .attr("transform", "translate(0, " + xAxisTranslate  +")")
            .call(x_axis);
            
var y_axis = d3.axisLeft()
        	.scale(y)
        	.tickFormat(d3.format(",d"));
	var yAxisTranslate = margin.left;

    	svg.append("g")
            .attr("transform", "translate("+ yAxisTranslate + ", 0" +")")
            .call(y_axis);
            
  svg.append("text")
		.attr("class", "title")
       		.attr("x", (width / 2))             
        	.attr("y", 20)
        	.style("text-anchor", "middle")
		      .style("text decoration", "bold")
        	.text("Liczba żyjących kobiet w danym wieku");
        	
  svg.append("text")             
      .attr("transform",
            "translate(" + (width/2) + " ," + 
                           (height - margin.top + 0) + ")")
      .style("text-anchor", "middle")
      .text("Wiek");
      
      svg.append("text")
      .attr("transform", "rotate(-90)")
      .attr("x", 0 - height/2)
      .attr("y", 0)      
      .attr("dy", "1em")
      .style("text-anchor", "middle")
      .text("Liczba kobiet");  