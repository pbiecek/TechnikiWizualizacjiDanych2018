d3.csv("population.csv").then(function (data) {

  var margin = { top: 10, right: 10, bottom: 10, left: 10 }
    , width = 1600 - margin.left - margin.right
    , height = 700 - margin.top - margin.bottom
    , gutter = 30
    , pyramid_h = height - 105
    , dom_age = d3.extent(data, d => +d.Age)
    , dom_value = d3.extent(data, d => +d.Tx)
    , formatter = d3.format(".2s")
    , barheight = (pyramid_h / (dom_age[1] - dom_age[0])) - 0.5
    , cx = width / 2;

  var svg = d3.select('.chart').append('svg')
    .attr('width', width + margin.left + margin.right)
    .attr('height', height + margin.top + margin.bottom)
    .append('g')
    .attr('transform', `translate(${margin.left},${margin.top})`);

  var div = d3.select(".chart").append("div")
    .attr("class", "tooltip")
    .style("opacity", 0);

  var bot_box = d3.select(".chart").append("div")
    .attr("class", "bot_box")
    .attr("align", "center")
    .attr("width", width)
    .html('Click on a bar to get comparison');

  // age axis
  var s_age = d3.scaleLinear()
    .domain(dom_age.concat().reverse())
    .range([0, pyramid_h]);

  var ax_age_l = d3.axisLeft(s_age)
    .tickFormat(d => s_age(d) ? '' + d : '');

  var ax_age_svg = svg.append('g')
    .attr('class', 'axis age')
    .attr('transform', `translate(${cx + gutter / 2 + 10},0)`)
    .call(ax_age_l);

  ax_age_svg.append('text')
    .attr('dy', '.32em')
    .text('Age');

  ax_age_svg.selectAll('text')
    .attr('x', -gutter / 2 - 10)
    .style('text-anchor', 'middle');

  var ax_age_r = d3.axisRight(s_age)
    .tickFormat(d => '');

  svg.append('g')
    .attr('class', 'axis age')
    .attr('transform', `translate(${cx - gutter / 2 - 10},0)`)
    .call(ax_age_r);

  // population axen
  var s_value = d3.scaleLinear()
    .domain(dom_value)
    .range([0, 250]);

  // male population axis
  var s_male = d3.scaleLinear()
    .domain(dom_value.reverse())
    .range([0, 250]);

  var ax_male = d3.axisBottom(s_male)
    .ticks(5)
    .tickFormat(formatter);

  svg.append('g')
    .attr('class', 'axis population male')
    .attr('transform',
      `translate(${cx - 250 - gutter},${pyramid_h + 5})`)
    .call(ax_male);

  // female population axis
  var ax_female = d3.axisBottom(s_value)
    .ticks(5)
    .tickFormat(formatter);

  svg.append('g')
    .attr('class', 'axis population female')
    .attr('transform', `translate(${cx + gutter},${pyramid_h + 5})`)
    .call(ax_female);

  // population bars
  var bars = svg.append('g')
    .attr('class', 'pyramid population');

  var isMale = d => d.Gender === 'Male'
    , x_pos = d => {
      return isMale(d)
        ? cx - gutter - s_value(+d.Tx)
        : cx + gutter;
    };

  var bar = bars.selectAll('.bar').data(data);

  bar.enter().append('rect')
    .attr('class', d => 'bar ' + d.Gender)
    .attr('height', barheight)
    .attr('width', d => s_value(+d.Tx))
    .attr('x', x_pos)
    .attr('y', d => s_age(+d.Age) - barheight / 2)
    .on("mouseover", function (d) {
      div.transition()
        .duration(200)
        .style("opacity", 0.9);
      div.html(
        "Age: " + d.Age + "<br/>" +
        "Survivability: " + d.Tx)
        .style("left", (d3.event.pageX) + "px")
        .style("top", (d3.event.pageY) + "px");;
    })
    .on("mouseout", () => 
      div.transition()
      .duration(500)
      .style("opacity", 0))
    .on("click", function (d) {
      var d_age = data.filter(nd => nd.Age == d.Age),
        d_female = d_age.filter(d => d.Gender == 'Female')[0],
        d_male = d_age.filter(d => d.Gender == 'Male')[0],
        prop = d_male.Tx / d_female.Tx;
      bot_box.html(
        "Age: " + d.Age + "</br>" +
        "Man's survivability: " + d_male.Tx + "</br>" +
        "Woman's survivability: " + d_female.Tx + "</br>" +
        "Absolute difference: " + (d_male.Tx - d_female.Tx) + "</br>" +
        "Man's proportion: " + prop.toPrecision(2))
    });
});