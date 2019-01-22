const svg = d3.select("svg");
const margin = {top: 50, right: 50, bottom: 50, left: 50};
const width = +svg.attr("width");
const height = +svg.attr("height");

// utils
const runner = (allDragons, filteredDragons) => {
  const xAxis = getXAxis(allDragons, d => d.bmi);
  const yAxis = getYAxis(allDragons, d => d.life_length);
  const hexbin = getHexBin(xAxis, yAxis);
  const hexData = hexbin(filteredDragons);
  const colorScale = getColorScale(hexData, h => h.length);

  return f => f(colorScale, hexData, hexbin.hexagon(), xAxis, yAxis);
}

const drawHexagon = (hexagon, colorScale) => path =>
  path.attr("d", _ => hexagon)
    .attr("transform", d => `translate(${d.x}, ${d.y})`)
    .attr("fill", d => colorScale(d.length));

const span = (data, access) => [
  d3.min(data, access), 
  d3.max(data, access)
];

const expand = (span, percent) =>
  _.zip(span, [1 - percent / 100, 1 + percent / 100])
    .map(xy => xy[0] * xy[1])

// create plot
const getAxisScale = (data, access) =>
  d3.scaleLinear()
    .domain(expand(span(data, access), 10))

const getXAxis = (data, access) =>
  getAxisScale(data, access)
    .range([margin.left, width - margin.right]);

const getYAxis = (data, access) =>
  getAxisScale(data, access)
    .range([height - margin.bottom, margin.top]);

const getHexBin = (xAxis, yAxis) =>
  d3.hexbin()
    .x(d => xAxis(d.bmi))
    .y(d => yAxis(d.life_length))
    .size([width, height])
    .radius(15);

const getExactLogScale = (data, access) =>
  d3.scaleLog()
    .domain(span(data, access))

const getColorScale = (data, access) =>
  getExactLogScale(data, access)
    .range(["black", "steelblue"])
    .interpolate(d3.interpolateLab);

const appendColorLegend = (scale) => {
  const legend = d3.legendColor()
    .scale(scale)
    .title("Dragon count");
  svg.append("g")
    .call(legend)
    .attr("transform", `translate(${width - margin.left - margin.right}, ${margin.top})`);
}

const appendXAxis = (xAxis) => 
  svg.append("g")
    .attr("transform", `translate(0, ${height - margin.top})`)
    .call(d3.axisBottom(xAxis));

const appendYAxis = (yAxis) =>
  svg.append("g")
    .attr("transform", `translate(${margin.left}, 0)`)
    .call(d3.axisLeft(yAxis));

const appendHexPlot = (hexData, hexagon, colorScale) => {
  svg.append("g")
    .attr("class", "hexagon")
    .selectAll("path")
    .data(hexData)
    .enter()
      .append("path")
      .call(drawHexagon(hexagon, colorScale))
  
  svg.append("text")
    .attr("x", width / 2)
    .attr("y", margin.top / 2)
    .attr("text-anchor", "middle") 
    .text("Relation between dragon's BMI and life span");
  
  svg.append("text")
    .attr("x", width / 2)
    .attr("y", margin.top)
    .attr("text-anchor", "middle") 
    .text("(Depicted number of dragons in log scale)");
}

const drawHexPlot = (colorScale, hexData, hexagon, xAxis, yAxis) => {
  appendColorLegend(colorScale);
  appendXAxis(xAxis);
  appendYAxis(yAxis);
  appendHexPlot(hexData, hexagon, colorScale);
}

const init = (dragons) =>
  runner(dragons, dragons)(drawHexPlot);

// update plot
const updateHexPlot = (colorScale, hexData, hexagon) => {
  const hexagons = svg.select(".hexagon")
    .selectAll("path")
    .data(hexData);

  hexagons.exit().remove();
  hexagons.enter()
    .append("path")
    .call(drawHexagon(hexagon, colorScale));
  hexagons.transition()
    .duration(0)
    .call(drawHexagon(hexagon, colorScale));
}

const redraw = (allDragons, state) => {
  const dragons = allDragons.filter(d => d.year_of_birth > state[0] && d.year_of_birth < state[1]);
  runner(allDragons, dragons)(updateHexPlot)
}

// slider
const createSlider = (dragons) =>
  d3.sliderBottom()
    .min(d3.min(dragons, d => d.year_of_birth))
    .max(d3.max(dragons, d => d.year_of_birth))
    .width(width - margin.left - margin.right)
    .ticks(20)
    .default([-800, 1200])
    .fill("steelblue")
    .on("onchange", s => redraw(dragons, s));

const appendSlider = (slider) =>
  d3.select("#ui")
    .append("svg")
    .attr("width", width)
    .attr("height", 100)
    .append("g")
    .call(slider)
    .attr("transform", `translate(${margin.left}, 30)`);

// data
const parse = (data) => ({
  year_of_birth: +data.year_of_birth,
  height: +data.height,
  weight: +data.weight,
  life_length: +data.life_length
});

const extract = (parsed) => ({
  year_of_birth: parsed.year_of_birth,
  bmi: 1e3 * parsed.weight / parsed.height / parsed.height,
  life_length: parsed.life_length
});

const onData = (error, dragons) => {
  if (error) throw error;

  const slider = createSlider(dragons);
  appendSlider(slider);

  init(dragons);
}

// main
d3.queue()
  .defer(d3.csv, "dragons.csv", data => extract(parse(data)))
  .await(onData);