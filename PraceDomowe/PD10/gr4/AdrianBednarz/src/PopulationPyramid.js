import React, {Component} from "react";
import * as d3 from "d3";
import * as d3s from "d3-simple-slider";

// todo: format caption

function split(data, binSize) {
  const originalNBins = data.levels.length;
  const nbins = Math.ceil(originalNBins / binSize);
  const lastBin = originalNBins - binSize * (nbins - 1);
  const split = Array(nbins - 1).fill(binSize);
  split.push(lastBin);

  const cumSplit = [];
  split.reduce((x, y, i) => cumSplit[i] = x + y, 0);

  const result = [];
  for (let datum of data.values) {
    let i = 0;

    for (; i < cumSplit.length; i++) {
      if (datum.level < cumSplit[i]) {
        break
      }
    }

    if (result[i * 2 + datum.idx]) {
      result[i * 2 + datum.idx].value += datum.value;
    } else {
      result[i * 2 + datum.idx] = {...datum};
    }
  }

  return result;
}

function uptext(root) {
  const lines = root.selectAll("tspan").data(root.datum());
  lines.text(d => d);
  lines.exit().remove();
  lines.enter().append("tspan")
    .attr("x", 0)
    .attr("dy", (d, i) => (i * 1.2) + "em")
    .text(d => d);
  return root;
}

export class PopulationPyramid extends Component {
  constructor(props) {
    super(props);

    this.create = this.create.bind(this);
    this.update = this.update.bind(this);
  }

  componentDidUpdate() {
    this.scale = this.props.perBin;
    this.data = {...this.props.data, values: split(this.props.data, this.props.perBin)};

    this.create();
  }

  getValue(element, scale) {
    return scale(this.getAbsoluteValue(element))
  }

  getAbsoluteValue(element) {
    const diff = (level, idx) => {
      const values = this.data.values.filter(x => x.level === level);
      const diff = values[0].value - values[1].value;
      if (diff > 0)
        return idx === values[0].idx ? diff : 0;
      else
        return idx === values[0].idx ? 0 : -diff;
    };

    return this.data.mode[element.level] ? diff(element.level, element.idx) : element.value;
  }

  getColor(d) {
    return "lightsteelblue";
  }

  updateAxes() {
    const dom_value = d3.extent(this.data.values, d => d.value);

    const s_value = d3.scaleLinear()
      .domain(dom_value)
      .range([0, this.barwidth]);

    const ax_female = d3.axisBottom()
      .scale(s_value)
      .ticks(5)
      .tickFormat(this.formatter);

    this.svg.select("g.axis.population.female")
      .attr("transform", `translate(${this.cx + this.gutter}, ${this.pyramid_h + 5})`)
      .call(ax_female);

    const s_male = d3.scaleLinear()
      .domain(dom_value.reverse())
      .range([0, this.barwidth]);

    const ax_male = d3.axisBottom()
      .scale(s_male)
      .ticks(5)
      .tickFormat(this.formatter);

    this.svg.select("g.axis.population.male")
      .attr("transform", `translate(${this.cx - this.barwidth - this.gutter}, ${this.pyramid_h + 5})`)
      .call(ax_male);
  }

  updatePlot(withTransition) {
    const self = this;
    const dom_value = d3.extent(this.data.values, d => d.value);
    const barheight = (self.pyramid_h / (self.dom_age[1] - self.dom_age[0])) * this.scale - 0.5;

    const s_age = d3.scaleLinear()
      .domain(this.dom_age.concat().reverse())
      .range([0, this.pyramid_h]);

    const s_value = d3.scaleLinear()
      .domain(dom_value)
      .range([0, self.barwidth]);

    const x_pos = d => (d.idx === 0) ? self.cx - self.gutter - this.getValue(d, s_value) : self.cx + self.gutter;

    const bar = self.bars.selectAll(".bar").data(this.data.values);
    let barExisting = bar;
    if (withTransition)
      barExisting = barExisting.transition().duration(500);
    barExisting.attr("width", d => this.getValue(d, s_value))
      .attr("height", barheight)
      .attr("x", x_pos)
      .attr("y", d => s_age(d.level) - barheight);

    bar.exit().remove();

    bar.enter().append("rect")
      .attr("class", d => "bar " + this.data.classes[d.idx])
      .attr("height", barheight)
      .attr("width", d => this.getValue(d, s_value))
      .attr("x", x_pos)
      .attr("y", d => s_age(d.level) - barheight)
      .on("mouseover", d => {
        self.div
          .style("background", self.getColor(d))
          .transition()
          .duration(200)
          .style("opacity", .9);
        self.div.html(self.formatter(self.getAbsoluteValue(d)))
          .style("left", (d3.event.pageX) + "px")
          .style("top", (d3.event.pageY - 14) + "px");
      })
      .on("mousemove", d => {
        self.div.html(self.formatter(self.getAbsoluteValue(d)))
          .style("left", (d3.event.pageX - (d.idx === 1 ? 0 : 60)) + "px")
          .style("top", (d3.event.pageY - 14) + "px");
      })
      .on("mouseout", _ => {
        self.div.transition()
          .duration(500)
          .style("opacity", 0);
      }).on("click", d => {
        self.data.mode[d.level] = !self.data.mode[d.level];
        self.update(true);
      });
  }

  update(withTransition) {
    this.updateAxes();
    this.updatePlot(withTransition);
  }

  initParameters() {
    this.margin = {top: 10, right: 10, bottom: 10, left: 10};
    this.width = 1160 - this.margin.left - this.margin.right;
    this.height = 800 - this.margin.top - this.margin.bottom;
    this.gutter = 30;
    this.pyramid_h = this.height - 105;
    this.dom_age = d3.extent(this.props.data.values, d => d.level);
    this.formatter = d3.format(",d");
    this.barwidth = this.width * 0.35;
    this.cx = this.width / 2;
  }

  initCanvas() {
    this.svg = d3.select("#canvas")
      .attr("width", this.width + this.margin.left + this.margin.right)
      .attr("height", this.height + this.margin.top + this.margin.bottom)
      .append("g")
      .attr("transform", `translate(${this.margin.left},${this.margin.top})`);
  }

  createLeftXAxis() {
    const dom_value = d3.extent(this.data.values, d => d.value);

    // male population axis
    const s_male = d3.scaleLinear()
      .domain(dom_value.reverse())
      .range([0, this.barwidth]);

    const ax_male = d3.axisBottom()
      .scale(s_male)
      .ticks(5)
      .tickFormat(this.formatter);

    this.svg.append("g")
      .attr("class", "axis population male")
      .call(ax_male);
  }

  createRightXAxis() {
    const dom_value = d3.extent(this.data.values, d => d.value);

    // population axen
    const s_value = d3.scaleLinear()
      .domain(dom_value)
      .range([0, this.barwidth]);

    // female population axis
    const ax_female = d3.axisBottom()
      .scale(s_value)
      .ticks(5)
      .tickFormat(this.formatter);

    this.svg.append("g")
      .attr("class", "axis population female")
      .call(ax_female);
  }

  createXAxis() {
    this.createLeftXAxis();
    this.createRightXAxis();
  }

  createYAxis() {
    const s_age = d3.scaleLinear()
      .domain(this.dom_age.concat().reverse())
      .range([0, this.pyramid_h]);

    const ax_age_l = d3.axisLeft()
      .scale(s_age)
      .tickFormat(d => this.data.levels[d]);

    const ax_age_svg = this.svg.append("g")
      .attr("class", "axis agel")
      .attr("transform", `translate(${this.cx + this.gutter / 2 + 10}, 0)`)
      .call(ax_age_l);

    ax_age_svg.selectAll("text")
      .attr("x", -this.gutter / 2 - 10)
      .style("text-anchor", "middle");

    const ax_age_r = d3.axisRight()
      .scale(s_age)
      .tickFormat(_ => "");

    this.svg.append("g")
      .attr("class", "axis ager")
      .attr("transform", `translate(${this.cx - this.gutter / 2 - 10}, 0)`)
      .call(ax_age_r);
  }

  createAxes() {
    this.createXAxis();
    this.createYAxis();
  }

  createPlot() {
    this.bars = this.svg.append("g")
      .attr("class", "pyramid population");
  }

  createCaptions() {
    const svg_text_m = this.svg.append("text")
      .attr("transform", `translate(${this.cx - 250},10)`)
      .style("font", "15px sans-serif")
      .attr("text-anchor", "start");

    const svg_text_f = this.svg.append("text")
      .attr("transform", `translate(${this.cx + 250},10)`)
      .style("font", "15px sans-serif")
      .attr("text-anchor", "end");

    const total_0 = d3.sum(this.data.values.filter(d => d.idx === 0).map(d => d.value));
    const total_1 = d3.sum(this.data.values.filter(d => d.idx === 1).map(d => d.value));

    svg_text_m.datum([this.data.classes[0], this.formatter(total_0)])
      .call(uptext);

    svg_text_f.datum([this.data.classes[1], this.formatter(total_1)])
      .call(uptext);
  }

  createToggle() {
    d3.select("button")
      .attr("class", "tooltip")
      .text("Toggle differences")
      .style("opacity", 1)
      .on("click", e => {
        this.data.mode = this.data.mode.map(x => !x);
        this.update(true);
      });
  }

  createSlider() {
    const svgSlider = d3.select("#slider");
    const sliderSimple = d3s
      .sliderBottom()
      .min(1)
      .max(20)
      .width(300)
      .step(1)
      .ticks(15)
      .default(this.props.perBin)
      .on("onchange", bins => {
        this.data = {...this.props.data, values: split(this.props.data, bins)};
        this.scale = bins;
        this.update();
      });
    svgSlider.call(sliderSimple);
  }

  createControls() {
    this.createToggle();
    this.createSlider();
  }

  createTooltip() {
    this.div = d3.select("body").append("div")
      .attr("class", "tooltip")
      .style("opacity", 0);
  }

  create() {
    this.initParameters();
    this.initCanvas();
    this.createPlot();
    this.createCaptions();
    this.createTooltip();
    this.createAxes();
    this.createControls();

    this.update(false);
  }

  render() {
    return (
      <div>
        <svg id="canvas" />
        <div id="controls">
          <svg viewBox="-100 -10 500 50" id="slider" />
          <button />
        </div>
      </div>
    );
  }
}