import React, { Component } from 'react';
import './App.css';
import {PopulationPyramid} from "./PopulationPyramid"
import * as d3 from "d3";

class App extends Component {
  constructor(props) {
    super(props);

    this.state = {
      data: {
        levels: [],
        values: [],
        mode: [],
        classes: []
      }
    };

    const convert_data = (dataset) => {
      const levels = dataset.filter(x => x.Gender === "Male").map(x => x.Age);
      const modes = levels.map(_ => false);
      const values = dataset.map(x => ({idx: (x.Gender === "Male" ? 0 : 1), value: parseInt(x.Tx), level: levels.indexOf(x.Age)}));

      return {
        levels: levels,
        values: values,
        mode: modes,
        classes: ["Male", "Female"]
      }
    };

    d3.csv("przezycia.csv")
      .then(d => convert_data(d))
      .then((data) => this.setState({data: data}))
  }

  render() {
    return (
      <div>
        <PopulationPyramid data={this.state.data} perBin={1} />
        <div id="description">
          <p>This visualization shows population chart comparing age of men and women in 2009.</p>

          <p>Graph is interactive. Try clicking on a column to see difference between of men and women at given age.</p>

          <p>By clicking button "Toggle differences" you can toggle between differences and absolute population sizes.</p>

          <p>Slider can be used to change number of years per column.</p>

          <p>Though it requires some changes it can (and will) be adapted to a React.js library for population visualizations. Internally it uses d3.js.</p>
        </div>
      </div>
    );
  }
}

export default App;
