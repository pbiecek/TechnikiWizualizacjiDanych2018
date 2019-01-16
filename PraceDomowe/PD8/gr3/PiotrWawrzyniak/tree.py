import numpy as np
import pandas as pd
import plotly.offline as py
import plotly.graph_objs as go
import math

rate = 1 / (2 * math.pi)
factor = rate / 3

class Spiral():
    def __init__(self, foreground, angleoffset, factor):
        self.foreground = foreground
        self.angleoffset = angleoffset
        self.factor = factor

    def get_trace(self):
        t_range = range(0, 3*1200)
        t_values = [t / 100 for t in t_range]
        x = [t * self.factor * math.sin(t+self.angleoffset) for t in t_values]
        y = [t * self.factor * math.cos(t+self.angleoffset) for t in t_values]
        z = [-t for t in t_values]

        return go.Scatter3d(
            x=x,
            y=y,
            z=z,
            mode='lines+markers',
            marker=dict(
                size=4,
                color = self.foreground,
            )
        )

spirals = [
    Spiral("#330000", math.pi * 0.92, 0.90 * factor),
    Spiral("#003322", math.pi * 0.08, 0.90 * factor),
    Spiral("#770000", math.pi * 0.95, 0.93 * factor),
    Spiral("#007711", math.pi * 0.05, 0.93 * factor),
    Spiral("#ee1100", math.pi * 1.00, factor),
    Spiral("#00ff44", 0, factor)
]

layout = go.Layout(
    paper_bgcolor='rgba(0,0,0,0.95)',
    plot_bgcolor='rgba(0,0,0,0.95)'
)

data = [spiral.get_trace() for spiral in spirals]

py.plot(go.Figure(data=data, layout=layout), filename='tree.html')
