import pandas as pd
from bokeh.plotting import figure, output_file, save
from bokeh.colors import RGB
from bokeh.models import Label

df = pd.read_csv("final.csv")

output_file("tree.html")
p = figure(plot_width=400,
           plot_height=800,
           tools="")

p.axis.visible = False
p.grid.visible = False

for row in df.itertuples():
    gray = (row.y / 20) ** 0.5 * 200
    gray = RGB(gray, gray, gray)

    red = RGB(183, 28, 28)

    if row.r > 0.45:
        p.circle(x=row.x, y=row.y, radius=row.r, fill_color=red, line_alpha=0, fill_alpha=0.9)
    else:
        p.circle(x=row.x, y=row.y, radius=row.r, fill_color=gray, line_alpha=0, fill_alpha=0.9)

p.add_layout(Label(x=3.5, y=-0.75, text_font="Harrington", text_font_size="18pt", text="Xmas 2k18", text_color=RGB(183, 28, 28)))
save(p)
