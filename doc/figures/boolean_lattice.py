"""
Draw a Hasse diagram using daft
"""

import matplotlib
matplotlib.rc("font", family="serif", size=12)
matplotlib.rc("text", usetex=True)
matplotlib.rcParams["text.latex.preamble"]=[r"\usepackage{amsmath}"]
import daft

pgm = daft.PGM([3, 4], origin=[-0.5, -0.5])

# Bottom
pgm.add_node(daft.Node("bottom", r"$\bot$", 1.0, 0.0, plot_params={"alpha": 0}))

# Atoms
pgm.add_node(daft.Node("a1", r"$a$",  0.0, 1.0, plot_params={"alpha": 0.0}))
pgm.add_node(daft.Node("a2", r"$b$",  1.0, 1.0, plot_params={"alpha": 0.0}))
pgm.add_node(daft.Node("a3", r"$c$",  2.0, 1.0, plot_params={"alpha": 0.0}))

# Next layer
pgm.add_node(daft.Node("a1va2", r"$a \vee b$", 0.0, 2.0, plot_params={"alpha": 0.0}))
pgm.add_node(daft.Node("a1va3", r"$a \vee c$", 1.0, 2.0, plot_params={"alpha": 0.0}))
pgm.add_node(daft.Node("a2va3", r"$b \vee c$", 2.0, 2.0, plot_params={"alpha": 0.0}))

# Top
pgm.add_node(daft.Node("top", r"$a \vee b \vee c$", 1.0, 3.0, plot_params={"alpha": 0.0}))

# Add the edges
pgm.add_edge("bottom", "a1")
pgm.add_edge("bottom", "a2")
pgm.add_edge("bottom", "a3")
pgm.add_edge("a1", "a1va2")
pgm.add_edge("a1", "a1va3")
pgm.add_edge("a2", "a1va2")
pgm.add_edge("a2", "a2va3")
pgm.add_edge("a3", "a1va3")
pgm.add_edge("a3", "a2va3")
pgm.add_edge("a1va2", "top")
pgm.add_edge("a1va3", "top")
pgm.add_edge("a2va3", "top")

# Add the plates
#pgm.add_plate(daft.Plate([-1., -1., 2., 2.], label=r"Objects $i=1, ..., N$"))

pgm.render()
pgm.figure.savefig("boolean_lattice.pdf", bbox_inches="tight")

