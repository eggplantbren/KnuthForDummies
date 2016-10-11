"""
Draw a Hasse diagram using daft
"""

import matplotlib
matplotlib.rc("font", family="serif", size=12)
matplotlib.rc("text", usetex=True)
matplotlib.rcParams["text.latex.preamble"]=[r"\usepackage{amsmath}"]
import daft

pgm = daft.PGM([4, 5], origin=[-1.5, -0.5])

# Bottom
pgm.add_node(daft.Node("bottom", r"$\bot$", 1.0, 0.0, plot_params={"alpha": 0}))

# "Ideal" questions
pgm.add_node(daft.Node("a1", r"$A$",  0.0, 1.0, plot_params={"alpha": 0.0}))
pgm.add_node(daft.Node("a2", r"$B$",  1.0, 1.0, plot_params={"alpha": 0.0}))
pgm.add_node(daft.Node("a3", r"$C$",  2.0, 1.0, plot_params={"alpha": 0.0}))

# Next layer
pgm.add_node(daft.Node("a1va2", r"$A \cup B$", 0.0, 2.0, plot_params={"alpha": 0.0}))
pgm.add_node(daft.Node("a1va3", r"$A \cup C$", 1.0, 2.0, plot_params={"alpha": 0.0}))
pgm.add_node(daft.Node("a2va3", r"$B \cup C$", 2.0, 2.0, plot_params={"alpha": 0.0}))

# Central issue
pgm.add_node(daft.Node("top", r"$A \cup B \cup C$", 1.0, 3.0, plot_params={"alpha": 0.0}))

# Join central issue with ideal questions
pgm.add_node(daft.Node("AB", r"$AB$", -1.0, 3.0, plot_params={"alpha": 0.0}))

# Add the edges
pgm.add_edge("bottom", "a1", directed=False)
pgm.add_edge("bottom", "a2", directed=False)
pgm.add_edge("bottom", "a3", directed=False)
pgm.add_edge("a1", "a1va2", directed=False)
pgm.add_edge("a1", "a1va3", directed=False)
pgm.add_edge("a2", "a1va2", directed=False)
pgm.add_edge("a2", "a2va3", directed=False)
pgm.add_edge("a3", "a1va3", directed=False)
pgm.add_edge("a3", "a2va3", directed=False)
pgm.add_edge("a1va2", "top", directed=False)
pgm.add_edge("a1va3", "top", directed=False)
pgm.add_edge("a2va3", "top", directed=False)

pgm.add_edge("a1va2", "AB", directed=False)

# Add the plates
#pgm.add_plate(daft.Plate([-1., -1., 2., 2.], label=r"Objects $i=1, ..., N$"))

pgm.render()
pgm.figure.savefig("question_lattice.pdf", bbox_inches="tight")
pgm.figure.savefig("question_lattice.svg", bbox_inches="tight")
