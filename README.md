# polydex
Data structure for fast access, update, and exploration of high dimensional data sets. 

Just as traditional indexes bring a well structured and disciplined approach to storing ordered data in a way that facilitates fast access, update and navigation, the polydex brings a disciplined approach to storing large, high dimensional datasets in a way that similarly facilitates fast access, update and exploration.

Like many conventional indexes, such as binary search trees, a ploydex utilizes partitions of the space of possible data points into subsets at multiple different scales with subsets at one scale linked to others at adjacent scales. However, unlike traditional indexes such as the binary search tree, these subsets need not be entirely contained in a single subset of the partition of the space at the next higher level. Indeed, in a polydex, a subset at one level will typically intersect many different subsets at both the next higher, as well as the next lower level of a family of partitions of the space at different scales.

In order to achieve this in a relatively simple, natural, and extensible way, we can leverage the already existing, and extensive theory of error-correcting codes.

In this package, we will demonstrate this principle by building a data structure that allows us to quickly find nearby values in the space of binary strings of length n for any given binary string of that length (we will view these points as vertices of an n-dimensional hypercube). We will achieve this by utilizing the well known Reed–Muller codes, punctured Reed-Muller codes, as well as the extensive array of decoding algorithms that exist for these codes.

The punctured Reed-Muller codes make our task very simple as they provide a very easy to construct a family of (n + 1) different partitions of the (2^n - 1) dimensional hypercube.

Additionally, if we want to extend the precision of one or more of the dimensions of this space from the binary space of 0 and 1, to a more extensive space such as the 2^n numbers encoded as fixed point binary strings of length m. We can do this easily and in a natural way by simply considering the axis parallel mirror reflections of the original partitions of the hypercube.


The files included in this repo demonstrate a functional approach to quickly calculating Reed-Muller code words reresenting the centers of equally sized 'spherical' subsets of a hypercube.

Michael Tighe Haag

