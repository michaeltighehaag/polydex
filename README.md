# polydex
Data structure for fast access, update, and exploration of high dimensional data sets. 

Just as traditional indexes bring a well structured and disciplined approach to 
storing ordered data in a way that facilitates fast access, update and navigation,
the polydex brings a disciplined approach to storing large, high dimentional datasets
in a way that similarly facilitates fast access, update and exploration.
 
Like many conventional indexes, such as binary search trees, 
a ploydex utilizes partitions of the space of possible data points into 
subsets at multiple different scales with subsets at one scale linked to 
others at adjacent scales. However, unlike taditional indexes such as the binary search tree, these 
subsets need not be entirely contaianed in a single subset of the partion 
of the space at the next higher level. Indeed, in a polydex, a subset at one level 
will typically only partially intersect many different subsets at both the next higher, as well as
the next lower level of a family of partitions of the space at different scales.

In order to achieve this in a relativly simple, natural, and extensible way, we 
can leverage the already existing, and extensive theory of error-correcting codes.

In this package, we will demonstrate this principle by building a data structure 
that allows us to quickly find nearby values in the space of binary strings of 
length n for any given binary string of that length (we will view these points 
as verticies of an n-dimensional hypercube). We will achive this by utilizing 
the well known Reed–Muller codes, punctured reed-muller codes, as weel as the extensive 
array of decoding algorithms that exist for these codes.

The punctured reed-muller codes make our task very simple as they provide 
a very easy to construct a family of (2^n + 1) different partitions of the (2^(2^n - 1)) dimensional hypercube.

Additionally, if we want to extend our data structure from the binary 
space of 0 and 1, to a more extensive space such as the fixed point 
binary numbers of length 2^m for one or more of the n dimensions of this space, 
we can do so in a natural way by simply considering the axis parallel 
mirror reflections of the codes from this origonal hypercube. 


