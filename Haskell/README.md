# Hash-based Linear Co-Clustering (Haskell version)

Source code for the Hash-based Linear Co-Clustering algorithm (HBLCoClust) introduced in *de França, Fabrício Olivetti. "A hash-based co-clustering algorithm for categorical data." Expert Systems with Applications 64 (2016): 24-35.*.

This algorithm uses the Locality Sensitive Hashing algorithm to find subsets of objects and features that likely contains dense co-clusters. These regions are searched by means of an enumerative algorithm (InClose2).

As such, this algorithm has a linear complexity with respect to the number of binary relations present in the dataset.

TODO:

- create a utility function to suggest values for r and b
- create a function to expand co-clusters relaxing the dense restriction

In order to build:

```bash
chmod +x install.sh run.sh scripts/sortcommand
./install.sh
```

Run with:

```bash
./run.sh dataset_name number_bands hashes_per_band minrows mincols
```

Citation:

```
@article{de2016hash,
  title={A hash-based co-clustering algorithm for categorical data},
  author={de Fran{\c{c}}a, Fabr{\'\i}cio Olivetti},
  journal={Expert Systems with Applications},
  volume={64},
  pages={24--35},
  year={2016},
  publisher={Elsevier}
}
```
