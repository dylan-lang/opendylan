=====================
Dispatch Optimization
=====================

Since Dylan is dynamically typed and uses multiple dispatch, this has to be fast. This is in most applications the major performance bottleneck (unless annotated with types such that the dispatches are done at compile time).

We've recently updated Dylan to use the C3 algorithm for superclass linearization, which leads to monotonic method orderings.

Eric Kidd already wrote a technical report about the possibility of compressing the dispatch tables, which waits to be implemented. Additionally further research by Yoav Zibin evaluates other optimizations, which need to be evaluated and applied to Dylan.

For this project some algorithmic knowledge is needed. The code will be mainly in the Dylan runtime.

Useful links:

- `Eric Kidds technical report on compression of generic function tables <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.68.9146&rep=rep1&type=pdf>`_
- `Yoav Zibin thesis about efficient runtime algorithms <http://www.flashtogether.com/yoavzibin/publications/my-thesis.pdf>`_
