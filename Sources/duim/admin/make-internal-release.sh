tar -cvf duim.tar duim/README duim/examples/*/README duim/outline.text duim/TAGS
tar -rvf duim.tar duim/*.lid duim/*.dylan
tar -rvf duim.tar duim/*/*.lid duim/*/*.dylan duim/*/*/*.lid duim/*/*/*.dylan
tar -rvf duim/capi/patches/*.lisp duim/registry
