# Mycorec
### (Work in progress)

The plan is to create a tool for converting a list of fungi field records into a list of scientific and common names.

For example, the following list:

```
cep
False Yellow Bolete
Pysatherela multipedata
```

Would be transformed into:

```
Boletus edulis,Penny Bun / Cep
Boletus luridiformis var. discolor,False Yellow Bolete
Psathyrella multipedata,Clustered Brittlestem
```

So in essence, you can record finds using either common or scientific names and the output will include both. It'll also do fuzzy matching to fix typos in the original field record.

* * *

Currently the data structures to represent scientific and common names are implemented and also the parsing of the source `csv` records from the [bms](http://www.britmycolsoc.org.uk/library/english-names/) is working.
