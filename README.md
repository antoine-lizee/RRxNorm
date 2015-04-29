# RRxNorm

This project aims at projecting medication names from an arbitrary provider onto the [RxNorm ontology](http://www.nlm.nih.gov/research/umls/rxnorm/) of drugs.
It connects to the RxNorm [API](http://rxnav.nlm.nih.gov/RxNormAPIs.html#) for fuzzy matching and then property retrieval.

Most of the code is general purpose but the script is written in the context of the [rephetio](http://thinklab.com/p/rephetio) project, hosted on the collaborative platform [ThinkLab](http://thinklab.com/). This project aims at building a network of information from many data sources for drug reporpusing. This piece of code is needed for creating a robust Drug-Indication dictionary, as explained [here](http://thinklab.com/discussion/how-should-we-construct-a-catalog-of-drug-indications/21).
