# RRxNorm

This project aims at projecting medication names from an arbitrary provider onto the [RxNorm ontology](http://www.nlm.nih.gov/research/umls/rxnorm/) of drugs.
It connects to the RxNorm [API](http://rxnav.nlm.nih.gov/RxNormAPIs.html#) for fuzzy matching and then property retrieval.

Most of the code is general purpose but the script is written in the context of the [rephetio](http://thinklab.com/discussion/how-should-we-construct-a-catalog-of-drug-indications/21#169) project, hosted on the collaborative platform [ThinkLab](http://thinklab.com/). This project aims at building a network of information from many data sources for drug reporpusing. This piece of code is needed for creating a robust Drug-Indication dictionary, as explained [here](http://thinklab.com/discussion/how-should-we-construct-a-catalog-of-drug-indications/21).

The input file here is a processed version of the supplementary material of:
> McCoy et al. (2012) Development and evaluation of a crowdsourcing methodology for knowledge base construction: identifying relationships between clinical problems and medications. Journal of the American Medical Informatics Association doi:10.1136/amiajnl-2012-000852
