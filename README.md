# mHealthR package

#### Author: [Qu Tang](mailto: tqshelly@gmail.com)

#### Version: 0.2.0

---

## Intro

This package is used to *validate*, *read*, *write*, *plot*, *convert* and *manipulate* annotated mobile health data (such as physical and phisiological sensor data) store in [mhealth specification](https://qutang.github.io/project/mhealth-specification/).

## API overview

* [`mhealth`]() (named list)

    Stores mhealth specification related constants

* [`mhealth.validate`]() (function)

    Validates filename or data frame against mhealth specification
    
* [`mhealth.read`]() (function)

    Reads csv or gzipped csv files in mhealth specification as data frame
