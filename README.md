Suspension-Rates-by-ELL

Suspension Rate by Race reports the percentage of students who have recieved at least one sanction (ISS, OSS, EXP) during a school year, for students with English learner status.

Data Source: <http://edsight.ct.gov/SASPortal/main.do>

## License MIT

## Getting Setup

We recommend approaching data processing as just another software development project. That means a few specific thing 
for us.

1. Version Control
2. Dedicated Environments
3. Automated Testing
4. Continuous Deployment


### Version control

We use git as our VCS. In most cases we can commit our full processing directory, but in cases where we are responsible 
for data suppression, we specifically exclude raw files from version control.

### Virtual Environments

Processing typically happens in either Python or R. However, testing is done with Python. We recommend setting up a 
virtual environment for managing any specific dependencies for testing a given dataset as follow:

For Python >= 3.6:

`python3 -m venv /path/to/new/virtual/environment`

For Python <= 3.5:

'virtualenv -p python3 venv'

You can then install the requirements like so:

`pip install -r requirements.txt`


### Metadata

We implement many of the practices and tools from the [Frictionless Data](http://frictionlessdata.io/) paradigm. 
Metadata should be specified in the generated `datapackage.json` file. A number of fields are pre-populated, but 
complete specification is necessary in order to use our dataset testing framework and our publishing tools.
 
We add a number of additional properties to facilitate specific testing and publishing workflows. The `ckan_extras` 
dictionary contains a number of additional required metadata fields. These fields are required by
our CKAN extension.

Entries should follow the following structure:

```json
 "key": {
      "ckan_name": "name when published to ckan",
      "value": "value",
      "type": "string",
      "constraints": {
        "enum": ["v1", "v2", "v3"]
      }
    }
```

We use the convention specified by the JSON Table Schema to add constraints or limitations to expected values. Our 
testing framework will evaluate the `value` property against the `constraints`.

Resources follow the standard form as described in the [JSON Table Schema]() with one exception; a boolean field 
titled `dimensions` should be added to each field in the `schema`. This field controls which fields should be 
populated and enumerated for CKAN, which in turn controls the filter options that are presented to the user.

The final extra property that should be present is an array of spot check tests which should be specified in the 
`spot_tests` array using the following form:

```json
"spot_checks": [
  {
    "filters": {
      "field1": "field_name",
      "field2": "field_name",
      "field3": "field_name",
    },
    "expected_value": {
      "value": value_as_a_numeric,
      "type": "integer"
    }
  }
]
```

The filters should be sufficiently comprehensive so as to return only one result, which will then be compared against
 the `expected_value` property within the testing framework.
 
 
 **TODO: Explain factor relationship specification as provided within the PyTest plugin tests**
  
### Automated Testing

Testing relies on PyTest and a custom CTData PyTest plugin which is installed as a requirement dependency.

An example testing script is included in the `/tests` directory. Running `pytest -v` will execute this and 
other tests.

Our custom PyTest plugin will bootstrap a number of fixtures with values that can be tested without additional logic. 

If spot checks are provide in metadata, they will be automatically run as part of the basic testing suite. Spot checks 
should provide a series of keys corresponding to the factor level selections required to extract a single row from the 
final dataset. The value should also be provide, along with any required format conversions [NEED TO EXPAND].

In addition to spot checking, if the data processing does things like calculate percentages of a whole and there is an 
expectation that all subgroups are accounted for, it would be a good practice to extract subgroups and test that these 
percentages sum to 1 (or 100 depending on formatting).

### Deployment

We use a custom CLI tool to publish datasets to our CKAN installation. This CLI is setup in the requirements.txt file and will be installed
upon the creation of the virtual environment.

Deployment should only take place when testing is complete and test coverage is 100%.

The standard publish command is:

`$ publish --ckan <ckan-url> --datapackage <path-to-datapackage.json> --ckanapikey <your-ckan-apikey>`

The ckan url and the ckan api key can be read from environment variables named `CKANURL` and `CKANAPIKEY` respectively.

In that case, the publish command is much simpler:

`$ publish --datapackage <path-to-datapackage.json>`

The path that is passed in should be a relative path from the current directory.
