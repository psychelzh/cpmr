# Default threshold method works

    {
      "type": "double",
      "attributes": {
        "dim": {
          "type": "integer",
          "attributes": {},
          "value": [10, 3]
        },
        "dimnames": {
          "type": "list",
          "attributes": {},
          "value": [
            {
              "type": "NULL"
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["both", "pos", "neg"]
            }
          ]
        }
      },
      "value": [-0.39584799, -0.39584799, -0.39584799, -0.39584799, -0.39584799, -0.39584799, -0.39584799, -0.39584799, -0.39584799, -0.39584799, -0.39584799, -0.39584799, -0.39584799, -0.39584799, -0.39584799, -0.39584799, -0.39584799, -0.39584799, -0.39584799, -0.39584799, -0.39584799, -0.39584799, -0.39584799, -0.39584799, -0.39584799, -0.39584799, -0.39584799, -0.39584799, -0.39584799, -0.39584799]
    }

---

    {
      "type": "logical",
      "attributes": {
        "dim": {
          "type": "integer",
          "attributes": {},
          "value": [10, 2]
        },
        "dimnames": {
          "type": "list",
          "attributes": {},
          "value": [
            {
              "type": "NULL"
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["pos", "neg"]
            }
          ]
        }
      },
      "value": [false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false]
    }

---

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["covariates", "thresh_method", "thresh_level", "na_action", "bias_correct"]
        }
      },
      "value": [
        {
          "type": "logical",
          "attributes": {},
          "value": [false]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["alpha"]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0.01]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["fail"]
        },
        {
          "type": "logical",
          "attributes": {},
          "value": [true]
        }
      ]
    }

---

    Code
      result
    Output
      CPM results:
        Call: fit(object = cpm_spec(), conmat = conmat, behav = behav)
        Number of observations: 10
          Complete cases: 10
        Number of edges: 10
        Parameters:
          Covariates:       FALSE
          Threshold method: alpha
          Threshold level:  0.01
          Bias correction:  TRUE

# Alternative threshold method works

    {
      "type": "double",
      "attributes": {
        "dim": {
          "type": "integer",
          "attributes": {},
          "value": [10, 3]
        },
        "dimnames": {
          "type": "list",
          "attributes": {},
          "value": [
            {
              "type": "NULL"
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["both", "pos", "neg"]
            }
          ]
        }
      },
      "value": [-0.47986299, -0.4235422, -0.31121857, -0.76666707, -1.27905638, 0.29190563, -0.53251339, -0.6619815, -0.29101707, 0.4954736, -0.50153461, -0.77729984, -0.33661358, -0.40260447, -0.95285926, 0.3039217, -0.1406691, -0.83778146, -0.53134056, 0.21830125, -0.41713269, -0.08233393, -0.33073634, -0.91491306, -1.13193216, -0.06866617, -0.82542811, -0.36418378, -0.1220451, 0.29889139]
    }

---

    {
      "type": "logical",
      "attributes": {
        "dim": {
          "type": "integer",
          "attributes": {},
          "value": [10, 2]
        },
        "dimnames": {
          "type": "list",
          "attributes": {},
          "value": [
            {
              "type": "NULL"
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["pos", "neg"]
            }
          ]
        }
      },
      "value": [true, true, true, false, false, true, true, true, true, false, false, false, false, true, true, false, false, false, false, true]
    }

---

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["covariates", "thresh_method", "thresh_level", "na_action", "bias_correct"]
        }
      },
      "value": [
        {
          "type": "logical",
          "attributes": {},
          "value": [false]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["sparsity"]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0.01]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["fail"]
        },
        {
          "type": "logical",
          "attributes": {},
          "value": [true]
        }
      ]
    }

---

    Code
      result
    Output
      CPM results:
        Call: fit(object = cpm_spec(thresh_method = "sparsity"), conmat = conmat, 
          behav = behav)
        Number of observations: 10
          Complete cases: 10
        Number of edges: 10
        Parameters:
          Covariates:       FALSE
          Threshold method: sparsity
          Threshold level:  0.01
          Bias correction:  TRUE

# Different threshold levels works

    {
      "type": "double",
      "attributes": {
        "dim": {
          "type": "integer",
          "attributes": {},
          "value": [10, 3]
        },
        "dimnames": {
          "type": "list",
          "attributes": {},
          "value": [
            {
              "type": "NULL"
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["both", "pos", "neg"]
            }
          ]
        }
      },
      "value": [-0.39584799, -0.39584799, -0.39584799, -0.39584799, -0.39584799, -0.39584799, -0.39584799, -0.39584799, -0.39584799, -0.39584799, -0.39584799, -0.39584799, -0.39584799, -0.39584799, -0.39584799, -0.39584799, -0.39584799, -0.39584799, -0.39584799, -0.39584799, -0.67019166, -0.45072701, -0.2980433, 0.1292616, -0.85121814, 0.11563021, -1.25882169, -0.93600393, -0.06411956, 0.32575354]
    }

---

    {
      "type": "logical",
      "attributes": {
        "dim": {
          "type": "integer",
          "attributes": {},
          "value": [10, 2]
        },
        "dimnames": {
          "type": "list",
          "attributes": {},
          "value": [
            {
              "type": "NULL"
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["pos", "neg"]
            }
          ]
        }
      },
      "value": [false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true]
    }

---

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["covariates", "thresh_method", "thresh_level", "na_action", "bias_correct"]
        }
      },
      "value": [
        {
          "type": "logical",
          "attributes": {},
          "value": [false]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["alpha"]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0.1]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["fail"]
        },
        {
          "type": "logical",
          "attributes": {},
          "value": [true]
        }
      ]
    }

---

    Code
      result
    Output
      CPM results:
        Call: fit(object = cpm_spec(thresh_level = 0.1), conmat = conmat, behav = behav)
        Number of observations: 10
          Complete cases: 10
        Number of edges: 10
        Parameters:
          Covariates:       FALSE
          Threshold method: alpha
          Threshold level:  0.10
          Bias correction:  TRUE

# `na_action` argument works

    Code
      result
    Output
      CPM results:
        Call: fit(object = cpm_spec(), conmat = conmat, behav = behav, na_action = "exclude")
        Number of observations: 10
          Complete cases: 9
        Number of edges: 10
        Parameters:
          Covariates:       FALSE
          Threshold method: alpha
          Threshold level:  0.01
          Bias correction:  TRUE

---

    Code
      result
    Output
      CPM results:
        Call: fit(object = cpm_spec(), conmat = conmat, behav = behav, covariates = covariates, 
          na_action = "exclude")
        Number of observations: 10
          Complete cases: 8
        Number of edges: 10
        Parameters:
          Covariates:       TRUE
          Threshold method: alpha
          Threshold level:  0.01
          Bias correction:  TRUE

---

    Code
      result
    Output
      CPM results:
        Call: fit(object = cpm_spec(), conmat = conmat, behav = behav, covariates = covariates, 
          na_action = "exclude")
        Number of observations: 10
          Complete cases: 8
        Number of edges: 10
        Parameters:
          Covariates:       TRUE
          Threshold method: alpha
          Threshold level:  0.01
          Bias correction:  TRUE

