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
              "value": ["joint", "positive", "negative"]
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
              "value": ["positive", "negative"]
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
          "value": ["selection", "construction", "model", "covariates", "na_action"]
        }
      },
      "value": [
        {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["type", "method", "criterion", "level"]
            }
          },
          "value": [
            {
              "type": "character",
              "attributes": {},
              "value": ["cor"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["pearson"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["p_value"]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0.01]
            }
          ]
        },
        {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["type", "polarity", "weight_scale", "standardize_edges"]
            }
          },
          "value": [
            {
              "type": "character",
              "attributes": {},
              "value": ["summary"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["separate"]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0]
            },
            {
              "type": "logical",
              "attributes": {},
              "value": [false]
            }
          ]
        },
        {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["type"]
            }
          },
          "value": [
            {
              "type": "character",
              "attributes": {},
              "value": ["lm"]
            }
          ]
        },
        {
          "type": "logical",
          "attributes": {},
          "value": [false]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["fail"]
        }
      ]
    }

---

    Code
      result
    Output
      CPM fit:
        Call: fit(object = cpm_spec(), conmat = conmat, behav = behav)
        Number of observations: 10
          Complete cases: 10
        Candidate edges: 10
        Parameters:
          Covariates:            none
          Selection method:      pearson
          Selection criterion:   p_value
          Selection level:       0.01
          Construction polarity: separate
          Edge weighting:        none
          Weight scale:          none
          Edge standardization:  none
          Streams:               joint, positive, negative
          Outcome model:         linear regression

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
              "value": ["joint", "positive", "negative"]
            }
          ]
        }
      },
      "value": [-0.67019166, -0.45072701, -0.2980433, 0.1292616, -0.85121814, 0.11563021, -1.25882169, -0.93600393, -0.06411956, 0.32575354, -0.39584799, -0.39584799, -0.39584799, -0.39584799, -0.39584799, -0.39584799, -0.39584799, -0.39584799, -0.39584799, -0.39584799, -0.67019166, -0.45072701, -0.2980433, 0.1292616, -0.85121814, 0.11563021, -1.25882169, -0.93600393, -0.06411956, 0.32575354]
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
              "value": ["positive", "negative"]
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
          "value": ["selection", "construction", "model", "covariates", "na_action"]
        }
      },
      "value": [
        {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["type", "method", "criterion", "level"]
            }
          },
          "value": [
            {
              "type": "character",
              "attributes": {},
              "value": ["cor"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["pearson"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["p_value"]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0.1]
            }
          ]
        },
        {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["type", "polarity", "weight_scale", "standardize_edges"]
            }
          },
          "value": [
            {
              "type": "character",
              "attributes": {},
              "value": ["summary"]
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["separate"]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0]
            },
            {
              "type": "logical",
              "attributes": {},
              "value": [false]
            }
          ]
        },
        {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["type"]
            }
          },
          "value": [
            {
              "type": "character",
              "attributes": {},
              "value": ["lm"]
            }
          ]
        },
        {
          "type": "logical",
          "attributes": {},
          "value": [false]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["fail"]
        }
      ]
    }

---

    Code
      result
    Output
      CPM fit:
        Call: fit(object = cpm_spec(selection = cpm_selection_cor(level = 0.1)), 
          conmat = conmat, behav = behav)
        Number of observations: 10
          Complete cases: 10
        Candidate edges: 10
        Parameters:
          Covariates:            none
          Selection method:      pearson
          Selection criterion:   p_value
          Selection level:       0.1
          Construction polarity: separate
          Edge weighting:        none
          Weight scale:          none
          Edge standardization:  none
          Streams:               joint, positive, negative
          Outcome model:         linear regression

# `na_action` argument works

    Code
      result
    Output
      CPM fit:
        Call: fit(object = cpm_spec(), conmat = conmat, behav = behav, na_action = "exclude")
        Number of observations: 10
          Complete cases: 9
        Candidate edges: 10
        Parameters:
          Covariates:            none
          Selection method:      pearson
          Selection criterion:   p_value
          Selection level:       0.01
          Construction polarity: separate
          Edge weighting:        none
          Weight scale:          none
          Edge standardization:  none
          Streams:               joint, positive, negative
          Outcome model:         linear regression

---

    Code
      result
    Output
      CPM fit:
        Call: fit(object = cpm_spec(), conmat = conmat, behav = behav, covariates = covariates, 
          na_action = "exclude")
        Number of observations: 10
          Complete cases: 8
        Candidate edges: 10
        Parameters:
          Covariates:            included
          Selection method:      pearson
          Selection criterion:   p_value
          Selection level:       0.01
          Construction polarity: separate
          Edge weighting:        none
          Weight scale:          none
          Edge standardization:  none
          Streams:               joint, positive, negative
          Outcome model:         linear regression

---

    Code
      result
    Output
      CPM fit:
        Call: fit(object = cpm_spec(), conmat = conmat, behav = behav, covariates = covariates, 
          na_action = "exclude")
        Number of observations: 10
          Complete cases: 8
        Candidate edges: 10
        Parameters:
          Covariates:            included
          Selection method:      pearson
          Selection criterion:   p_value
          Selection level:       0.01
          Construction polarity: separate
          Edge weighting:        none
          Weight scale:          none
          Edge standardization:  none
          Streams:               joint, positive, negative
          Outcome model:         linear regression

