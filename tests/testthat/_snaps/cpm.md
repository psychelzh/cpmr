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
          "value": ["covariates", "association_method", "threshold_method", "threshold_level", "feature_space", "edge_weighting", "weighting_scale", "model", "na_action", "standardize_edges"]
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
          "value": ["pearson"]
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
          "value": ["separate"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["binary"]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0.05]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["lm"]
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
      CPM fit:
        Call: fit(object = cpm_spec(), conmat = conmat, behav = behav)
        Number of observations: 10
          Complete cases: 10
        Candidate edges: 10
        Parameters:
          Covariates:       none
          Association:      pearson
          Threshold method: alpha
          Threshold level:  0.01
          Feature space:    separate
          Edge weighting:   binary
          Weighting scale:  0.05
          Outcome model:    linear regression
          Streams:          joint, positive, negative
          Edge standardization: z-score

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
          "value": ["covariates", "association_method", "threshold_method", "threshold_level", "feature_space", "edge_weighting", "weighting_scale", "model", "na_action", "standardize_edges"]
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
          "value": ["pearson"]
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
          "value": ["separate"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["binary"]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0.05]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["lm"]
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
      CPM fit:
        Call: fit(object = cpm_spec(screen = cpm_screen(threshold = cpm_threshold(level = 0.1))), 
          conmat = conmat, behav = behav)
        Number of observations: 10
          Complete cases: 10
        Candidate edges: 10
        Parameters:
          Covariates:       none
          Association:      pearson
          Threshold method: alpha
          Threshold level:  0.1
          Feature space:    separate
          Edge weighting:   binary
          Weighting scale:  0.05
          Outcome model:    linear regression
          Streams:          joint, positive, negative
          Edge standardization: z-score

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
          Covariates:       none
          Association:      pearson
          Threshold method: alpha
          Threshold level:  0.01
          Feature space:    separate
          Edge weighting:   binary
          Weighting scale:  0.05
          Outcome model:    linear regression
          Streams:          joint, positive, negative
          Edge standardization: z-score

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
          Covariates:       included
          Association:      pearson
          Threshold method: alpha
          Threshold level:  0.01
          Feature space:    separate
          Edge weighting:   binary
          Weighting scale:  0.05
          Outcome model:    linear regression
          Streams:          joint, positive, negative
          Edge standardization: z-score

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
          Covariates:       included
          Association:      pearson
          Threshold method: alpha
          Threshold level:  0.01
          Feature space:    separate
          Edge weighting:   binary
          Weighting scale:  0.05
          Outcome model:    linear regression
          Streams:          joint, positive, negative
          Edge standardization: z-score

