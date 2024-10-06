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
      "value": [-0.36089704, -0.46837374, -0.4124209, -0.40121526, -0.33409571, -0.43482802, -0.3526195, -0.25450422, -0.39758371, -0.54194184, -0.36089704, -0.46837374, -0.4124209, -0.40121526, -0.33409571, -0.43482802, -0.3526195, -0.25450422, -0.39758371, -0.54194184, -0.36089704, -0.53040432, -0.4124209, -0.40121526, -0.33409571, -0.43482802, -0.3526195, -0.25450422, -0.39758371, -0.54194184]
    }

---

    {
      "type": "double",
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
      "value": [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1]
    }

---

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["confounds", "thresh_method", "thresh_level", "kfolds", "bias_correct"]
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
          "type": "integer",
          "attributes": {},
          "value": [10]
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
        Call: cpm(conmat = conmat, behav = behav)
        Number of observations: 10
        Number of edges: 10
        Parameters:
          Confounds:        FALSE
          Threshold method: alpha
          Threshold level:  0.01
          CV folds:         10
          Bias correction:  TRUE

# `kfolds` works

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
      "value": [-0.19751643, -0.60963073, -0.45834504, -0.33241485, -0.33241485, -0.45834504, -0.34916862, -0.19751643, -0.34916862, -0.92500993, -0.19751643, -0.60963073, -0.45834504, -0.33241485, -0.33241485, -0.45834504, -0.34916862, -0.19751643, -0.34916862, -0.92500993, -0.19751643, -0.64179503, -0.45834504, -0.33241485, -0.33241485, -0.45834504, -0.34916862, -0.19751643, -0.34916862, -0.64179503]
    }

---

    {
      "type": "double",
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
      "value": [0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    }

---

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["confounds", "thresh_method", "thresh_level", "kfolds", "bias_correct"]
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
          "type": "double",
          "attributes": {},
          "value": [5]
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
        Call: cpm(conmat = conmat, behav = behav, kfolds = 5)
        Number of observations: 10
        Number of edges: 10
        Parameters:
          Confounds:        FALSE
          Threshold method: alpha
          Threshold level:  0.01
          CV folds:         5
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
      "value": [-0.4483117, -0.93444812, -0.28979535, -1.0184311, -1.22715776, 0.27139805, 0.49221098, 0.39722219, -0.27275375, -1.05667388, -0.4727088, -1.13102784, -0.30731623, -0.39982201, -0.81405633, 0.45584186, 0.55325269, -0.03339286, -0.562466, -0.73762965, -0.38224409, -0.1340772, -0.3410544, -1.25039233, -0.98049407, -0.32868202, -0.1470275, 0.54417272, -0.06844446, -1.13204492]
    }

---

    {
      "type": "double",
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
      "value": [9, 9, 9, 1, 2, 9, 9, 9, 9, 0, 1, 1, 1, 9, 8, 1, 1, 1, 1, 10]
    }

---

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["confounds", "thresh_method", "thresh_level", "kfolds", "bias_correct"]
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
          "type": "integer",
          "attributes": {},
          "value": [10]
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
        Call: cpm(conmat = conmat, behav = behav, thresh_method = "sparsity")
        Number of observations: 10
        Number of edges: 10
        Parameters:
          Confounds:        FALSE
          Threshold method: sparsity
          Threshold level:  0.01
          CV folds:         10
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
      "value": [-0.36089704, -0.46837374, -0.4124209, -0.40121526, -0.33409571, -0.43482802, -0.3526195, -0.25450422, -0.39758371, -0.39395137, -0.36089704, -0.46837374, -0.4124209, -0.40121526, -0.33409571, -0.43482802, -0.3526195, -0.25450422, -0.39758371, -0.83412333, -0.66417678, -0.53040432, -0.30399265, 0.25624007, -0.82873672, 0.15696941, -1.57202923, -0.36423604, -0.01093361, 0.06133675]
    }

---

    {
      "type": "double",
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
      "value": [0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 10]
    }

---

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["confounds", "thresh_method", "thresh_level", "kfolds", "bias_correct"]
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
          "type": "integer",
          "attributes": {},
          "value": [10]
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
        Call: cpm(conmat = conmat, behav = behav, thresh_level = 0.1)
        Number of observations: 10
        Number of edges: 10
        Parameters:
          Confounds:        FALSE
          Threshold method: alpha
          Threshold level:  0.10
          CV folds:         10
          Bias correction:  TRUE

# Works with confounds

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
      "value": [0.03747695, -0.08184062, -0.00361471, -0.00804253, 0.05332356, -0.04522479, 0.03893979, 0.14452379, 0.00353565, -0.13907709, 0.03747695, -0.08184062, -0.00361471, -0.00804253, 0.05332356, -0.04522479, 0.03893979, 0.14452379, 0.00353565, -0.13907709, 0.03747695, -0.08184062, -0.00361471, 0.76576079, 0.05332356, -0.04522479, 0.03893979, 0.14452379, 0.00353565, -0.13907709]
    }

---

    {
      "type": "double",
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
      "value": [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1]
    }

---

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["confounds", "thresh_method", "thresh_level", "kfolds", "bias_correct"]
        }
      },
      "value": [
        {
          "type": "logical",
          "attributes": {},
          "value": [true]
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
          "type": "integer",
          "attributes": {},
          "value": [10]
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
        Call: cpm(conmat = conmat, behav = behav, confounds = confounds)
        Number of observations: 10
        Number of edges: 10
        Parameters:
          Confounds:        TRUE
          Threshold method: alpha
          Threshold level:  0.01
          CV folds:         10
          Bias correction:  TRUE

# `return_edges` argument works

    Code
      result
    Output
      CPM results:
        Call: cpm(conmat = conmat, behav = behav, return_edges = "none")
        Number of observations: 10
        Number of edges: unknown
        Parameters:
          Confounds:        FALSE
          Threshold method: alpha
          Threshold level:  0.01
          CV folds:         10
          Bias correction:  TRUE

---

    {
      "type": "logical",
      "attributes": {
        "dim": {
          "type": "integer",
          "attributes": {},
          "value": [10, 2, 10]
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
            },
            {
              "type": "NULL"
            }
          ]
        }
      },
      "value": [false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false]
    }

---

    Code
      result
    Output
      CPM results:
        Call: cpm(conmat = conmat, behav = behav, return_edges = "all")
        Number of observations: 10
        Number of edges: 10
        Parameters:
          Confounds:        FALSE
          Threshold method: alpha
          Threshold level:  0.01
          CV folds:         10
          Bias correction:  TRUE

# `na_action` argument works

    {
      "type": "double",
      "attributes": {},
      "value": [0.25688371, -0.24669188, -0.3475426, -0.95161857, -0.04502772, -0.78490447, -1.66794194, -0.38022652, 0.91899661]
    }

---

    {
      "type": "double",
      "attributes": {
        "dim": {
          "type": "integer",
          "attributes": {},
          "value": [9, 3]
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
      "value": [-0.43811964, -0.37517269, -0.36256635, -0.28705685, -0.40038071, -0.30789611, -0.19751643, -0.35848086, -0.52088375, -0.43811964, -0.37517269, -0.36256635, -0.28705685, -0.40038071, -0.30789611, -0.19751643, -0.35848086, -0.52088375, -0.43811964, -0.37517269, -0.36256635, -0.28705685, -0.40038071, -0.30789611, -0.19751643, -0.35848086, -0.52088375]
    }

---

    {
      "type": "double",
      "attributes": {},
      "value": ["NA", 0.25688371, -0.24669188, -0.3475426, -0.95161857, -0.04502772, -0.78490447, -1.66794194, -0.38022652, 0.91899661]
    }

---

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
      "value": ["NA", -0.43811964, -0.37517269, -0.36256635, -0.28705685, -0.40038071, -0.30789611, -0.19751643, -0.35848086, -0.52088375, "NA", -0.43811964, -0.37517269, -0.36256635, -0.28705685, -0.40038071, -0.30789611, -0.19751643, -0.35848086, -0.52088375, "NA", -0.43811964, -0.37517269, -0.36256635, -0.28705685, -0.40038071, -0.30789611, -0.19751643, -0.35848086, -0.52088375]
    }

