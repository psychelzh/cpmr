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
      "type": "NULL"
    }

---

    Code
      result
    Output
      CPM results based on leave-one-out cross validation.

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
      "type": "NULL"
    }

---

    Code
      result
    Output
      CPM results based on 5-fold cross validation.

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
      "type": "NULL"
    }

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
      "type": "NULL"
    }

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
      "type": "NULL"
    }

# `return_edges` argument works

    {
      "type": "logical",
      "attributes": {
        "dim": {
          "type": "integer",
          "attributes": {},
          "value": [10, 10, 2]
        },
        "dimnames": {
          "type": "list",
          "attributes": {},
          "value": [
            {
              "type": "NULL"
            },
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
      "value": [false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true, false]
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

