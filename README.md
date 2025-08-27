# R Code Reasoning & Benchmark Portfolio 👩🏽‍💻📊

> 🚀 Focus: Algorithm Design • Code Reasoning • Benchmarking • Test Automation  
> 📍 Created by Michelle Goulbourne Robinson – Candidate for Turing Sr. R Engineer Role  

![R](https://img.shields.io/badge/R-Programming-blue?logo=r&logoColor=white)
![GitHub repo size](https://img.shields.io/github/repo-size/mrobinson102/R-CodeReasoning-Portfolio)
![License: MIT](https://img.shields.io/badge/License-MIT-green.svg)

---

## 🔍 Overview

This repository showcases **algorithmically rich R code** designed to test **reasoning, correctness, and performance**, directly aligned with Turing’s **Sr. R Engineer – Code Reasoning & Benchmark** role.

Each folder demonstrates problem-solving across **data structures, algorithms, graphs, number theory, and test automation**, with structured metadata for evaluation.

---

## 📂 Repository Structure

| Folder                     | Content                                                                 |
|----------------------------|-------------------------------------------------------------------------|
| `01_data_structures/`      | Implementation of core structures (Segment Tree, Union Find)            |
| `02_algorithms/`           | Greedy, Dynamic Programming examples                                    |
| `03_graphs/`               | Graph algorithms (Dijkstra’s shortest paths)                            |
| `04_number_theory/`        | Modular arithmetic (modular inverse)                                    |
| `05_testing_framework/`    | Automated validation using `testthat` and micro-benchmarks              |
| `06_problem_design/`       | YAML-based problem specifications with taxonomy, difficulty, reliability |

---

## ⚙️ Example Problems

- **Segment Tree** → Fast range sum queries in `O(log n)` and point updates  
- **Union Find (DSU)** → Connectivity queries with path compression + union by rank  
- **Greedy Scheduler** → Select jobs by earliest deadline while maximizing throughput  
- **DP Pathfinding** → Minimal path sum on a grid (bottom-up DP)  
- **Dijkstra** → Single-source shortest paths on weighted directed graphs  
- **Modular Inverse** → Extended Euclidean Algorithm with `O(log m)` complexity  
- **Testing Harness** → Validate solutions and benchmark runtime performance  

---

## 🧪 Running the Code

1. **Clone**  
   ```bash
   git clone https://github.com/mrobinson102/R-CodeReasoning-Portfolio.git
   cd R-CodeReasoning-Portfolio
   ```

2. **Install dependencies**  
   ```R
   source("requirements.R")
   ```

3. **Run all tests**  
   ```R
   source("05_testing_framework/test_runner.R")
   ```

---

## 🧠 About Me

I’m **Michelle Goulbourne Robinson**, a Federal Data Architect & R Developer with 20+ years of experience designing algorithmic solutions, benchmarks, and mission-critical systems across **DoD, NAVSEA, USDA, and NASA**.

- **Expertise:** R Programming, Algorithm Design, Benchmarking, CI/CD, Cloud Data Systems  
- **Email:** MichelleGRobinson1@gmail.com

---

## 📜 License

This project is licensed under the MIT License – see the [LICENSE](LICENSE) file for details.

![R-CI](https://github.com/mrobinson102/R-CodeReasoning-Portfolio/actions/workflows/r-ci.yml/badge.svg)

## Interactive UI (Shiny)
Run the demos locally:

```r
source("run_app.R")
```

Tabs include **Segment Tree**, **Union–Find**, **Dijkstra**, **DP Min Path**, **Modular Inverse**, and (if present) **Topological Sort**.
