# knowledgeConvergence

The `knowledgeConvergence` R package provides tools for analyzing how individuals or groups semantically align over time during communication. It implements a method known as **Knowledge Convergence (KC)**, based on Latent Semantic Analysis (LSA), to visualize and quantify convergence toward a group semantic centroid.

This package is particularly useful for researchers studying group communication, collaboration, decision-making, or dialogue in contexts such as education, engineering, cognitive science, and human-automation interaction.

---

## 📚 Background

The approach implemented in this package is inspired by the work of **Andy Dong** in the foundational paper:

> Dong, A. (2005). *The latent semantic approach to studying design team communication.* Design Studies, 26(5), 445–461.  
> [https://doi.org/10.1016/j.destud.2004.12.003](https://doi.org/10.1016/j.destud.2004.12.003)

Dong introduced the concept of a **semantic centroid** for a team, and measured how individual contributions converge toward or diverge from this centroid over time. This method has since been adapted for broader use in communication analysis.

---

## 🔧 What the Package Does

- **Preprocess communication data**: from structured logs of conversations (e.g., transcribed dialogue)
- **Build semantic representations**: using LSA on a document-term matrix
- **Calculate running centroids**: for individuals and the group
- **Measure convergence**: via cosine similarity between running and group centroids
- **Visualize**: group and participant trajectories

---

## 📥 Inputs

- A `data.frame` with at minimum:
  - A `participant` identifier column
  - A `text` column (containing cleaned/transcribed dialogue)
  - An optional `time` or `utterance order` column

---

## 📤 Outputs

- A group semantic centroid (`vector`)
- Running convergence scores for each participant (`list`)
- Running group-level centroid progression (`data.frame`)
- Plots showing individual and group trajectories (`ggplot` objects)

---

## 🚀 Installation

```r
# Development version
devtools::install_github("Hollancm1/knowledgeConvergence")