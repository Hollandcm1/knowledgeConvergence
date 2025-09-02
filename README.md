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
devtools::install_github("Hollandcm1/knowledgeConvergence")
```


## Example Output and Explination

The Knowledge Convergence (KC) figure provides a visual summary of how participants in a conversation align conceptually over time. The plot has two main elements:

- **Group trajectory (bold line):** This line shows the running similarity of the group’s centroid — a mathematical representation of the collective semantic position — to itself over the course of the conversation. It represents the evolving “shared understanding” within the group.  
- **Individual trajectories (lighter lines):** Each participant’s line shows their running similarity to the group centroid. These curves illustrate how closely each person’s contributions align with the group’s evolving shared conceptual space.

Together, the figure highlights:
- **Moments of convergence**, where individual lines cluster tightly around the group trajectory (indicating participants are “on the same page”).  
- **Moments of divergence**, where lines spread apart (indicating one or more participants are introducing different concepts).  
- **Temporal dynamics**, showing how alignment changes throughout the conversation rather than just as a static summary.

This visualization is valuable for spotting patterns such as whether a group gradually builds shared understanding, whether some participants lead or lag behind, or whether alignment fluctuates in response to task phases or key discussion points.

<img width="3042" height="3042" alt="combined_plot_points" src="https://github.com/user-attachments/assets/69d7271e-b0c5-4101-87f4-6112790a9402" />

