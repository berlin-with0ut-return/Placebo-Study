# Placebo-Study
Study on placebo effect and memory, analysis in R

# Abstract
The placebo effect has been shown to have significant impacts across many psychological domains. This study investigated the impact of an expectancy-inducing placebo on declarative memory. 21 participants were asked to memorize a series of nonsense syllables after viewing either a memory-positive or memory-neutral placebo stimulus. Participants were also assessed on their knowledge of the placebo effect. No significant differences were observed in terms of memory performance between treatment and control, or across different levels of placebo knowledge; however, a significant interaction effect emerged, with larger treatment-control differences in those with higher levels of placebo knowledge. These results highlight the importance of expectancies in conscious processing.

# Hypotheses
The hypotheses investigated were two univariate models, and one multivariate model.
|                                                                 |                                                  Null Hypothesis                                                 |                                                                Alternative Hypothesis                                                               |   |   |
|-----------------------------------------------------------------|:----------------------------------------------------------------------------------------------------------------:|:---------------------------------------------------------------------------------------------------------------------------------------------------:|---|---|
| Hypothesis 1 [memory ~ placebo + error]                         | Watching a placebo-inducing video will have no effect on participants’ performance on a declarative memory task. | Watching a placebo-inducing video will increase participants’ performance on a declarative memory task.                                             |   |   |
| Hypothesis 2 [memory ~ placebo knowledge (PK) + error]          | More knowledge about the placebo effect has no effect on memory.                                                 | More knowledge about the placebo effect will cause a change in memory performance, as compared to those with no knowledge about the placebo effect. |   |   |
| Hypothesis 3 [memory ~ placebo * placebo knowledge (PK)+ error] | Psychological knowledge has no effect on the placebo effect or memory.                                           | For those with more knowledge about the placebo effect, the impact of the placebo effect on memory improvement will be reduced.                     |   |   |

# Variables Measured
7 variables were measured in total. 3 were demographic; one DV (memory performance) and two IVs (placebo or none, placebo knowledge) were measured.
|       Type of Measure      |                                       Variable Name                                      |                                                                                                             How Will you Measure / Manipulate this Variable?                                                                                                             | # of questions in Measure | Type of Data |
|:--------------------------:|:----------------------------------------------------------------------------------------:|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------:|:-------------------------:|:------------:|
|         Demographic        |                                            Age                                           |                                                                                                              What is your age? (open-ended numeric response)                                                                                                             |             1             |  Continuous  |
|         Demographic        |                                            Sex                                           |                                                                                                          What is your gender?  (Female, Male, fill-in response)                                                                                                          |             1             |   Discrete   |
|         Demographic        |                                         Education                                        |                                                                      What is your highest education level (completed or currently pursuing)l? (< high school, high school, college, graduate school)                                                                     |             1             |   Discrete   |
|     Dependent Variable     |                                          Memory                                          |                                                                                                Performance on recall of 2 lists of nonsense syllables (numeric, out of 20)                                                                                               |             20            |  Continuous  |
| Independent Variable (IV1) |                                      Placebo effect                                      |                                                                                          Placebo manipulation (condition with memory-enhancing expectancy vs. condition without)                                                                                         |            n/a            |   Discrete   |
|  Moderator Variable (IV2)  | Awareness  (if the participant knows about placebo effect/other psychological phenomena) | Have you ever taken a psychology class? (Yes: only an intro class, Yes: upper-division/multiple, No) Have you ever heard of the placebo effect? (Yes, no) If yes, how familiar are you with the placebo effect? (not familiar at all, somewhat familiar, very familiar)  |             3             |   Discrete   |

# Study Procedure
1. Participants were emailed an anonymous survey, with either placebo-enhancing video or placebo-neutral video.
2. Participants were asked to recall 2 sets of 10 nonsense syllables to assess memory.

# Results