# Free Throw Analysis

This repository contains an R script that analyzes the performance of basketball players during free throws in tense game moments versus normal moments. The analysis relies on the data of free throws made by each player during different game situations.

## Dependencies

The script requires the following R libraries:

- `cowplot`
- `ggforce`
- `ggplot2`

Ensure that these libraries are installed before running the script.

## Usage

```bash
Rscript free_throw_analysis.R
```

### Script Walkthrough

The script performs the following operations:

1. Loads a CSV file `free_throws.csv` containing free throw data for different players.
2. The script then processes this data to calculate various parameters like the total game time in seconds, the difference in scores between the two teams at the moment of the free throw, and identifies whether a free throw was successful or not.
3. It identifies "tense" moments in the game as those within the last 5 minutes (modifiable by `final_x_minutes` variable) and where the score difference between the teams is less than 10 (modifiable by `min_score_difference` variable).
4. It splits the data into two categories: "tense" and "normal" (chill) moments.
5. For each player, it calculates the proportion of successful shots in tense and chill moments.
6. It then identifies players who show a significant difference in their performance under tense vs chill situations.
7. Finally, it generates two plots: a dot plot showing the proportion of successful shots for each player in tense and chill moments, and a bar plot showing the difference in performance between the two conditions. The two plots are combined and saved as a PDF file `free_throws_analysis.pdf`.

## Input File

The script expects a CSV file `free_throws.csv` in a `data` directory. This file should contain free throw data with the following columns: `game_id`, `time`, `period`, `play`, `score`, `player`, `shot_made`.

## Output

The script outputs a PDF file `free_throws_analysis.pdf` containing two plots. The first plot shows the proportion of successful shots for each player in tense and chill moments. The second plot is a bar plot showing the difference in performance between the two conditions.
