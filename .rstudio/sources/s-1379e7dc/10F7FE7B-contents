library(tidyverse)
library(rstatix)
library(ggpubr)
library(readxl)
library(openxlsx)

# Output directory
dir.create('output', showWarning = FALSE)

# Measure average molarity for each sample
if (length(unique(read_excel('samples.xlsx')$...1)) == 26) {
  
  data <- read_excel('samples.xlsx') %>%
    group_by(...1) %>%
    mutate(
      Proportion = Molarity/sum(Molarity),
      `Total Proportion` = sum(Proportion),
      Percentage =  str_squish(round((Molarity/sum(Molarity)*100), 2)),
      `# of Peaks` = length(...1)
    )
  
  write.xlsx(data, file.path('output', 'clean_data.xlsx'))
}


# Determine concentrations of target sequences (~600bp)
filtered <- read_excel('samples.xlsx') %>%
  group_by(...1) %>%
  mutate(
    Proportion = Molarity/sum(Molarity),
    `Total Proportion` = sum(Proportion),
    Percentage =  str_squish(round((Molarity/sum(Molarity)*100), 2)),
    `# of Peaks` = length(...1)
  ) %>%
  filter( between(Size, 580, 640) )

if (length(unique(filtered$...1))==26) write.xlsx(filtered,
                                                file.path('output','filtered_length.xlsx'))
