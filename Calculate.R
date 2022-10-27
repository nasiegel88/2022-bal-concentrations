library(tidyverse)
library(rstatix)
library(ggpubr)
library(readxl)
library(openxlsx)

df <- data.frame(
  Sample = c('A', 'A', 'B', 'C'),
  Length = c('100', '110', '99', '102'),
  Molarity = c(5,4,6,7)
)

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

# Calculations for pooling samples
pooling <- filtered %>%
  select(`...1`, Size, Molarity) %>%
  
  group_by(`...1`) %>%
  
  # Rename to specify units
  rename(
    Samples = `...1`,
    `Molarity (pmol/l)` = Molarity
    ) %>%
  
  # Convert pmol/l to nmol/ul
  mutate(`Molarity (nmol/ul)` = `Molarity (pmol/l)`*(1/0.001)*(1/10^6)) %>%
  
  # Determine how many moles are in each sample which consist of 20ul
  mutate(`Total Nano Mols` = `Molarity (nmol/ul)`*(20)) %>%
  
  # Arrange in decending order
  arrange(`Total Nano Mols`) %>%
  
  mutate(Size = as.character(Size)) %>%
  
  summarise(Size = toString(Size),
            `Molarity (pmol/l)` = sum(`Molarity (pmol/l)`),
            `Molarity (nmol/ul)` = sum(`Molarity (nmol/ul)`),
            `Total Nano Mols` = sum(`Total Nano Mols`),
            )  %>%

  # Calculate the volume need for all samples to have the same number of mols
  mutate(
    `Volume Needed to Pool Samples` = round(1.940/`Molarity (nmol/ul)`, 2),
    `Actual Volume Needed` = if_else(
      `Total Nano Mols` < 1.940, paste('Use whole sample'),
      paste(round(`Volume Needed to Pool Samples`,0)))
  )

write.xlsx(pooling, file.path('output', 'pooling.xlsx'))
