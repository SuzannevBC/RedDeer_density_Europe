# Numerical top-down effects on red deer (Cervus elaphus) are mainly shaped by humans rather than large carnivores across Europe
Suzanne T.S van Beeck Calkoen, Dries P.J. Kuijper, M. Apollonio, Lena Blondel, Carsten F. Dormann, Ilse Storch, Marco Heurich

## Abstract
1. Terrestrial ecosystems are shaped by interacting top-down and bottom-up processes, with the magnitude of top-down control by large carnivores largely depending on environmental productivity. While carnivore-induced numerical effects on ungulate prey populations have been demonstrated in large, relatively undisturbed ecosystems, whether large carnivores can play a similar role in more human-dominated systems is a clear knowledge gap. As humans influence both predator and prey in a variety of ways, the ecological impacts of large carnivores can be largely modified. We quantified the interactive effects of human activities and large carnivore presence on red deer (Cervus elaphus) population density and how their impacts interacted and varied with environmental productivity
2. Data on red deer density were collected based on a literature survey encompassing 492 study sites across 28 European countries. Variation in density across study sites was analysed using a generalised additive model in which productivity, carnivore presence (grey wolf, European lynx, Brown bear), human activities (hunting, intensity of human land-use activity), site protection status and climatic variables served as predictors. 
3. The results showed that a reduction in deer density only occurred when wolf, lynx and bear co-occurred within the same site. In the absence of large carnivores, red deer density varied along a productivity gradient without a clear pattern. Although a linear relationship with productivity in the presence of all three large carnivore species was found, this was not statistically significant. Moreover, hunting by humans had a stronger effect than the presence of all large carnivores in reducing red deer density and red deer density increased with increasing intensity of human land-use, with stronger large carnivore effects (all three carnivore species present) at sites with low human land-use activities. 
4. Synthesis and applications. This study provides evidence for the dominant role played by humans (i.e. hunting, land-use activities) relative to large carnivores in reducing red deer density across European human-dominated landscapes. These findings suggest that when we would like large carnivores to exert numeric effects, we should focus on minimizing human impacts to allow the ecological impacts of large carnivores on ecosystem functioning.

Keywords: Cervus elaphus, top-down control, numerical effects ,large carnivores, environmental productivity, hunting by humans, human land-use activities

## Extract files
To extract all Data and Rscripts, download the zip file and unpack on your own device. 

## Running the Rscripts
To run all scripts, open the R project "SvBC_2023_RedDeer" and run the Rscripts saved within. All data will be saved and shown within the respective folders. 

## Rscript structure
01. Model
- Data preparation
- Pre-modelling exploratory analyses
- Model
- Post-modelling diagnostics
- Results

02. Figures
- Repetition model
- Boxplots parametric coefficients
- Plot smooths
	- Human influence index * Predation
	- Net primary productivity * Predation
	- Environmental plots (tree canopy cover, Normalized difference snow index , Palmer drough severity index)

03. SuppInfo_II
- 5-km buffer
	- Data preparation
	- Pre-modelling exploratory analyses
	- Model
	- Post-modelling diagnostics
	- Results
- 5000 x 5000 resolution
	- Data preparation
	- Pre-modelling exploratory analyses
	- Model
	- Post-modelling diagnostics
	- Results

04. SuppInfo_III
- Check stability model, randomly deleting 10% of the data
	- Data preparation
	- Model
	- Post-modelling diagnostics
	- Results
	- Figures
- Randomly change density estimates by 60%
	- Data preparation
	- Model
	- Post-modelling diagnostics
	- Results
	- Figures
- Delta deviance explained
