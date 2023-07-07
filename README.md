
# fastproj

The goal of fastproj is for fast and convenient population projection,
and to provide corresponding Word reports.

## Installation

``` r
devtools::install_github("yanyuteng/fastproj")
library(fastproj)
```

## Example

``` r
library(fastproj)
#infile_pop = read_pop('~/Pop_Projection/1_Input/Data_Population.xlsx') # 1. input the baseline population
head(infile_pop[[1]])
#inparm_scene = read_param('~/Pop_Projection/1_Input/Data_Parameter.xlsx') # 2. input the parameters of different scenes
head(inparm_scene[[1]])

inparm_lt = lx_modify(q0_5_m = 0.00318, e0_m = 76.75,
                      q0_5_f = 0.00264, e0_f = 82.22) #2020 Guangdong province, ##  3. Modified by Wilmoth Method from 'DemoTools'
#inparm_lt = list(lt_1year(c(0:100),infile_pop[[1]][,4]),lt_1year(c(0:100),infile_pop[[1]][,5]))

path_out_table = '~/Desktop/Pop_Projection/2_Output_Table'
f.projection(infile_pop, inparm_scene, inparm_lt, path_out_table) # 4. Projection


path_out_pic = c('~/Pop_Projection/3_Output_Pic_pop1',
                 '~/Pop_Projection/3_Output_Pic_pop2')
path_out = c('~/Pop_Projection')
area_name = 'Guangdong Province'
pop_name = c('Changzhu','Huji')
f.projection2word(path_out_table, path_out_pic, path_out,
                  initial_year = 2020, x_by = 5, area_name, pop_name) # 5. Write the Output
```
