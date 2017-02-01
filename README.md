# medibalance

##Install

1. install.packages("devtools")
2. devtools::install_github("mjholt02/medibalance")



##Functions

1. calc_N: Used to calculate N based on predetermined block sizes
  * calc_N(blocks, levels, approx)
  * calc_N(blocks=c(3,24), levels=3,approx=225)
2. calc_blocks: Used to determine possible block sizes for a given N
  * calc_blocks(num_blocks, levels, N)
  * calc_blocks(num_blocks=3, levels=3, N=216)
3. create_list: Create your randomization list for Medidata balance.
  * create_list(list_name, myseed, site_n, n, block_sizes, arm_codes, arm_names,
  out_loc)
  * create_list(list_name = "test_list",
           myseed = 213,
           site_n = 1,
           n = 216,
           block_sizes = c(3,24),
           arm_codes = c("a","b","c"),
           arm_names = c("Drug A description", "Drug B description", "Drug C description"),
           out_loc = "C:/Users/Desktop/")
