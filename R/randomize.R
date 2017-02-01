#' Calculate block size options based on predetermined N
#'
#' @param num_blocks Number of blocks used, only works for 1:3.
#' @param levels Number of treatment levels in the study.  Used to determine odd/even block sizes.
#' @param N The total number of randomization slots you'll need.
#' @return A nx4 data.frame with all options for block sizes for your given parameters.
#' @examples
#' calc_blocks(num_blocks=3, levels=3, N=216)


calc_blocks <- function(num_blocks, levels, N){
  #test inputs
  if(1==2){
    num_blocks=3
    levels=2
    N=200
  }
  my_list <- list()
  blocks <- seq(levels,15,levels)


  #Data frame for a single block
  temp <- data.frame(blocks[(N %% blocks)==0],N / blocks[(N %% blocks)==0])
  names(temp) <- c("block 1","#")
  my_list[[1]] <- temp

  #Data frame for two block sizes
  x<- y <- z <- NULL
  counter <- 1
  for(i in blocks){
    for(j in blocks[blocks>i]){
      if(N %% (i+j)==0){
        x[counter]<- i
        y[counter] <- j
        z[counter] <- N / (i+j)
        counter <- counter + 1
      }
    }
  }

  temp <- data.frame(x,y,z)
  if(nrow(temp)>0){
    names(temp) <- c("block 1","block 2","#")
    my_list[[2]] <- temp
  }else{my_list[[2]] <- "There were 0 matches" }

  #Data frame for three block sizes
  w<-x<-y<-z<-NULL
  counter <- 1

  for(i in blocks){
    for(j in blocks[blocks>i]){
      for(k in blocks[blocks>j]){
        if(N %% (i+j+k)==0){
          w[counter]<- i
          x[counter]<- j
          y[counter] <- k
          z[counter] <- N / (i+j+k)
          counter <- counter + 1
        }
      }
    }
  }
  temp <- data.frame(w,x,y,z)
  if(nrow(temp)>0){
    names(temp) <- c("block 1","block 2","block 3","#")
    my_list[[3]] <- temp
  }else{my_list[[3]] <- "There were 0 matches" }


  #ouput the correct list
  my_list[[num_blocks]]
}


#'Calculate N based on predetermined block sizes
#'
#' @param blocks A vector of block sizes you want to use.
#' @param levels Number of treatment levels in the study.
#' @param approx The Approx. total number of randomization slots you'll want.
#' @return A vector of 4 options of N based off the given inputs.
#' @examples
#' calc_N(blocks=c(3,24), levels=3,approx=225)


calc_N <- function(blocks, levels,approx){
  #test inputs
  if(1==2){
    blocks <- c(2,4,6)
    levels = 2
    approx = 150
  }

  if(sum(blocks %% levels) != 0){print("Your levels do not fit in your block sizes")
  }else{

    n_mid <- approx %/% sum(blocks)
    n_counts <- sum(blocks) * seq(n_mid-1 , n_mid+2,1)

    print("For your block sizes you can use an N of:")
    n_counts

  }

}


#'Create a randomization list for upload into Medidata
#'
#' @param list_name A name to identify your list, also the filename.
#' @param myseed Randomization seed.
#' @param site_n The number of sites in your study.
#' @param n The number of randomization slots per site.
#' @param block_sizes A list of block sizes.
#' @param arm_codes A vector of Arm codes.
#' @param arm_names A vector or Arm descriptions.
#' @param out_loc Where the CSV will get output to.
#'
#' @return A randomization list
#' @examples
#' create_list(list_name = "test_list",
#'            myseed = 213,
#'            site_n = 1,
#'            n = 216,
#'            block_sizes = c(3,24),
#'            arm_codes = c("a","b","c"),
#'            arm_names = c("Drug A description", "Drug B description", "Drug C description"),
#'            out_loc = "C:/Users/Desktop/")


create_list <- function(list_name , myseed, site_n,n,block_sizes,arm_codes,arm_names,out_loc){
  #############

  #test inputs
  if(1==2){
    myseed <- 20
    site_n <- 5
    n <- 216
    block_sizes <- c(6,9,12)
    list_name <- paste0("test_list",Sys.Date() )
    arm_codes <- c("a","b","c")
    arm_names <- c("Drug A description", "Drug B description", "Drug C description")
    out_loc <- "C:/Users/michael/Desktop/"
  }

  set.seed(myseed)
  total_n <- n*site_n
  block_n <- n/sum(block_sizes)
  block_list <- rep(block_sizes,block_n*site_n)

  random_df <- data.frame(matrix(0,nrow = total_n, ncol = 8), stringsAsFactors=FALSE)
  names(random_df) <- c("List Name","Block Size","Block Random num","Block ID","Randomization ID","Arm Code","Arm Name","Arm Random num")
  random_df[,1] <- list_name

  ii <- 1
  ij <- 1
  for(i in block_list){
    temp_rnorm <- rnorm(1)
    for(j in 1:i){
      random_df[ii,2] <- i #block size
      random_df[ii,3] <- temp_rnorm #random number
      random_df[ii,4] <- ij #block id

      ii <- ii + 1
    }
    ij <- ij+1
  }

  random_df[,6] <- rep(arm_codes,total_n/length(arm_codes))
  random_df[,7] <- rep(arm_names,total_n/length(arm_codes))
  random_df[,8] <- rnorm(total_n)

  ordered_data <- random_df[order(random_df[,1],as.numeric(random_df[,3]),as.numeric(random_df[,8])),]
  ordered_data[,5] <- 1:nrow(ordered_data) #randomization ID


  final_data <- ordered_data[,c(1,4,5,6,7)]



  write.csv(final_data, paste(out_loc,list_name,".csv",sep=""),row.names=FALSE)

}

