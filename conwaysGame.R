#' @title get neighbours of a cell
#' 
#' @description Creates a 3x3 neighbour matrix for a given element
#'
#' @author Selina Müller
#'
#' @param env Environment containing game matrix: M and dataframe: colorMapping
#' @param i row index of matrix element
#' @param j column index of matrix element
#'
#' @return 3x3 matrix of neighbours for an element with given indices
#' 
#' @examples
#' M <- matrix( c(0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0), nrow=15, ncol=15) 
#' N <- getNeighbours(M, 5,8)
#' 
getNeighbours <- function(env, i,j) {
  
  ncM <- ncol(env$M)
  N <- matrix(nrow = 3, ncol = 3);
  
  if(i == 1 || i == ncM || j == 1 || j == ncM ){
    
    a <- i-1
    if(a == 0) a <-ncM
    b <- i+1
    if(b == ncM+1) b <-1
    c <- j-1
    if(c == 0)c <-ncM
    d <- j+1
    if(d==ncM+1) d <- 1
    
    N[1,1] <- env$M[a,c]
    N[2,1] <- env$M[i,c]
    N[3,1] <- env$M[b,c]
    N[1,2] <- env$M[a,j]
    N[2,2] <- env$M[i,j]
    N[3,2] <- env$M[b,j]
    N[1,3] <- env$M[a,d]
    N[2,3] <- env$M[i,d]
    N[3,3] <- env$M[b,d]
    
  }else{
    
    e<-i-1
    f<-i+1
    g<-j-1
    h<-j+1
    N<-env$M[e:f,g:h]
  }
  
  return(N)
}


#' @titel implement the Game of live Rules
#'
#' @description decides if the matrix central cell schould live or die depending of the number and the state of it neighbours
#' 
#' @author Leila Feddoul
#'
#' @param N  3x3 matrix of neighbours for an element with given indices.
#'
#' @return 1 if the central cell of the matrix should live or 0 when it should die.
#'
#' @examples
#' N = matrix( c(0, 0, 0, 0, 1, 0, 0, 0, 0), nrow=3, ncol=3) 
#' computeIsAlive(N)
#'
computeIsAlive <- function(N) {
  # return 0(Tot) oder 1(Lebend)
  alive<-0
  for ( i in 1:3 )
  {
    for(j in 1:3)
    {
      if(N[i,j]== 1) alive<-alive+1
      
    }
  }
  
  if(N[2,2]==1)
  {
    alive <- alive-1
    if(alive<2 || alive>3) {return(0)}
    else
    {
      if(alive==2 || alive==3) return(1)
    }
  }
  if(N[2,2]==0)
  {
    if(alive==3) return(1)
  }
  return(0)
}


#' Alternative implementation of computeIsAlive function.
#' Decides if a given matrix cell remains alive/dead or not.
#'
#' @author Anastasia Aftakhova
#' 
#' @param N 3x3 neighbours matrix for one cell 
#'
#' @return binary cell value; 1 if alive; 0 if not
#' 
computeIsAlive2 <- function(N) {
  allElem <- sum(N)
  if (N[2,2] == 1) {
    if (allElem < 5 && allElem > 2) {
      return(1)
    } else {
      return(0)
    }
  } else {
    if (allElem == 3) {
      return(1)
    } else {
      return(0)
    }
  }
}


#' @title Update matrix
#' 
#' @description 
#' Determines for each cell in Matrix M, if it survives or dies 
#' and saves updated matrix in R
#' 
#' @author Selina Müller
#'
#' @param env Environment containing game matrix: M and dataframe: colorMapping
#' 
#' @example 
#' M <- matrix( c(0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0), nrow=15, ncol=15) 
#' R <- computeAll(M)
#'
computeAll <- function(env) {
  R <- matrix(data=0, ncol=ncol(env$M), nrow=nrow(env$M))
  
  ncM <- ncol(env$M)
  nrM <- nrow(env$M)
  
  for (i in 1:nrM){
    for(j in 1:ncM){
      N<-getNeighbours(env,i,j)
      R[i,j] <- computeIsAlive2(N)
    }
  }
  env$M <- R
}


#' Visualises the cell matrix in a plot.
#' 
#' @author Anastasia Aftakhova
#'
#' @param env Environment containing the cell matrix and the color mapping
#' @param plotname name of the plotted image
#' 
visualise <- function(env, plotname) {
  tryCatch({
    library(plotrix)
  }, error = function(e) {
    install.packages("plotrix")
    library(plotrix)
  })
  
  cellcol <- array(, dim=dim(env$M))
  for (i in 1:dim(env$colorMapping)[1]) {
    cellcol[which(env$M == env$colorMapping$num[i])] <- env$colorMapping$col[i]
  }
  color2D.matplot(env$M, cellcolors = cellcol, border=NA, main=plotname)
}


#' Saves the current cell matrix plot image into the input path directory
#'
#' @author Anastasia Aftakhova
#' 
#' @param save_path full path string of a directory to save the image into
#' @param iter_id number of current game iteration
#' 
save <- function(save_path, iter_id) {
  dev.copy(png, sprintf('%s/cgofl_%s.png', save_path, iter_id), width=500, height=500, units="px")
  dev.off()
}

#' @title generate a random matrix
#'
#' @description creates a random nxn matrix of ones and zeros.
#' 
#' @author Leila Feddoul
#' 
#' @param matrix_size  the size of the matrix.
#'
#' @return the generated matrix .
#'
#' @examples
#' createMatrix(3)
#'
createMatrix <- function(matrix_size) {
  matrix = replicate(matrix_size,sample(c(0,1),matrix_size,replace = TRUE ,c(0.5,0.5)))
  return(matrix)
}


#' Decolors the matrix. Replaces color numbers in the cell matrix with the default alive value.
#'
#' @author Anastasia Aftakhova
#'
#' @param col Environment containing the cell matrix
#' 
decolorM <- function(env) {
  env$M[which(env$M >0)] = 1;
}


#' Searches for given pattern in the cell matrix and colors it if found.
#' 
#' @author Anastasia Aftakhova
#' 
#' @param env Environment containing the cell matrix an color mapping
#' @param pattern 2D matrix of a pattern to be recognized 
#' @param colNum a color number which is set for alive cells of a found pattern
#' 
findPatternMatch <- function(env, pattern, colNum) {
  location = matrix(, ncol=2)
  for (i in 1:(nrow(env$M)-nrow(pattern)-1)) {
    for (j in 1:(ncol(env$M)-ncol(pattern)-1)) {
      subM = env$M[i:(i-1+nrow(pattern)), j:(j-1+ncol(pattern))];
      if (identical(subM,pattern)) {
        subM[which(subM == 1)] = colNum
        env$M[i:(i-1+nrow(pattern)), j:(j-1+ncol(pattern))] = subM;
      }
    }
  }
}

#' @titel matrix equality check
#'
#' @description checks if two matrices are equal : are both of them matrices and have the same dimension
#' 
#' @author Leila Feddoul
#' 
#' @param x the first Matrix.
#' @param y the second Matrix.
#'
#' @return true if the two matrices are equal otherwise false  .
#'
#' @examples
#' matequal(A,B)
#'
matequal <- function(x, y)
  is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)


#' @titel find out pattern matching   
#'
#' @description Searches for given pattern in the cell matrix and colors it if found ( Alternative implementation for findPatternMatch function)
#' 
#' @author Leila Feddoul
#' 
#' @param env Environment containing the cell matrix an color mapping
#' @param mask 2D matrix of a pattern to be recognized 
#' @param col a color number which is set for alive cells of a found pattern
#' 
findPatternMatch_2<-function(env, mask, col)
{
  maskSize<-dim(mask)
  nrM<-nrow(env$M)
  ncM<-ncol(env$M)
  for(i in 1:(nrM-maskSize[1]))
  {
    for(j in 1:(ncM-maskSize[1]))
    {
      N<-env$M[i:(i+(maskSize[1]-1)),j:(j+(maskSize[1]-1))]
      if(matequal(mask,N))
      {
        N[which(N == 1)] <- col
        env$M[i:(i+(maskSize[1]-1)),j:(j+(maskSize[1]-1))]<-N
      }
    }
  }
}

#' @title load masks that are saved in a txt-file
#' 
#' @description 
#' Reads a .txt-file containing matrix patterns that ought to be recognized
#' and saves all patterns in a list of matrices. The object "creature" 
#' is created wich contains said list and its assigned colour. 
#' 
#' @author Selina Müller
#'
#' @param path file path containing the pattern as a .txt-file
#'
#' @return an object containing all patterns and assigned colour of given file path
#' 
#' @example 
#' load.masks("C:/Users/me/Documents/ConwayPatterns/glider.txt")
#' 
load.masks <- function(path) {
  creature <- {}
  patterns <- {}
  c <- as.integer(scan(file=path, skip =1, nlines = 1, what = "numerical", quiet = TRUE))
  r <- as.integer(scan(file=path, skip =2, nlines = 1, what = "numerical", quiet = TRUE))
  dat = read.table(file = path, skip = 3, header = FALSE, comment.char = "")
  con <- file(path, "r")
  line.number <- 1
  while(TRUE){
    line <- readLines(con, n=1)
    if(length(line) == 0){
      break
    }
    if(line.number > 3){
      pattern <- matrix(scan(text = line, what = "numeric", quiet = TRUE), nrow=r, ncol = c, byrow = TRUE)
      class(pattern) <- "numeric"
      patterns <- append(patterns, list(pattern))
    }
    line.number <- line.number + 1
  }
  close(con)
  colour <- scan(file=path, nlines =1, comment.char = "", what="character", quiet = TRUE)
  creature$color <- colour
  creature$patterns <- patterns
  return(creature)
}

#' @title Create object that holds all patterns as lists
#' 
#' @description 
#' Iterates through list of file paths containing all patterns and saving them in an object called creatures.
#'
#' @author Selina Müller
#'
#' @param paths list of file paths, each containing patterns as a .txt-file
#'
#' @return a vector containing objects holding all patterns and assigned colour of each file in paths
#' 
#' @example 
#' getAllPatterns("C:/Users/me/Documents/ConwayPatterns/glider.txt", "C:/Users/me/Documents/ConwayPatterns/still.txt")
#' 
getAllPatterns <- function(paths) {
  # für alle paths creatures auslesen und als vector von objekten zurückgeben
  creatures <- list()
  for (i in 1:length(paths)) {
    creatures[[length(creatures) +1]] <- addRotatedPatterns(load.masks(paths[i]))
  }
  return(creatures);
}

#' Recognizes known patterns for a specific iteration of a game.
#' 
#' @author Anastasia Aftakhova
#' 
#' @param env Environment with specified game matrix and
#' creature object list (= patterns or figures to recognize)
#'
detectPatterns <- function(env) {
  for(i in env$creatures){
    col = env$colorMapping$num[which(env$colorMapping$col == i$color)];
    for(p in i$patterns){
      findPatternMatch(env, p, col);
    }
  }
}


#' Automatically creates a color mapping for the environment 
#' with the given creature list
#'
#' @author Anastasia Aftakhova
#'
#' @param env Environment with creature object list
#' 
createColorMapping <- function(env) {
  numbers <- c(0,1)
  colors <- c('white', 'black')
  
  for (i in env$creatures) {
    if (sum(colors == i$color) == 0) {
      numbers <- union(numbers, numbers[length(numbers)]+1)
      colors <- union(colors, i$color)    
    }
  }
  env$colorMapping = data.frame(num=numbers, col=colors, stringsAsFactors = FALSE)
}


#' Rotates the given matrix 90 degrees on the right
#'
#' @author Anastasia Aftakhova
#'
#' @param pattern input 2D matrix
#'
#' @return a rotated input matrix
#' 
rotatePattern90degRight <- function(pattern) {
  rotatedPattern <- t(pattern[nrow(pattern):1,])
  return(rotatedPattern)
}


#' Adjusts creature patterns list with the rotated patterns 
#' in order to detect all rotations of patterns
#'
#' @author Anastasia Aftakhova
#'
#' @param creature a list object containing color and a list 
#' of 2D matrices with patterns for this "creature"
#'
#' @return modified creature object
#' 
addRotatedPatterns <- function(creature) {
  patterns <- creature$patterns
  for (p in creature$patterns) {
    pat <- p
    for( i in 1:3) {
      pat <- rotatePattern90degRight(pat)
      patternExists <- FALSE
      for (existingPat in patterns) {
        if (identical(pat, existingPat)) {
          patternExists <- TRUE
          break()
        }
      }
      if (!patternExists) {
        patterns <- append(patterns, list(pat))  
      }
    }
  }
  creature$patterns <- patterns
  return(creature)
}

#' @title main game-loop
#' 
#' @description 
#' Main game loop. Manages creation os start matrix and its updating, as well as pattern detection 
#' and visualization of matrix with each iteration
#' 
#' @author Anastasia Aftakhova, Leila Feddoul, Selina Müller
#'
#' @param iter_number number of iterations of the game (life cycles) 
#' @param matrix_or_size containing either a start matrix for the game of life or the size the start matrix should have
#' @param save_path path where matrices are saved as images
#' @param paths list of file paths, each containing patterns as a .txt-file
#'
#' @example 
#' starteSpiel(iter_number = 10, matrix_or_size = 100, save_path = "C:/Users/me/Documents/ConwayPatterns", paths = c("C:/Users/me/Documents/ConwayPatterns/glider.txt", "C:/Users/me/Documents/ConwayPatterns/still.txt"))
#' 
starteSpiel <- function(iter_number, matrix_or_size, save_path, paths) {
  gameenv <- new.env()
  
  if (is.matrix(matrix_or_size)) {
    gameenv$M <- matrix_or_size
  } else {
    gameenv$M <- createMatrix(matrix_or_size)  
  }
  
  gameenv$creatures <- getAllPatterns(paths)
  gameenv$colorMapping <- createColorMapping(gameenv)
  
  for (i in c(1:iter_number)) { # check if for-Loop correct
    M = detectPatterns(gameenv)
    visualise(gameenv, sprintf('Iteration %d', i));
    #Sys.sleep(1);
    
    # save M if needed
    if (i%%1 == 0) {
      save(save_path, i)
    }
    
    # setzte M zurück (= entfärben)
    decolorM(gameenv);
    
    # compute next live iteration
    computeAll(gameenv)
  }
}


#-------- SET UP -------

# save_path = '/home/selina/Documents/github/tmp' 
# save_path = '/home/te74zej/Dokumente/M.Sc./SS2017/Programmierung  mit R/Projekt/game_test'
# setwd('/home/te74zej/Dokumente/M.Sc./SS2017/Programmierung  mit R/Projekt/ConwayGame')
# save_path = 'C:/Users/aftak/Documents/FSU/M.Sc/SS2017/Programmierung mit R/conway_snapshots'
setwd('/home/te74zej/Dokumente/M.Sc./SS2017/Programmierung  mit R/uebungen')
save_path = '/home/te74zej/Dokumente/M.Sc./SS2017/Programmierung  mit R/uebungen/game_snapshots'

#--------  TEST -------

# initialize data paths for known patterns
path.blinker <- 'color and pattern/blinker.txt'
path.block <- 'color and pattern/block.txt'
path.glider <- 'color and pattern/glider.txt'
path.fourglidertub <- 'color and pattern/four_glider_tub.txt'
path.tub <- 'color and pattern/tub.txt'
path.test <- 'color and pattern/test.txt'
paths = array(data=c(path.blinker, path.block, path.glider, path.tub)) #,path.fourglidertub))

# start the game
starteSpiel(100, 100, save_path, paths)
