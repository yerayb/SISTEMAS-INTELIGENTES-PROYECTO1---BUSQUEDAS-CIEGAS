uniform.cost.search = function(problem,
                               max_iterations = 1000, 
                               count_print = 100, 
                               trace = FALSE,
                               graph_search = FALSE) {
  
  name_method      <- paste0("Uniform Cost Search", ifelse(graph_search, " + GS", ""))
  state_initial    <- problem$state_initial
  state_final      <- problem$state_final
  actions_possible <- problem$actions_possible
  
  # Get Start time
  start_time       <- Sys.time()
  
  node <- list(parent = c(),
               state = state_initial,
               actions = c(),
               depth = 0,
               cost = 0,
               evaluation = 0)
  frontier <- list(node)
  
  if (graph_search) {
    expanded_nodes <- list()     
  }
  
  count <- 1
  end_reason <- 0
  
  #Initialization of information for further analysis
  report <- data.frame(iteration = numeric(),
                       nodes_frontier = numeric(),
                       depth_of_expanded = numeric(),
                       nodes_added_frontier = numeric())
  
  #Perform "max_iterations" iterations of the expansion process of the first node in the frontier list
  while (count <= max_iterations) {
    # Print a search trace for each "count_print" iteration
    if (count %% count_print == 0) {
      print(paste0("Count: ", count, ", Nodes in the frontier: ", length(frontier)), quote = F)
    }
    
    #If the frontier list remains empty, the algorithm ends without finding a solution
    if (length(frontier) == 0) {
      end_reason <- "Frontier"
      break
    }
    
    #Remove the first node of the frontier list
    node_first <- frontier[[1]]
    frontier[[1]] = NULL
    
    #The graph search stores the expanded states to check for repeated states
    if (graph_search) {
      expanded_nodes <- append(expanded_nodes, list(node_first))
    }
    
    #If "trace" is on, the information of each node extracted from frontier is displayed
    if (trace) {
      print(" ", quote = F)
      print("------------------------------", quote = F)
      print("State extracted from frontier:", quote = F)
      to.string(node_first$state)
      print(paste0("(depth=", node_first$depth, ", cost=", node_first$depth, ")"), quote = F)
    }
    
    #If the node extracted from frontier contains the final state
    #the algorithm ends because the solution has be founded
    if (is.final.state(node_first$state, state_final)) {
      end_reason <- "Solution"
      break
    }
    
    #The node extracted from frontier is expanded and its successor nodes are inserted into frontier
    sucessor_nodes <- expand.node(node_first, actions_possible, problem)
    
    if (length(sucessor_nodes) > 0) {
      #Tree Search implementation
      if (!graph_search) {
        
        #NOTE: Successor nodes are added at the back of the list
        frontier <- c(frontier, sucessor_nodes)
        
        nodes_added_frontier <- length(sucessor_nodes)
        
        #If "trace" is on, the information of each new node is displayed
        if (trace) {
          for (i in 1:length(sucessor_nodes)) {
            print(paste0("State added to frontier: - (depth=", sucessor_nodes[[i]]$depth, ", cost=", sucessor_nodes[[i]]$depth, ")"), quote = F)
            to.string(sucessor_nodes[[i]]$state)
          }
        }
        #Graph Search implementation
      } else {
        nodes_added_frontier = 0
        
        for (i in 1:length(sucessor_nodes)) {
          sucessor_node <- sucessor_nodes[[i]]
          #Check if the new node is frontier list or in the list of expanded states
          if (!any(sapply(frontier, function (x) identical(x$state, sucessor_node$state)))) {
            if (!any(sapply(expanded_nodes, function (x) identical(x$state, sucessor_node$state)))) {
              
              #NOTE: Successor node id added at the back of the list
              frontier <- c(frontier, list(sucessor_node))
              
              nodes_added_frontier <- nodes_added_frontier + 1
              #If "trace" is on, the information of each new node is displayed
              if (trace) {
                print(paste0("State added to frontier: - (depth=", sucessor_node$depth, ", cost=", sucessor_node$depth, ")"), quote = F)
                to.string(sucessor_node$state)
              }
            }
          }
        }
      }
      
      #Frontier list is ordered according to COST
      frontier = frontier[order(sapply(frontier, function (x) x$cost))]
    } # length(sucessor_nodes) > 0
    
    if (trace) {
      print(paste0("Total states in the frontier: ", length(frontier)),quote = F)
    }
    
    #Add of information for further analysis
    report <- rbind(report,
                    data.frame(iteration = count,
                               nodes_frontier = length(frontier),
                               depth_of_expanded = node_first$depth,
                               nodes_added_frontier = nodes_added_frontier))
    count <- count + 1
  }
  
  # Get runtime
  end_time <- Sys.time()
  
  result <- list()
  result$name    <- name_method
  result$runtime <- end_time - start_time
  
  # Show the obtained (or not) final solution
  if (end_reason == "Solution") {
    print("Solution found!!", quote=F)
    to.string(node_first$state)
    print("Executed Actions: ", quote=F)
    print(node_first$actions, quote=F)
    result$state_final = node_first
  } else {
    if (end_reason == "Frontier") {
      print("Frontier is empty. No Solution found", quote=F)
    } else {
      print("Maximum Number of iterations reached. No Solution found", quote = F)
    }
    
    result$state_final <- NA
  }
  
  result$report <- report
  
  return(result)
}