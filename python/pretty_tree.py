#!/usr/bin/env python3
import argparse

parser = argparse.ArgumentParser()
parser.add_argument('tree', type=str)
args = parser.parse_args()

def format_tree(string):
    isSubtree = 0
    
    fullTree = []
    for index, char in enumerate(string):
        if char == "(":
            if isSubtree == 0: 
                firstOp = index
            isSubtree += 1

        elif char == ")":
            isSubtree -= 1

        if isSubtree == 0:
            if char == ")":
                fullTree.append((string[firstOp+1], format_tree(string[firstOp+2:index])))
                
            else:
                fullTree.append( (char, []) )
    
    return fullTree

formattedTree = format_tree(args.tree)

def print_tree(tree, 
               nParent = 0,     # What number sibling is the current node's parent
               nParentSibl = 1, # How many siblings does this node's parent have
               branch="", baseExtend="|  ", blankExtend="   ", endExtend="+--"):
    
    (root, children) = tree
    
    print(branch, root, sep="")
    
    nSibl = len(children)
    
    for n, child in enumerate(children):
        if branch == "":
            print_tree(child, n, nSibl, branch + endExtend)
        
        elif nParent == nParentSibl-1:
            print_tree(child, n, nSibl, branch[:-3] + blankExtend + branch[-3:])
            
        else:
            print_tree(child, n, nSibl, branch[:-3] + baseExtend + branch[-3:])
    
print_tree(*formattedTree)

