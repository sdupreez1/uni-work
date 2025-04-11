#!/usr/bin/env python3

import argparse

# Get tree string as argument from CLI
parser = argparse.ArgumentParser()
parser.add_argument('tree', type=str)
args = parser.parse_args()

treeStr = args.tree

def find_all_char(string, char, posList=[]):
    
    # Finds all instances of char in string
    
    # List of indexes of all currently known instances of char in string
    positions = posList.copy()
    try:
        # If there are no known instances yet...
        if len(positions) == 0:
            # Add the lowest index instance to positions
            positions.append(string.index(char))
            
            # Next search begins from 1 character after this detected instance
            newStr = string[positions[-1]+1:]
       
        # If there are known instances...
        else:
            # Add the lowest index instance in the cut string to positions
            positions.append(string.index(char))
            
            # Start next search from 1 char after this instance
            newStr = string[positions[-1]+1:]
            
            # Index detected here is wrt newString, so adjust value to be 
            # wrt the original string
            positions[-1] += positions[-2] + 1
        
        # Perform next search on cut string
        return find_all_char(newStr, char, positions)
    
    # If no instances of char can be found in string, then original string either
    # contained no instances, or has been cut to the point where it no longer does
    except ValueError:
        return positions

def format_tree(string):
    
    # Turns a tree string eg '(AB(CD)(EF(GH))I)' into (node, [children]) format
    
    # Remove parent from string
    parent = string[1]    
    noParent = string[2:]
    
    # Find where all brackets are in string
    openBrackets = find_all_char(noParent, "(")
    closeBrackets = find_all_char(noParent, ")")
    
    # If there are no children, there are no brackets in any substrings
    if openBrackets == [] and closeBrackets == []:
        return []
    
    # List all children and any sub-trees they are attatched to
    children = []
    
    # All sub-tree children are contained in brackets '(...)'
    for i, op in enumerate(openBrackets):
        for j, cl in enumerate(closeBrackets):
            
            # Look at a bracket bound substring
            s = noParent[op:cl+1]
            sOpenBrackets = find_all_char(s, "(")
            sCloseBrackets = find_all_char(s, ")")
            
            # A valid child sub-tree has each ( appear before a )
            s_valid = [sOpenBrackets[n] < sCloseBrackets[n] \
                       for n in range(min(len(sOpenBrackets), len(sCloseBrackets)))]
            
            # Find all the highest-generation '(...)' substrings
            if len(find_all_char(s, "(")) == len(find_all_char(s, ")")) \
            and op < cl \
            and all(s_valid): # This prevents finds of eg 'ABC)(DEF' rather than '(1234)'
                
                # Add this sub-tree to children
                children.append(s)
                
                # Start a new search after the detected '(...)' rather than 
                # finding any more '(...)' within this one
                    # Next op to use will be the lowest that is larger than the
                    # cl that gave a valid subTree
                openBrackets = [op for op in openBrackets if op > cl]
                closeBrackets = closeBrackets[j+1:]
                
                # If openBrackets is empty, op stays as 0
                # Otherwise, start from new search start pos
                if openBrackets != []:
                    op = openBrackets[0]

    # Remaining characters in noParent are siblings of these child sub-trees
    siblRemainder = noParent
    for child in children:
        siblRemainder = siblRemainder.replace(child[:-1], "")
            # [:-1] here means that siblRemainder has ')'s which represent which
            # parent in the above generation the characters preceding the ')'
            # are children of [see below]
    
    # '(A123(BC...)DEF(HIJ)Z)' --> ['(BC...)', '(HIJ)' '123)DEF)Z'] 
    
    # Brackets in final entry of list means '123' comes before (BC...), 
    # 'DEF' comes after (BC...) and before (HIJ), and Z comes after (HIJ)
    
    # Add remaining siblings to children
    children.append(siblRemainder)
    
    # Put siblRemainder nodes into children in correct order
    
    # -posToAdd gives position (from the right) to add siblings
    posToAdd = len(children)
    while siblRemainder != "":
        # Find first instance of ')' in siblRemainder
        cl = siblRemainder.find(")")
        
        # Everything preceding that ')' is a child to add to children in 
        # (node, [children]) format with no children
        toAdd = [(node, []) for node in siblRemainder[:cl]]
        
        # Add toAdd in correct position in children
        preAdded = children[:-posToAdd]
        postAdded = children[-posToAdd:]
        children = preAdded + toAdd + postAdded
        
        # Update siblRemainder by removing now-added siblings
        siblRemainder = siblRemainder[cl+1:]
        posToAdd -= 1
    
    # Remove siblRemainders string from children
    children = children[:-1]
    
    # Put tree into (node, [children]) format
    tree = (parent, children)
    
    # Format all children correctly
    for i, child in enumerate(tree[1]):
        # The only entries that need to be formatted (ie the added siblings) 
        # are still strings
        if type(child) == str: 
            tree[1][i] = format_tree(child)
    
    return tree

formattedTree = format_tree(treeStr)

def print_tree(tree, 
               nParent = 0,   # What number sibling is the current node's parent
               nParentSibl=1, # How many siblings does this nodes' parent have
               branch="", baseExtend="|  ", blankExtend="   ", endExtend="+--"):
    
    # Split tree into current highest node and its children
    (node, children) = tree
    
    # Print current node with all branching relevant to its position in the tree
    print(branch, node, sep="")
    
    # How many siblings does this node's children have
    nSibl = len(children)
    
    # Print each child's sub-tree
    for n, child in enumerate(children):
        # If only first node has been printed...
        if branch == "":
            # Print next line with '+--' extension
            print_tree(child, n, nSibl, branch + endExtend)
        
        # If parent is last sibling of their generation...
        elif nParent == nParentSibl-1:
            # Parent will not be connected to any other nodes, so print next
            # line with no "|" connector, just blank extension
            print_tree(child, n, nSibl, branch[:-3] + blankExtend + branch[-3:])
            
        # In general...
        else:
            # Current node is a sibling of another furhter down, displayed by
            # printing next line with a "|" connector between parent and its sibling
            print_tree(child, n, nSibl, branch[:-3] + baseExtend + branch[-3:])
    
print_tree(formattedTree)

