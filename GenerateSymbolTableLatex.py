from tabulate import tabulate
def name( obj ):
    return type(obj).__name__ 



class SymbolTree():

    def __init__(self):


        self.node_dict = {}
        self.root = None

        # Intrinsic Procedures in Fortran

        intrinsic_proc = { 'dot_product' : 'Function' , 'matmul' : 'Function' , 'mod' : 'Function' , 'modulo' : 'Function' , 'int' : 'Function' , 'real' : 'Function' , 'max' : 'Function' , 'min' : 'Function' , 'product' : 'Function' , 'random_number' : 'Subroutine' , 'random_seed' : 'Subroutine' , 'shape' : 'Function' , 'size' : 'Function' , 'sum' : 'Function' , 'not' : 'Function' , 'abs' : 'Function' , 'allocated' : 'Function' , 'any' : 'Function'  , 'ceiling' : 'Function', 'dble' : 'Function' , 'epsilon' : 'Function' , 'exp' : 'Function' , 'exponent' : 'Function' , 'floor' : 'Function' , 'kind' : 'Function' , 'log' : 'Function' , 'log10' : 'Function' , 'sqrt' : 'Function' , 'transpose' : 'Function' , 'norm2' : 'Function'  }


        self.intrinsic_proc = SymbolTable()

        # Adding intrinsic procedures in symbol table

        for proc in intrinsic_proc:
            self.intrinsic_proc.AddSymbol((proc,intrinsic_proc[proc]))

    # Adds new scope to the Symbol tree
    def AddNode(self,node):
        self.node_dict[node.scope] = node


    def GST_util( self , head , parent ):

        ntype = type(head).__name__
        
        if( ntype == 'Execution_Program' ) : 

            program_units = head.program_units      
            table = SymbolTable()
            children = {}
          
            for program_unit in program_units:
                title = program_unit.title

                # Adding program unit to symbol table
                table.AddSymbol(program_unit)

                children[title] = self.GST_util(program_unit,'global')

            curr_node = Node('global',table,parent,children)
            self.AddNode(curr_node)
            return curr_node



        elif( ntype == 'Main' or ntype == 'Function' or ntype == 'Subroutine' or ntype == 'Module' ) :
      
            specs = head.specs
            table = SymbolTable()
            children = {}
            internal_subprograms = head.internal_subprograms


            for declaration in specs.dec_stmts:
                # Adding variable declaration to symbol table of scope of program unit
                table.AddSymbol(declaration)


            for subprogram in internal_subprograms:
                title = subprogram.title
                table.AddSymbol(subprogram)
                children[title] = self.GST_util(subprogram,head.title)

            curr_node = Node(head.title,table,parent,children)
            self.AddNode(curr_node)
            return curr_node

    def GenerateSymbolTable( self , head , parent ):
        self.root = self.GST_util(head,parent)

    def Lkp_util(self,curr_node,Id):
        
        # Searches id in the current scope 
        # If found then returns the reference else searches in the parent scope
 
        table = curr_node.table
        lkp = table.Lookup(Id)

        if( lkp is not None ): return lkp
 
        if( curr_node.parent is None ): 
            # Id not found even in root scope therefore search in intrinsic procedures
            lkp = self.intrinsic_proc.Lookup(Id)
            if ( lkp is not None ): return lkp
            return None
        
        return self.Lkp_util(self.node_dict[curr_node.parent],Id)
        

    def Lookup(self,scope,Id):
        curr_node = None
        if( scope in self.node_dict ):
            curr_node = self.node_dict[scope]
        else:
            return None

        return self.Lkp_util(curr_node,Id)

    def PrintSymbolTable( self, stfile ,root = None ):

        # In order printing of symbol tree

        if ( root is None ):
            root = self.root 
	
        stfile.write('\\begin{center}\n')	
        stfile.write('\\begin{longtable}{|p{3.5cm}|p{1.5cm}|p{1.5cm}|p{1.5cm}|p{1cm}|p{2cm}|p{4cm}| }\n')		
        stfile.write('\hline\n')
        if(root.scope.find("_") != -1):
                idx = root.scope.index("_")
                root.scope = root.scope[:idx] + "\\" + root.scope[idx:]
        if ( root.scope == 'global' ):
                stfile.write('\multicolumn{7}{|c|}{\\textbf{Scope : }  \\textbf{\\textit{' + root.scope + '}}} \\\ \n')
        else :
                if(self.node_dict[root.parent].table.Lookup(root.scope) == None):
                        stfile.write('\multicolumn{7}{|c|}{\\textbf{Scope} -  \\textbf{\\textit{' + root.scope + '}}} \\\ \n')
                else:
                        stfile.write('\multicolumn{7}{|c|}{\\textbf{Scope : \qquad}  \\textbf{\\textit{' + root.scope +\
                       ' - }' + self.node_dict[root.parent].table.Lookup(root.scope)['type'] + '}}\\\ \n')
        stfile.write('\hline\n')	
          
        root.table.PrintTable(stfile)
        children = root.children

        stfile.write('\end{longtable}\n')	
        stfile.write('\end{center}\n')	
        stfile.write('\n \\vspace{1cm}\n\n')

        for title,child in children.items() :
            
            self.PrintSymbolTable(stfile,child)

            
class SymbolTable():

    def __init__(self):
        self.table = {}
        # Columns in symbol table
        self.headers = [ 'Id' , 'type' , 'precision' , 'Dtype' , 'ndim' , 'Attributes' , 'ref' ]

    def CreateRow(self):
        # create empty row
        row = {}
        for col in self.headers:
            row[col] = None

        return row
        
    
    def AddSymbol(self,node):

        # Adding symbols to symbol table

        ntype = type(node).__name__
        table = self.table

        if ( ntype == 'tuple' ):
            Id = node[0]
            node_type = node[1]

            table[Id] = self.CreateRow()
            table[Id]['Id'] = Id
            table[Id]['type'] = node_type
            table[Id]['ref'] = node
            
        elif ( ntype == 'Main' ):

            title = node.title
            table[title] = self.CreateRow()
            
            table[title]['Id'] = title
            table[title]['type'] = 'Main'
            table[title]['ref'] = node

            
        elif ( ntype == 'Function' ):
          
            title = node.title
            result = node.result

            table[title] = self.CreateRow()
            
            table[title]['Id'] = title   
            table[title]['type'] = 'Function'
            table[title]['Attributes'] = 'result(' + result + ')'  
            table[title]['ref'] = node


        elif ( ntype == 'Subroutine' ):

            title = node.title            
            table[title] = self.CreateRow()

            
            table[title]['Id'] = title
            table[title]['type'] = 'Subroutine'
            table[title]['ref'] = node

        elif ( ntype == 'Module' ):

            title = node.title
            table[title] = self.CreateRow()
                        
            table[title]['Id'] = title
            table[title]['type'] = 'Module'
            table[title]['ref'] = node

        elif ( ntype == 'Declaration' ):


            Dtype = repr(node.Type)
            
            Attributes = node.Attributes

            Attribute_lis = [ repr(attr) for attr in Attributes ]
            Var_lis = node.Var_lis
 
            for var in Var_lis:

                Id = var.name
                vtype = type(var).__name__
                table[Id] = self.CreateRow()
                table[Id]['Id'] = Id
                table[Id]['Dtype'] = Dtype
                table[Id]['type' ] = vtype
                if ( node.Type.size ):
                    table[Id]['precision'] = node.Type.size
                elif ( node.Type.name == 'real' or node.Type.name == 'integer' ):
                    # default precision if precision not provided
                    table[Id]['precision'] = '4'
                if ( vtype == 'Array' ):
                    table[Id]['ndim'] = len(var.dim_lis)
                table[Id]['Attributes'] = Attribute_lis
                table[Id]['ref'] = node
                
        elif ( ntype == 'Interface module' ):

            title = node.title
            table[title] = self.CreateRow()
            table[title]['Id'] = title
            table[title]['type'] = 'Interface'
            table[title]['ref'] = node
                        

        elif ( ntype == 'Interface_argument' ):
            
            title = node.title
            if ( title is not None ):
                table[title] = self.CreateRow()
                table[title]['Id'] = title
                table[title]['type'] = 'Interface'
                table[title]['ref'] = node

            for proc in node.proc_lis :
                self.AddSymbol(proc)
            
        
    def Lookup(self,Id):
        if(Id in self.table):
            return self.table[Id]
        else:
            return None
            
    def check(string, sub_str):
        if (string.find(sub_str) == -1):
                print("NO")
        else:
                print("YES")


    def PrintTable(self,stfile):
        table = self.table
        table_lis = []

        for key in table:
            table_lis.append(list(table[key].values()))

        for i in range(len(self.headers)-1):
                stfile.write('\\textbf{'+self.headers[i]+'} & ')
        stfile.write('\\textbf{'+self.headers[-1]+'} \\\\\hline\n\n')
        
        for list_lis in table_lis:
                for elem in list_lis[:-1]:
                       if elem:
#                                stfile.write('%s & '% elem)
                                if (isinstance(elem, str)):
                                        if(elem.find("_") == -1):
                                                stfile.write('%s & '% elem)
                                        else:
                                                idx = elem.index("_")
                                                elem = elem[:idx] + "\\" + elem[idx:]
                                                stfile.write('%s & '% elem)
                                else:
                                        stfile.write('%s & '% elem)

                       else:
                                stfile.write(' & ')         
                stfile.write('%s \\\\\hline\n\n'% list_lis[-1])

#        print(table_lis)
#        print('\n\n\n')


    
# Types - Main , Subroutine , Function , Scalar , Array 
  
class Node():

    def __init__(self,scope,table,parent,children):
        self.scope = scope
        self.table = table
        self.parent = parent 
        self.children = children
