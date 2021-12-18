from lexer import Lexer
from parser import Parser
import sys,os
import _pickle as pickle
from GenerateSymbolTableLatex import *
from ast import *

'''
Convert Fortran AST to Go Source code
>> Another approach could be Fortran AST >> Go AST >> Go source code

Incomplete requires addition features for converting fortran interface , module , internal subprograms, static variable , array constructor etc

Go does not support matrix vector operations . 


May require additional manual editing in output go code to make code suitable for compiling

'''

# Go import statements
go_header = 'import "fmt"\nimport "math"\nimport "math/rand"'

def space(level):
    s = ''
    for i in range(level):
        s = s + '    '
    return s

# Utility function to convert expr to expr-num

def SubtractExpr( expr , num ):

    '''
      Fortran uses 1 based index and go uses 0-based indexing therefore utility function to convert 
      expr to expr - num 
   
    '''

    expr_type = type(expr).__name__

    if ( expr_type == 'Number' ):

        val_up = int(expr.value) - num

        if ( val_up < 0 ):
            expr.value = str(num - int(expr.value))
            return UnarySub(expr)
        else :
            expr.value = str(int(expr.value) - num)

        return expr
    elif ( expr_type == 'Add' ):
        return Sub(expr,Number(str(num)))
    elif ( expr_type == 'Sub' ):
        return Sub(expr,Number(str(num)))
    elif ( expr_type == 'Mul' ):
        return Sub(expr,Number(str(num)))
    elif ( expr_type == 'Div' ):
        return Sub(expr,Number(str(num)))
    elif ( expr_type == 'UnaryAdd' ):

        operand = expr.operand
        if ( type(operand).__name__ == 'Number' ):
            val_up = int(operand.value)-num
            if ( val_up < 0 ):
                operand.value = str(-int(operand.value)+num)
                return UnarySub(operand)
            else :
                operand.value = str(int(operand.value)-num)
                return (operand)
                
        return Sum(operand,num)

    elif ( expr_type == 'UnarySub' ):
        operand = expr.operand
        if ( type(operand).__name__ == 'Number' ):
            val_up = int(operand.value)+num
            if ( val_up < 0 ):
                operand.value = str(int(operand.value)+num)
                return operand
            else :
                operand.value = str(int(operand.value)+num)
                return UnarySub(operand)
                
        return Sub(operand,num)
            
    elif ( expr_type == 'Parenthesis' ):
        return Sub(expr,Number(str(num)))
    elif ( expr_type == 'ArrayRef' ):
        return Sub(expr,Number(str(num)))
    elif ( expr_type == 'VariableRef' ):
        return Sub(expr,Number(str(num)))
    elif ( expr_type == 'Real' ):
        return Sub(expr,Number(str(num)))
    elif ( expr_type == 'Integer' ):
        return Sub(expr,Number(str(num)))
    elif ( expr_type == 'Function_call' ):
        return Sub(expr,Number(str(num)))






def Unparse(node,stree,curr_scope,level,code_file):

    '''
        node - Current Node of AST
        stree - Symbol tree used to get additional information from ID
        curr_scope - Current scope 
        level - Maintain Nesting level for writing code in proper indentation
        code_file - Output file for source code

    '''

    ntype = type(node).__name__


    if ( ntype == 'Execution_Program' ):

        # Go file Header
        code_file.write('package main\n\n')
        code_file.write(go_header+'\n')

       
        program_units = node.program_units
        for program_unit in program_units:
            Unparse(program_unit,stree,program_unit.title,level,code_file)


    elif ( ntype == 'Main' ):

        text = space(level)+'func main() {'+'\n'
        code_file.write(text)

        # Import statements
        use_stmts = (node.specs).use_stmts 
        

        Unparse(node.specs,stree,curr_scope,level+1,code_file)

        statements = node.statements

        for statement in statements:
            Unparse(statement,stree,curr_scope,level+1,code_file)

        
        text = '\n' + space(level) + '}' + '\n\n'
        
        code_file.write(text)
        

    elif ( ntype == 'Function' ):

        title = node.title
        args = node.args
        specs = node.specs
        statements = node.statements
        result = node.result
        internal_subprograms = node.internal_subprograms

        
        text = space(level)+'func '+title+' ( ' 
        code_file.write(text)


        if ( len(args) != 0 ) :


            code_file.write(args[0].name)
            code_file.write(' ')          

            # Looking up argument in symbol tree
            lkp = stree.Lookup(curr_scope,args[0].name)

            # If argument is array
            if ( lkp['type'] == 'Array' ):
                code_file.write('[]'*lkp['ndim'])       
 
            # Go does not support Scalar argument pass by reference therefore using pointer 
            if ( (('intent(out)' in lkp['Attributes']) or ( 'intent(inout)' in lkp['Attributes'] )) and lkp['type'] == 'Scalar' ):
                code_file.write('*')                
 
            # Unparsing type of argument
            Unparse(lkp['ref'].Type,stree,curr_scope,level,code_file)

            for i in range(1,len(args)):
                code_file.write(',')

                code_file.write(args[i].name)
                code_file.write(' ')
                # Looking up argument in symbol tree
                lkp = stree.Lookup(curr_scope,args[i].name)
                # If argument is array
                if ( lkp['type'] == 'Array' ):
                    code_file.write('[]'*lkp['ndim'])   
                # Go does not support Scalar argument pass by reference therefore using pointer
                if ( (('intent(out)' in lkp['Attributes']) or ( 'intent(inout)' in lkp['Attributes'] )) and lkp['type'] == 'Scalar' ):
                    code_file.write('*')
                # Unparsing type of argument
                Unparse(lkp['ref'].Type,stree,curr_scope,level,code_file)

        code_file.write(' ) ')
    
        # Function result
        if result is not None:
            code_file.write('( ')

            lkp = stree.Lookup(curr_scope,result)
            if ( lkp['type'] == 'Array' ):
                code_file.write('[]'*lkp['ndim'])      
            Unparse(lkp['ref'].Type,stree,curr_scope,level,code_file)                       
            code_file.write(' )')
           
               
        code_file.write(' {\n')
        
        Unparse(node.specs,stree,curr_scope,level+1,code_file)

        for statement in statements:
            Unparse(statement,stree,curr_scope,level+1,code_file)

        code_file.write('\n'+space(level+1)+'return '+result+'\n')
    
        text = '\n' + space(level) + '}\n\n'
        
        code_file.write(text)


        pass

    elif ( ntype == 'Subroutine' ):

        title = node.title
        args = node.args
        specs = node.specs
        statements = node.statements
        internal_subprograms = node.internal_subprograms


        text = space(level)+'func '+title+' ( ' 
        code_file.write(text)
        if ( len(args) != 0 ) :

            
            code_file.write(args[0].name)
            code_file.write(' ')     

            # Looking up argument in symbol tree     
            lkp = stree.Lookup(curr_scope,args[0].name)
            # If argument is array
            if ( lkp['type'] == 'Array' ):
                code_file.write('[]'*lkp['ndim'])   
            # Go does not support Scalar argument pass by reference therefore using pointer
            if ( (('intent(out)' in lkp['Attributes']) or ( 'intent(inout)' in lkp['Attributes'] )) and lkp['type'] == 'Scalar' ):
                code_file.write('*')                
  
            # Unparsing type of argument
            Unparse(lkp['ref'].Type,stree,curr_scope,level,code_file)

            for i in range(1,len(args)):
                code_file.write(' , ')

                code_file.write(args[i].name)
                code_file.write(' ')
                # Looking up argument in symbol tree
                lkp = stree.Lookup(curr_scope,args[i].name)
                # If argument is array
                if ( lkp['type'] == 'Array' ):
                    code_file.write('[]'*lkp['ndim'])   
                # Go does not support Scalar argument pass by reference therefore using pointer
                if ( (('intent(out)' in lkp['Attributes']) or ( 'intent(inout)' in lkp['Attributes'] )) and lkp['type'] == 'Scalar' ):
                    code_file.write('*')
                # Unparsing type of argument
                Unparse(lkp['ref'].Type,stree,curr_scope,level,code_file)

        code_file.write(' ) ')

        code_file.write(' {\n')
        
        Unparse(node.specs,stree,curr_scope,level+1,code_file)

        for statement in statements:
            Unparse(statement,stree,curr_scope,level+1,code_file)


        code_file.write('\n'+space(level+1)+'return\n')
    
        text = '\n' + space(level) + '}\n\n'
        
        code_file.write(text)

        pass

    elif ( ntype == 'Interface_module' ):
        # Requires work
        pass

    elif ( ntype == 'Module_procedure' ):
        # Requires work
        pass

    elif ( ntype == 'Interface_argument' ):
        # Requires work
        pass

    elif ( ntype == 'Module' ):
        # Requires work
        pass


    elif ( ntype == 'Specifications' ):

        dec_stmts = node.dec_stmts
     
        if ( len( dec_stmts ) != 0 ):   

            code_file.write('\n')

            for dec_stmt in dec_stmts:
                Unparse(dec_stmt,stree,curr_scope,level,code_file)        

        code_file.write('\n')



    elif ( ntype == 'If_condition' ):

        expr = node.expr
        if_stmts = node.if_stmts
        elif_stmts = node.elif_stmts
        else_stmts = node.else_stmts



        text = space(level)


        code_file.write( '\n' + text + 'if  ' )
        Unparse(node.expr,stree,curr_scope,level,code_file)
        code_file.write(' {\n\n')
    
        for statement in if_stmts:
            Unparse(statement,stree,curr_scope,level+1,code_file)

        code_file.write('\n')

        for elif_stmt in elif_stmts:
            text = space(level)
            code_file.write(text) 
            code_file.write('} else if  ')
            Unparse(elif_stmt[0],stree,curr_scope,level,code_file)
            code_file.write(' {\n\n')
                
            for statement in elif_stmt[1]:
                Unparse(statement,stree,curr_scope,level+1,code_file)
            
            code_file.write('\n')
          
        if len(else_stmts) != 0:
            code_file.write(text+'} else {\n\n')
            for statement in else_stmts:
                Unparse(statement,stree,curr_scope,level+1,code_file)

            code_file.write('\n')

   
        code_file.write(text+'}\n\n')

       
        

    elif ( ntype == 'Do_loop' ):

        text = space(level)
        
        var = node.var
        start = node.start
        end = node.end
        step = node.step 
        statements = node.statements
        
        
        code_file.write('\n' + text + 'for ')

        Unparse(node.var,stree,curr_scope,level,code_file)
        code_file.write(' = ')
        Unparse(node.start,stree,curr_scope,level,code_file)
        code_file.write(' ; ')

        Unparse(node.var,stree,curr_scope,level,code_file)
        code_file.write(' <= ')
        Unparse(node.end,stree,curr_scope,level,code_file)
        code_file.write(' ; ')
       
        Unparse(node.var,stree,curr_scope,level,code_file)
        code_file.write(' += ')
        
        if ( node.step is not None ):
            Unparse(node.step,stree,curr_scope,level,code_file)    
        else :
            code_file.write('1')
 
        code_file.write(' {\n\n')

        for statement in statements:
            Unparse(statement,stree,curr_scope,level+1,code_file)
        
        code_file.write('\n'+text)
        code_file.write('}\n\n')
        
    elif ( ntype == 'Forall_loop' ):
        # Requires work
        pass

    elif ( ntype == 'Loop_lim' ):
        # Requires work
        pass

    elif ( ntype == 'While_loop' ):
        # Requires work
        pass

    elif ( ntype == 'Subroutine_call' ):

        name = node.name
        parameters = node.parameters
        text = space(level)

        # Looking up subroutine in symbol tree
        lkp = stree.Lookup(curr_scope,name)

        # If subroutine is random_number ( intrinsic procedure )
        if ( type(lkp['ref']).__name__ == 'tuple' and name == 'random_number' ):

            lkp = stree.Lookup(curr_scope,parameters[0].name)


            if ( lkp['ndim'] == 1 ):

                code_file.write(text+'for i := 0 ; i < len('+parameters[0].name+')')
                code_file.write(' ; i++ {\n')
                code_file.write(text+'    '+parameters[0].name+'[i] = rand.Float64()\n')
                code_file.write(text+'}\n\n')
                return

            elif ( lkp['ndim'] == 2 ):

                code_file.write(text+'for i := 0 ; i < len('+parameters[0].name+')')
                code_file.write(' ; i++ {\n')

                code_file.write(text+'    '+'for j := 0 ; j < len('+parameters[0].name+'[0])')
                code_file.write(' ; j++ {\n')
   
                code_file.write(text+'        '+parameters[0].name+'[i][j] = rand.Float64()\n')
                code_file.write(text+'    '+'}\n\n')
                code_file.write(text+'}\n\n')
                return

  
        # If subroutine is some other intrinsic procedure 
        # Manually required to find suitable replacement in go
        if ( type(lkp['ref']).__name__ == 'tuple' ):
         
         
            code_file.write(node.name + '(')
        
            parameters = node.parameters
            if ( len(parameters) != 0 ):
                Unparse(parameters[0],stree,curr_scope,0,code_file)
                for i in range(1,len(parameters)):
                    code_file.write(',')
                    Unparse(parameters[i],stree,curr_scope,0,code_file)
      

            code_file.write(')') 
            return

        
        args = lkp['ref'].args
        code_file.write(text)

        code_file.write(node.name + '(')
        
        

        if ( len(parameters) != 0 ):

            # Looking up argument in symbol tree
            lkp = stree.Lookup(node.name,args[0].name)

            # If argument is scalar and pass by reference then send address of parameter
            if ( (('intent(out)' in lkp['Attributes']) or ( 'intent(inout)' in lkp['Attributes'] )) and lkp['type'] == 'Scalar' ):
                code_file.write('&')

            Unparse(parameters[0],stree,curr_scope,0,code_file)

            for i in range(1,len(parameters)):
                code_file.write(',')
                # Looking up argument in symbol tree
                lkp = stree.Lookup(node.name,args[i].name)
                # If argument is scalar and pass by reference then send address of parameter
                if ( (('intent(out)' in lkp['Attributes']) or ( 'intent(inout)' in lkp['Attributes'] )) and lkp['type'] == 'Scalar' ):
                    code_file.write('&')
                Unparse(parameters[i],stree,curr_scope,0,code_file)
      

        code_file.write(')\n')


      
    elif ( ntype == 'Function_call' ): 

        name = node.name
        parameters = node.parameters
        lkp = stree.Lookup(curr_scope,name)
        text = space(level)
       
        # If function is size ( intrinsic procedure )
        if ( type(lkp['ref']).__name__ == 'tuple' and name == 'size' ):
             
            ndim = parameters[1].value

            if ( ndim.isdigit() ):

                code_file.write('len(') 
                Unparse(parameters[0],stree,curr_scope,0,code_file)
                ndim = int(ndim)
                for i in range(ndim-1):
                    code_file.write('[0]')

                code_file.write(')')
                return

        # If function is sqrt ( intrinsic procedure )
        if ( type(lkp['ref']).__name__ == 'tuple' and name == 'sqrt' ):
         
         
            code_file.write('math.Sqrt' + '(')
        
            parameters = node.parameters
            if ( len(parameters) != 0 ):
                Unparse(parameters[0],stree,curr_scope,0,code_file)
                for i in range(1,len(parameters)):
                    code_file.write(',')
                    Unparse(parameters[i],stree,curr_scope,0,code_file)
      

            code_file.write(')') 
            return



        # If function is some other intrinsic procedure 
        # Manually required to find suitable replacement in go  
        if ( type(lkp['ref']).__name__ == 'tuple' ):
         
         
            code_file.write(node.name + '(')
        
            parameters = node.parameters
            if ( len(parameters) != 0 ):
                Unparse(parameters[0],stree,curr_scope,0,code_file)
                for i in range(1,len(parameters)):
                    code_file.write(',')
                    Unparse(parameters[i],stree,curr_scope,0,code_file)
      

            code_file.write(')') 
            return



        call_type = None
        if lkp is not None:
            call_type = lkp['type'] 
        else :
            call_type = 'Function'

        

        if call_type == 'Function':
 
            # If Function call
 
            args = lkp['ref'].args

            code_file.write(node.name + '(')
        
            parameters = node.parameters

            if ( len(parameters) != 0 ):
                # Looking up argument in symbol tree
                lkp = stree.Lookup(node.name,args[0].name)
                # If argument is scalar and pass by reference then send address of parameter
                if ( (('intent(out)' in lkp['Attributes']) or ( 'intent(inout)' in lkp['Attributes'] )) and lkp['type'] == 'Scalar' ):
                    code_file.write('&')
                Unparse(parameters[0],stree,curr_scope,0,code_file)
                for i in range(1,len(parameters)):
                    code_file.write(',')
                    # Looking up argument in symbol tree
                    lkp = stree.Lookup(node.name,args[i].name)
                    # If argument is scalar and pass by reference then send address of parameter
                    if ( (('intent(out)' in lkp['Attributes']) or ( 'intent(inout)' in lkp['Attributes'] )) and lkp['type'] == 'Scalar' ):
                        code_file.write('&')
                    Unparse(parameters[i],stree,curr_scope,0,code_file)
      

            code_file.write(')')

        elif call_type == 'Array':

            # If Array reference

            code_file.write(name)

            for i in range(len(parameters)):
                code_file.write('[')
                Unparse(SubtractExpr(parameters[i],1),stree,curr_scope,level,code_file)
                code_file.write(']')

        

       
    elif ( ntype == 'Allocate' ):

        '''
           Supports dynamic allocation 1d and 2d array only 
           Requires work for nd array  

        '''
        
        array_lis = node.array_lis
        text = space(level)
        for var in array_lis:     
            
            lkp = stree.Lookup(curr_scope,var.name)
            Type = lkp['ref'].Type
            
            if ( len(var.dim_lis) == 1 ):
                code_file.write(text+var.name+' = ')
                code_file.write('make( '+'[]')
                Unparse(Type,stree,curr_scope,level,code_file)
                code_file.write(' , ')
                Unparse(var.dim_lis[0],stree,curr_scope,level,code_file)
                code_file.write(' )\n')
            elif ( len(var.dim_lis) == 2 ):

                code_file.write(text+var.name+' = ')
                code_file.write('make( '+'[][]')
                Unparse(Type,stree,curr_scope,level,code_file)
                code_file.write(' , ')
                Unparse(var.dim_lis[0],stree,curr_scope,level,code_file)
                code_file.write(' )\n')

                code_file.write(text+'for i := 0 ; i < ')
                Unparse(var.dim_lis[0],stree,curr_scope,level,code_file)
                code_file.write(' ; i++ {\n')
                code_file.write(text+'    '+var.name+'[i] = make( []')
                Unparse(Type,stree,curr_scope,level,code_file)
                code_file.write(' , ')
                Unparse(var.dim_lis[1],stree,curr_scope,level,code_file)
                code_file.write(' )\n')
                code_file.write(text+'}\n\n')

            

    elif ( ntype == 'Deallocate' ):
        # Go has built in garbage collection system . Does not require to deallocate
        pass
    elif ( ntype == 'Number' ):
        code_file.write(node.value)
    elif ( ntype == 'Subsection' ):
        pass
    elif ( ntype == 'String' ):
        val = (node.value).strip("\"")
        val = (node.value).strip("'")
        code_file.write("\""+val+"\"")
    elif ( ntype == 'Boolean' ):
        val = node.value
        val = val.strip('.')
        code_file.write(val)
    elif ( ntype == 'ArrayConstructor' ):
        pass
    elif ( ntype == 'Add' ):

        Unparse(node.left,stree,curr_scope,level,code_file)
        code_file.write(' + ')
        Unparse(node.right,stree,curr_scope,level,code_file)

    elif ( ntype == 'Sub' ):
        
        Unparse(node.left,stree,curr_scope,level,code_file)
        code_file.write(' - ')
        Unparse(node.right,stree,curr_scope,level,code_file)

    elif ( ntype == 'Mul' ):

        Unparse(node.left,stree,curr_scope,level,code_file)
        code_file.write(' * ')
        Unparse(node.right,stree,curr_scope,level,code_file)

    elif ( ntype == 'Div' ):

        Unparse(node.left,stree,curr_scope,level,code_file)
        code_file.write(' / ')
        Unparse(node.right,stree,curr_scope,level,code_file)

    elif ( ntype == 'Power' ):

        code_file.write('math.Pow(')
        Unparse(node.left,stree,curr_scope,level,code_file)
        code_file.write(' , ')
        Unparse(node.right,stree,curr_scope,level,code_file)
        code_file.write(')')

    elif ( ntype == 'Equal' ):

        Unparse(node.left,stree,curr_scope,level,code_file)
        code_file.write(' == ')
        Unparse(node.right,stree,curr_scope,level,code_file)
        
    elif ( ntype == 'NEqual' ):

        Unparse(node.left,stree,level,code_file)
        code_file.write(' != ')
        Unparse(node.right,stree,level,code_file)


    elif ( ntype == 'Lt' ):

        Unparse(node.left,stree,curr_scope,level,code_file)
        code_file.write(' < ')
        Unparse(node.right,stree,curr_scope,level,code_file)

    elif ( ntype == 'Le' ):

        Unparse(node.left,stree,curr_scope,level,code_file)
        code_file.write(' <= ')
        Unparse(node.right,stree,curr_scope,level,code_file)

    elif ( ntype == 'Gt' ):

        Unparse(node.left,stree,curr_scope,level,code_file)
        code_file.write(' > ')
        Unparse(node.right,stree,curr_scope,level,code_file)

    elif ( ntype == 'Ge' ):

        Unparse(node.left,stree,curr_scope,level,code_file)
        code_file.write(' >= ')
        Unparse(node.right,stree,curr_scope,level,code_file)

    elif ( ntype == 'And' ):

        Unparse(node.left,stree,curr_scope,level,code_file)
        code_file.write(' && ')
        Unparse(node.right,stree,curr_scope,level,code_file)

    elif ( ntype == 'Or' ):

        Unparse(node.left,stree,curr_scope,level,code_file)
        code_file.write(' || ')
        Unparse(node.right,stree,curr_scope,level,code_file)

    elif ( ntype == 'Xor' ):

        Unparse(node.left,stree,curr_scope,level,code_file)
        code_file.write(' ^ ')
        Unparse(node.right,stree,curr_scope,level,code_file)

    elif ( ntype == 'UnaryAdd' ):

        code_file.write('+ ')
        Unparse(node.operand,stree,curr_scope,level,code_file)

    elif ( ntype == 'UnarySub' ):

        code_file.write('- ')
        Unparse(node.operand,stree,curr_scope,level,code_file)

    elif ( ntype == 'Not' ):

        code_file.write('! ')
        Unparse(node.operand,stree,curr_scope,level,code_file)

    elif ( ntype == 'Assignment' ):

        text = space(level)
        code_file.write(text)

        variable_ref = node.variable_ref
        expr = node.expr
        Unparse(variable_ref,stree,curr_scope,level,code_file)
        code_file.write(' = ')
        Unparse(expr,stree,curr_scope,level,code_file)

        code_file.write('\n')

    elif ( ntype == 'Arg_assignment' ):
        pass
    elif ( ntype == 'Declaration' ):
 
        Type = node.Type
        Attributes = node.Attributes
        Var_lis = node.Var_lis

        text = space(level)


        attr_lis = [ attr.__repr__() for attr in Attributes ]
 

        if ( "allocatable" in attr_lis ):

            # Empty go slice
             
            for var in Var_lis:
                               
                code_file.write(text+'var '+var.name+' '+'[]'*len(var.dim_lis))
                Unparse(Type,stree,curr_scope,level,code_file)
                code_file.write('\n')

            
        elif ( "intent(in)" in attr_lis ):
            # Handled already in function subroutine signature
            pass
        elif ( "intent(out)" in attr_lis ):
            # Handled already in function subroutine signature
            pass
        elif ( "intent(inout)" in attr_lis ):
            # Handled already in function subroutine signature
            pass
        elif ( "parameter" in attr_lis ):
            for var in Var_lis:
                code_file.write(text+"const "+var.name+' = ')
                Unparse(var.init_expr,stree,curr_scope,level,code_file)
                code_file.write('\n')
        else :

        
            for var in Var_lis:

                var_type = type(var).__name__

                if ( var_type == 'Scalar' ):

                    # If Scalar variable

                    code_file.write(text+'var '+var.name+' ')
                    Unparse(Type,stree,curr_scope,level,code_file)
                    if ( var.init_expr is not None ):
                        code_file.write(' = ')
                        Unparse(var.init_expr,stree,curr_scope,level,code_file)
                    code_file.write('\n')

                elif ( var_type == 'Array' ):

                    # If Array variable

                    '''
                        Supports dynamic allocation 1d and 2d array only 
                        Requires work for nd array  

                    '''

                    code_file.write(text+'var '+var.name+' '+'[]'*len(var.dim_lis))
                    Unparse(Type,stree,curr_scope,level,code_file)
                    code_file.write('\n')

                    if ( len(var.dim_lis) == 1 ):
                        code_file.write(text+var.name+' = ')
                        code_file.write('make( '+'[]')
                        Unparse(Type,stree,curr_scope,level,code_file)
                        code_file.write(' , ')
                        Unparse(var.dim_lis[0],stree,curr_scope,level,code_file)
                        code_file.write(' )\n')
                    elif ( len(var.dim_lis) == 2 ):

                        code_file.write(text+var.name+' = ')
                        code_file.write('make( '+'[][]')
                        Unparse(Type,stree,curr_scope,level,code_file)
                        code_file.write(' , ')
                        Unparse(var.dim_lis[0],stree,curr_scope,level,code_file)
                        code_file.write(' )\n')

                        code_file.write(text+'for i := 0 ; i < ')
                        Unparse(var.dim_lis[0],stree,curr_scope,level,code_file)
                        code_file.write(' ; i++ {\n')
                        code_file.write(text+'    '+var.name+'[i] = make( []')
                        Unparse(Type,stree,curr_scope,level,code_file)
                        code_file.write(' , ')
                        Unparse(var.dim_lis[1],stree,curr_scope,level,code_file)
                        code_file.write(' )\n')
                        code_file.write(text+'}\n\n')


    elif ( ntype == 'Use' ):
        # Requires work
        pass
    elif ( ntype == 'Parenthesis' ):

        code_file.write('( ')
        Unparse(node.inside,stree,curr_scope,level,code_file)
        code_file.write(' )')
      
    elif ( ntype == 'Scalar' ):
        pass
    elif ( ntype == 'Array' ):
        pass
    elif ( ntype == 'VariableType' ):

        name = node.name
        size = node.size
        
        if ( name == 'integer' ):
            code_file.write('int')
            if ( size == '4' ):
                code_file.write('32')
            elif ( size == '8' ):
                code_file.write('64')
        
        elif ( name == 'real' ):
            code_file.write('float')
            if ( size == '4' ):
                code_file.write('32')
            elif ( size == '8' ):
                code_file.write('64')
            else :
                code_file.write('64')
        elif ( name == 'character' ):
            code_file.write('string')

    elif ( ntype == 'VariableRef' ):
        lkp = stree.Lookup(curr_scope,node.name)
        # If argument is pass by reference and scalar then pointer
        if ( (('intent(out)' in lkp['Attributes']) or ( 'intent(inout)' in lkp['Attributes'] )) and lkp['type'] == 'Scalar' ):
            code_file.write('*')
        code_file.write(node.name)

    elif ( ntype == 'ArrayRef' ):

        code_file.write(node.name)

        for i in range(len(node.dim_lis)):
            code_file.write('[')
            Unparse(SubtractExpr(node.dim_lis[i],1),stree,curr_scope,level,code_file)
            code_file.write(']')

    elif ( ntype == 'Real' ):

        code_file.write('float')
        if ( node.Kind == '4' ):
            code_file.write('32')
        elif ( node.Kind == '8' ):
            code_file.write('64')
        else:
            code_file.write('64')

        code_file.write('(')
        Unparse(node.A,stree,curr_scope,level,code_file)
        code_file.write(')')


    elif ( ntype == 'Integer' ):

        code_file.write('int')
        if ( node.Kind == '4' ):
            code_file.write('32')
        elif ( node.Kind == '8' ):
            code_file.write('64')
        code_file.write('(')
        Unparse(node.A,stree,curr_scope,level,code_file)
        code_file.write(')')

    elif ( ntype == 'Intent' ):
        pass
    elif ( ntype == 'Attribute_val' ):
        pass
    elif ( ntype == 'Print' ):

        text = space(level)

        code_file.write( text + 'fmt.Println(')


        print_lis = node.print_lis

        if ( len(print_lis) != 0 ):
            Unparse(print_lis[0],stree,curr_scope,level,code_file)
            for i in range(1,len(print_lis)):
                code_file.write(',')
                Unparse(print_lis[i],stree,curr_scope,level,code_file)

        code_file.write(')\n')
        
    elif ( ntype == 'Stop' ):
        # Requires work
        pass
    elif ( ntype == 'Exit' ):
        text = space(level)
        code_file.write(text+'break'+'\n')
    elif ( ntype == 'Cycle' ):
        text = space(level)
        code_file.write(text+'continue'+'\n')
    elif ( ntype == 'Return' ):
        text = space(level)
        code_file.write(text+'return'+'\n')



    

        
        
    

    

 



