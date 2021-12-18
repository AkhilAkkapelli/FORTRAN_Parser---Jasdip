'''
Convert Fortran AST to Fortran Source code

'''
from lexer import Lexer
from parser import Parser
import sys,os
import _pickle as pickle
from GenerateSymbolTableLatex import *




def space(level):
    s = ''
    for i in range(level):
        s = s + '    '
    return s


def Unparse(node,stree,level,code_file):

    '''
        node - Current Node of AST
        stree - Symbol tree ( Dummy , Not used in this function )
        level - Maintain Nesting level for writing code in proper indentation
        code_file - Output file for source code


    '''

    ntype = type(node).__name__

    if ( ntype == 'Execution_Program' ):

        program_units = node.program_units
        for program_unit in program_units:
            Unparse(program_unit,stree,level,code_file)


    elif ( ntype == 'Main' ):

        title = node.title 
        text = space(level)+'program '+title+'\n'
        code_file.write(text)

        Unparse(node.specs,stree,level+1,code_file)

        statements = node.statements

        for statement in statements:
            Unparse(statement,stree,level+1,code_file)

        internal_subprograms = node.internal_subprograms

        if( len(internal_subprograms) != 0 ):
            code_file.write('\n'+space(level+1)+'contains\n\n')
  
        for subprogram in internal_subprograms:
            Unparse(subprogram,stree,level+2,code_file)
        
        text = '\n' + space(level) + 'end program ' + title + '\n\n'
        
        code_file.write(text)
 
        

    elif ( ntype == 'Function' ):


        title = node.title
        args = node.args
        specs = node.specs
        statements = node.statements
        result = node.result
        internal_subprograms = node.internal_subprograms


        text = space(level)+'function '+title+' ( ' 
        code_file.write(text)
        if ( len(args) != 0 ) :
            Unparse(args[0],stree,level,code_file)
        
            for i in range(1,len(args)):
                code_file.write(',')
                Unparse(args[i],stree,level,code_file)

        code_file.write(' )')
    
        if result is not None:
            code_file.write(' result( ' + result + ' )')
        
        code_file.write('\n')

        Unparse(node.specs,stree,level+1,code_file)

        for statement in statements:
            Unparse(statement,stree,level+1,code_file)

        internal_subprograms = node.internal_subprograms

        if( len(internal_subprograms) != 0 ):
            code_file.write(space(level+1)+'contains\n\n')
  
        for subprogram in internal_subprograms:
            Unparse(subprogram,stree,level+2,code_file)


        text = '\n' + space(level) + 'end function ' + title + '\n\n'
        
        code_file.write(text)
    
    elif ( ntype == 'Subroutine' ):

        title = node.title
        args = node.args
        specs = node.specs
        statements = node.statements
        internal_subprograms = node.internal_subprograms


        text = space(level)+'subroutine '+title+' ( ' 
        code_file.write(text)
        if ( len(args) != 0 ) :
            Unparse(args[0],stree,level,code_file)
        
            for i in range(1,len(args)):
                code_file.write(',')
                Unparse(args[i],stree,level,code_file)

        code_file.write(' )\n')
        
        Unparse(node.specs,stree,level+1,code_file)

        for statement in statements:
            Unparse(statement,stree,level+1,code_file)

        internal_subprograms = node.internal_subprograms

        if( len(internal_subprograms) != 0 ):
            code_file.write(space(level+1)+'contains\n\n')
  
        for subprogram in internal_subprograms:
            Unparse(subprogram,stree,level+2,code_file)


        text = '\n' + space(level) + 'end subroutine ' + title + '\n\n'
        
        code_file.write(text)

    elif ( ntype == 'Interface_module' ):
 

        title = node.title
        mod_proc_lis = node.mod_proc_lis

        text = space(level)

        code_file.write( text + 'interface ' + node.title + '\n\n' ) 
   
        for i in range(len(mod_proc_lis)):
            Unparse(mod_proc_lis[i],stree,level+1,code_file)
    
        code_file.write( '\n' + text + 'end interface ' + node.title + '\n\n' ) 

    elif ( ntype == 'Module_procedure' ):
        text = space(level)
        proc_lis = node.proc_lis
        code_file.write(text + 'module procedure ' + proc_lis[0])
        for i in range(1,len(proc_lis)):
            code_file.write(',' + proc_lis[i])
        code_file.write('\n')

    elif ( ntype == 'Interface_argument' ):
        

        text = space(level)
        title = node.title
        proc_lis = node.proc_lis

        code_file.write( text + 'interface ' )
        if ( title is not None ):
            code_file.write(title)
        code_file.write('\n\n')

        for proc in proc_lis:
            Unparse(proc,stree,level+1,code_file)
 
        code_file.write( '\n' + text + 'end interface ')

        if ( title is not None ):
            code_file.write(title)
        code_file.write('\n\n')

    elif ( ntype == 'Module' ):


        title = node.title 
        text = space(level)+'module '+title+'\n'
        code_file.write(text)

        Unparse(node.specs,stree,level+1,code_file)


        internal_subprograms = node.internal_subprograms

        if( len(internal_subprograms) != 0 ):
            code_file.write(space(level+1)+'contains\n\n')
  
        for subprogram in internal_subprograms:
            Unparse(subprogram,stree,level+2,code_file)
        
        text = '\n' + space(level) + 'end module ' + title + '\n\n'
        
        code_file.write(text)


    elif ( ntype == 'Specifications' ):

      
        use_stmts = node.use_stmts
        dec_stmts = node.dec_stmts

        
        if ( len( use_stmts ) != 0 ):

            code_file.write('\n')

            for use_stmt in use_stmts:
                Unparse(use_stmt,stree,level,code_file)

        
        code_file.write( '\n' + space(level) + 'implicit none\n' )
     
        if ( len( dec_stmts ) != 0 ):   

            code_file.write('\n')

            for dec_stmt in dec_stmts:
                Unparse(dec_stmt,stree,level,code_file)        

        code_file.write('\n')

    elif ( ntype == 'If_condition' ):
       
        

        expr = node.expr
        if_stmts = node.if_stmts
        elif_stmts = node.elif_stmts
        else_stmts = node.else_stmts



        text = space(level)


        code_file.write( '\n' + text + 'if ( ' )
        Unparse(node.expr,stree,level,code_file)
        code_file.write(' ) then\n\n')
    
        for statement in if_stmts:
            Unparse(statement,stree,level+1,code_file)

        code_file.write('\n')

        for elif_stmt in elif_stmts:
            text = space(level)
            code_file.write(text) 
            code_file.write('else if ( ')
            Unparse(elif_stmt[0],stree,level,code_file)
            code_file.write(' ) then\n\n')
                
            for statement in elif_stmt[1]:
                Unparse(statement,stree,level+1,code_file)
            
            code_file.write('\n')
          
        if len(else_stmts) != 0:
            code_file.write(text+'else \n\n')
            for statement in else_stmts:
                Unparse(statement,stree,level+1,code_file)

            code_file.write('\n')
   
        code_file.write(text+'end if\n\n')


    elif ( ntype == 'Do_loop' ):
        

        text = space(level)
        
        var = node.var
        start = node.start
        end = node.end
        step = node.step 
        statements = node.statements
        
        
        code_file.write('\n' + text + 'do ')
        Unparse(node.var,stree,level,code_file)
        code_file.write(' = ')
        Unparse(node.start,stree,level,code_file)
        code_file.write(',')
        Unparse(node.end,stree,level,code_file)
        if ( node.step is not None ):
            code_file.write(',')
            Unparse(node.step,stree,level,code_file)    

        code_file.write('\n\n')

        for statement in statements:
            Unparse(statement,stree,level+1,code_file)
        
        code_file.write('\n'+text)
        code_file.write('end do\n\n')


    elif ( ntype == 'Forall_loop' ):


        text = space(level)
         
        loop_lim_lis = node.loop_lim_lis
        mask = node.mask
        statements = node.statements
 
        code_file.write( '\n' + text + 'forall ( ')
        
        Unparse(loop_lim_lis[0],stree,level,code_file)

        for i in range(1,len(loop_lim_lis)):
            code_file.write(',')
            Unparse(loop_lim_lis[i],stree,level,code_file)
        
        if ( mask is not None ):
            code_file.write(',')
            Unparse(mask,stree,level,code_file) 

        code_file.write(' )\n\n')

        for statement in statements:
            Unparse(statement,stree,level+1,code_file)
        
        code_file.write('\n'+text)
        code_file.write('end forall\n\n')
        

    elif ( ntype == 'Loop_lim' ):

        var = node.var
        subsection = node.subsection
        code_file.write(var+' = ')
        Unparse(subsection,stree,level,code_file)

    elif ( ntype == 'While_loop' ):
        

#self,log_expr,statements
        text = space(level)

        log_expr = node.log_expr
        statements = node.statements
                  
        code_file.write('\n' + text + 'do while ( ')

        Unparse(node.log_expr,stree,level,code_file)
       
        code_file.write(' )')
  
        code_file.write('\n\n')
      
      
        for statement in statements:
            Unparse(statement,stree,level+1,code_file)
        
        code_file.write('\n'+text)
        code_file.write('end do\n\n')

    elif ( ntype == 'Subroutine_call' ):

        

        text = space(level)
        code_file.write(text)

        code_file.write('call ' + node.name + '(')
        
        parameters = node.parameters
        if ( len(parameters) != 0 ):
            Unparse(parameters[0],stree,0,code_file)
            for i in range(1,len(parameters)):
                code_file.write(',')
                Unparse(parameters[i],stree,0,code_file)
      

        code_file.write(')\n')

    elif ( ntype == 'Function_call' ): 
 
        code_file.write(node.name+'(')

        parameters = node.parameters
        if ( len(parameters) != 0 ):
            Unparse(parameters[0],stree,0,code_file)
            for i in range(1,len(parameters)):
                code_file.write(',')
                Unparse(parameters[i],stree,0,code_file)
      
        code_file.write(')')
    
    elif ( ntype == 'Allocate' ):

        text = space(level)
        code_file.write(text)
        code_file.write('allocate(')

        var_lis = node.array_lis
        stat_var = node.stat_var

        Unparse(var_lis[0],stree,level,code_file)
        for i in range(1,len(var_lis)):
            code_file.write(',')
            Unparse(var_lis[i],stree,level,code_file)

        if( stat_var is not None ):
            code_file.write(',')
            code_file.write('STAT = '+stat_var)         

        code_file.write(')\n')    



    elif ( ntype == 'Deallocate' ):

        text = space(level)
        code_file.write(text)
        code_file.write('deallocate(')

        var_lis = node.var_lis
        stat_var = node.stat_var

        Unparse(var_lis[0],stree,level,code_file)
        for i in range(1,len(var_lis)):
            code_file.write(',')
            Unparse(var_lis[i],stree,level,code_file)

        if( stat_var is not None ):
            code_file.write(',')
            code_file.write('STAT = '+stat_var)         

        code_file.write(')\n')    


    elif ( ntype == 'Number' ):

        code_file.write(node.value)

    elif ( ntype == 'Subsection' ):

        start = node.start
        end = node.end
        step = node.step
        if ( start is not None ):
            Unparse(node.start,stree,level,code_file)
        code_file.write(':')
        if ( end is not None ):
            Unparse(node.end,stree,level,code_file)
        if( step is not None ):
            code_file.write(':')
            Unparse(node.step,stree,level,code_file)

    elif ( ntype == 'String' ):

        code_file.write(node.value)

    elif ( ntype == 'Boolean' ):

        code_file.write(node.value)

    elif ( ntype == 'ArrayConstructor' ):

        code_file.write('(/ ')

        index_lis = node.index_lis
        
        Unparse(index_lis[0],stree,level,code_file)
        
        for i in range(1,len(index_lis)):
            code_file.write(',')
            Unparse(index_lis[i],stree,level,code_file)
 
        code_file.write(' /)')
        

    elif ( ntype == 'Add' ):

        Unparse(node.left,stree,level,code_file)
        code_file.write(' + ')
        Unparse(node.right,stree,level,code_file)


    elif ( ntype == 'Sub' ):

        Unparse(node.left,stree,level,code_file)
        code_file.write(' - ')
        Unparse(node.right,stree,level,code_file)


    elif ( ntype == 'Mul' ):

        Unparse(node.left,stree,level,code_file)
        code_file.write(' * ')
        Unparse(node.right,stree,level,code_file)


    elif ( ntype == 'Div' ):

        Unparse(node.left,stree,level,code_file)
        code_file.write(' / ')
        Unparse(node.right,stree,level,code_file)

    elif ( ntype == 'Power' ):

        Unparse(node.left,stree,level,code_file)
        code_file.write(' ** ')
        Unparse(node.right,stree,level,code_file)


    elif ( ntype == 'Equal' ):

        Unparse(node.left,stree,level,code_file)
        code_file.write(' == ')
        Unparse(node.right,stree,level,code_file)

    elif ( ntype == 'NEqual' ):

        Unparse(node.left,stree,level,code_file)
        code_file.write(' /= ')
        Unparse(node.right,stree,level,code_file)

    elif ( ntype == 'Lt' ):

        Unparse(node.left,stree,level,code_file)
        code_file.write(' < ')
        Unparse(node.right,stree,level,code_file)

    elif ( ntype == 'Le' ):

        Unparse(node.left,stree,level,code_file)
        code_file.write(' <= ')
        Unparse(node.right,stree,level,code_file)

    elif ( ntype == 'Gt' ):

        Unparse(node.left,stree,level,code_file)
        code_file.write(' > ')
        Unparse(node.right,stree,level,code_file)

    elif ( ntype == 'Ge' ):

        Unparse(node.left,stree,level,code_file)
        code_file.write(' >= ')
        Unparse(node.right,stree,level,code_file)

    elif ( ntype == 'And' ):

        Unparse(node.left,stree,level,code_file)
        code_file.write(' .and. ')
        Unparse(node.right,stree,level,code_file)

    elif ( ntype == 'Or' ):

        Unparse(node.left,stree,level,code_file)
        code_file.write(' .or. ')
        Unparse(node.right,stree,level,code_file)

    elif ( ntype == 'Xor' ):

        Unparse(node.left,stree,level,code_file)
        code_file.write(' .xor. ')
        Unparse(node.right,stree,level,code_file)

    elif ( ntype == 'UnaryAdd' ):
        code_file.write('+ ')
        Unparse(node.operand,stree,level,code_file)
        
    elif ( ntype == 'UnarySub' ):

        code_file.write('- ')
        Unparse(node.operand,stree,level,code_file)

    elif ( ntype == 'Not' ):
        
        code_file.write('.not. ')
        Unparse(node.operand,stree,level,code_file)

    elif ( ntype == 'Assignment' ):

        text = space(level)
        code_file.write(text)

        variable_ref = node.variable_ref
        expr = node.expr
        Unparse(variable_ref,stree,level,code_file)
        code_file.write(' = ')
        Unparse(expr,stree,level,code_file)

        code_file.write('\n')

    elif ( ntype == 'Arg_assignment' ):

        key = node.key
        expr = node.expr
        code_file.write( key + ' = ' )
        Unparse(expr,stree,level,code_file)

        

   
    elif ( ntype == 'Declaration' ):

        text = space(level)
        code_file.write(text)
 
        Type = node.Type
        Attributes = node.Attributes
        var_lis = node.Var_lis

        Unparse(Type,stree,level,code_file)

        for attr in Attributes:
            code_file.write(',')
            Unparse(attr,stree,level,code_file)

        code_file.write(' :: ')

        Unparse(var_lis[0],stree,level,code_file)

        for i in range(1,len(var_lis)):
            code_file.write(',')
            Unparse(var_lis[i],stree,level,code_file)
                   
        code_file.write('\n')

    elif ( ntype == 'Use' ):

        text = space(level)
        code_file.write(text)

        code_file.write('use ')
        code_file.write(node.mod_lis[0])
        for i in range(1,len(node.mod_lis)):
            code_file.write(',' + node.mod_lis[i])

        code_file.write('\n')

    elif ( ntype == 'Parenthesis' ):

        code_file.write('( ')
        Unparse(node.inside,stree,level,code_file)
        code_file.write(' )')
        

    elif ( ntype == 'Comment' ):

        text = '\n'+space(level)
        code_file.write(text)

        code_file.write('! ' + node.comment + '\n' )


    elif ( ntype == 'Scalar' ):

        code_file.write(node.name)

        if ( node.init_expr is not None ):
            code_file.write(' = ')
            Unparse(node.init_expr,stree,level,code_file)       

    elif ( ntype == 'Array' ):

        code_file.write(node.name+'(')
        Unparse(node.dim_lis[0],stree,level,code_file)

        for i in range(1,len(node.dim_lis)):
            code_file.write(',')
            Unparse(node.dim_lis[i],stree,level,code_file)

        code_file.write(')')

        if ( node.init_expr is not None ):
            code_file.write(' = ')
            Unparse(node.init_expr,stree,level,code_file)

    elif ( ntype == 'VariableType' ):
        code_file.write(node.name)
        if( node.size is not None ):
            if ( (node.name).lower() == 'character' ):
                code_file.write('( len = '+node.size+' )')
            else : code_file.write('('+node.size+')')

    elif ( ntype == 'VariableRef' ):
        code_file.write(node.name)
        
    elif ( ntype == 'ArrayRef' ):

        code_file.write(node.name+'(')
        Unparse(node.dim_lis[0],stree,level,code_file)

        for i in range(1,len(node.dim_lis)):

            code_file.write(',')
            Unparse(node.dim_lis[i],stree,level,code_file)
        code_file.write(')')
        
    elif ( ntype == 'Real' ):

        code_file.write('real(')
        Unparse(node.A,stree,level,code_file)
        if ( node.Kind is not None ):
            code_file.write(',')
            Unparse(node.Kind,stree,level,code_file)
        code_file.write(')')

    elif ( ntype == 'Integer' ):

        code_file.write('int(')
        Unparse(node.A,stree,level,code_file)
        if ( node.Kind is not None ):
            code_file.write(',')
            Unparse(node.Kind,stree,level,code_file)
        code_file.write(')')

    elif ( ntype == 'Intent' ):
        code_file.write('intent('+node.intent+')')
    elif ( ntype == 'Attribute_val' ):
        code_file.write(node.value)

    elif ( ntype == 'Print' ):
     
        text = space(level)

        code_file.write( text + 'print * ')
        
        print_lis = node.print_lis
        for i in range(len(print_lis)):
            code_file.write(',')
            Unparse(print_lis[i],stree,level,code_file)

        code_file.write('\n')
                
    elif ( ntype == 'Stop' ):
        text = space(level)
        code_file.write(text+'stop'+'\n')
    elif ( ntype == 'Exit' ):
        text = space(level)
        code_file.write(text+'exit'+'\n')
    elif ( ntype == 'Cycle' ):
        text = space(level)
        code_file.write(text+'cycle'+'\n')
    elif ( ntype == 'Return' ):
        text = space(level)
        code_file.write(text+'return'+'\n')


    

        
        
    

    

 



