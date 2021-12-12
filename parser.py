from rply import ParserGenerator
from ast import *


class Parser():
    def __init__(self):

        self.pg = ParserGenerator(
            # A list of all token names accepted by the parser.
          ['PROGRAM','END','NUMBER','ID','ASSIGNMENT','REAL','INTEGER','CHARACTER','LEN','OPEN_PAREN','CLOSE_PAREN','COMMA','IF','ELSE','ELIF','THEN','DO', 'WHILE' , 'FORALL' ,'CALL','STRING',
  'POWER', 'MUL' , 'DIV' , 'PLUS' , 'MINUS' , 'EQUAL' , 'NEQUAL' , 'LT' , 'LE' , 'GT' , 'GE' , 'NOT' , 'AND' , 'OR' ,'XOR','BOOLEAN','OPEN_ARRAY_CONST','CLOSE_ARRAY_CONST','COLON','ALLOCATABLE','INTENT','INT','IN','OUT','INOUT','POINTER','PARAMETER','SAVE', 'SUBROUTINE' , 'FUNCTION', 'MODULE' , 'RESULT' , 'CONTAINS' , 'USE' , 'IMPLICIT' , 'NONE' , 'ALLOCATE' , 'DEALLOCATE' , 'PRINT' , 'STOP' , 'EXIT' , 'CYCLE' , 'RETURN' , 'INTERFACE' , 'PROCEDURE'
],

         precedence = [

                    ('right',['THEN','ELSE']),
                    ('left',['XOR']), 
                    ('left',['OR']), 
                    ('left',['AND']),
                    ('right',['NOT']),
                    ('left',['EQUAL','NEQUAL','LT','LE','GT','GE']),
                    ('left', ['PLUS', 'MINUS']),
                    ('right',['UPLUS','UMINUS']),
                    ('left', ['MUL', 'DIV']), 
                    ('right',['POWER']),
                    ('left' ,['PARENTHESIS']) 

         ]

        )



    def parse(self):

        # Executable Program - List of program units

        @self.pg.production('executable_program : program_units')
        def executable_program(p):
            return Execution_Program(p[0])


        # Program Units - Main , Function , Subroutine , Module
        
        @self.pg.production('program_units : program_units main_program')
        @self.pg.production('program_units : program_units function_subprogram')
        @self.pg.production('program_units : program_units subroutine_subprogram')
        @self.pg.production('program_units : program_units module')
        def program_units(p):
            program_lis = p[0]
            program_lis.append(p[1])
            return program_lis
 

        @self.pg.production('program_units :')
        def program_units(p):
            return []



        # Module

        @self.pg.production('module : MODULE ID spec_lis contains_stmt END MODULE ID')
        def module(p):
            return Module(p[1].value,p[2],p[3])

        
        # Main Subprogram   

        @self.pg.production('main_program : PROGRAM ID spec_lis statement_lis contains_stmt END PROGRAM ID')
        def program(p):
            return Main(p[1].value,p[2],p[3],p[4])

        # Subroutine    

        @self.pg.production('subroutine_subprogram : SUBROUTINE ID OPEN_PAREN para_lis CLOSE_PAREN spec_lis statement_lis contains_stmt END SUBROUTINE ID')
        def subroutine_subprogram(p):
            return Subroutine(p[1].value,p[3],p[5],p[6],p[7])

        # Function without result parameter 

        @self.pg.production('function_subprogram : FUNCTION ID OPEN_PAREN para_lis CLOSE_PAREN spec_lis statement_lis contains_stmt END FUNCTION ID')
        def function_subprogram(p):
            return Function(p[1].value,p[3],p[5],p[6],p[1].value,p[7])
 

        # Function with result parameter
        
        @self.pg.production('function_subprogram : FUNCTION ID OPEN_PAREN para_lis CLOSE_PAREN result spec_lis statement_lis contains_stmt END FUNCTION ID')
        def function_subprogram(p):
            return Function(p[1].value,p[3],p[6],p[7],p[5],p[8])

        # Function result

        @self.pg.production('result : RESULT OPEN_PAREN ID CLOSE_PAREN')
        def result(p):
            return p[2].value

        # Contains statement nested subprograms

        @self.pg.production('contains_stmt : ')
        def contains_stmt(p):
            return []

        @self.pg.production('contains_stmt : CONTAINS program_units')  # program_units should not be Main
        def contains_stmt(p):
            return p[1]


        # Program unit specification

        @self.pg.production('spec_lis : use_stmts implicit_stmt dec_stmts')
        def spec_lis(p):
            return Specifications(p[0],p[2])

        # Implicit statement 

        @self.pg.production('implicit_stmt : IMPLICIT NONE')
        @self.pg.production('implicit_stmt : ')
        def implicit_stmt(p):
            return None
       
        # List of use statements

        @self.pg.production('use_stmts : ')
        def use_stmts(p):
            return []

        @self.pg.production('use_stmts : use_stmts use_stmt')
        def use_stmts(p):
            use_lis = p[0]
            use_lis.append(p[1])
            return use_lis


        # List of declaration statements
        
        @self.pg.production('dec_stmts : ')
        def dec_stmts(p):
            return []

        @self.pg.production('dec_stmts : dec_stmts declaration')
        @self.pg.production('dec_stmts : dec_stmts interface_module')
        @self.pg.production('dec_stmts : dec_stmts interface_argument')
        def dec_stmts(p):
            dec_lis = p[0]
            dec_lis.append(p[1])
            return dec_lis
        

        # Use statement

        @self.pg.production('use_stmt : USE id_lis')
        def use_stmt(p):
            return Use(p[1])


        # List of Id

        @self.pg.production('id_lis : id_lis COMMA ID')
        def id_lis(p):
            mod_lis = p[0]
            mod_lis.append(p[2].value)
            return mod_lis


        @self.pg.production('id_lis : ID')
        def mod_lis(p):
            return [ p[0].value ]


        # interface fortran used for overloading of function , subroutine
        # example - 
        '''
            interface Add

                module procedure AddInteger 
                module procedure AddReal

            end interface Add

        '''

        @self.pg.production('interface_module : INTERFACE ID mod_proc_lis END INTERFACE ID')
        def interface_block(p):
            return Interface_module(p[1].value,p[2])

        # List of module procedure statements

        @self.pg.production('mod_proc_lis : mod_proc_lis mod_proc')
        def mod_proc_lis(p):
            proc_lis = p[0]
            proc_lis.append(p[1])
            return proc_lis

        @self.pg.production('mod_proc_lis : ')
        def mod_proc_lis(p):
            return []


        # Module procedure statement
        # example - module procedure Addsingle

        @self.pg.production('mod_proc : MODULE PROCEDURE id_lis')
        def mod_proc(p):
            return Module_procedure(p[2])


        # interface fortran used for passing function , subroutine as argument
        # example - 
        '''
            subroutine BinaryOp( x , y , z , operator )

                integer , intent(in) :: x , y 
                integer , intent(out) :: z
                interface 

                    function operator(x,y) result(res)
  
                        integer , intent(in) :: x , y 
                        integer :: res
  
                    end function operator

                end interface


            end subroutine BinaryOp

        '''
        
        @self.pg.production('interface_argument : INTERFACE ID program_spec_units END INTERFACE ID')
        def interface_argument(p):
            return Interface_argument(p[1].value,p[2])

        @self.pg.production('interface_argument : INTERFACE program_spec_units END INTERFACE')
        def interface_argument(p):
            return Interface_argument(None,p[1])


        # Function Subroutine Definition within interface 

        @self.pg.production('program_spec_units : program_spec_units function_subprogram')
        @self.pg.production('program_spec_units : program_spec_units subroutine_subprogram')
        def program_spec_units(p):
            program_lis = p[0] 
            program_lis.append(p[1])
            return program_lis

        @self.pg.production('program_spec_units : function_subprogram')
        @self.pg.production('program_spec_units : subroutine_subprogram')
        def program_spec_units(p):
            return [p[0]]


        # Variable Declaration

        @self.pg.production('declaration : Type Attributes COLON COLON var_lis')
        def declaration(p):
            return Declaration(p[0],p[1],p[4])

        # Variable Type More can be added

        @self.pg.production('Type : REAL OPEN_PAREN NUMBER CLOSE_PAREN')
        @self.pg.production('Type : INTEGER OPEN_PAREN NUMBER CLOSE_PAREN')
        def Type(p):
            return VariableType(p[0].value,p[2].value)
 
        

        @self.pg.production('Type : REAL')
        @self.pg.production('Type : INTEGER')
        @self.pg.production('Type : CHARACTER')
        def Type(p):
            return VariableType(p[0].value,None)


        @self.pg.production('Type : CHARACTER OPEN_PAREN LEN ASSIGNMENT NUMBER CLOSE_PAREN')
        def Type(p):
            return VariableType(p[0].value,p[4].value)


       
        # List of Variable Attributes

        @self.pg.production('Attributes : Attributes COMMA Attribute')
        def Attributes(p):
            attr_lis = p[0]
            attr_lis.append(p[2])
            return attr_lis


        @self.pg.production('Attributes :')
        def Attributes(p):
            return []

      
        # Variable Attributes

        @self.pg.production('Attribute : INTENT OPEN_PAREN IN CLOSE_PAREN')
        @self.pg.production('Attribute : INTENT OPEN_PAREN OUT CLOSE_PAREN')
        @self.pg.production('Attribute : INTENT OPEN_PAREN INOUT CLOSE_PAREN')
        def Attribute(p):
            return Intent(p[2].value)


        @self.pg.production('Attribute : ALLOCATABLE')
        @self.pg.production('Attribute : POINTER')
        @self.pg.production('Attribute : PARAMETER')
        @self.pg.production('Attribute : SAVE')
        def Attribute(p):
            return Attribute_val(p[0].value)



        # List of variables         

        # Scalar Variable
        @self.pg.production('var_lis : var_lis COMMA ID')
        def var_lis(p):
            v_lis = p[0]
            v_lis.append(Scalar(p[2].value))
            return v_lis

        # Scalar Variable with initial value        
        @self.pg.production('var_lis : var_lis COMMA ID ASSIGNMENT expression')
        def var_lis(p):
            v_lis = p[0]
            v_lis.append(Scalar(p[2].value,p[4]))
            return v_lis


        # Array variable 
        @self.pg.production('var_lis : var_lis COMMA ID OPEN_PAREN dim_lis CLOSE_PAREN')
        def var_lis(p):
            v_lis = p[0]
            v_lis.append(Array(p[2].value,p[4]))
            return v_lis

        # Array variable with initial value
        @self.pg.production('var_lis : var_lis COMMA ID OPEN_PAREN dim_lis CLOSE_PAREN ASSIGNMENT expression')
        def var_lis(p):
            v_lis = p[0]
            v_lis.append(Array(p[2].value,p[4],p[7]))
            return v_lis


        
        @self.pg.production('var_lis : ID')
        def var_lis(p):
            v_lis = [ Scalar(p[0].value) ]
            return v_lis

        @self.pg.production('var_lis : ID ASSIGNMENT expression')
        def var_lis(p):
            v_lis = [ Scalar(p[0].value,p[2]) ]
            return v_lis

        @self.pg.production('var_lis : ID OPEN_PAREN dim_lis CLOSE_PAREN')
        def var_lis(p):
            v_lis = [Array(p[0].value,p[2])]
            return v_lis

        @self.pg.production('var_lis : ID OPEN_PAREN dim_lis CLOSE_PAREN ASSIGNMENT expression')
        def var_lis(p):
            v_lis = [Array(p[0].value,p[2],p[5])]
            return v_lis



        # List of dimensions of array

        @self.pg.production('dim_lis : dim_lis COMMA subsection')
        def dim_lis(p):
            lis = p[0]
            lis.append(p[2])
            return lis

        @self.pg.production('dim_lis : dim_lis COMMA expression')
        def dim_lis(p):
            lis = p[0]
            lis.append(p[2])
            return lis

        @self.pg.production('dim_lis : subsection')
        @self.pg.production('dim_lis : expression')
        def dim_lis(p):
            return [ p[0] ]



        # List of statements 

        @self.pg.production('statement_lis : statement_lis statement')
        def statement_lis(p):
            stmt_lis = p[0]
            stmt_lis.append(p[1])
            return stmt_lis

        @self.pg.production('statement_lis : ')
        def statement_lis(p):
            stmt_lis = []
            return stmt_lis


        # Statements

        @self.pg.production('statement : assignment')
        @self.pg.production('statement : if_statement')
        @self.pg.production('statement : do_loop')
        @self.pg.production('statement : forall_loop')
        @self.pg.production('statement : while_loop')
        @self.pg.production('statement : subroutine_call')
        @self.pg.production('statement : allocate')
        @self.pg.production('statement : deallocate')
        @self.pg.production('statement : print_stmt')
        @self.pg.production('statement : stop_stmt')
        @self.pg.production('statement : exit_stmt')
        @self.pg.production('statement : cycle_stmt')
        @self.pg.production('statement : return_stmt')
        def statement(p):
            return p[0]
 
      
        # Stop Statement  
        @self.pg.production('stop_stmt : STOP')
        def stop_stmt(p):
            return Stop()

        # Exit Statement
        @self.pg.production('exit_stmt : EXIT')
        def exit_stmt(p):
            return Exit()

        # Cycle Statement
        @self.pg.production('cycle_stmt : CYCLE')    
        def cycle_stmt(p):
            return Cycle()      

        # Return Statement
        @self.pg.production('return_stmt : RETURN')
        def return_stmt(p):
            return Return()      
        
        # Allocate Statement example - allocate(A(2,2))
        @self.pg.production('allocate : ALLOCATE OPEN_PAREN variable_ref_lis CLOSE_PAREN')
        def allocate(p):
            return Allocate(p[2])

        # Allocate Statement example - allocate(A(2,2),STAT = chk)
        @self.pg.production('allocate : ALLOCATE OPEN_PAREN variable_ref_lis COMMA ID ASSIGNMENT ID CLOSE_PAREN')
        def allocate(p):
            return Allocate(p[2],p[6].value)

        # Deallocate Statement example - deallocate(A)
        @self.pg.production('deallocate : DEALLOCATE OPEN_PAREN variable_ref_lis CLOSE_PAREN')
        def deallocate(p):
            return Deallocate(p[2])

        # Deallocate Statement example - deallocate(A,STAT=chk)
        @self.pg.production('deallocate : DEALLOCATE OPEN_PAREN variable_ref_lis COMMA ID ASSIGNMENT ID CLOSE_PAREN')
        def deallocate(p):
            return Deallocate(p[2],p[6].value)

        # Print Statement
        @self.pg.production('print_stmt : PRINT MUL COMMA print_lis')
        def print_stmt(p):
            return Print(p[3])

        # List of things to print
        @self.pg.production('print_lis : print_lis COMMA expression')
        def print_lis(p):
            p_lis = p[0]
            p_lis.append(p[2])
            return p_lis
        
        @self.pg.production('print_lis : expression')
        def print_lis(p):
            return [p[0]]

        
        # List of variable references
        @self.pg.production('variable_ref_lis : variable_ref_lis COMMA variable_ref')
        def variable_ref_lis(p):
            ref_lis = p[0]
            ref_lis.append(p[2])
            return ref_lis

        @self.pg.production('variable_ref_lis : variable_ref')
        def variable_ref_lis(p):
            return [p[0]]



        # Variable Assignment
        
        @self.pg.production('assignment : variable_ref ASSIGNMENT expression')
        def assignment(p):
            return Assignment(p[0],p[2])

   
        # Scalar variable ref
        @self.pg.production('variable_ref : ID')
        def variable_ref(p):
            return VariableRef(p[0].value)

        # Array element or slice reference
        @self.pg.production('variable_ref : ID OPEN_PAREN para_lis CLOSE_PAREN')
        def variable_ref(p):
            return ArrayRef(p[0].value,p[2])

        
        # If construct
        @self.pg.production('if_statement : IF OPEN_PAREN expression CLOSE_PAREN THEN statement_lis elif_lis ELSE statement_lis END IF')
        def if_statement(p):
            return If_condition(p[2],p[5],p[6],p[8])
        
        @self.pg.production('if_statement : IF OPEN_PAREN expression CLOSE_PAREN THEN statement_lis elif_lis END IF')
        def if_statement(p):
            return If_condition(p[2],p[5],p[6],[])


        @self.pg.production('elif_lis : elif_lis ELIF OPEN_PAREN expression CLOSE_PAREN THEN statement_lis')
        def elif_lis(p):
            lis = p[0]
            lis.append([p[3],p[6]])
            return lis
        
        @self.pg.production('elif_lis :')
        def elif_lis(p):
            return []



        # Do Loop Construct 

        @self.pg.production('do_loop : DO variable_ref ASSIGNMENT expression COMMA expression COMMA expression statement_lis END DO')
        def do_loop(p):
            var = p[1]
            start = p[3]
            end = p[5]
            step = p[7]
            return Do_loop(var,start,end,step,p[8]) 

        @self.pg.production('do_loop : DO variable_ref ASSIGNMENT expression COMMA expression statement_lis END DO')
        def do_loop(p):
            var = p[1]
            start = p[3]
            end = p[5]
            step = None
            return Do_loop(var,start,end,step,p[6])


        # Forall Loop Construct

        @self.pg.production('forall_loop : FORALL OPEN_PAREN loop_lim_lis mask CLOSE_PAREN statement_lis END FORALL')
        def forall_loop(p):
            return Forall_loop(p[2],p[3],p[5])
        
        @self.pg.production('loop_lim_lis : loop_lim_lis COMMA loop_lim')
        def loop_lim_lis(p):
            loop_lis = p[0]
            loop_lis.append(p[2])
            return loop_lis


        # List of limits of loop in forall loop

        @self.pg.production('loop_lim_lis : loop_lim')
        def loop_lim_lis(p):
            return [p[0]]

        @self.pg.production('loop_lim : ID ASSIGNMENT subsection')        
        def loop_lim(p):
            return Loop_lim(p[0].value,p[2])


        # mask in forall loop

        @self.pg.production('mask : COMMA expression')
        def mask(p):
            return p[1]
      
        @self.pg.production('mask : ')
        def mask(p):
            return None

       
        # While loop construct

        @self.pg.production('while_loop : DO WHILE OPEN_PAREN expression CLOSE_PAREN statement_lis END DO')
        def while_loop(p):
            log_expr = p[3]
            statement_lis = p[5]
            return While_loop(log_expr,statement_lis)

      
        # Subroutine call
        
        @self.pg.production('subroutine_call : CALL ID OPEN_PAREN para_lis CLOSE_PAREN')
        def subroutine_call(p):
            name = p[1].value
            parameters = p[3]
            return Subroutine_call(name,parameters)

        # List of parameters

        @self.pg.production('para_lis : para_lis COMMA subsection')
        @self.pg.production('para_lis : para_lis COMMA expression')
        @self.pg.production('para_lis : para_lis COMMA arg_assignment')
        def para_lis(p):
            p_lis = p[0]
            p_lis.append(p[2])
            return p_lis
        
        @self.pg.production('para_lis : subsection')
        @self.pg.production('para_lis : expression')
        @self.pg.production('para_lis : arg_assignment')
        def parameter(p):
            return [p[0]]

        
        @self.pg.production('para_lis : ')
        def parameter(p):
            return []


        # Argument assignment 
        # example - kind = 8 in real( A , kind = 8 )
        @self.pg.production('arg_assignment : ID ASSIGNMENT expression')
        def arg_assignment(p):
            return Arg_assignment(p[0].value,p[2])
        
        
        # Expression 


        @self.pg.production('expression : expression POWER expression')
        @self.pg.production('expression : expression MUL expression')
        @self.pg.production('expression : expression DIV expression')
        @self.pg.production('expression : expression PLUS expression')
        @self.pg.production('expression : expression MINUS expression')
        @self.pg.production('expression : expression EQUAL expression')
        @self.pg.production('expression : expression NEQUAL expression')
        @self.pg.production('expression : expression LT expression')
        @self.pg.production('expression : expression LE expression')
        @self.pg.production('expression : expression GT expression')
        @self.pg.production('expression : expression GE expression')
        @self.pg.production('expression : expression AND expression')
        @self.pg.production('expression : expression OR expression')
        @self.pg.production('expression : expression XOR expression') 
        def expression(p):
            operator = p[1].gettokentype()
            left = p[0]
            right = p[2]
            if( operator == 'POWER' ):
                return Power(left,right)
            elif( operator == 'MUL' ):
                return Mul(left,right)
            elif( operator == 'DIV' ):
                return Div(left,right)
            elif( operator == 'PLUS' ):
                return Add(left,right)
            elif( operator == 'MINUS' ):
                return Sub(left,right)
            elif( operator == 'EQUAL' ):
                return Equal(left,right)
            elif( operator == 'NEQUAL' ):
                return NEqual(left,right)
            elif( operator == 'LT' ):
                return Lt(left,right)
            elif( operator == 'LE' ):
                return Le(left,right)
            elif( operator == 'GT' ):
                return Gt(left,right)
            elif( operator == 'GE' ):
                return Ge(left,right)
            elif( operator == 'AND' ):
                return And(left,right)
            elif( operator == 'OR' ):
                return Or(left,right)
            elif( operator == 'XOR' ):
                return Xor(left,right)



        @self.pg.production('expression : PLUS expression',precedence='UPLUS')
        @self.pg.production('expression : MINUS expression',precedence='UMINUS')
        def primary(p):
            if(p[0].gettokentype() == 'PLUS'):
                return UnaryAdd(p[1])
            elif(p[0].gettokentype() == 'MINUS'):
                return UnarySub(p[1])


        @self.pg.production('expression : NOT expression')
        def primary(p):
            if(p[0].gettokentype() == 'NOT'):
                return Not(p[1])

        @self.pg.production('expression : primary')
        def expression(p):
            return p[0]


        # Primary Operands for expression 

        @self.pg.production('primary : OPEN_PAREN expression CLOSE_PAREN',precedence='PARENTHESIS')
        def primary(p):
            return Parenthesis(p[1])        


        @self.pg.production('primary : NUMBER')
        @self.pg.production('primary : ID')
        @self.pg.production('primary : STRING')
        @self.pg.production('primary : BOOLEAN')
        def primary(p):
            
            ptype = p[0].gettokentype()
            if( ptype == 'NUMBER' ):
                return Number(p[0].value)
            elif( ptype == 'ID'):
                return VariableRef(p[0].value)
            elif( ptype == 'STRING' ):
                return String(p[0].value)
            elif( ptype == 'BOOLEAN' ):
                return Boolean((p[0].value))



        @self.pg.production('primary : Array_constructor')
        @self.pg.production('primary : Array_Function_ref')
        @self.pg.production('primary : Type_conversion')
        def primary(p):
            return p[0]        

        
        # Array reference or Function call
 
        @self.pg.production('Array_Function_ref : ID OPEN_PAREN para_lis CLOSE_PAREN')
        def Array_Function_ref(p):
            name = p[0].value
            parameters = p[2]
            return Function_call(name,parameters)
    

        # Type conversion fortran 


        # example - real(A)        
        @self.pg.production('Type_conversion : REAL OPEN_PAREN expression CLOSE_PAREN')
        @self.pg.production('Type_conversion : INT OPEN_PAREN expression CLOSE_PAREN')
        def Type_conversion(p):
            if ( p[0].gettokentype() == 'REAL' ):
                return Real(p[2])
            elif ( p[0].gettokentype() == 'INT' ):
                return Integer(p[2])

        # example - real(A,8)
        @self.pg.production('Type_conversion : REAL OPEN_PAREN expression COMMA expression CLOSE_PAREN')
        @self.pg.production('Type_conversion : INT OPEN_PAREN expression COMMA expression CLOSE_PAREN')
        def Type_conversion(p):
            if ( p[0].gettokentype() == 'REAL' ):
                return Real(p[2],p[4])
            elif ( p[0].gettokentype() == 'INT' ):
                return Integer(p[2],p[4])

        # example - real(A, kind = 8)
        @self.pg.production('Type_conversion : REAL OPEN_PAREN expression COMMA assignment CLOSE_PAREN')
        @self.pg.production('Type_conversion : INT OPEN_PAREN expression COMMA assignment CLOSE_PAREN')
        def Type_conversion(p):
            if ( p[0].gettokentype() == 'REAL' ):
                return Real(p[2],p[4].expr)
            elif ( p[0].gettokentype() == 'INT' ):
                return Integer(p[2],p[4].expr)


        
        # Array constructor
        @self.pg.production('Array_constructor : OPEN_ARRAY_CONST para_lis CLOSE_ARRAY_CONST')
        def Array_constructor(p):
            return ArrayConstructor(p[1])
        
        
        # Array slice index 
        # example - A(1:5) - [ A(1) , A(2) , A(3) , A(4) , A(5) ]
        @self.pg.production('subsection : index COLON index stride')
        def subsection(p):
            return Subsection(p[0],p[2],p[3])        

   
        @self.pg.production('index : expression')
        def index(p):
            return p[0]

        @self.pg.production('index :')
        def index(p):
            return None

        # Optional Step size

        @self.pg.production('stride : COLON expression')
        def stride(p):
            return p[1]

        @self.pg.production('stride :')
        def stride(p):
            return None
        
       

        @self.pg.error
        def error_handle(token):
            raise ValueError(token)

    def get_parser(self):
        return self.pg.build()
