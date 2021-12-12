# List of program units - Main , Subroutine , Function
class Execution_Program():
    def __init__(self,program_units):
        self.program_units = program_units


# Main Subprogram

class Main():
    def __init__(self,title,specs,statements,internal_subprograms):
        self.title = title
        self.specs = specs
        self.statements = statements
        self.internal_subprograms = internal_subprograms
    

# Function Subprogram    

class Function():
    def __init__(self,title,args,specs,statements,result,internal_subprograms):
        self.title = title
        self.specs = specs
        self.args = args
        self.statements = statements
        self.result = result
        self.internal_subprograms = internal_subprograms

# Subroutine Subprogram

class Subroutine():
    
    def __init__(self,title,args,specs,statements,internal_subprograms):
        self.title = title
        self.specs = specs
        self.args = args
        self.statements = statements
        self.internal_subprograms = internal_subprograms


# Module 

class Module():

    def __init__(self,title,specs,internal_subprograms):
        self.title = title
        self.specs = specs
        self.internal_subprograms = internal_subprograms
    

# Program unit Specifications - Use statements , Declarations

class Specifications():
    def __init__(self,use_stmts,dec_stmts):
        self.use_stmts = use_stmts
        self.dec_stmts = dec_stmts

   
# If construct

class If_condition():
    def __init__(self,expr,if_stmts,elif_stmts,else_stmts):
        self.expr = expr 
        self.if_stmts = if_stmts
        self.elif_stmts = elif_stmts
        self.else_stmts = else_stmts

# Do construct

class Do_loop():

    def __init__(self,var,start,end,step,statements):
        self.var = var
        self.start = start
        self.end = end
        self.step = step 
        self.statements = statements

# forall construct

class Forall_loop():

    def __init__(self,loop_lim_lis,mask,statements):
        self.loop_lim_lis = loop_lim_lis
        self.mask = mask
        self.statements = statements

# Loop range example - i = 1 , 10  

class Loop_lim():

    def __init__(self,var,subsection):
        self.var = var
        self.subsection = subsection


# While construct

class While_loop():
    def __init__(self,log_expr,statements):
        self.log_expr = log_expr
        self.statements = statements


# Subroutine call statement

class Subroutine_call():
    def __init__(self,name,parameters):
        self.name = name
        self.parameters = parameters


# Function call or Array reference( In fortran syntax of function call and array ref is same therefore needs symbol table )
class Function_call():
    def __init__(self,name,parameters):
        self.name = name
        self.parameters = parameters

# Allocate array fortran
class Allocate():
    def __init__(self,array_lis,stat_var=None):
        self.array_lis = array_lis
        self.stat_var = stat_var
        
# Deallocate Fortran array
class Deallocate():
    def __init__(self,var_lis,stat_var=None):
        self.var_lis = var_lis
        self.stat_var = stat_var

# Integer or Floating point number    
class Number():
    def __init__(self, value):
        self.value = value

    def eval(self):
        return self.value


# Array subsection example - A(1:10:2) which represents [ A(1) , A(3) , A(5) , A(7) , A(9) ]    
class Subsection():
    def __init__(self,start=None,end=None,step=None):
        self.start = start
        self.end = end
        self.step = step



# String example - 'A' , "ABCS"
class String():
    def __init__(self,value):
        self.value = value

# Boolean - .true. and .false.
class Boolean():
    def __init__(self,value):
        self.value = value.lower()


# Array constructor example A( (/ 1 , 2 , 3 /) ) represents [ A(1) , A(2) , A(3) ]
class ArrayConstructor():
    def __init__(self,index_lis):
        self.index_lis = index_lis

# Binary Operations 

class BinaryOp():
    def __init__(self, left, right):
        self.left = left
        self.right = right


# Add operator 
class Add(BinaryOp):
    def eval(self):
        return self.left.eval() + self.right.eval()

# Subtract operator
class Sub(BinaryOp):
    def eval(self):
        return self.left.eval() - self.right.eval()

# Multiply operator
class Mul(BinaryOp):
    def eval(self):
        return self.left.eval() * self.right.eval()

# Division operator 
class Div(BinaryOp):
    def eval(self):
        return self.left.eval() / self.right.eval()

# Power operator
class Power(BinaryOp):
    def eval(self): 
        return self.left.eval() ** self.right.eval()

# Equal to operator 
class Equal(BinaryOp):
    def eval(self):
        return self.left.eval() == self.right.eval()

# Not Equal to operator
class NEqual(BinaryOp):
    def eval(self): 
        return self.left.eval() != self.right.eval()

# Less than operator 
class Lt(BinaryOp):
    def eval(self):
        return self.left.eval() < self.right.eval()

# Less than equal to operator
class Le(BinaryOp):
    def eval(self): 
        return self.left.eval() <= self.left.eval()

# Greater than operator
class Gt(BinaryOp):
    def eval(self):
        return self.left.eval() > self.left.eval()

# Greater than equal to operator
class Ge(BinaryOp):
    def eval(self):
        return self.left.eval() >= self.left.eval() 

# And operator
class And(BinaryOp):
    pass

# Or operator
class Or(BinaryOp):
    pass

# Xor operator
class Xor(BinaryOp):
    pass




   
 
# Unary Operations 

class UnaryOp():
    def __init__(self,operand):
        self.operand = operand

# Unary Add operator       
class UnaryAdd(UnaryOp):
    pass
# Unary Subtract operator
class UnarySub(UnaryOp):
    pass
# Not operator
class Not(UnaryOp):
    pass


# Assignment operator
class Assignment():
    def __init__(self,variable_ref,expr):
        self.variable_ref = variable_ref
        self.expr = expr

# Variable Declaration Statement 
# Type - type of variable
# Attributes - attributes of variable example - allocatable , parameter , intent(in) etc
class Declaration():
    def __init__(self,Type,Attributes,Var_lis):
        self.Type = Type
        self.Attributes = Attributes
        self.Var_lis = Var_lis

# Use statement 
class Use():
    def __init__(self,mod_lis):
        self.mod_lis = mod_lis

# Parenthesis
class Parenthesis():
    def __init__(self,inside):
        self.inside = inside

# Scalar variable 
# init_expr - initial value during declaration example - real :: x = 2 
class Scalar():
    def __init__(self,name,init_expr=None):
        self.name = name
        self.init_expr = init_expr
        

# Array variable
# dim_lis - list of dimensions example - real :: x(2,2) implies dim_lis = [2,2] , x(:,:) implies dim_lis = [ None , None ] 
class Array():
    def __init__(self,name,dim_lis,init_expr=None):
        self.name = name
        self.dim_lis = dim_lis
        self.init_expr = init_expr


# Type of variable
# size - kind of variable example real(8) implies name = real and size is 8 .
class VariableType():

    def __init__(self,name,size):
        self.name = name.lower()
        self.size = size

    def __repr__(self):
   
        Type = self.name
        if self.size is not None :
            Type = Type# + '(' + self.size + ')'
        
        return Type

# Reference to a variable which may be an array or scalar         
class VariableRef():
    def __init__(self,name):
        self.name = name
        
# Reference to a element of array or a slice of array example - A(1,1) or A(1,:)
class ArrayRef():
    def __init__(self,name,dim_lis):
        self.name = name
        self.dim_lis = dim_lis
    
# Fortran Intrinsic Function for Type Conversion

# converting to real type
class Real():
    def __init__(self,A,Kind=None):
        self.A = A
        self.Kind = Kind

# converting to integer type
class Integer():
    def __init__(self,A,Kind=None):
        self.A = A
        self.Kind = Kind

# Attributes of variable declared during declaration example - allocatable , parameter
class Attribute_val():

    def __init__(self,value):
        self.value = value.lower()

    def __repr__(self):
        return self.value

# intent of argument example intent(in) implies intent = 'in'
class Intent():
    def __init__(self,intent):
        self.intent = intent.lower()

    def __repr__(self):
        return 'intent('+self.intent+')'

# Print statement fortran 
# example - print *, "hello" , x implies print_lis = [ x , "hello" ]
class Print():
    def __init__(self,print_lis):
        self.print_lis = print_lis


# stop statement
class Stop():
    def __init__(self):
        pass

# exit statement
class Exit():
    def __init__(self):
        pass

# cycle statement
class Cycle():
    def __init__(self):
        pass

# return statement
class Return():
    def __init__(self):
        pass

# interface fortran used for overloading of function , subroutine
# example - 
'''
 interface Add

   module procedure AddInteger 
   module procedure AddReal

 end interface Add

'''
class Interface_module():

    def __init__(self,title,mod_proc_lis):
        self.title = title
        self.mod_proc_lis = mod_proc_lis


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
class Interface_argument():
    
    def __init__(self,title,proc_lis):
        self.title = title
        self.proc_lis = proc_lis

# Module procedure statement
# example - module procedure Addsingle
class Module_procedure():

    def __init__(self,proc_lis):
        self.proc_lis = proc_lis

# Argument assignment 
# example - kind = 8 in real( A , kind = 8 )
class Arg_assignment():
    def __init__(self,key,expr):
        self.key = key
        self.expr = expr







