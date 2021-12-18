from rply import LexerGenerator

class Lexer():
    def __init__(self):
        self.lexer = LexerGenerator()

    def _add_tokens(self):

        #self.lexer.add('COMMENT',r'(!)([^\n\r]+)')

        self.lexer.add('PROGRAM',r'(?i)program(?!([A-Za-z_$0-9]+))')
        self.lexer.add('END',r'(?i)end(?=(program(?!([A-Za-z_$0-9]+))|subroutine(?!([A-Za-z_$0-9]+))|function(?!([A-Za-z_$0-9]+))|interface(?!([A-Za-z_$0-9]+))|module(?!([A-Za-z_$0-9]+))|do(?!([A-Za-z_$0-9]+))|if(?!([A-Za-z_$0-9]+))))')      

        
        self.lexer.add('END',r'(?i)end(?!(([A-Za-z_$0-9]+)))')

        self.lexer.add('ALLOCATABLE',r'(?i)allocatable(?!([A-Za-z_$0-9]+))')
    
        self.lexer.add('INTENT',r'(?i)intent(?!([A-Za-z_$0-9]+))')

        self.lexer.add('SUBROUTINE',r'(?i)subroutine(?!([A-Za-z_$0-9]+))')
   
        self.lexer.add('FUNCTION',r'(?i)function(?!([A-Za-z_$0-9]+))')

        self.lexer.add('INTERFACE',r'(?i)interface(?!([A-Za-z_$0-9]+))')
     
        self.lexer.add('PROCEDURE',r'(?i)procedure(?!([A-Za-z_$0-9]+))')

        self.lexer.add('MODULE',r'(?i)module(?!([A-Za-z_$0-9]+))')

        self.lexer.add('CONTAINS',r'(?i)contains(?!([A-Za-z_$0-9]+))')

        self.lexer.add('RESULT',r'(?i)result(?!([A-Za-z_$0-9]+))')

        self.lexer.add('POINTER',r'(?i)pointer(?!([A-Za-z_$0-9]+))')

        self.lexer.add('PARAMETER',r'(?i)parameter(?!([A-Za-z_$0-9]+))')
     
        self.lexer.add('SAVE',r'(?i)save(?!([A-Za-z_$0-9]+))')

        self.lexer.add('ALLOCATE',r'(?i)allocate(?!([A-Za-z_$0-9]+))')
        
        self.lexer.add('DEALLOCATE',r'(?i)deallocate(?!([A-Za-z_$0-9]+))')

        self.lexer.add('RETURN',r'(?i)return(?!([A-Za-z_$0-9]+))')

        self.lexer.add('CALL',r'(?i)call(?!([A-Za-z_$0-9]+))')

        self.lexer.add('DO',r'(?i)do(?!([A-Za-z_$0-9]+))')

        self.lexer.add('FORALL',r'(?i)forall(?!([A-Za-z_$0-9]+))')

        self.lexer.add('WHILE',r'(?i)while(?!([A-Za-z_$0-9]+))')

        self.lexer.add('IF',r'(?i)if(?!([A-Za-z_$0-9]+))')
 
        self.lexer.add('ELIF',r'(?i)else if')

        self.lexer.add('ELSE',r'(?i)else(?!([A-Za-z_$0-9]+))')

        self.lexer.add('THEN',r'(?i)then(?!([A-Za-z_$0-9]+))')

        self.lexer.add('NONE',r'(?i)none(?!([A-Za-z_$0-9]+))')

        self.lexer.add('USE',r'(?i)use(?!([A-Za-z_$0-9]+))')

        self.lexer.add('IMPLICIT',r'(?i)implicit(?!([A-Za-z_$0-9]+))')

        self.lexer.add('PRINT',r'(?i)print(?!([A-Za-z_$0-9]+))')

        self.lexer.add('STOP',r'(?i)stop(?!([A-Za-z_$0-9]+))')
        
        self.lexer.add('EXIT',r'(?i)exit(?!([A-Za-z_$0-9]+))')

        self.lexer.add('CYCLE',r'(?i)cycle(?!([A-Za-z_$0-9]+))')


        # Boolean

        self.lexer.add('BOOLEAN','(?i)((\.true\.)|(\.false\.))')


         
        # Data Types
        
        self.lexer.add('REAL',r'(?i)real')
        self.lexer.add('INTEGER',r'(?i)integer')
        self.lexer.add('CHARACTER',r'(?i)character')

        self.lexer.add('LEN',r'(?i)len')
 
        

        self.lexer.add('NUMBER',r'([0-9]+)([\.]([0-9]+))?([eEdD][-+]?[0-9]+)?([_][0-9]+)?')
 
        self.lexer.add('STRING',r'((".*")|(\'.*\'))')
        
        self.lexer.add('EQUAL',r'==')
        self.lexer.add('NEQUAL',r'/=')


        self.lexer.add('OPEN_ARRAY_CONST',r'\(/')
        self.lexer.add('CLOSE_ARRAY_CONST',r'/\)')

        self.lexer.add('INT',r'(?i)int(?!([A-Za-z_$0-9]+))')

        self.lexer.add('INOUT',r'(?i)inout(?!([A-Za-z_$0-9]+))')
        self.lexer.add('IN',r'(?i)in(?!([A-Za-z_$0-9]+))')
        self.lexer.add('OUT',r'(?i)out(?!([A-Za-z_$0-9]+))')

        # Operators 

        self.lexer.add('POWER',r'\*\*')
        self.lexer.add('MUL',r'\*')
        self.lexer.add('DIV',r'/')
        self.lexer.add('PLUS',r'\+')
        self.lexer.add('MINUS',r'\-')
        
        self.lexer.add('LE',r'<=')
        self.lexer.add('LT',r'<')
        self.lexer.add('GE',r'>=')
        self.lexer.add('GT',r'>')

        # Boolean Operators

        self.lexer.add('NOT',r'(?i)\.not\.')
        self.lexer.add('AND',r'(?i)\.and\.')  
        self.lexer.add('OR',r'(?i)\.or\.')
        self.lexer.add('XOR',r'(?i)\.xor\.')

        self.lexer.add('ASSIGNMENT',r'=')

        self.lexer.add('OPEN_PAREN',r'\(')
        
        self.lexer.add('CLOSE_PAREN',r'\)')
        
        # Identifier example - variable name , function name , subroutine name etc
        self.lexer.add('ID',r'[A-Za-z][A-Za-z_$0-9]*')
        
        self.lexer.add('COLON',r':')

        self.lexer.add('COMMA',',')
        
        # Ignore space , comment etc
        self.lexer.ignore('\s+')
        self.lexer.ignore('&+')        
        self.lexer.ignore(r'(!)([^\n\r]+)')

        
           
    def get_lexer(self):
        self._add_tokens()
        return self.lexer.build()
