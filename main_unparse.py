from lexer import Lexer
from parser import Parser
import sys,os
import _pickle as pickle
from GenerateSymbolTableLatex import *
from tabulate import tabulate
from unparse_fortran import *

if ( len(sys.argv) == 5 ):
    inputfortranpath = sys.argv[1]
    outputgopath = sys.argv[2]
    outputtexpath = sys.argv[3]
    outputpdfpath = sys.argv[4]
else :
    print("Wrong input arguments")
    print("Correct Format - python main_unparse.py <input_fortran_filepath> <output_fortran_filepath> <output_tex_filepath> <output_pdf_filepath>")
    exit()

inputfortranlist = os.listdir(inputfortranpath)
outputgolist = [s[:-4] + "_st.go" for s in inputfortranlist]
outputtexlist = [s[:-4] + "_st.tex" for s in inputfortranlist]
outputpdflist = [s[:-4] + "_st.pdf" for s in inputfortranlist]

for i in range(len(inputfortranlist)): 
    input_fortran = inputfortranpath + inputfortranlist[i]
# Reading Input source file
    text_input = open(input_fortran, 'r').read()
# Converting input source file to lexemes 
    lexer = Lexer().get_lexer()
    tokens = lexer.lex(text_input)
# Parsing Lexemes to generate AST
    pg = Parser()
    pg.parse()
    parser = pg.get_parser()
    x = parser.parse(tokens)
# Generating Symbol Tree from AST
    tree = SymbolTree()
    tree.GenerateSymbolTable(x, None)
# Printing symbol tree to Tex file
    output_tex = outputtexpath + outputtexlist[i]
#    open(st_file,'w').close()
    stfile = open(output_tex,'a')
    stfile.write('\documentclass{report}\n')
    stfile.write('\\usepackage{geometry}\n')
    stfile.write('\\usepackage{longtable}\n')
    stfile.write('\\usepackage[T1]{fontenc}\n\n')
    title = os.path.splitext(os.path.basename(output_tex))[0]
    idx = title.index('_')
    title = title[:idx] + '\\' + title[idx:]
    stfile.write('\\title{\\bfseries Symbol Table - \\textit{'+ title +'}}\n\n')
    stfile.write('\\begin{document}\n\n')
    stfile.write('\maketitle\pagebreak\n\n')
    stfile.write('\\renewcommand{\\arraystretch}{1.5}\n')	
    stfile.write('\\addtolength{\\textwidth}{3cm}\n\n\n')			
    tree.PrintSymbolTable(stfile)
    stfile.write('\end{document}')
    stfile.close()
# Converting Tex file to PDF
#    output_pdf = outputpdfpath + outputpdflist[i]
    os.system("pdflatex -output-directory="+ outputpdfpath +" "+ output_tex + " > ./Junk/log.txt")      
# Converting Fortran AST to Fortran Source code
    output_go = outputgopath + outputgolist[i]
#    open(output_fortran, 'w').close()
    code_file = open(output_go,'a')
    Unparse(x,tree,0,code_file)
    code_file.close()





