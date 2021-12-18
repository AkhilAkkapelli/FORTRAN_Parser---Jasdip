inputfortranpath="./SymbolTableGen/InputFortranPrograms/"
outputgopath="./SymbolTableGen/OutputGoPrograms/"
outputtexpath="./SymbolTableGen/STOutputTexCodes/"
outputpdfpath="./SymbolTableGen/STOutputPdfFiles/"

python3.7 main_unparse.py $inputfortranpath $outputgopath $outputtexpath $outputpdfpath
