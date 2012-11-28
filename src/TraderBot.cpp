/**
*   @file TraderBot.cpp
*
*   @author Paulo Barros Alencar
*   @date 27/11/2012
*/

#include <cstdio>
#include <cstdlib>

#include <SymbolData.h>


int main(int argc, char *argv[])
{

    //parse the shit

    SymbolData *symbol = new SymbolData(argv[1]);

    //symbol->plot();

    symbol->print();

    delete symbol;
}
