/**
*   @file SymbolData.cpp
*
*   @author Paulo Barros Alencar
*   @date 27/11/2012
*/

#include <string>
#include <list>
#include <fstream>
#include <iostream>

#include <SymbolData.h>

using namespace std;

SymbolData::SymbolData()
{
    dataSize = 0;
}

SymbolData::SymbolData(string symbolName)
{
    tickData = list<TickData>();

    int ret = SymbolData::loadData(symbolName);

    if(ret <= 0)
    {
        ;//faio
    }

    dataSize = ret;
}

int SymbolData::loadData(string symbolName)
{
    ifstream inFile;

    inFile.open(string("data/" + symbolName).c_str());

    if (inFile.fail())
    {
        return -1;
    }

    string line;
    int i;

    if( !getline(inFile, line) || !inFile.good() )
    {
        return -2;
    }

    for (i = 0; getline(inFile, line) && inFile.good(); i++)
    {
        tickData.push_front(TickData(line));

        //todo se erro retorna erro
    }
    inFile.close();

    return i;
}

void SymbolData::print()
{
    for(list<TickData>::iterator it = tickData.begin(); it != tickData.end(); it++)
    {
        cout << it->getDate() << ":\t" << it->avgHLC() << "\n";
    }

}
