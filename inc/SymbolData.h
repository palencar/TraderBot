/**
*   @file SymbolData.h
*
*   @author Paulo Barros Alencar
*   @date 27/11/2012
*/

#include <cstdlib>
#include <string>
#include <list>

#include <TickData.h>

#ifndef SYMBOLDATA_H_
#define SYMBOLDATA_H_

using namespace std;

class SymbolData
{
private:
    int dataSize;
    list<TickData> tickData;

public:
    SymbolData();

    SymbolData(string symbolName);

    int loadData(string symbolName);

    void print();
};

#endif /* SYMBOLDATA_H_ */
