/**
*   @file TickData.h
*
*   @author Paulo Barros Alencar
*   @date 27/11/2012
*/

#include <ctime>
#include <string>

#ifndef TICKDATA_H_
#define TICKDATA_H_

using namespace std;

class TickData
{
private:
    int date;
    time_t unixTime;
    float open, high, low, close, adjVolume;
    long long volume;

public:
    TickData();

    TickData(string line);  //string date, string open, string high, string low, string close, string volume, string adjVolume);

/*
    void setDate(int year, int mon, int day);

    void setOpen(float open);

    void setHigh(float high);

    void setLow(float low);

    void setClose(float close);

    void setVolume(long long volume);

    void setAdjVolume(float adjVolume);
*/

    float avgHLC();

    int getDate();
};

#endif /* TICKDATA_H_ */
