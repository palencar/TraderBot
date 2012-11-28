/**
*   @file TickData.cpp
*
*   @author Paulo Barros Alencar
*   @date 27/11/2012
*/

#include <string>
#include <sstream>
#include <cstdio>

#include <TickData.h>

using namespace std;

TickData::TickData()
{
    date = 0;
    open = 0.0;
    high = 0.0;
    low = 0.0;
    close = 0.0;
    volume = 0LL;
    adjVolume = 0.0;
    unixTime = 0;
}

TickData::TickData(string line)
{
    stringstream lineStream(line);
    string s_date, s_open, s_high, s_low, s_close, s_volume, s_adjVolume;

    getline(lineStream, s_date, ',');
    getline(lineStream, s_open, ',');
    getline(lineStream, s_high, ',');
    getline(lineStream, s_low, ',');
    getline(lineStream, s_close, ',');
    getline(lineStream, s_volume, ',');
    getline(lineStream, s_adjVolume, ',');

    int year, month, day;
    sscanf(s_date.c_str(), "%4d-%2d-%2d", &year, &month, &day);
    date = year * 10000L + month * 100L + day;
    sscanf(s_open.c_str(), "%f", &open);
    sscanf(s_high.c_str(), "%f", &high);
    sscanf(s_low.c_str(), "%f", &low);
    sscanf(s_close.c_str(), "%f", &close);
    sscanf(s_volume.c_str(), "%lld", &volume);
    sscanf(s_adjVolume.c_str(), "%f", &adjVolume);

    unixTime = 0;
}

/*
void TickData::setOpen(float open)
{

}

void TickData::setHigh(float high)
{

}

void TickData::setLow(float low)
{

}

void TickData::setClose(float close)
{

}

void TickData::setVolume(long long volume)
{

}

void TickData::setAdjVolume(float adjVolume)
{

}
*/

float TickData::avgHLC()
{
    //quando nao existir o close (tick aberto) ??
    return (high + low + close) / 3;
}

int TickData::getDate()
{
    return date;
}
