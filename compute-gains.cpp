#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <list>
#include <ctime>
#include <cstdlib>
#include <cstring>
#include <cstdio>
#include <map>
#include <algorithm>
#include <iomanip>
#include <sstream>

using namespace std;
 
void csvline_populate(vector<string> &record, const string& line, char delimiter);

class Date {
private:
  int year, mon, day;
public:
  Date() {};
  
  Date(string str)
  {
    sscanf(str.c_str(), "%02d/%02d/%02d", &day, &mon, &year);
    year += 2000;
  }
  
  int getDay(){
    return day;
  }
  
  int getMon(){
    return mon;
  }
  
  int getYear(){
    return year;
  }
};

typedef struct {
  string name;
  Date date;
  char type;
  int size;
  float meanPrice;
} trans;

typedef struct {
  string name;
  int size;
  float meanPrice;
} invent;

int main(int argc, char *argv[])
{
    vector<string> row;
    string line;
    ifstream in("Acoes.csv");
    if (in.fail())  { cout << "File not found" <<endl; return 0; }
 
    int lines = 0;
    
    invent inv;
    
    map<string, invent> m;
    
    string mon = string("");
        
    while(getline(in, line)  && in.good())
    {
        csvline_populate(row, line, ',');
	
	if(lines++ == 0)
	  continue;
	
	trans trn;
	trn.name = row[0];
	trn.date = Date(row[1]);
	trn.type = row[2].c_str()[0];
	trn.size = atoi(row[3].c_str());
	replace(row[11].begin(), row[11].end(), ',', '.');
	trn.meanPrice = atof(row[11].c_str());
	
	std::map<string, invent>::iterator it = m.find(trn.name);
	
	std::ostringstream sstream;
	sstream << setw(2) << trn.date.getMon() << "/" << setw(4) << trn.date.getYear();
	
	string tmon = sstream.str();
	
	if(mon.compare(tmon) != 0)
	{
	  mon = tmon;
	  cout << tmon << endl;
	}
	
	if(it == m.end())
	{
	  if(trn.type == 'C')
	  {
	    invent ivt;
	    ivt.name = trn.name;
	    ivt.size = trn.size;
	    ivt.meanPrice = trn.meanPrice;
	    
	    m.insert(pair<string, invent>(ivt.name, ivt));
	  }
	  if(trn.type == 'V')
	  {
	    cout << "Venda de " << trn.name << " sem o ativo no inventario" << endl;
	  }
	}
	else
	{
	  if(trn.type == 'C')
	  {
	    (it->second).size += trn.size;
	    (it->second).meanPrice += trn.meanPrice;
	  }
	  if(trn.type == 'V')
	  {
	    float gain = (trn.meanPrice*(trn.size/(it->second).size)) - ((it->second).meanPrice*(trn.size/(it->second).size));
	    
	    if((it->second).size == trn.size)
	      m.erase(it);
	    else {
	      (it->second).meanPrice *= (trn.size-(it->second).size)/(it->second).size;
	      (it->second).size -= trn.size;
	    }
	    
	    cout << trn.name << " " << trn.size << " " << setiosflags(ios::fixed) << setprecision(2) << gain << endl;
	  }
	}
    }
    
    in.close();
    return 0;
}
 
void csvline_populate(vector<string> &record, const string& line, char delimiter)
{
    int linepos=0;
    int inquotes=false;
    char c;
    int i;
    int linemax=line.length();
    string curstring;
    record.clear();
 
    while(line[linepos]!=0 && linepos < linemax)
    {
 
        c = line[linepos];
 
        if (!inquotes && curstring.length()==0 && c=='"')
        {
            //beginquotechar
            inquotes=true;
        }
        else if (inquotes && c=='"')
        {
            //quotechar
            if ( (linepos+1 <linemax) && (line[linepos+1]=='"') )
            {
                //encountered 2 double quotes in a row (resolves to 1 double quote)
                curstring.push_back(c);
                linepos++;
            }
            else
            {
                //endquotechar
                inquotes=false;
            }
        }
        else if (!inquotes && c==delimiter)
        {
            //end of field
            record.push_back( curstring );
            curstring="";
        }
        else if (!inquotes && (c=='\r' || c=='\n') )
        {
            record.push_back( curstring );
            return;
        }
        else
        {
            curstring.push_back(c);
        }
        linepos++;
    }
    record.push_back( curstring );
    return;
}