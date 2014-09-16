#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <list>
#include <ctime>
#include <cstdlib>
#include <cstring>
 
using namespace std;
 
void csvline_populate(vector<string> &record, const string& line, char delimiter);

typedef struct {
  string name;
  time_t date;
  char type;
  int size;
  float meanPrice;
} trans;

int main(int argc, char *argv[])
{
    vector<string> row;
    string line;
    ifstream in("Acoes.csv");
    if (in.fail())  { cout << "File not found" <<endl; return 0; }
 
    //multimap<string, trans> m;
    list<trans> lst;
 
    int lines = 0;
    
    while(getline(in, line)  && in.good())
    {
        csvline_populate(row, line, ',');
	
	if(lines++ == 0)
	  continue;
	
	trans trn;
	
	//memset(&trn, 0, sizeof(trans));
	trn.name = row[0];
	
	struct tm tm;
	time_t t;
	
	strptime(row[1].c_str(), "%d/%m/%y", &tm);
	t = mktime(&tm);
	
	trn.date = t;
	
	trn.type = row[2].c_str()[0];
	
	trn.size = atoi(row[3].c_str());
	
	trn.meanPrice = atof(row[11].c_str());
	
	//m.insert(pair<string, trans>(row[0], trn));
	lst.push_back(trn);
	
        //cout << endl;
    }
    
    while (!lst.empty())
    {
      std::cout << lst.front().name << endl;
      lst.pop_front();  
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