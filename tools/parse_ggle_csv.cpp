#include <cstdio>
#include <ctime>
#include <cctype>
#include <cstring>

static char *getDate(time_t epoch)
{
  static char date[40];
  struct tm *tm = localtime(&epoch);
  strftime(date, sizeof(date), "%Y-%m-%d %H:%M:%S", tm);	//todo verificar erro
  return date;
}

int main(int argc, char **argv)
{
  char line[1024+1];
  gets(line);
  unsigned long long lastEpoch = 0, epoch = 0, volume;
  unsigned long interval = 0;
  float close, high, low, open;
  
  if(argc < 2)
  {
    return -3;
  }
  
  while(gets(line) != NULL)
  {
    if(strncmp("INTERVAL", line, 8) == 0)
    {
      if(sscanf(line+9, "%u", &interval) != 1)
      {
	return -1;
      }
    }
    if(isdigit(line[0]))
    {
      if(sscanf(line, "%lld,%f,%f,%f,%f,%llu", &epoch, &close, &high, &low, &open, &volume) != 6)
      {
	 return -1;
      }
      if(lastEpoch == 0 || interval == 0)
      {
	return -2;
      }
      printf("%s,%s,%f,%f,%f,%f,%u\n", argv[1], getDate(time_t(lastEpoch + (epoch*interval))), open, high, low, close, volume);
    }
    else if(line[0] == 'a')
    {
      if(sscanf(line+1, "%lld,%f,%f,%f,%f,%llu", &epoch, &close, &high, &low, &open, &volume) != 6)
      {
	 return -1;
      }
      lastEpoch = epoch;
      printf("%s,%s,%f,%f,%f,%f,%llu\n", argv[1], getDate(time_t(epoch)), open, high, low, close, volume);
    }
    //else ignore
  }
}