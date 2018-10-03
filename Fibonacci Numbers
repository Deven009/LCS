#include <bits/stdc++.h>

using namespace std;

long long arr[105];

long long recursion(long long n)
{
  if(n==0)
  {
    return 0;
  }
  if(n==1)
  {
    return 1;
  }
  return recursion(n-1) + recursion(n-2);
}

long long dp(long long n)
{
  if(n==0)
  {
    return 0;
  }
  if(n==1)
  {
    return 1;
  }
  if(arr[n] != -1)
  {
    return arr[n];
  }
  return arr[n] = dp(n-1) + dp(n-2);
}

int main()
{
  /*
  Fibonacci numbers are 
  0,1,1,2,3,5,8,13,21,.....
  
  ith number is sum of (i-1)th and (i-2)th numbers.
  */
  
  long long n;
  n = 10;
  
  long long using_recur = recursion(n);
  
  for(int i=0;i<100;i++)
  {
    arr[i] = -1;
  }
  long long using_dp = dp(n);
  
  cout<<"Recursion: "<<using_recur<<endl;
    
  cout<<"Dynamic_Programming: "<<using_dp<<endl;
    
  return 0;
}
