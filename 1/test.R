a=2
if(a%%2==0) print('他是偶数') else print('他是奇数')
#{}代表命令行是一起输入的

{if(a%%2==0) print('他是偶数') 
else print('他是奇数')}


{if(a%%2==0) print(a^2) 
else print(a-a%/%10*10)}



for(x in 2:100){
	if(x==2) {print(x);next}
	d=TRUE;
	for(y in 2:x){
	if(x%%y==0 & x!=y) {d=FALSE;break}
}
	if(d) print(x)
}

for(i in 2:100)
{  if(i==2) { print(i); next; }
   ok=TRUE; tmp=as.integer(sqrt(i))+1
   for(j in 2:tmp)
       if( i %% j==0) { ok=FALSE;  break; }
    if(ok) print(i)
}

fun1=function(n=1,m=100){
for(i in n:m)
{  if(i==1) next
	if(i==2) { print(i); next; }
   ok=TRUE; tmp=as.integer(sqrt(i))+1
   for(j in 2:tmp)
       if( i %% j==0) { ok=FALSE;  break; }
    if(ok) print(i)
}
}




