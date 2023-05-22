
# ���� �������� LiteC2C

������� �������� ��� ����� ��������� ������ � ����� � ������� �� C


# ������������

int a = 
	2 + 2 * 2
=>
int a = 2 + 2 * 2;

# ���������

if c1 then
	a
else if c2 then
	b
else
	c
=>
if(c1){
	a;
}else if(c2){
	b;
}else{
	c;
}
# ��������� ��������

int x = if c then a else b
=>
int x = c?a:b

# ���� while

while a do
	b
=>
while(a){
	b;
}

# ���� do-while

do
	b
while a
=>
do{
	b;
}while(a)

# ���� for

for int i = 0; i < x ; i++ do
	a
=>
for(int i=0;i < x;i++){
	a;
}

# ���������� �������

int fact x = 
	int a = 1
	for int i = 2; i <= x; i++ do
		a*=i
	return a
=>
int fact(x){
	int a = 1;
	for(int i = 2; i <= x; i++){
		a*=i;
	}
	return a;
}
int main () = 
	return 0
=>
int main(){
	return 0;
}

# ����� �������

printf "%i" 1
=>
printf("%i",1)

bar (foo x y) z
=> 
bar(foo(x,y),z)


