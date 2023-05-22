
# цель создания LiteC2C

умбрать ненужный шум ввиде фигурыных скобок и точек с запятой из C


# присваивание

int a = 
	2 + 2 * 2
=>
int a = 2 + 2 * 2;

# ветвления

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
# тернарный оператор

int x = if c then a else b
=>
int x = c?a:b

# цикл while

while a do
	b
=>
while(a){
	b;
}

# цикл do-while

do
	b
while a
=>
do{
	b;
}while(a)

# цикл for

for int i = 0; i < x ; i++ do
	a
=>
for(int i=0;i < x;i++){
	a;
}

# обьявление функций

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

# вызов функции

printf "%i" 1
=>
printf("%i",1)

bar (foo x y) z
=> 
bar(foo(x,y),z)


