
# цель создания LiteC2C

убрать ненужный шум ввиде фигурыных скобок и точек с запятой из C


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

# конструкция switch-case

switch s of
	case c1 do
	case c2 do
		b
	case c3 do
		c
		break
=>
switch(s){
	case c1:
	case c2:
		b;
	case c3:
		c;
		break;
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

# вложынный блок кода

do
	a
	b
	c
=>
{
	a;
	b;
	c;
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

int a int x
int b int x = 
    return a x
int a int x = 
    return x
=>
int a(int x);
int b(int x){
   return a(x);
}
int a(int x){
   return x;
}r

# вызов функции

printf "%i" 1
=>
printf("%i",1)

bar (foo x y) z
=> 
bar(foo(x,y),z)

doSomething()
=>
doSomthing()

# break, continue, return

break
continue
return a 1 2
=>
break;
continue;
return a(1,2);

# goto и метки

label meow
goto meow
=>
moew:
goto meow;

# struct и union

struct p = 
	int x
	int y
=>
struct p{
	int x;
	int y;
};
union q = 
	int i
	float f
=>
union q{
	int i;
	float f;
};

# typedef

typedef length = int
=>
typedef int length;

typedef struct p = 
	int x
	int y
=>
struct p{ 
	int x;
	int y;
};
typedef struct p p;

# указатель на функцию

typedef int2int = int -> int
=>
typedef int(*int2int)(int);

typedef bin = int,int -> int
=>
typedef int(*bin)(int,int)