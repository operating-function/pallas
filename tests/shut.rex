% """ Shut Prefix/Infix
, %a
, a+b
, %a+b
, (%a)+b
, a+b+c
, %a+b+c
, (%a)+b+c
, +a+b+c
, +(a+b+c)
, (+a)+b+c
, :red&3
, :red&3&4
, :red-rover

% """ Shut Wrapped
, %(a)
, (a)
, (%a)
, (a+b)
, (%a)+b
, ((%a)+b)
, (a+b+c)
, (%a)+b+c
, (a+b+c)
, ((%a)+b+c)
, ((+a)+b+c)
, ((:red)&3)
, ((:red)&3&4)

% """ Shut Overload
, +(+(+a))
, +(+(+(a)))
, a+(b*b)+(c*c)+d
, ^a+(-b*b)+(-c*c)+d

% """ Shut Pre / Shut Post
, +(a)
, (+a)
, (+a+b+c)
, (~a)+(~b)+(~c)
, (~a + ~b + ~c)
